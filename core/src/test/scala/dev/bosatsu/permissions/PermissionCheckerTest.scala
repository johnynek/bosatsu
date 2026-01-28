package dev.bosatsu.permissions

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import cats.data.NonEmptyList
import Permission.{SimpleP, ScopedP, simple, scoped}
import PermissionChecker.{CheckResult, UserContext, ResourceContext, Hierarchy, checkScoped, checkAll, checkWithTemplate, batchCheck}

// Alias to avoid conflict with ScalaCheck's check
private def permCheck = PermissionChecker.check

object PermissionCheckerTest extends Properties("PermissionChecker") {
  import PermissionGen._

  // Helper to create user with specific permissions
  def userWith(perms: String*): UserContext =
    UserContext("user-1", Some("team-1"), Some("org-1"), perms.toSet)

  // Helper to create resource with specific ownership
  def resourceOwnedBy(ownerId: String): ResourceContext =
    ResourceContext(Some(ownerId), Some("team-1"), Some("org-1"))

  property("check allows when user has exact permission") = forAll(simplePermission) { perm =>
    val user = userWith(perm.format)
    permCheck(SimpleP(perm), user) == CheckResult.Allowed
  }

  property("check denies when user lacks permission") = forAll(simplePermission) { perm =>
    val user = userWith() // No permissions
    permCheck(SimpleP(perm), user) match {
      case CheckResult.Denied(_, _) => true
      case _ => false
    }
  }

  property("wildcard permission allows all actions") = forAll(simplePermission) { perm =>
    val user = userWith(s"${perm.resource}:*")
    permCheck(SimpleP(perm), user) == CheckResult.Allowed
  }

  property("global wildcard allows everything") = forAll(permission) { perm =>
    val user = userWith("*:*")
    permCheck(perm, user) == CheckResult.Allowed
  }

  property("scoped permission requires scope check") = {
    val perm = ScopedPermission("users", "read", Scope.Own)
    val user = userWith("users:read:own")

    // Without resource context, needs late check
    permCheck(ScopedP(perm), user) match {
      case CheckResult.NeedsLateCheck(_, _) => true
      case _ => false
    }
  }

  property("own scope allowed when user owns resource") = {
    val perm = ScopedPermission("users", "read", Scope.Own)
    val user = userWith("users:read:own")
    val resource = resourceOwnedBy("user-1") // Same as user

    checkScoped(perm, user, Some(resource)) == CheckResult.Allowed
  }

  property("own scope denied when user doesn't own resource") = {
    val perm = ScopedPermission("users", "read", Scope.Own)
    val user = userWith("users:read:own")
    val resource = resourceOwnedBy("other-user") // Different owner

    checkScoped(perm, user, Some(resource)) match {
      case CheckResult.Denied(_, _) => true
      case _ => false
    }
  }

  property("simple permission implies scoped permission") = {
    val perm = ScopedPermission("users", "read", Scope.Own)
    val user = userWith("users:read") // Non-scoped permission

    // Simple permission grants access to any scope
    checkScoped(perm, user, Some(resourceOwnedBy("anyone"))) == CheckResult.Allowed
  }

  property("any scope requires explicit any permission") = {
    val perm = ScopedPermission("users", "read", Scope.Any)
    val user = userWith("users:read:any")

    permCheck(ScopedP(perm), user) == CheckResult.Allowed
  }

  property("team scope allowed when same team") = {
    val perm = ScopedPermission("users", "read", Scope.Team)
    val user = userWith("users:read:team")
    val resource = ResourceContext(Some("other-user"), Some("team-1"), Some("org-1"))

    checkScoped(perm, user, Some(resource)) == CheckResult.Allowed
  }

  property("team scope denied when different team") = {
    val perm = ScopedPermission("users", "read", Scope.Team)
    val user = userWith("users:read:team")
    val resource = ResourceContext(Some("other-user"), Some("team-2"), Some("org-1"))

    checkScoped(perm, user, Some(resource)) match {
      case CheckResult.Denied(_, _) => true
      case _ => false
    }
  }

  property("org scope allowed when same org") = {
    val perm = ScopedPermission("users", "read", Scope.Org)
    val user = userWith("users:read:org")
    val resource = ResourceContext(Some("other-user"), Some("team-2"), Some("org-1"))

    checkScoped(perm, user, Some(resource)) == CheckResult.Allowed
  }

  property("org scope denied when different org") = {
    val perm = ScopedPermission("users", "read", Scope.Org)
    val user = userWith("users:read:org")
    val resource = ResourceContext(Some("other-user"), Some("team-2"), Some("org-2"))

    checkScoped(perm, user, Some(resource)) match {
      case CheckResult.Denied(_, _) => true
      case _ => false
    }
  }

  property("template scope needs late check") = {
    val perm = ScopedPermission("doc", "read", Scope.Template("doc.ownerId"))
    val user = userWith("doc:read:{doc.ownerId}")

    checkScoped(perm, user, None) match {
      case CheckResult.NeedsLateCheck(_, _) => true
      case _ => false
    }
  }

  property("checkAll allows when all permissions present") = {
    val perms = List(
      simple("users", "read"),
      simple("posts", "read")
    )
    val user = userWith("users:read", "posts:read")

    checkAll(perms, user) == CheckResult.Allowed
  }

  property("checkAll denies when any permission missing") = {
    val perms = List(
      simple("users", "read"),
      simple("posts", "read")
    )
    val user = userWith("users:read") // Missing posts:read

    checkAll(perms, user) match {
      case CheckResult.DeniedMultiple(missing) =>
        missing.toList.exists(_.format == "posts:read")
      case _ => false
    }
  }

  property("template resolution works with matching context") = {
    val template = PermissionTemplate("doc", "read", "{ownerId}")
    val user = userWith("doc:read:user-1")
    val context = Map("ownerId" -> "user-1")

    checkWithTemplate(template, user, context) == CheckResult.Allowed
  }

  property("template resolution fails with missing context") = {
    val template = PermissionTemplate("doc", "read", "{ownerId}")
    val user = userWith("doc:read:user-1")
    val context = Map.empty[String, String] // Missing ownerId

    checkWithTemplate(template, user, context) match {
      case CheckResult.Denied(_, reason) => reason.contains("missing")
      case _ => false
    }
  }

  property("batchCheck is efficient with wildcard") = forAll(Gen.listOfN(10, permission)) { perms =>
    val user = userWith("*:*")
    batchCheck(perms, user).isValid
  }

  property("batchCheck accumulates all missing permissions") = {
    val perms = List(
      simple("users", "read"),
      simple("posts", "read"),
      simple("comments", "read")
    )
    val user = userWith() // No permissions

    batchCheck(perms, user).isInvalid
  }

  // Hierarchy tests
  property("hierarchy: wildcard action implies specific action") = {
    val broader = simple("users", "*")
    val narrower = simple("users", "read")

    Hierarchy.implies(broader, narrower)
  }

  property("hierarchy: any scope implies own scope") = {
    val broader = scoped("users", "read", Scope.Any)
    val narrower = scoped("users", "read", Scope.Own)

    Hierarchy.implies(broader, narrower)
  }

  property("hierarchy: org scope implies team scope") = {
    val broader = scoped("users", "read", Scope.Org)
    val narrower = scoped("users", "read", Scope.Team)

    Hierarchy.implies(broader, narrower)
  }

  property("hierarchy: team scope implies own scope") = {
    val broader = scoped("users", "read", Scope.Team)
    val narrower = scoped("users", "read", Scope.Own)

    Hierarchy.implies(broader, narrower)
  }

  property("hierarchy: same permission implies itself") = forAll(permission) { perm =>
    Hierarchy.implies(perm, perm)
  }

  property("hierarchy: expand wildcard action") = {
    val perm = simple("users", "*")
    val expanded = Hierarchy.expand(perm)

    expanded.exists(_.format == "users:read") &&
      expanded.exists(_.format == "users:create") &&
      expanded.exists(_.format == "users:update") &&
      expanded.exists(_.format == "users:delete")
  }

  property("hierarchy: expand any scope") = {
    val perm = scoped("users", "read", Scope.Any)
    val expanded = Hierarchy.expand(perm)

    expanded.exists(_.format == "users:read:own") &&
      expanded.exists(_.format == "users:read:team") &&
      expanded.exists(_.format == "users:read:org")
  }
}
