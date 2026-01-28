package dev.bosatsu.permissions

import org.scalacheck.{Arbitrary, Gen}
import Permission.{SimpleP, ScopedP}

/**
 * ScalaCheck generators for permission types.
 */
object PermissionGen {

  // Common resources and actions for realistic tests
  val resources: Gen[String] = Gen.oneOf(
    "users", "accounts", "transactions", "posts", "comments",
    "orders", "products", "categories", "teams", "organizations"
  )

  val actions: Gen[String] = Gen.oneOf(
    "read", "create", "update", "delete", "list", "admin", "manage"
  )

  // Alphanumeric identifiers
  val identifier: Gen[String] = Gen.alphaLowerStr.suchThat(_.nonEmpty)

  // UUID-like strings
  val uuidString: Gen[String] = for {
    a <- Gen.listOfN(8, Gen.hexChar).map(_.mkString)
    b <- Gen.listOfN(4, Gen.hexChar).map(_.mkString)
    c <- Gen.listOfN(4, Gen.hexChar).map(_.mkString)
    d <- Gen.listOfN(4, Gen.hexChar).map(_.mkString)
    e <- Gen.listOfN(12, Gen.hexChar).map(_.mkString)
  } yield s"$a-$b-$c-$d-$e"

  // Scope generators
  val scopeOwn: Gen[Scope] = Gen.const(Scope.Own)
  val scopeAny: Gen[Scope] = Gen.const(Scope.Any)
  val scopeTeam: Gen[Scope] = Gen.const(Scope.Team)
  val scopeOrg: Gen[Scope] = Gen.const(Scope.Org)

  val scopeTemplate: Gen[Scope] = for {
    placeholder <- Gen.oneOf("doc.ownerId", "resource.teamId", "user.orgId", "item.createdBy")
  } yield Scope.Template(placeholder)

  val scopeSpecific: Gen[Scope] = uuidString.map(Scope.Specific)

  val scope: Gen[Scope] = Gen.frequency(
    (3, scopeOwn),
    (2, scopeAny),
    (2, scopeTeam),
    (1, scopeOrg),
    (1, scopeTemplate),
    (1, scopeSpecific)
  )

  // Simple permission generator
  val simplePermission: Gen[SimplePermission] = for {
    resource <- resources
    action <- actions
  } yield SimplePermission(resource, action)

  // Scoped permission generator
  val scopedPermission: Gen[ScopedPermission] = for {
    resource <- resources
    action <- actions
    s <- scope
  } yield ScopedPermission(resource, action, s)

  // Permission (either simple or scoped)
  val permission: Gen[Permission] = Gen.frequency(
    (1, simplePermission.map(SimpleP)),
    (2, scopedPermission.map(ScopedP))
  )

  // Permission template generator
  val permissionTemplate: Gen[PermissionTemplate] = for {
    resource <- resources
    action <- actions
    placeholder <- Gen.oneOf("ownerId", "teamId", "orgId", "createdBy")
  } yield PermissionTemplate(resource, action, s"{$placeholder}")

  // Permission config generator
  val permissionConfig: Gen[PermissionConfig] = for {
    requiresCount <- Gen.choose(0, 3)
    requires <- Gen.listOfN(requiresCount, permission)
    deniesCount <- Gen.choose(0, 1)
    denies <- Gen.listOfN(deniesCount, permission)
    scopeCount <- Gen.choose(0, 2)
    scopeKeys <- Gen.listOfN(scopeCount, identifier)
    scopeValues <- Gen.listOfN(scopeCount, Gen.oneOf(Scope.Own, Scope.Any, Scope.Team, Scope.Org))
    scopes = scopeKeys.zip(scopeValues).toMap
  } yield PermissionConfig(requires, denies, scopes)

  // User context generator
  val userContext: Gen[PermissionChecker.UserContext] = for {
    userId <- uuidString
    teamId <- Gen.option(uuidString)
    orgId <- Gen.option(uuidString)
    permCount <- Gen.choose(1, 10)
    perms <- Gen.listOfN(permCount, permission.map(_.format))
    roleCount <- Gen.choose(0, 3)
    roles <- Gen.listOfN(roleCount, Gen.oneOf("admin", "user", "guest", "moderator"))
  } yield PermissionChecker.UserContext(userId, teamId, orgId, perms.toSet, roles.toSet)

  // Resource context generator
  val resourceContext: Gen[PermissionChecker.ResourceContext] = for {
    ownerId <- Gen.option(uuidString)
    teamId <- Gen.option(uuidString)
    orgId <- Gen.option(uuidString)
    attrCount <- Gen.choose(0, 3)
    attrKeys <- Gen.listOfN(attrCount, identifier)
    attrValues <- Gen.listOfN(attrCount, identifier)
    attrs = attrKeys.zip(attrValues).toMap
  } yield PermissionChecker.ResourceContext(ownerId, teamId, orgId, attrs)

  // Permission manifest generator
  val permissionManifest: Gen[PermissionManifest] = for {
    name <- identifier
    staticCount <- Gen.choose(0, 5)
    statics <- Gen.listOfN(staticCount, permission)
    dynCount <- Gen.choose(0, 2)
    dynamics <- Gen.listOfN(dynCount, permissionTemplate)
    condCount <- Gen.choose(0, 2)
    condNames <- Gen.listOfN(condCount, identifier)
    condPerms <- Gen.listOfN(condCount, Gen.listOfN(Gen.choose(1, 2).sample.getOrElse(1), permission))
    conditionals = condNames.zip(condPerms)
  } yield PermissionManifest(name, statics, dynamics, conditionals)

  // Arbitraries for property-based testing
  implicit val arbScope: Arbitrary[Scope] = Arbitrary(scope)
  implicit val arbSimple: Arbitrary[SimplePermission] = Arbitrary(simplePermission)
  implicit val arbScoped: Arbitrary[ScopedPermission] = Arbitrary(scopedPermission)
  implicit val arbPermission: Arbitrary[Permission] = Arbitrary(permission)
  implicit val arbTemplate: Arbitrary[PermissionTemplate] = Arbitrary(permissionTemplate)
  implicit val arbConfig: Arbitrary[PermissionConfig] = Arbitrary(permissionConfig)
  implicit val arbUserContext: Arbitrary[PermissionChecker.UserContext] = Arbitrary(userContext)
  implicit val arbResourceContext: Arbitrary[PermissionChecker.ResourceContext] = Arbitrary(resourceContext)
  implicit val arbManifest: Arbitrary[PermissionManifest] = Arbitrary(permissionManifest)
}
