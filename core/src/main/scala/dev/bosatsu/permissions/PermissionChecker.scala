package dev.bosatsu.permissions

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import Permission.{SimpleP, ScopedP, simple, scoped}

/**
 * Runtime permission checking for Bosatsu applications.
 *
 * This provides three levels of permission checking:
 *
 * 1. Static (build time) - Verify permission annotations are complete
 * 2. Early (request entry) - Check non-scoped permissions upfront
 * 3. Late (after I/O) - Check scoped permissions after data is loaded
 *
 * The checker supports:
 * - Simple permission matching (exact resource:action)
 * - Scoped permission matching (own, team, org, specific IDs)
 * - Permission hierarchy (admin:* implies admin:read)
 * - Template resolution for dynamic scopes
 */
object PermissionChecker {

  /**
   * Result of a permission check.
   */
  sealed trait CheckResult derives CanEqual {
    def isAllowed: Boolean
    def isDenied: Boolean = !isAllowed
  }

  object CheckResult {
    case object Allowed extends CheckResult {
      def isAllowed: Boolean = true
    }

    case class Denied(
        required: Permission,
        reason: String
    ) extends CheckResult {
      def isAllowed: Boolean = false
    }

    case class DeniedMultiple(
        missing: NonEmptyList[Permission]
    ) extends CheckResult {
      def isAllowed: Boolean = false
    }

    case class NeedsLateCheck(
        template: PermissionTemplate,
        reason: String
    ) extends CheckResult {
      def isAllowed: Boolean = false
    }
  }

  /**
   * User context for permission checking.
   *
   * Contains the user's granted permissions and identity info
   * for scope resolution.
   */
  case class UserContext(
      userId: String,
      teamId: Option[String],
      orgId: Option[String],
      permissions: Set[String], // Raw permission strings
      roles: Set[String] = Set.empty
  ) {
    def hasRole(role: String): Boolean = roles.contains(role)

    def parsedPermissions: Set[Permission] =
      permissions.flatMap(Permission.parse)

    def hasScopedPermission(resource: String, action: String, scope: Scope): Boolean = {
      val perm = ScopedPermission(resource, action, scope)
      permissions.contains(perm.format) ||
        permissions.contains(SimplePermission(resource, action).format) ||
        hasWildcardPermission(resource, action)
    }

    def hasWildcardPermission(resource: String, action: String): Boolean = {
      permissions.contains(s"$resource:*") ||
        permissions.contains("*:*") ||
        permissions.contains(s"*:$action")
    }
  }

  /**
   * Resource context for scoped permission checking.
   *
   * Contains the resource's ownership info for matching against
   * the user's scoped permissions.
   */
  case class ResourceContext(
      ownerId: Option[String],
      teamId: Option[String],
      orgId: Option[String],
      attributes: Map[String, String] = Map.empty
  )

  /**
   * Check if a user has a specific permission.
   */
  def check(
      required: Permission,
      user: UserContext
  ): CheckResult = {
    required match {
      case SimpleP(s) =>
        checkSimple(s, user)
      case ScopedP(s) =>
        checkScoped(s, user, None)
    }
  }

  /**
   * Check if a user has all required permissions.
   */
  def checkAll(
      required: List[Permission],
      user: UserContext
  ): CheckResult = {
    val results = required.map(p => (p, check(p, user)))
    val denied = results.collect { case (p, CheckResult.Denied(_, _)) => p }
    val needsLate = results.collect { case (_, r: CheckResult.NeedsLateCheck) => r }

    if (denied.nonEmpty) {
      NonEmptyList.fromList(denied) match {
        case Some(nel) => CheckResult.DeniedMultiple(nel)
        case None => CheckResult.Allowed
      }
    } else if (needsLate.nonEmpty) {
      // Return the first late check needed
      needsLate.head
    } else {
      CheckResult.Allowed
    }
  }

  /**
   * Check a simple (non-scoped) permission.
   */
  def checkSimple(
      perm: SimplePermission,
      user: UserContext
  ): CheckResult = {
    if (user.permissions.contains(perm.format)) {
      CheckResult.Allowed
    } else if (user.hasWildcardPermission(perm.resource, perm.action)) {
      CheckResult.Allowed
    } else {
      CheckResult.Denied(
        SimpleP(perm),
        s"User lacks permission: ${perm.format}"
      )
    }
  }

  /**
   * Check a scoped permission against user and optional resource context.
   */
  def checkScoped(
      perm: ScopedPermission,
      user: UserContext,
      resource: Option[ResourceContext]
  ): CheckResult = {
    // First check if user has the non-scoped version (grants all scopes)
    if (user.permissions.contains(perm.simple.format)) {
      return CheckResult.Allowed
    }

    // Check if user has wildcard
    if (user.hasWildcardPermission(perm.resource, perm.action)) {
      return CheckResult.Allowed
    }

    // Check the specific scoped permission
    perm.scope match {
      case Scope.Own =>
        checkOwnScope(perm, user, resource)

      case Scope.Team =>
        checkTeamScope(perm, user, resource)

      case Scope.Org =>
        checkOrgScope(perm, user, resource)

      case Scope.Any =>
        // 'any' scope requires explicit permission
        if (user.permissions.contains(perm.format)) {
          CheckResult.Allowed
        } else {
          CheckResult.Denied(
            ScopedP(perm),
            s"User lacks permission: ${perm.format}"
          )
        }

      case Scope.Template(placeholder) =>
        // Templates need late checking with resolved context
        CheckResult.NeedsLateCheck(
          PermissionTemplate(perm.resource, perm.action, s"{$placeholder}"),
          s"Permission requires late check: $placeholder must be resolved"
        )

      case Scope.Specific(id) =>
        checkSpecificScope(perm, user, id, resource)
    }
  }

  private def checkOwnScope(
      perm: ScopedPermission,
      user: UserContext,
      resource: Option[ResourceContext]
  ): CheckResult = {
    // Check if user has 'own' scoped permission
    val hasOwnPerm = user.permissions.contains(perm.format) ||
      user.permissions.contains(perm.simple.withScope(Scope.Any).format)

    if (!hasOwnPerm) {
      return CheckResult.Denied(
        ScopedP(perm),
        s"User lacks permission: ${perm.format}"
      )
    }

    // If we have resource context, verify ownership
    resource match {
      case Some(ctx) =>
        ctx.ownerId match {
          case Some(ownerId) if ownerId == user.userId =>
            CheckResult.Allowed
          case Some(_) =>
            CheckResult.Denied(
              ScopedP(perm),
              "Resource is not owned by user"
            )
          case None =>
            CheckResult.NeedsLateCheck(
              PermissionTemplate(perm.resource, perm.action, "{ownerId}"),
              "Resource owner unknown, requires late check"
            )
        }
      case None =>
        // No resource context - need late check
        CheckResult.NeedsLateCheck(
          PermissionTemplate(perm.resource, perm.action, "{ownerId}"),
          "No resource context, requires late check"
        )
    }
  }

  private def checkTeamScope(
      perm: ScopedPermission,
      user: UserContext,
      resource: Option[ResourceContext]
  ): CheckResult = {
    val hasTeamPerm = user.permissions.contains(perm.format) ||
      user.permissions.contains(perm.simple.withScope(Scope.Any).format)

    if (!hasTeamPerm) {
      return CheckResult.Denied(
        ScopedP(perm),
        s"User lacks permission: ${perm.format}"
      )
    }

    (user.teamId, resource.flatMap(_.teamId)) match {
      case (Some(userTeam), Some(resourceTeam)) if userTeam == resourceTeam =>
        CheckResult.Allowed
      case (Some(_), Some(_)) =>
        CheckResult.Denied(
          ScopedP(perm),
          "Resource is not in user's team"
        )
      case _ =>
        CheckResult.NeedsLateCheck(
          PermissionTemplate(perm.resource, perm.action, "{teamId}"),
          "Team context unknown, requires late check"
        )
    }
  }

  private def checkOrgScope(
      perm: ScopedPermission,
      user: UserContext,
      resource: Option[ResourceContext]
  ): CheckResult = {
    val hasOrgPerm = user.permissions.contains(perm.format) ||
      user.permissions.contains(perm.simple.withScope(Scope.Any).format)

    if (!hasOrgPerm) {
      return CheckResult.Denied(
        ScopedP(perm),
        s"User lacks permission: ${perm.format}"
      )
    }

    (user.orgId, resource.flatMap(_.orgId)) match {
      case (Some(userOrg), Some(resourceOrg)) if userOrg == resourceOrg =>
        CheckResult.Allowed
      case (Some(_), Some(_)) =>
        CheckResult.Denied(
          ScopedP(perm),
          "Resource is not in user's organization"
        )
      case _ =>
        CheckResult.NeedsLateCheck(
          PermissionTemplate(perm.resource, perm.action, "{orgId}"),
          "Organization context unknown, requires late check"
        )
    }
  }

  private def checkSpecificScope(
      perm: ScopedPermission,
      user: UserContext,
      specificId: String,
      resource: Option[ResourceContext]
  ): CheckResult = {
    // Check if user has permission with the specific ID
    if (user.permissions.contains(perm.format)) {
      CheckResult.Allowed
    } else {
      CheckResult.Denied(
        ScopedP(perm),
        s"User lacks permission for specific scope: $specificId"
      )
    }
  }

  /**
   * Resolve a permission template with context and check it.
   */
  def checkWithTemplate(
      template: PermissionTemplate,
      user: UserContext,
      context: Map[String, String]
  ): CheckResult = {
    template.resolve(context) match {
      case Some(resolved) =>
        checkScoped(resolved, user, None)
      case None =>
        val missing = template.placeholders.filterNot(context.contains)
        CheckResult.Denied(
          simple(template.resource, template.action),
          s"Cannot resolve template: missing ${missing.mkString(", ")}"
        )
    }
  }

  /**
   * Batch check multiple permissions efficiently.
   *
   * Groups permissions by type for faster checking:
   * 1. Check wildcards first (if user has *, skip all checks)
   * 2. Check simple permissions as a set
   * 3. Check scoped permissions individually
   */
  def batchCheck(
      required: List[Permission],
      user: UserContext
  ): ValidatedNel[Permission, Unit] = {
    // Short-circuit if user has wildcard
    if (user.permissions.contains("*:*")) {
      return ().validNel
    }

    val results = required.map { perm =>
      check(perm, user) match {
        case CheckResult.Allowed => ().validNel
        case CheckResult.Denied(p, _) => p.invalidNel
        case CheckResult.DeniedMultiple(ps) =>
          ps.toList.traverse_(_.invalidNel)
        case CheckResult.NeedsLateCheck(t, _) =>
          simple(t.resource, t.action).invalidNel
      }
    }

    results.sequence_
  }

  /**
   * Permission hierarchy helpers.
   *
   * These define common permission hierarchies where broader
   * permissions imply narrower ones.
   */
  object Hierarchy {
    /**
     * Check if a permission implies another.
     *
     * Examples:
     * - users:* implies users:read
     * - admin:* implies admin:anything
     * - users:read:any implies users:read:own
     */
    def implies(broader: Permission, narrower: Permission): Boolean = {
      if (broader == narrower) return true

      (broader, narrower) match {
        case (SimpleP(b), SimpleP(n)) =>
          b.resource == n.resource && b.action == "*"

        case (SimpleP(b), ScopedP(n)) =>
          b.resource == n.resource && (b.action == n.action || b.action == "*")

        case (ScopedP(b), ScopedP(n)) =>
          b.resource == n.resource &&
            (b.action == n.action || b.action == "*") &&
            scopeImplies(b.scope, n.scope)

        case _ => false
      }
    }

    private def scopeImplies(broader: Scope, narrower: Scope): Boolean = {
      (broader, narrower) match {
        case (Scope.Any, _) => true
        case (Scope.Org, Scope.Team) => true
        case (Scope.Org, Scope.Own) => true
        case (Scope.Team, Scope.Own) => true
        case (a, b) => a == b
      }
    }

    /**
     * Expand a permission to all permissions it implies.
     */
    def expand(perm: Permission): List[Permission] = {
      perm match {
        case SimpleP(SimplePermission(res, "*")) =>
          // Wildcard action implies common actions
          List("read", "create", "update", "delete", "list").map { act =>
            simple(res, act)
          } :+ perm

        case ScopedP(ScopedPermission(res, act, Scope.Any)) =>
          // 'any' scope implies all other scopes
          List(Scope.Org, Scope.Team, Scope.Own).map { s =>
            scoped(res, act, s)
          } :+ perm

        case _ => List(perm)
      }
    }
  }
}
