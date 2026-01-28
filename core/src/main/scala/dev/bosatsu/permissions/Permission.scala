package dev.bosatsu.permissions

import cats.{Eq, Order, Show}
import cats.data.NonEmptyList
import cats.implicits._

/**
 * Permission system for Bosatsu applications.
 *
 * This provides a shared permission layer for Service, UI, and Explorer,
 * ported from BurritoScript's permission system.
 *
 * Permission format: 'resource:action' or 'resource:action:scope'
 *
 * Examples:
 *   - 'users:read'       - read any user
 *   - 'users:read:own'   - read own user only
 *   - 'budget:update:team' - update team budget
 *
 * Scopes:
 *   - 'own'  - only resources owned by the current user
 *   - 'any'  - all resources (if permitted)
 *   - 'team' - resources within the user's team
 *   - 'org'  - resources within the user's organization
 *   - Specific ID templates like '{doc.ownerId}'
 */

/**
 * A permission scope restricts what resources a permission applies to.
 */
sealed abstract class Scope(val name: String) {
  override def toString: String = name
}

object Scope {
  case object Own extends Scope("own")
  case object Any extends Scope("any")
  case object Team extends Scope("team")
  case object Org extends Scope("org")
  case class Template(placeholder: String) extends Scope(s"{$placeholder}")
  case class Specific(id: String) extends Scope(id)

  def parse(s: String): Scope = s.toLowerCase match {
    case "own" => Own
    case "any" => Any
    case "team" => Team
    case "org" => Org
    case t if t.startsWith("{") && t.endsWith("}") =>
      Template(t.substring(1, t.length - 1))
    case other => Specific(other)
  }

  implicit val scopeOrder: Order[Scope] = Order.by(_.name)
  implicit val scopeShow: Show[Scope] = Show.show(_.name)
  implicit val scopeEq: Eq[Scope] = Eq.fromUniversalEquals
}

/**
 * A simple permission without scope.
 *
 * Format: 'resource:action'
 */
case class SimplePermission(resource: String, action: String) {
  def format: String = s"$resource:$action"

  def withScope(scope: Scope): ScopedPermission = ScopedPermission(resource, action, scope)

  override def toString: String = format
}

object SimplePermission {
  def parse(s: String): Option[SimplePermission] = {
    val parts = s.split(':').toList
    parts match {
      case resource :: action :: Nil => Some(SimplePermission(resource, action))
      case _ => None
    }
  }

  implicit val simpleOrder: Order[SimplePermission] = Order.by(p => (p.resource, p.action))
  implicit val simpleShow: Show[SimplePermission] = Show.show(_.format)
  implicit val simpleEq: Eq[SimplePermission] = Eq.fromUniversalEquals
}

/**
 * A scoped permission with a specific access scope.
 *
 * Format: 'resource:action:scope'
 */
case class ScopedPermission(resource: String, action: String, scope: Scope) {
  def format: String = s"$resource:$action:${scope.name}"

  def simple: SimplePermission = SimplePermission(resource, action)

  override def toString: String = format
}

object ScopedPermission {
  def parse(s: String): Option[ScopedPermission] = {
    val parts = s.split(':').toList
    parts match {
      case resource :: action :: scopeStr :: Nil =>
        Some(ScopedPermission(resource, action, Scope.parse(scopeStr)))
      case _ => None
    }
  }

  implicit val scopedOrder: Order[ScopedPermission] = Order.by(p => (p.resource, p.action, p.scope))
  implicit val scopedShow: Show[ScopedPermission] = Show.show(_.format)
  implicit val scopedEq: Eq[ScopedPermission] = Eq.fromUniversalEquals
}

/**
 * A permission can be either simple or scoped.
 */
sealed trait Permission {
  def resource: String
  def action: String
  def format: String
  def isScoped: Boolean
  def toSimple: SimplePermission
}

object Permission {
  case class SimpleP(perm: SimplePermission) extends Permission {
    def resource: String = perm.resource
    def action: String = perm.action
    def format: String = perm.format
    def isScoped: Boolean = false
    def toSimple: SimplePermission = perm
  }

  case class ScopedP(perm: ScopedPermission) extends Permission {
    def resource: String = perm.resource
    def action: String = perm.action
    def format: String = perm.format
    def isScoped: Boolean = true
    def toSimple: SimplePermission = perm.simple
  }

  def parse(s: String): Option[Permission] = {
    val parts = s.split(':').toList
    parts match {
      case resource :: action :: Nil =>
        Some(SimpleP(SimplePermission(resource, action)))
      case resource :: action :: scopeStr :: Nil =>
        Some(ScopedP(ScopedPermission(resource, action, Scope.parse(scopeStr))))
      case _ => None
    }
  }

  def simple(resource: String, action: String): Permission =
    SimpleP(SimplePermission(resource, action))

  def scoped(resource: String, action: String, scope: Scope): Permission =
    ScopedP(ScopedPermission(resource, action, scope))

  implicit val permissionOrder: Order[Permission] = Order.by(_.format)
  implicit val permissionShow: Show[Permission] = Show.show(_.format)
  implicit val permissionEq: Eq[Permission] = Eq.fromUniversalEquals
}

/**
 * A permission template with placeholders that are resolved at runtime.
 *
 * Example: 'doc:read:{doc.ownerId}'
 *
 * The placeholder '{doc.ownerId}' is resolved when the actual document
 * is loaded, allowing dynamic permission checks.
 */
case class PermissionTemplate(
    resource: String,
    action: String,
    scopeTemplate: String
) {
  def format: String = s"$resource:$action:$scopeTemplate"

  def resolve(context: Map[String, String]): Option[ScopedPermission] = {
    val pattern = """\{([^}]+)\}""".r
    val phs = placeholders
    // Check all placeholders are present first
    if (phs.exists(!context.contains(_))) {
      None
    } else {
      val resolvedScope = pattern.replaceAllIn(scopeTemplate, m => {
        val key = m.group(1)
        context(key)
      })
      Some(ScopedPermission(resource, action, Scope.parse(resolvedScope)))
    }
  }

  def placeholders: List[String] = {
    val pattern = """\{([^}]+)\}""".r
    pattern.findAllMatchIn(scopeTemplate).map(_.group(1)).toList
  }

  override def toString: String = format
}

object PermissionTemplate {
  def parse(s: String): Option[PermissionTemplate] = {
    val parts = s.split(':').toList
    parts match {
      case resource :: action :: scopeTemplate :: Nil if scopeTemplate.contains("{") =>
        Some(PermissionTemplate(resource, action, scopeTemplate))
      case _ => None
    }
  }

  implicit val templateOrder: Order[PermissionTemplate] = Order.by(_.format)
  implicit val templateShow: Show[PermissionTemplate] = Show.show(_.format)
  implicit val templateEq: Eq[PermissionTemplate] = Eq.fromUniversalEquals
}

/**
 * A permission requirement describes what permission is needed
 * and whether it's known statically or depends on runtime values.
 */
sealed trait PermissionRequirement {
  def toPermissions: List[Permission]
}

object PermissionRequirement {
  /**
   * A static requirement known at compile time.
   */
  case class Static(permissions: NonEmptyList[Permission]) extends PermissionRequirement {
    def toPermissions: List[Permission] = permissions.toList
  }

  /**
   * A dynamic requirement that depends on runtime values.
   * The template will be resolved when the actual values are known.
   */
  case class Dynamic(template: PermissionTemplate) extends PermissionRequirement {
    def toPermissions: List[Permission] = Nil
  }

  /**
   * A conditional requirement that depends on a runtime condition.
   */
  case class Conditional(
      condition: String, // Description of the condition
      ifTrue: PermissionRequirement,
      ifFalse: PermissionRequirement
  ) extends PermissionRequirement {
    def toPermissions: List[Permission] =
      ifTrue.toPermissions ++ ifFalse.toPermissions
  }

  def single(perm: Permission): PermissionRequirement =
    Static(NonEmptyList.one(perm))

  def multiple(perms: NonEmptyList[Permission]): PermissionRequirement =
    Static(perms)
}

/**
 * Permission configuration for a function or handler.
 */
case class PermissionConfig(
    requires: List[Permission],
    denies: List[Permission] = Nil,
    scopes: Map[String, Scope] = Map.empty
) {
  def isEmpty: Boolean = requires.isEmpty && denies.isEmpty

  def merge(other: PermissionConfig): PermissionConfig =
    PermissionConfig(
      requires = (requires ++ other.requires).distinct,
      denies = (denies ++ other.denies).distinct,
      scopes = scopes ++ other.scopes
    )
}

object PermissionConfig {
  val empty: PermissionConfig = PermissionConfig(Nil)

  def require(perms: Permission*): PermissionConfig =
    PermissionConfig(perms.toList)

  def deny(perms: Permission*): PermissionConfig =
    PermissionConfig(Nil, perms.toList)
}

/**
 * A permission manifest summarizing all permissions for a module or handler.
 */
case class PermissionManifest(
    name: String,
    staticPermissions: List[Permission],
    dynamicPermissions: List[PermissionTemplate],
    conditionalPermissions: List[(String, List[Permission])]
) {
  def allStaticPermissions: List[Permission] = staticPermissions

  def hasDynamicPermissions: Boolean = dynamicPermissions.nonEmpty

  def hasConditionalPermissions: Boolean = conditionalPermissions.nonEmpty
}

object PermissionManifest {
  def empty(name: String): PermissionManifest =
    PermissionManifest(name, Nil, Nil, Nil)

  def static(name: String, perms: List[Permission]): PermissionManifest =
    PermissionManifest(name, perms, Nil, Nil)
}
