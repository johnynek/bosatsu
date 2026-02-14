package dev.bosatsu

import dev.bosatsu.NonNullFold.*
import dev.bosatsu.rankn.Type
import org.typelevel.paiges.Doc

/** Shared rendering rules for user-facing type names.
  *
  * Rules:
  * - Drop the current package prefix.
  * - Drop `Bosatsu/Predef::` when there is no local shadowing type.
  * - Keep other package prefixes.
  */
object TypeRenderer {
  final case class Context(
      packageName: PackageName,
      localTypeNames: Set[TypeName]
  )

  private def nonNull(s: String | Null): String =
    s.foldNN("")(x => x)

  def render(typeValue: Type, ctx: Context, width: Int): String =
    abbreviate(
      Type.fullyResolvedDocument.document(typeValue).render(width),
      ctx
    )

  def document(typeValue: Type, ctx: Context, width: Int): Doc =
    Doc.text(render(typeValue, ctx, width))

  def documents(
      typeValues: List[Type],
      ctx: Context,
      width: Int
  ): Map[Type, Doc] =
    typeValues.iterator
      .map(t => (t, document(t, ctx, width)))
      .toMap

  def typeNamePrefix(
      typePackage: PackageName,
      typeName: TypeName,
      ctx: Context
  ): String =
    if (dropPackagePrefix(typePackage, typeName, ctx)) ""
    else typePackage.asString + "::"

  private def dropPackagePrefix(
      typePackage: PackageName,
      typeName: TypeName,
      ctx: Context
  ): Boolean =
    (typePackage == ctx.packageName) ||
      ((typePackage == PackageName.PredefName) && !ctx.localTypeNames(
        typeName
      ))

  private def abbreviate(raw: String, ctx: Context): String = {
    val localPrefix = java.util.regex.Pattern.quote(ctx.packageName.asString + "::")
    val localRe = (localPrefix + "([A-Z][A-Za-z0-9_]*)").r
    val localShort = localRe.replaceAllIn(raw, m => nonNull(m.group(1)))

    val predefRe = "Bosatsu/Predef::([A-Z][A-Za-z0-9_]*)".r
    predefRe.replaceAllIn(localShort, { m =>
      val typeName = TypeName(nonNull(m.group(1)))
      if (ctx.localTypeNames(typeName)) nonNull(m.matched)
      else typeName.asString
    })
  }
}
