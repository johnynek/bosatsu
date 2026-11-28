package dev.bosatsu

import scala.quoted.{Expr, Quotes}

/** Checks, when the compiler is compiled, the Predef names it spells out
  * against the Predef source.
  *
  * Each check takes string literals and expands to the name itself, so it
  * costs nothing at runtime. A name the file doesn't define is a compile error
  * that says which name and which list it was checked against, instead of a
  * failure when some program reaches it.
  *
  * The file is read while core compiles, so build.sbt lists it in
  * `coreMacroInputPaths`: that way, editing it recompiles core. Core embeds
  * the Predef source anyway, so that recompile is needed regardless. That is
  * why these check only Predef: the other reserved packages and the C runtime
  * headers are checked by a test instead (`ReservedNamesTest`), so that
  * editing them doesn't recompile core.
  */
object CheckedNames {

  /** The Predef source, relative to the repository root */
  inline val predefSource = "core/src/main/resources/bosatsu/predef.bosatsu"

  /** `name`, which Predef's export list names as a value */
  inline def exportedValue(inline name: String): String =
    ${ exportedValueImpl('name) }

  /** `name`, which Predef's export list names as a type */
  inline def exportedType(inline name: String): String =
    ${ exportedTypeImpl('name) }

  /** `name`, a constructor of a type that Predef exports with its
    * constructors
    */
  inline def constructor(inline name: String): String =
    ${ constructorImpl('name) }

  /** `name`, which Predef declares `external` */
  inline def external(inline name: String): String =
    ${ externalImpl('name) }

  def exportedValueImpl(name: Expr[String])(using Quotes): Expr[String] =
    checkPredefName(name, "exported values")(_.exportedValues)

  def exportedTypeImpl(name: Expr[String])(using Quotes): Expr[String] =
    checkPredefName(name, "exported types")(_.exportedTypes)

  def constructorImpl(name: Expr[String])(using Quotes): Expr[String] =
    checkPredefName(name, "constructors of the types exported with ()")(
      _.constructors
    )

  def externalImpl(name: Expr[String])(using Quotes): Expr[String] =
    checkPredefName(name, "external declarations")(_.externals)

  private def literal(expr: Expr[String], what: String)(using
      quotes: Quotes
  ): String =
    expr.value.getOrElse(
      quotes.reflect.report.errorAndAbort(
        s"expected a string literal for the $what, found: ${expr.show}",
        expr
      )
    )

  private def checkPredefName(
      nameExpr: Expr[String],
      listName: String
  )(select: BosatsuSourceNames => List[String])(using
      quotes: Quotes
  ): Expr[String] = {
    import quotes.reflect.report

    val path = predefSource
    val name = literal(nameExpr, "name")
    val names =
      BosatsuSourceNames.scan(path, Macro.readRepoFile(path)) match {
        case Right(names) => names
        // $COVERAGE-OFF$ these error paths run only while the compiler
        // compiles, and only if the Predef source's layout changes
        case Left(err) => report.errorAndAbort(err, nameExpr)
        // $COVERAGE-ON$
      }
    val known = select(names)
    if (known.contains(name)) Expr(name)
    else if (known.isEmpty) {
      // $COVERAGE-OFF$
      // a scan that finds nothing must not quietly reject every name
      report.errorAndAbort(
        s"found no $listName in $path, so `$name` can't be one of them",
        nameExpr
      )
      // $COVERAGE-ON$
    } else
      report.errorAndAbort(
        notFound(name, s"the $listName of ${names.packageName} in $path", known),
        nameExpr
      )
  }

  /** The message for a `name` that isn't one of `known`, with the nearest
    * known names as suggestions
    */
  private[bosatsu] def notFound(
      name: String,
      listDescription: String,
      known: Iterable[String]
  ): String = {
    val hint = closest(name, known) match {
      case Nil        => ""
      case one :: Nil => s" Did you mean `$one`?"
      case several    =>
        several.mkString(" Did you mean one of `", "`, `", "`?")
    }
    s"`$name` is not one of $listDescription.$hint"
  }

  // The known names nearest `name`, if any are near: those at the least edit
  // distance, at most a third of the length of `name`.
  private def closest(name: String, known: Iterable[String]): List[String] = {
    val limit = (name.length / 3).max(1).min(3)
    val near = known.iterator
      .map(k => (editDistance(name, k), k))
      .filter(_._1 <= limit)
      .toList
    if (near.isEmpty) Nil
    else {
      val best = near.iterator.map(_._1).min
      near.collect { case (d, k) if d == best => k }.sorted.take(3)
    }
  }

  // The optimal string alignment distance: the number of insertions,
  // deletions, substitutions and swaps of adjacent characters.
  private def editDistance(a: String, b: String): Int = {
    val dist = Array.ofDim[Int](a.length + 1, b.length + 1)
    (0 to a.length).foreach(i => dist(i)(0) = i)
    (0 to b.length).foreach(j => dist(0)(j) = j)
    (1 to a.length).foreach { i =>
      (1 to b.length).foreach { j =>
        val cost = if (a.charAt(i - 1) == b.charAt(j - 1)) 0 else 1
        var d =
          (dist(i - 1)(j) + 1).min(dist(i)(j - 1) + 1).min(dist(i - 1)(j - 1) + cost)
        if (
          (i > 1) && (j > 1) &&
          (a.charAt(i - 1) == b.charAt(j - 2)) &&
          (a.charAt(i - 2) == b.charAt(j - 1))
        ) d = d.min(dist(i - 2)(j - 2) + 1)
        dist(i)(j) = d
      }
    }
    dist(a.length)(b.length)
  }
}
