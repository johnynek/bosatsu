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
  * the Predef source anyway, so that recompile is needed regardless. Use these
  * only for files that build.sbt tracks. The other reserved packages and the C
  * runtime headers are checked by a test instead (`ReservedNamesTest`), so
  * that editing them doesn't recompile core.
  */
object CheckedNames {

  /** `name`, which the export list of the source at `path` names as a value */
  inline def exportedValue(inline path: String, inline name: String): String =
    ${ exportedValueImpl('path, 'name) }

  /** `name`, which the export list of the source at `path` names as a type */
  inline def exportedType(inline path: String, inline name: String): String =
    ${ exportedTypeImpl('path, 'name) }

  /** `name`, a constructor of a type that the source at `path` exports with
    * its constructors
    */
  inline def constructor(inline path: String, inline name: String): String =
    ${ constructorImpl('path, 'name) }

  /** `name`, which the source at `path` declares `external` */
  inline def external(inline path: String, inline name: String): String =
    ${ externalImpl('path, 'name) }

  def exportedValueImpl(path: Expr[String], name: Expr[String])(using
      Quotes
  ): Expr[String] =
    checkSourceName(path, name, "exported values")(_.exportedValues)

  def exportedTypeImpl(path: Expr[String], name: Expr[String])(using
      Quotes
  ): Expr[String] =
    checkSourceName(path, name, "exported types")(_.exportedTypes)

  def constructorImpl(path: Expr[String], name: Expr[String])(using
      Quotes
  ): Expr[String] =
    checkSourceName(path, name, "constructors of the types exported with ()")(
      _.constructors
    )

  def externalImpl(path: Expr[String], name: Expr[String])(using
      Quotes
  ): Expr[String] =
    checkSourceName(path, name, "external declarations")(_.externals)

  private def literal(expr: Expr[String], what: String)(using
      quotes: Quotes
  ): String =
    expr.value.getOrElse(
      quotes.reflect.report.errorAndAbort(
        s"expected a string literal for the $what, found: ${expr.show}",
        expr
      )
    )

  private def sourceNames(path: String, pathExpr: Expr[String])(using
      quotes: Quotes
  ): BosatsuSourceNames =
    BosatsuSourceNames.scan(path, Macro.readRepoFile(path)) match {
      case Right(names) => names
      case Left(err)    => quotes.reflect.report.errorAndAbort(err, pathExpr)
    }

  private def checkSourceName(
      pathExpr: Expr[String],
      nameExpr: Expr[String],
      listName: String
  )(select: BosatsuSourceNames => List[String])(using
      quotes: Quotes
  ): Expr[String] = {
    import quotes.reflect.report

    val path = literal(pathExpr, "source path")
    val name = literal(nameExpr, "name")
    val names = sourceNames(path, pathExpr)
    val known = select(names)
    if (known.contains(name)) Expr(name)
    else if (known.isEmpty) {
      // a scan that finds nothing must not quietly reject every name
      report.errorAndAbort(
        s"found no $listName in $path, so `$name` can't be one of them",
        nameExpr
      )
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
