package dev.bosatsu

import scala.util.matching.Regex

/** The names a Bosatsu source declares, which the built-in names the compiler
  * spells out are checked against: Predef's when the compiler is compiled (see
  * [[CheckedNames]]), the other reserved packages' by a test.
  *
  * This is a line scan, not a Bosatsu parser. It relies on the layout of the
  * repository's own sources: top-level declarations start in column 0, an
  * export list is `export (...)`, which may span lines, or `export a, b` on
  * one line, and an enum's variants follow its `:` on the same line or on the
  * indented lines below it.
  *
  * @param packageName
  *   the name on the `package` line
  * @param exportedValues
  *   the lower-case names in the export list
  * @param exportedTypes
  *   the type names in the export list, with or without `()`
  * @param constructors
  *   the constructors of the types the export list names with `()`
  * @param externals
  *   the names of the `external def`s and `external` values
  */
final case class BosatsuSourceNames(
    packageName: String,
    exportedValues: List[String],
    exportedTypes: List[String],
    constructors: List[String],
    externals: List[String]
)

object BosatsuSourceNames {
  private val upperName: Regex = "[A-Z][A-Za-z0-9_]*".r
  private val lowerName: Regex = "[a-z_][A-Za-z0-9_]*".r
  private val packageLine: Regex = raw"package\s+(\S+)\s*".r
  private val exportLine: Regex = raw"export((?:\s|\().*)".r
  private val typeWithConstructors: Regex =
    raw"([A-Z][A-Za-z0-9_]*)\s*\(\s*\)".r
  private val operatorEntry: Regex = raw"operator\s.*".r
  private val typeDeclaration: Regex =
    raw"(enum|struct)\s+([A-Z][A-Za-z0-9_]*)(.*)".r
  private val externalDef: Regex =
    raw"external\s+def\s+([a-z_][A-Za-z0-9_]*).*".r
  private val externalValue: Regex =
    raw"external\s+([a-z_][A-Za-z0-9_]*)\s*:.*".r

  /** Scans `text`, the contents of the source at `path`, or says why it
    * can't. A source without a `package` line or an export list fails, so a
    * change of layout that the scan no longer reads is an error rather than
    * empty lists that would reject every name.
    */
  def scan(path: String, text: String): Either[String, BosatsuSourceNames] = {
    val lines = text.linesIterator.map(stripComment).toVector
    for {
      packageName <- lines
        .collectFirst { case packageLine(name) => name }
        .toRight(s"$path has no `package` line")
      entries <- exportEntries(path, lines)
      exported <- classify(path, entries)
      declared <- typeDeclarations(path, lines)
      constructors <- traverse(exported.withConstructors) { tpe =>
        declared
          .collectFirst { case (`tpe`, constructors) => constructors }
          .toRight(
            s"$path exports $tpe() but declares no top-level enum or struct $tpe"
          )
      }
      _ <-
        if (exported.values.isEmpty && exported.types.isEmpty)
          Left(s"$path has no export list")
        else Right(())
    } yield BosatsuSourceNames(
      packageName,
      exported.values,
      exported.types,
      constructors.flatten,
      lines.toList.flatMap {
        case externalDef(name)                     => name :: Nil
        case externalValue(name) if name != "type" => name :: Nil
        case _                                     => Nil
      }
    )
  }

  private final case class Exported(
      values: List[String],
      types: List[String],
      withConstructors: List[String]
  )

  private def stripComment(line: String): String = {
    val idx = line.indexOf('#')
    if (idx < 0) line else line.substring(0, idx)
  }

  private def traverse[A, B](
      items: List[A]
  )(fn: A => Either[String, B]): Either[String, List[B]] =
    items.foldRight(Right(Nil): Either[String, List[B]]) { (a, acc) =>
      for {
        b <- fn(a)
        bs <- acc
      } yield b :: bs
    }

  private def isOpen(c: Char): Boolean = c == '(' || c == '[' || c == '{'
  private def isClose(c: Char): Boolean = c == ')' || c == ']' || c == '}'

  // The index of the bracket that closes the one at `open`.
  private def closing(text: String, open: Int): Option[Int] = {
    var depth = 0
    var idx = open
    var result: Option[Int] = None
    while (result.isEmpty && idx < text.length) {
      val c = text.charAt(idx)
      if (isOpen(c)) depth += 1
      else if (isClose(c)) {
        depth -= 1
        if (depth == 0) result = Some(idx)
      }
      idx += 1
    }
    result
  }

  // The index of the first `target` outside any brackets.
  private def topLevelIndex(text: String, target: Char): Option[Int] = {
    var depth = 0
    var idx = 0
    var result: Option[Int] = None
    while (result.isEmpty && idx < text.length) {
      val c = text.charAt(idx)
      if (isOpen(c)) depth += 1
      else if (isClose(c)) depth -= 1
      else if ((c == target) && (depth == 0)) result = Some(idx)
      idx += 1
    }
    result
  }

  // The non-empty trimmed pieces of `text` between separators outside any
  // brackets.
  private def splitTopLevel(text: String, isSep: Char => Boolean): List[String] = {
    val pieces = List.newBuilder[String]
    val current = new StringBuilder
    var depth = 0
    text.foreach { c =>
      if (isOpen(c)) depth += 1
      else if (isClose(c)) depth -= 1

      if ((depth == 0) && isSep(c)) {
        pieces += current.result()
        current.clear()
      } else {
        current += c
      }
    }
    pieces += current.result()
    pieces.result().map(_.trim).filter(_.nonEmpty)
  }

  private def exportEntries(
      path: String,
      lines: Vector[String]
  ): Either[String, List[String]] =
    traverse(lines.indices.toList.flatMap { idx =>
      lines(idx) match {
        case exportLine(rest) => (idx, rest) :: Nil
        case _                => Nil
      }
    }) { case (idx, rest) =>
      if (rest.trim.startsWith("(")) {
        val text = (rest +: lines.drop(idx + 1)).mkString("\n")
        val open = text.indexOf('(')
        closing(text, open)
          .map(close => splitTopLevel(text.substring(open + 1, close), _ == ','))
          .toRight(
            s"$path: the export list on line ${idx + 1} has no closing `)`"
          )
      } else Right(splitTopLevel(rest, _ == ','))
    }.map(_.flatten)

  private def classify(
      path: String,
      entries: List[String]
  ): Either[String, Exported] =
    entries
      .foldLeft(Right(Exported(Nil, Nil, Nil)): Either[String, Exported]) {
        (acc, entry) =>
          acc.flatMap { exp =>
            entry match {
              case operatorEntry() => Right(exp)
              case typeWithConstructors(tpe) =>
                Right(
                  exp.copy(
                    types = tpe :: exp.types,
                    withConstructors = tpe :: exp.withConstructors
                  )
                )
              case upperName() => Right(exp.copy(types = entry :: exp.types))
              case lowerName() => Right(exp.copy(values = entry :: exp.values))
              case _           =>
                Left(s"$path: can't read the export list entry `$entry`")
            }
          }
      }
      .map { exp =>
        Exported(exp.values.reverse, exp.types.reverse, exp.withConstructors.reverse)
      }

  // Each top-level enum or struct with its constructors, in source order.
  private def typeDeclarations(
      path: String,
      lines: Vector[String]
  ): Either[String, List[(String, List[String])]] =
    traverse(lines.indices.toList.flatMap { idx =>
      lines(idx) match {
        case typeDeclaration(kind, name, rest) => (idx, kind, name, rest) :: Nil
        case _                                 => Nil
      }
    }) { case (idx, kind, name, rest) =>
      if (kind == "struct") Right((name, name :: Nil))
      else
        topLevelIndex(rest, ':') match {
          case None =>
            Left(s"$path: enum $name on line ${idx + 1} has no `:`")
          case Some(colon) =>
            // the variants follow the `:` and continue on the indented or
            // blank lines below
            val body = (rest.substring(colon + 1) +: lines
              .drop(idx + 1)
              .takeWhile(line => line.trim.isEmpty || line.head.isWhitespace))
              .mkString("\n")
            val variants = splitTopLevel(body, c => (c == ',') || (c == '\n'))
            traverse(variants) { variant =>
              upperName
                .findPrefixOf(variant)
                .toRight(
                  s"$path: can't read the variant `$variant` of enum $name on line ${idx + 1}"
                )
            }.flatMap { constructors =>
              if (constructors.isEmpty)
                Left(s"$path: enum $name on line ${idx + 1} has no variants")
              else Right((name, constructors))
            }
        }
    }
}
