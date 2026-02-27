package dev.bosatsu

import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import org.typelevel.paiges.Doc

/** Heuristics to surface actionable hints for common parse mistakes.
  */
object ParserHints {
  type ParseFailure = Parser.Error.ParseFailure
  type Rule = (String, LocationMap, ParseFailure) => Option[Doc]

  private val rules: List[Rule] =
    interpolationStartInStringRule ::
    elseIfRule ::
      elseifSpellingRule ::
      assignmentInConditionRule ::
      missingColonAfterHeaderRule ::
      Nil

  def hints(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): List[Doc] = {
    val source0 =
      if (source.isEmpty) locations.fromString
      else source
    rules.flatMap(rule => rule(source0, locations, error))
  }

  def hints(error: ParseFailure): List[Doc] =
    hints(error.locations.fromString, error.locations, error)

  private def elseIfRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (
      !expectsColonAt(error.expected, pos) || locations.toLineCol(pos).isEmpty
    ) {
      None
    } else {
      val maybeHint =
        for {
          (currentWord, currentStart, _) <- wordAtOrAfter(source, pos)
          if currentWord == "if"
          (prevWord, _, _) <- wordBefore(source, currentStart)
          if prevWord == "else"
        } yield Doc.text(
          "hint: after else expected ':'. Use 'elif <condition>:' (not 'else if')."
        )

      maybeHint
    }
  }

  private def interpolationStartInStringRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (pos < 0 || pos > source.length || locations.toLineCol(pos).isEmpty) {
      None
    } else {
      unescapedInterpolationStartBefore(source, pos).map { _ =>
        Doc.text(
          """hint: parse failed after '${', which starts string interpolation (`${x}`). If you intended a literal '${', write `\${`."""
        )
      }
    }
  }

  private def unescapedInterpolationStartBefore(
      source: String,
      pos: Int
  ): Option[Int] = {
    if (source.isEmpty) {
      None
    } else {
      val searchFrom = if (pos >= source.length) source.length - 1 else pos

      @annotation.tailrec
      def loop(from: Int): Option[Int] =
        if (from < 0) {
          None
        } else {
          val start = source.lastIndexOf("${", from)
          if (start < 0) {
            None
          } else if (start > 0 && source.charAt(start - 1) == '\\') {
            loop(start - 1)
          } else {
            val close = source.indexOf('}', start + 2)
            if (close >= 0 && close < pos) {
              loop(start - 1)
            } else {
              Some(start)
            }
          }
        }

      loop(searchFrom)
    }
  }

  private def elseifSpellingRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (locations.toLineCol(pos).isEmpty) {
      None
    } else {
      wordAtOrAfter(source, pos)
        .collect {
          case (word, _, _) if (word == "elseif") || (word == "elsif") =>
            Doc.text(
              s"hint: use 'elif', not '$word'."
            )
        }
    }
  }

  private def assignmentInConditionRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (
      pos < 0 || pos > source.length || !expectsColonAt(error.expected, pos)
    ) {
      None
    } else {
      lineInfo(locations, pos).flatMap { case (row, col, line) =>
        val leadingText = line.take(col).dropWhile(_.isWhitespace)
        val head = headerFromLeading(leadingText).orElse {
          if (row > 0) {
            locations.getLine(row - 1).flatMap { prev =>
              headerFromLeading(prev.dropWhile(_.isWhitespace))
            }
          } else None
        }
        val tail = line.drop(col).dropWhile(_.isWhitespace)
        head match {
          case Some((keyword, lhs))
              if (keyword == "if" || keyword == "elif") &&
                tail.startsWith("=") && !tail.startsWith("==") =>
            val lhs1 = lhs.trim
            val rhs = tail.drop(1).takeWhile(_ != ':').trim
            val mentionsInt =
              containsIntegerLiteral(lhs1) || containsIntegerLiteral(rhs)
            val text =
              if (mentionsInt)
                "hint: this looks like equality in a condition. For Ints, use `eq_Int(a, b)`, or define/import `operator ==` (for example `operator == = eq_Int`)."
              else
                "hint: this looks like equality in a condition. Bosatsu does not compare with `=`. Use an equality function call, or define/import `operator ==`."
            Some(
              Doc.text(text)
            )
          case _ =>
            None
        }
      }
    }
  }

  private def missingColonAfterHeaderRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (!expectsColonAt(error.expected, pos)) {
      None
    } else {
      lineInfo(locations, pos).flatMap { case (row, col, line) =>
        val leading = line.take(col).dropWhile(_.isWhitespace)
        val head = headerFromLeading(leading).orElse {
          if (row > 0) {
            locations.getLine(row - 1).flatMap { prev =>
              headerFromLeading(prev.dropWhile(_.isWhitespace))
            }
          } else None
        }
        val tail = line.drop(col).dropWhile(_.isWhitespace)

        head match {
          case Some(("else", _)) if wordAtOrAfter(source, pos).exists {
                case (w, _, _) =>
                  w == "if"
              } =>
            // Handled by elseIfRule with a more specific message.
            None
          case Some((keyword, _))
              if (keyword == "if" || keyword == "elif") &&
                tail.startsWith("=") && !tail.startsWith("==") =>
            // Handled by assignmentInConditionRule with a more specific message.
            None
          case Some((k, _)) =>
            Some(Doc.text(s"hint: missing ':' after $k header."))
          case None =>
            None
        }
      }
    }
  }

  private def expectsColonAt(
      expected: NonEmptyList[P.Expectation],
      position: Int
  ): Boolean =
    expected.exists(expectationMentionsColonAt(_, position))

  private def expectationMentionsColonAt(
      e: P.Expectation,
      position: Int
  ): Boolean =
    e match {
      case P.Expectation.OneOfStr(offset, strs: List[String]) =>
        (offset == position) && strs.contains(":")
      case P.Expectation.InRange(offset, lower, upper) =>
        (offset == position) && (lower == ':') && (upper == ':')
      case P.Expectation.WithContext(_, inner) =>
        expectationMentionsColonAt(inner, position)
      case _ =>
        false
    }

  private def lineInfo(
      locations: LocationMap,
      position: Int
  ): Option[(Int, Int, String)] =
    locations
      .toLineCol(position)
      .flatMap { case (row, col) =>
        locations.getLine(row).map(line => (row, col, line))
      }

  private val headerKeywordSet: Set[String] =
    Set("if", "elif", "else", "def", "match", "recur", "loop", "case")

  private def headerFromLeading(leading: String): Option[(String, String)] = {
    val words = wordsIn(leading)
    words.reverseIterator
      .collectFirst {
        case (word, _, end) if headerKeywordSet(word) =>
          (word, leading.drop(end).trim)
      }
  }

  private def wordsIn(s: String): List[(String, Int, Int)] = {
    var i = 0
    val b = List.newBuilder[(String, Int, Int)]
    while (i < s.length) {
      if (isWordChar(s.charAt(i))) {
        val start = i
        i = i + 1
        while (i < s.length && isWordChar(s.charAt(i))) {
          i = i + 1
        }
        b += ((s.substring(start, i), start, i))
      } else {
        i = i + 1
      }
    }
    b.result()
  }

  private def isWordChar(c: Char): Boolean =
    c == '_' || c.isLetterOrDigit

  private val intLiteralRegex =
    raw"""[+-]?(?:0|[1-9][0-9_]*|0[bB][01_]+|0[oO][0-7_]+|0[xX][0-9a-fA-F_]+)""".r
  private def containsIntegerLiteral(s: String): Boolean =
    intLiteralRegex.findFirstIn(s).isDefined

  private def skipWhitespaceForward(source: String, from: Int): Int = {
    var i = if (from < 0) 0 else from
    while (i < source.length && source.charAt(i).isWhitespace) {
      i = i + 1
    }
    i
  }

  private def skipWhitespaceBackward(source: String, from: Int): Int = {
    var i =
      if (source.isEmpty) -1
      else if (from >= source.length) source.length - 1
      else from
    while (i >= 0 && source.charAt(i).isWhitespace) {
      i = i - 1
    }
    i
  }

  private def readWordAt(
      source: String,
      idx: Int
  ): Option[(String, Int, Int)] =
    if (idx < 0 || idx >= source.length || !isWordChar(source.charAt(idx))) {
      None
    } else {
      var start = idx
      while (start > 0 && isWordChar(source.charAt(start - 1))) {
        start = start - 1
      }

      var end = idx + 1
      while (end < source.length && isWordChar(source.charAt(end))) {
        end = end + 1
      }

      Some((source.substring(start, end), start, end))
    }

  private def wordAtOrAfter(
      source: String,
      from: Int
  ): Option[(String, Int, Int)] =
    readWordAt(source, skipWhitespaceForward(source, from))

  private def wordBefore(
      source: String,
      before: Int
  ): Option[(String, Int, Int)] =
    readWordAt(source, skipWhitespaceBackward(source, before - 1))
}
