package dev.bosatsu

import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import org.typelevel.paiges.Doc

/** Heuristics to surface actionable hints for common parse mistakes.
  */
object ParserHints {
  private val rawInterpolationFailMessage =
    "raw interpolation only accepts a single bindable; use braces for larger expressions or $$ for a literal $"

  type ParseFailure = Parser.Error.ParseFailure
  type Rule = (String, LocationMap, ParseFailure) => Option[Doc]

  private val rules: List[Rule] =
    interpolationStartInStringRule ::
      rawInterpolationRule ::
    elseIfRule ::
      elseifSpellingRule ::
      assignmentInConditionRule ::
      matchesHeaderKeywordRule ::
      missingColonAfterHeaderRule ::
      unexpectedIndentationRule ::
      missingTrailingExpressionAfterDefRule ::
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
      unescapedInterpolationStartBefore(source, pos).map { case (_, start) =>
        Doc.text(
          "hint: parse failed after '" + start + "', which starts string interpolation. Use `$foo` and `$.foo` for single bindables, `${...}` and `$.{...}` for larger expressions, and `$$` for a literal `$`."
        )
      }
    }
  }

  private def rawInterpolationRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val _ = source
    Option.when(
      locations.toLineCol(error.position).nonEmpty &&
        expectsFailWithMessage(
          error.expected,
          rawInterpolationFailMessage
        )
    )(
      Doc.text(
        "hint: `$foo` and `$.foo` are short forms for a single bindable. Use `${...}` or `$.{...}` for larger expressions, and `$$` for a literal `$`."
      )
    )
  }

  private def unescapedInterpolationStartBefore(
      source: String,
      pos: Int
  ): Option[(Int, String)] = {
    if (source.isEmpty) {
      None
    } else {
      val searchFrom = if (pos >= source.length) source.length - 1 else pos
      val starts = "${" :: "$.{" :: Nil

      @annotation.tailrec
      def loop(from: Int): Option[(Int, String)] =
        if (from < 0) {
          None
        } else {
          val start = starts
            .flatMap { token =>
              val idx = source.lastIndexOf(token, from)
              Option.when(idx >= 0)((idx, token))
            }
            .sortBy(_._1)
            .lastOption

          start match {
            case None =>
              None
            case Some((idx, _)) if idx > 0 && source.charAt(idx - 1) == '\\' =>
              loop(idx - 1)
            case Some((idx, token)) =>
              val close = source.indexOf('}', idx + token.length)
              if (close >= 0 && close < pos) loop(idx - 1)
              else Some((idx, token))
          }
        }

      loop(searchFrom)
    }
  }

  private def expectsFailWithMessage(
      expected: NonEmptyList[P.Expectation],
      message: String
  ): Boolean =
    expected.exists(expectationHasFailWithMessage(_, message))

  private def expectationHasFailWithMessage(
      expectation: P.Expectation,
      message: String
  ): Boolean =
    expectation match {
      case P.Expectation.FailWith(_, msg) =>
        msg == message
      case P.Expectation.WithContext(_, inner) =>
        expectationHasFailWithMessage(inner, message)
      case _ =>
        false
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
          case Some((keyword, _))
              if (keyword == "if" || keyword == "elif") &&
                wordAtOrAfter(source, pos).exists { case (word, _, _) =>
                  word == "match"
                } =>
            Some(
              Doc.text(
                "hint: this condition uses 'match'. It looks like you meant 'matches'."
              )
            )
          case Some((k, _)) =>
            Some(Doc.text(s"hint: missing ':' after $k header."))
          case None =>
            None
        }
      }
    }
  }

  private def matchesHeaderKeywordRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (pos < 0 || pos > source.length) {
      None
    } else {
      lineInfo(locations, pos).flatMap { case (_, col, line) =>
        val words = wordsIn(line)
        val wordAtError =
          words.find { case (_, start, end) => start <= col && col < end }
        val wordBeforeError =
          words.takeWhile { case (_, _, end) => end <= col }.lastOption

        (wordAtError.toList ::: wordBeforeError.toList).collectFirst {
          case ("matches", start, end)
              if startsExpressionLikePrefix(line, start) &&
                line.drop(end).contains(':') =>
            Doc.text(
              "hint: match headers start with 'match', not 'matches'. It looks like you meant 'match'."
            )
        }
      }
    }
  }

  private def missingTrailingExpressionAfterDefRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (pos < 0 || pos > source.length) {
      None
    } else {
      for {
        expectedIndent <- expectedIndentationAt(error.expected, pos)
        (row, col, line) <- lineInfo(locations, pos)
        if col <= expectedIndent.length
        actualIndent = leadingIndent(line)
        if actualIndent.length < expectedIndent.length
        _ <- nearestUnclosedDefLineBefore(locations, row, expectedIndent)
      } yield Doc.text(
        "hint: this def ended without a final expression at its indentation level. Add a trailing expression before dedenting."
      )
    }
  }

  private def unexpectedIndentationRule(
      source: String,
      locations: LocationMap,
      error: ParseFailure
  ): Option[Doc] = {
    val pos = error.position
    if (
      pos < 0 || pos >= source.length || !isIndentChar(source.charAt(pos))
    ) {
      None
    } else {
      for {
        (_, col, line) <- lineInfo(locations, pos)
        if isSignificantLine(line)
        actualIndent = leadingIndent(line)
        if col <= actualIndent.length
        expected <- expectedIndentColumnsAt(error.expected, pos, col, actualIndent)
        if actualIndent.length > expected
      } yield {
        val actual = actualIndent.length
        Doc.text(
          s"hint: unexpected indentation. This line is indented $actual spaces, but this block expects $expected."
        )
      }
    }
  }

  private def expectedIndentColumnsAt(
      expected: NonEmptyList[P.Expectation],
      position: Int,
      column: Int,
      actualIndent: String
  ): Option[Int] =
    expectedIndentationAt(expected, position)
      .map(_.length)
      .filter(actualIndent.take(_).forall(isIndentChar))
      .orElse {
        // Sometimes block indentation has already been consumed and the parser
        // is expecting either a comment (`#`) or an expression token.
        Option.when(
          expectsCharNear(expected, position, '#') && (column < actualIndent.length)
        )(column)
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

  private def expectsCharNear(
      expected: NonEmptyList[P.Expectation],
      position: Int,
      char: Char
  ): Boolean =
    expected.exists(expectationMentionsCharNear(_, position, char))

  private def expectationMentionsCharNear(
      e: P.Expectation,
      position: Int,
      char: Char
  ): Boolean =
    e match {
      case P.Expectation.InRange(offset, lower, upper) =>
        (math.abs(offset - position) <= 1) &&
          (lower == char) && (upper == char)
      case P.Expectation.OneOfStr(offset, strs: List[String]) =>
        (math.abs(offset - position) <= 1) &&
          strs.exists(s => (s.length == 1) && (s.charAt(0) == char))
      case P.Expectation.WithContext(_, inner) =>
        expectationMentionsCharNear(inner, position, char)
      case _ =>
        false
    }

  private def expectedIndentationAt(
      expected: NonEmptyList[P.Expectation],
      position: Int
  ): Option[String] =
    expected.toList
      .flatMap(expectationIndentationAt(_, position))
      .filter(isIndentString)
      .sortBy(_.length)
      .lastOption

  private def expectationIndentationAt(
      e: P.Expectation,
      position: Int
  ): Option[String] =
    e match {
      case P.Expectation.OneOfStr(offset, strs: List[String])
          if math.abs(offset - position) <= 1 =>
        strs.filter(isIndentString).sortBy(_.length).lastOption
      case P.Expectation.WithContext(_, inner) =>
        expectationIndentationAt(inner, position)
      case _ =>
        None
    }

  private def isIndentString(s: String): Boolean =
    s.nonEmpty && s.forall(c => c == ' ' || c == '\t')

  private def isIndentChar(c: Char): Boolean =
    c == ' ' || c == '\t'

  private def leadingIndent(line: String): String = {
    var i = 0
    while (i < line.length && (line.charAt(i) == ' ' || line.charAt(i) == '\t')) {
      i = i + 1
    }
    line.substring(0, i)
  }

  private def isSignificantLine(line: String): Boolean = {
    val trimmed = line.trim
    trimmed.nonEmpty && !trimmed.startsWith("#")
  }

  private def nearestUnclosedDefLineBefore(
      locations: LocationMap,
      row: Int,
      indent: String
  ): Option[Int] = {
    @annotation.tailrec
    def loop(r: Int): Option[Int] =
      if (r < 0) None
      else {
        locations.getLine(r) match {
          case Some(line) if isSignificantLine(line) =>
            val leading = leadingIndent(line)
            if (leading.startsWith(indent) && leading.length > indent.length) {
              loop(r - 1)
            } else if (leading == indent) {
              if (line.drop(indent.length).startsWith("def ")) Some(r)
              else None
            } else None
          case _ =>
            loop(r - 1)
        }
      }

    loop(row - 1)
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

  private def startsExpressionLikePrefix(line: String, wordStart: Int): Boolean = {
    val prevSignificant =
      line.take(wordStart).reverseIterator.find(!_.isWhitespace)
    prevSignificant.forall("=([{,:".contains(_))
  }

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
