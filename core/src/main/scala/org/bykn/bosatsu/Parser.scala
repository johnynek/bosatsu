package org.bykn.bosatsu

import cats.data.{Kleisli, Validated, ValidatedNel, NonEmptyList}
import org.typelevel.paiges.Doc

import org.bykn.bosatsu.parser.{Parser => P, Parser1 => P1}

import cats.implicits._

object Parser {
  /**
   * This is an indentation aware
   * parser, the input is the string that
   * should be parsed after a new-line to
   * continue the current indentation block
   */
  type Indy[A] = Kleisli[P1, String, A]

  object Indy {
    def apply[A](fn: String => P1[A]): Indy[A] =
      Kleisli(fn)

    def lift[A](p: P1[A]): Indy[A] =
      Kleisli.liftF(p)

    /**
     * Parse spaces, end of line, then the next indentation
     */
    val toEOLIndent: Indy[Unit] =
      apply { indent =>
        toEOL1 <* P.string(indent)
      }

    implicit class IndyMethods[A](val toKleisli: Indy[A]) extends AnyVal {
      def region: Indy[(Region, A)] =
        toKleisli.mapF(_.region)

      /**
       * Parse exactly the current indentation
       * starting now
       */
      def indentBefore: Indy[A] =
        apply(indent => P.string1(indent) *> toKleisli.run(indent))

      def nonEmptyList(sepIndy: Indy[Unit]): Indy[NonEmptyList[A]] =
        Indy { indent =>
          val pa = toKleisli(indent)
          val sep = sepIndy(indent)
          P.rep1Sep(pa, min = 1, sep)
        }

      def cutThen[B](that: Indy[B]): Indy[(A, B)] =
        Indy { indent =>
          toKleisli(indent) ~ that(indent)
        }

      def cutThenOpt[B](that: Indy[B]): Indy[(A, Option[B])] =
        Indy { indent =>
          toKleisli(indent) ~ that(indent).?
        }

      def cutLeftP(that: P[Any]): Indy[A] =
        Indy { indent =>
          toKleisli(indent) <* that
        }

      def cutRight[B](that: Indy[B]): Indy[B] =
        Indy { indent =>
          toKleisli(indent) *> that(indent)
        }

      /**
       * This optionally allows extra indentation that starts now
       */
      def maybeMore: Parser.Indy[A] =
        Indy { indent =>
          // run this one time, not each spaces are parsed
          val noIndent = toKleisli.run(indent)
          val someIndent: P1[A] = Parser
            .spaces
            .string
            .flatMap { thisIndent =>
              toKleisli.run(indent + thisIndent)
            }

          someIndent <+> noIndent
        }
    }
  }

  sealed trait Error {
    def showContext(errColor: LocationMap.Colorize): Option[Doc] =
      this match {
        case Error.PartialParse(_, pos, locations) =>
          locations.showContext(pos, 2, errColor)
        case Error.ParseFailure(pos, locations) =>
          locations.showContext(pos, 2, errColor)
      }
  }

  object Error {
    case class PartialParse[A](got: A, position: Int, locations: LocationMap) extends Error
    case class ParseFailure(position: Int, locations: LocationMap) extends Error
  }

  def parse[A](p: P[A], str: String): ValidatedNel[Error, (LocationMap, A)] = {
    val lm = LocationMap(str)
    p.parse(str) match {
      case Right(("", a)) =>
        Validated.valid((lm, a))
      case Right((rest, a)) =>
        val idx = str.indexOf(rest)
        Validated.invalidNel(Error.PartialParse(a, idx, lm))
      case Left(err) =>
        // TODO, we have much more detailed failure
        // information now, including a list of expectations
        // we had at a point.
        val idx = err.failedAtOffset
        Validated.invalidNel(Error.ParseFailure(idx, lm))
    }
  }

  def inRange(lower: Char, c: Char, upper: Char): Boolean =
    (lower <= c) && (c <= upper)

  def isNum(c: Char): Boolean =
    inRange('0', c, '9')

  def isLower(c: Char): Boolean =
    inRange('a', c, 'z')

  def isUpper(c: Char): Boolean =
    inRange('A', c, 'Z')

  def isSpace(c: Char): Boolean =
    (c == ' ') | (c == '\t')

  def identifierChar(c: Char): Boolean =
    isNum(c) || isUpper(c) || isLower(c) || (c == '_')

  // used to parse possibly empty indentation
  def indentation(str: String): P[Unit] =
    if (str.isEmpty) P.unit
    else P.string1(str).void

  // parse one or more space characters
  val spaces: P1[Unit] = P.charsWhile1(isSpace _).void
  val nonSpaces: P1[String] = P.charsWhile1 { c => !isSpace(c) }
  val maybeSpace: P[Unit] = spaces.?.void

  /** prefer to parse Right, then Left
   */
  def either[A, B](pb: P[B], pa: P[A]): P[Either[B, A]] =
    pa.map(Right(_)).orElse(pb.map(Left(_)))

  def maybeIndentedOrSpace(indent: String): P[Unit] =
    spaces.orElse1(P.string1("\n" + indent)).rep.void

  val spacesAndLines: P1[Unit] =
    P.charsWhile1(_.isWhitespace).void

  val maybeSpacesAndLines: P[Unit] =
    spacesAndLines.?.void

  val lowerIdent: P1[String] =
    (P.charIn('a' to 'z') ~ P.charsWhile(identifierChar _)).string

  val upperIdent: P1[String] =
    (P.charIn('A' to 'Z') ~ P.charsWhile(identifierChar _)).string

  val py2Ident: P1[String] =
    (P.charIn('_' :: ('A' to 'Z').toList ::: ('a' to 'z').toList) ~ P.charsWhile(identifierChar _)).string

  // requires a string longer than 1
  def tokenP[T](s: String, t: T): P1[T] =
    P.string1(s).as(t)

  // parse a keyword and some space or backtrack
  def keySpace(str: String): P1[Unit] =
    (P.string1(str) ~ spaces).void.backtrack

  val digit19: P1[Char] = P.charIn('1' to '9')
  val digit09: P1[Char] = P.charIn('0' to '9')
  /**
   * This parser allows _ between any two digits to allow
   * literals such as:
   * 1_000_000
   *
   * It will also parse terrible examples like:
   * 1_0_0_0_0_0_0
   * but I think banning things like that shouldn't
   * be done by the parser
   */
  val integerString: P1[String] = {

    val rest = (P.char('_').?.with1 ~ digit09).rep
    val nonZero: P1[Unit] = (digit19 ~ rest).void

    val positive: P1[Unit] = P.char('0').orElse1(nonZero)
    (P.charIn("+-").?.with1 *> positive).string
  }

  object JsonNumber {
    /**
     *  from: https://tools.ietf.org/html/rfc4627
     *     number = [ minus ] int [ frac ] [ exp ]
     *     decimal-point = %x2E       ; .
     *     digit1-9 = %x31-39         ; 1-9
     *     e = %x65 / %x45            ; e E
     *     exp = e [ minus / plus ] 1*DIGIT
     *     frac = decimal-point 1*DIGIT
     *     int = zero / ( digit1-9 *DIGIT )
     *     minus = %x2D               ; -
     *     plus = %x2B                ; +
     *     zero = %x30                ; 0
     */
    val digits: P[Unit] = digit09.rep.void
    val digits1: P1[Unit] = digit09.rep1.void
    val int: P1[Unit] = P.char('0') <+> (digit19 ~ digits).void
    val frac: P1[Any] = P.char('.') ~ digits1
    val exp: P1[Unit] = (P.charIn("eE") ~ P.charIn("+-").? ~ digits1).void

    val parser: P1[String] =
      (P.char('-').?.with1 ~ int ~ frac.? ~ exp.?).string

    // this gives you the individual parts of a floating point string
    case class Parts(negative: Boolean, leftOfPoint: String, floatingPart: String, exp: String) {
      def asString: String = {
        val neg = if (negative) "-" else ""
        s"$neg$leftOfPoint$floatingPart$exp"
      }
    }

    val partsParser: P1[Parts] =
      (P.char('-').as(true).?.with1 ~ int.string ~ frac.string.? ~ exp.string.?)
        .map { case (((optNeg, left), float), exp) =>
          Parts(optNeg.isDefined, left, float.getOrElse(""), exp.getOrElse(""))
        }
  }

  def escapedString(q: Char): P1[String] =
    StringUtil.escapedString(q)

  def escape(quoteChar: Char, str: String): String =
    StringUtil.escape(quoteChar, str)

  def unescape(str: String): Either[Int, String] =
    StringUtil.unescape(str)

  def nonEmptyListToList[T](p: P[NonEmptyList[T]]): P[List[T]] =
    p.?.map {
      case None => Nil
      case Some(ne) => ne.toList
    }

  /**
   * Parse python-like dicts: delimited by curlies "{" "}" and
   * keys separated by colon
   */
  def dictLikeParser[K, V](pkey: P1[K], pvalue: P1[V]): P1[List[(K, V)]] = {
    val ws = maybeSpacesAndLines
    val kv = (pkey ~ ((ws ~ P.char(':') ~ ws).with1 *> pvalue))
    val kvs = kv.nonEmptyListOfWs(ws)
    val kvlist = nonEmptyListToList(kvs)

    (P.char('{') ~ ws) *> kvlist <* (ws ~ P.char('}'))
  }

  implicit class Combinators[T](val item: P1[T]) extends AnyVal {
    def nonEmptyList: P1[NonEmptyList[T]] =
      nonEmptyListOfWs(maybeSpace)

    def nonEmptyListOfWs(ws: P[Unit]): P1[NonEmptyList[T]] =
      nonEmptyListOfWsSep(ws, P.char(','), allowTrailing = true)

    def maybeAp(fn: P[T => T]): P1[T] =
      (item ~ fn.?)
        .map {
          case (a, None) => a
          case (a, Some(f)) => f(a)
        }

    def nonEmptyListOfWsSep(ws: P[Unit], sep: P[Unit], allowTrailing: Boolean): P1[NonEmptyList[T]] = {
      val wsSep = ((ws ~ sep).backtrack ~ ws).void
      val trail =
        if (allowTrailing) (ws ~ sep).backtrack.?.void
        else P.unit

      P.rep1Sep(item, min = 1, sep = wsSep) <* trail
    }

    def bracketed[A, B](left: P[A], right: P[B]): P1[T] =
      left.with1 *> item <* right

    def nonEmptyListSyntax: P1[NonEmptyList[T]] = {
      val ws = maybeSpacesAndLines
      val ts = nonEmptyListOfWs(ws)
      (P.char('[') ~ ws) *> ts <* (ws ~ P.char(']'))
    }

    def listSyntax: P1[List[T]] = {
      val ws = maybeSpacesAndLines
      val lst = nonEmptyListToList(nonEmptyListOfWs(ws))

      (P.char('[') ~ ws) *> lst <* (ws ~ P.char(']'))
    }

    def region: P1[(Region, T)] =
      (P.index.with1 ~ item ~ P.index)
        .map { case ((s, t), e) => (Region(s, e), t) }

    def parensCut: P1[T] =
      bracketed(P.char('(') ~ maybeSpacesAndLines, maybeSpacesAndLines ~ P.char(')'))

    def parensLines1Cut: P1[NonEmptyList[T]] =
      item.nonEmptyListOfWs(maybeSpacesAndLines)
        .parensCut

    /**
     * either: a, b, c, ..
     * or (a, b, c, ) where we allow newlines:
     * return true if we do have parens
     */
    def itemsMaybeParens: P1[(Boolean, NonEmptyList[T])] = {
      val withP = item.parensLines1Cut.map((true, _))
      val noP = item.nonEmptyListOfWs(maybeSpace).map((false, _))
      withP.orElse1(noP)
    }

    /**
     * Parse a python-like tuple or a parens
     */
    def tupleOrParens: P1[Either[T, List[T]]] = {
      val ws = maybeSpacesAndLines
      val sep = ((ws.with1 ~ P.char(',')).backtrack ~ ws).void
      val twoAndMore = P.repSep(item, min = 0, sep = sep)
      val trailing = sep.?.map(_.isDefined)

      val either = (item ~ (sep *> twoAndMore <* trailing).?).?
        .map {
          case None => Right(Nil)
          case Some((a, None)) =>
            // 1 item, no trailing comment, that's a parens
            Left(a)
          case Some((a, Some(items))) =>
            // either more than one item or a single item with
            // a trailing comma
            Right(a :: items)
        }


      (P.char('(') ~ ws) *> either <* (ws ~ P.char(')'))
    }
  }

  val newline: P1[Unit] = P.char('\n')
  val toEOL: P[Unit] = (maybeSpace ~ newline.orElse(P.end)).void
  val toEOL1: P1[Unit] = (maybeSpace.with1 *> newline)

  def optionParse[A](pa: P[A], str: String): Option[A] =
    pa.parse(str) match {
      case Right(("", a)) => Some(a)
      case _ => None
    }

  def unsafeParse[A](pa: P[A], str: String): A =
    pa.parse(str) match {
      case Right(("", a)) => a
      // $COVERAGE-OFF$
      case Right((rest, _)) =>
        sys.error(s"partial parse of $str ignores: $rest")
      case Left(err) =>
        val idx = err.failedAtOffset
        sys.error(s"failed to parse: $str: at $idx: (${str.substring(idx)}) with errors: ${err.expected}")
      // $COVERAGE-ON$
    }

}
