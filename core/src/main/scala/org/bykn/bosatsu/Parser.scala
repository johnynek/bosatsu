package org.bykn.bosatsu

import cats.data.{Kleisli, Validated, ValidatedNel, NonEmptyList}
import org.typelevel.paiges.Doc

import cats.parse.{Parser0 => P0, Parser => P}

import cats.implicits._

object Parser {
  /**
   * This is an indentation aware
   * parser, the input is the string that
   * should be parsed after a new-line to
   * continue the current indentation block
   */
  type Indy[A] = Kleisli[P, String, A]

  object Indy {
    def apply[A](fn: String => P[A]): Indy[A] =
      Kleisli(fn)

    def lift[A](p: P[A]): Indy[A] =
      Kleisli.liftF(p)

    /**
     * Parse spaces, end of line, then the next indentation
     */
    val toEOLIndent: Indy[Unit] =
      apply { indent =>
        (toEOL1 *> P.string0(indent)).backtrack
      }

    implicit class IndyMethods[A](val toKleisli: Indy[A]) extends AnyVal {
      def region: Indy[(Region, A)] =
        toKleisli.mapF(_.region)

      /**
       * Parse exactly the current indentation
       * starting now
       */
      def indentBefore: Indy[A] =
        apply(indent => P.string0(indent).with1 *> toKleisli.run(indent))

      def nonEmptyList(sepIndy: Indy[Unit]): Indy[NonEmptyList[A]] =
        Indy { indent =>
          val pa = toKleisli(indent)
          val sep = sepIndy(indent)
          P.repSep(pa, min = 1, sep)
        }

      def cutThen[B](that: Indy[B]): Indy[(A, B)] =
        Indy { indent =>
          toKleisli(indent) ~ that(indent)
        }

      def cutThenOpt[B](that: Indy[B]): Indy[(A, Option[B])] =
        Indy { indent =>
          toKleisli(indent) ~ that(indent).?
        }

      def cutLeftP(that: P0[Any]): Indy[A] =
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
          val someIndent: P[A] = Parser
            .spaces
            .string
            .flatMap { thisIndent =>
              toKleisli.run(indent + thisIndent)
            }

          // TODO remove this backtracking
          // because if there is space, but incorrect
          // code inside, we don't point to the
          // correct location.
          someIndent.backtrack <+> noIndent
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

  def parse[A](p: P0[A], str: String): ValidatedNel[Error, (LocationMap, A)] = {
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

  // parse one or more space characters
  val spaces: P[Unit] = P.charsWhile(isSpace _).void
  val maybeSpace: P0[Unit] = spaces.?.void

  /** prefer to parse Right, then Left
   */
  def either[A, B](pb: P0[B], pa: P0[A]): P0[Either[B, A]] =
    pa.map(Right(_)).orElse(pb.map(Left(_)))

  def maybeIndentedOrSpace(indent: String): P0[Unit] =
    spaces.orElse(P.string("\n" + indent)).rep0.void

  val spacesAndLines: P[Unit] =
    P.charsWhile(_.isWhitespace).void

  val maybeSpacesAndLines: P0[Unit] =
    spacesAndLines.?.void

  val lowerIdent: P[String] =
    (P.charIn('a' to 'z') ~ P.charsWhile0(identifierChar _)).string

  val upperIdent: P[String] =
    (P.charIn('A' to 'Z') ~ P.charsWhile0(identifierChar _)).string

  val py2Ident: P[String] =
    (P.charIn('_' :: ('A' to 'Z').toList ::: ('a' to 'z').toList) ~ P.charsWhile0(identifierChar _)).string

  // parse a keyword and some space or backtrack
  def keySpace(str: String): P[Unit] =
    (P.string(str) ~ spaces).void.backtrack

  val digit19: P[Char] = P.charIn('1' to '9')
  val digit09: P[Char] = P.charIn('0' to '9')
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
  val integerString: P[String] = {

    val rest = (P.char('_').?.with1 ~ digit09).rep0
    val nonZero: P[Unit] = (digit19 ~ rest).void

    val positive: P[Unit] = P.char('0').orElse(nonZero)
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
    val digits: P0[Unit] = digit09.rep0.void
    val digits1: P[Unit] = digit09.rep.void
    val int: P[Unit] = P.char('0') <+> (digit19 ~ digits).void
    val frac: P[Any] = P.char('.') ~ digits1
    val exp: P[Unit] = (P.charIn("eE") ~ P.charIn("+-").? ~ digits1).void

    val parser: P[String] =
      (P.char('-').?.with1 ~ int ~ frac.? ~ exp.?).string

    // this gives you the individual parts of a floating point string
    case class Parts(negative: Boolean, leftOfPoint: String, floatingPart: String, exp: String) {
      def asString: String = {
        val neg = if (negative) "-" else ""
        s"$neg$leftOfPoint$floatingPart$exp"
      }
    }

    val partsParser: P[Parts] =
      (P.char('-').?.with1 ~ int.string ~ frac.string.? ~ exp.string.?)
        .map { case (((optNeg, left), float), exp) =>
          Parts(optNeg.isDefined, left, float.getOrElse(""), exp.getOrElse(""))
        }
  }

  def escapedString(q: Char): P[String] =
    StringUtil.escapedString(q)

  def escape(quoteChar: Char, str: String): String =
    StringUtil.escape(quoteChar, str)

  def unescape(str: String): Either[Int, String] =
    StringUtil.unescape(str)

  def nonEmptyListToList[T](p: P0[NonEmptyList[T]]): P0[List[T]] =
    p.?.map {
      case None => Nil
      case Some(ne) => ne.toList
    }

  /**
   * Parse python-like dicts: delimited by curlies "{" "}" and
   * keys separated by colon
   */
  def dictLikeParser[K, V](pkey: P[K], pvalue: P[V]): P[List[(K, V)]] = {
    val ws = maybeSpacesAndLines
    val kv = (pkey ~ ((ws ~ P.char(':') ~ ws).with1 *> pvalue))
    val kvs = kv.nonEmptyListOfWs(ws)
    val kvlist = nonEmptyListToList(kvs)

    (P.char('{') ~ ws) *> kvlist <* (ws ~ P.char('}'))
  }

  implicit class Combinators[T](val item: P[T]) extends AnyVal {
    def nonEmptyList: P[NonEmptyList[T]] =
      nonEmptyListOfWs(maybeSpace)

    def nonEmptyListOfWs(ws: P0[Unit]): P[NonEmptyList[T]] =
      nonEmptyListOfWsSep(ws, P.char(','), allowTrailing = true)

    def maybeAp(fn: P0[T => T]): P[T] =
      (item ~ fn.?)
        .map {
          case (a, None) => a
          case (a, Some(f)) => f(a)
        }

    def nonEmptyListOfWsSep(ws: P0[Unit], sep: P0[Unit], allowTrailing: Boolean): P[NonEmptyList[T]] = {
      val wsSep = ((ws ~ sep).backtrack ~ ws).void
      val trail =
        if (allowTrailing) (ws ~ sep).backtrack.?.void
        else P.unit

      P.repSep(item, min = 1, sep = wsSep) <* trail
    }

    def bracketed[A, B](left: P0[A], right: P0[B]): P[T] =
      left.with1 *> item <* right

    def nonEmptyListSyntax: P[NonEmptyList[T]] = {
      val ws = maybeSpacesAndLines
      val ts = nonEmptyListOfWs(ws)
      (P.char('[') ~ ws) *> ts <* (ws ~ P.char(']'))
    }

    def listSyntax: P[List[T]] = {
      val ws = maybeSpacesAndLines
      val lst = nonEmptyListToList(nonEmptyListOfWs(ws))

      (P.char('[') ~ ws) *> lst <* (ws ~ P.char(']'))
    }

    def region: P[(Region, T)] =
      (P.index.with1 ~ item ~ P.index)
        .map { case ((s, t), e) => (Region(s, e), t) }

    def parensCut: P[T] =
      parens(item)

    def parensLines1Cut: P[NonEmptyList[T]] =
      item.nonEmptyListOfWs(maybeSpacesAndLines)
        .parensCut

    /**
     * either: a, b, c, ..
     * or (a, b, c, ) where we allow newlines:
     * return true if we do have parens
     */
    def itemsMaybeParens: P[(Boolean, NonEmptyList[T])] = {
      val withP = item.parensLines1Cut.map((true, _))
      val noP = item.nonEmptyListOfWs(maybeSpace).map((false, _))
      withP.orElse(noP)
    }

    /**
     * Parse a python-like tuple or a parens
     */
    def tupleOrParens: P[Either[T, List[T]]] =
      parens {
        tupleOrParens0.?
          .map {
            case None => Right(Nil)
            case Some(Left(t)) => Left(t)
            case Some(Right(l)) => Right(l.toList)
          }
      }

    def tupleOrParens0: P[Either[T, NonEmptyList[T]]] = {
      val ws = maybeSpacesAndLines
      val sep = ((ws.with1 ~ P.char(',')).backtrack ~ ws).void
      val twoAndMore = P.repSep0(item, min = 0, sep = sep)
      val trailing = sep.?.map(_.isDefined)

      (item ~ (sep *> twoAndMore <* trailing).?)
        .map {
          case ((a, None)) =>
            // 1 item, no trailing comment, that's a parens
            Left(a)
          case ((a, Some(items))) =>
            // either more than one item or a single item with
            // a trailing comma
            Right(NonEmptyList(a, items))
        }
    }
  }

  def parens[A](pa: P0[A]): P[A] = {
    val ws = maybeSpacesAndLines
    (P.char('(') ~ ws) *> pa <* (ws ~ P.char(')'))
  }

  val newline: P[Unit] = P.char('\n')
  val toEOL: P0[Unit] = maybeSpace *> newline.orElse(P.end)
  val toEOL1: P[Unit] = maybeSpace.with1 *> newline

  def optionParse[A](pa: P0[A], str: String): Option[A] =
    pa.parseAll(str).toOption

  def unsafeParse[A](pa: P0[A], str: String): A =
    pa.parseAll(str) match {
      case Right(a) => a
      case Left(err) =>
        val idx = err.failedAtOffset
        sys.error(s"failed to parse: $str: at $idx: (${str.substring(idx)}) with errors: ${err.expected}")
      // $COVERAGE-ON$
    }

}
