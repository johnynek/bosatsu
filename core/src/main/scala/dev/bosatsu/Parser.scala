package dev.bosatsu

import cats.data.{Kleisli, Validated, ValidatedNel, NonEmptyList}
import cats.parse.{Parser0 => P0, Parser => P}
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

import cats.implicits._
import java.math.BigInteger

object Parser {

  /** This is an indentation aware parser, the input is the string that should
    * be parsed after a new-line to continue the current indentation block
    */
  type Indy[A] = Kleisli[P, String, A]

  object Indy {
    def apply[A](fn: String => P[A]): Indy[A] =
      Kleisli(fn)

    def lift[A](p: P[A]): Indy[A] =
      Kleisli.liftF(p)

    /** Parse spaces, end of line, then the next indentation
      */
    val toEOLIndent: Indy[Unit] =
      apply { indent =>
        toEOL1 *> P.string0(indent)
      }

    /** Parse EOL + indent and skip any immediately following comment-only lines
      * at the same base indentation.
      */
    val toEOLIndentWithComments: Indy[Unit] =
      apply { indent =>
        val commentLine =
          ((Parser.maybeSpace.with1.soft *> Parser.lineComment)
            .orElse(Parser.lineComment) <* toEOL1 <* P
            .string0(indent)).rep0.void
        (toEOL1 *> P.string0(indent)) *> commentLine
      }

    implicit class IndyMethods[A](val toKleisli: Indy[A]) extends AnyVal {
      def region: Indy[(Region, A)] =
        toKleisli.mapF(_.region)

      /** Parse exactly the current indentation starting now
        */
      def indentBefore: Indy[A] =
        apply(indent => P.string0(indent).with1 *> toKleisli.run(indent))

      def nonEmptyList(sepIndy: Indy[Unit]): Indy[NonEmptyList[A]] =
        Indy { indent =>
          val pa = toKleisli(indent)
          val sep = sepIndy(indent).backtrack
          P.repSep(pa, min = 1, sep)
        }

      def cutThen[B](that: Indy[B]): Indy[(A, B)] =
        Indy { indent =>
          toKleisli(indent) ~ that(indent)
        }

      def cutLeftP(that: P0[Any]): Indy[A] =
        Indy { indent =>
          toKleisli(indent) <* that
        }

      def cutRight[B](that: Indy[B]): Indy[B] =
        Indy { indent =>
          toKleisli(indent) *> that(indent)
        }

      /** This optionally allows extra indentation that starts now
        */
      def maybeMore: Parser.Indy[A] =
        Indy { indent =>
          // run this one time, not each spaces are parsed
          val noIndent = toKleisli.run(indent)
          val someIndent: P[A] = Parser.spaces.string
            .flatMap { thisIndent =>
              toKleisli.run(indent + thisIndent)
            }

          someIndent.orElse(noIndent)
        }
    }
  }

  sealed trait Error {
    def showContext(errColor: LocationMap.Colorize): Doc =
      this match {
        case Error.ParseFailure(_, locations, exps) =>
          Error.showExpectations(locations, exps, errColor)
      }
  }

  object Error {
    case class ParseFailure(
        position: Int,
        locations: LocationMap,
        expected: NonEmptyList[P.Expectation]
    ) extends Error

    def showExpectations(
        locations: LocationMap,
        expected: NonEmptyList[P.Expectation],
        errColor: LocationMap.Colorize
    ): Doc = {
      val errs: SortedMap[Int, NonEmptyList[P.Expectation]] =
        expected.groupBy(_.offset)

      def show(s: String): Doc = {
        val q = '\''
        if (s.forall(_.isWhitespace)) {
          val chars = s.length
          val plural = if (chars == 1) "char" else "chars"
          Doc.text(s"$chars whitespace $plural \"") + Doc.intercalate(
            Doc.empty,
            s.map {
              case '\t' => Doc.text("\\t")
              case '\n' => Doc.text("\\n")
              case '\r' => Doc.text("\\r")
              case c    => Doc.char(c)
            }
          ) + Doc.char('"')
        } else {
          Doc.char(q) + Doc.text(escape(q, s)) + Doc.char(q)
        }
      }

      def expToDoc(e: P.Expectation): Doc =
        e match {
          case P.Expectation.OneOfStr(_, strs: List[String]) =>
            strs match {
              case one :: Nil =>
                Doc.text("expected ") + show(one)
              case _ =>
                Doc.text("expected one of: ") + Doc
                  .intercalate(Doc.line, strs.map(show))
                  .grouped
                  .nested(4)
            }
          case P.Expectation.InRange(_, lower, upper) =>
            if (lower == upper) {
              Doc.text("expected char: ") + show(lower.toString)
            } else {
              Doc.text("expected char in range: [") + show(lower.toString) + Doc
                .text(", ") + show(upper.toString) + Doc.text("]")
            }
          case P.Expectation.StartOfString(_) =>
            Doc.text("expected start of the file")
          case P.Expectation.EndOfString(_, length) =>
            Doc.text(s"expected end of file but $length characters remaining")
          case P.Expectation.Length(_, expected, actual) =>
            Doc.text(
              s"expected $expected more characters but only $actual remaining"
            )
          case P.Expectation.ExpectedFailureAt(_, matched) =>
            Doc.text("expected failure but the parser matched: ") + show(
              matched
            )
          case P.Expectation.Fail(_) =>
            Doc.text("failed")
          case P.Expectation.FailWith(_, message) =>
            Doc.text(s"failed with message: $message")
          case P.Expectation.WithContext(_, expect) =>
            expToDoc(expect)
        }

      Doc.intercalate(
        Doc.hardLine,
        errs.map { case (pos, xs) =>
          locations.showContext(pos, 2, errColor).get + (Doc.hardLine + Doc
            .intercalate(Doc.comma + Doc.line, xs.toList.map(expToDoc))
            .grouped).nested(4)
        }
      )
    }
  }

  def parse[A](p: P0[A], str: String): ValidatedNel[Error, (LocationMap, A)] = {
    val lm = LocationMap(str)
    p.parseAll(str) match {
      case Right(a) =>
        Validated.valid((lm, a))
      case Left(err) =>
        val idx = err.failedAtOffset
        Validated.invalidNel(Error.ParseFailure(idx, lm, err.expected))
    }
  }

  val identifierCharsP: P0[String] =
    P.charIn(
      '_' :: ('a' to 'z').toList ::: ('A' to 'Z').toList ::: ('0' to '9').toList
    ).repAs0

  // parse one or more space characters
  val spaces: P[Unit] = P.charIn(Set(' ', '\t')).rep.void
  val maybeSpace: P0[Unit] = spaces.?.void

  private val lineTermination: P0[Unit] =
    P.char('\n').void.orElse(P.end)

  val lineComment: P[Unit] =
    (P.char('#') *> P.until0(lineTermination)).void

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

  /** Like [[spacesAndLines]], but also treats full-line # comments as trivia.
    * This is intended for separator contexts (lists, args, tuples, etc.).
    */
  val spacesAndCommentLines: P[Unit] = {
    val comment = (lineComment <* lineTermination).void
    (spacesAndLines | comment).rep.void
  }

  val maybeSpacesAndCommentLines: P0[Unit] =
    spacesAndCommentLines.?.void

  val lowerIdent: P[String] =
    (P.charIn('a' to 'z') ~ identifierCharsP).string

  val upperIdent: P[String] =
    (P.charIn('A' to 'Z') ~ identifierCharsP).string

  val py2Ident: P[String] =
    (P.charIn(
      '_' :: ('A' to 'Z').toList ::: ('a' to 'z').toList
    ) ~ identifierCharsP).string

  // parse a keyword and some space or backtrack
  def keySpace(str: String): P[Unit] =
    (P.string(str).soft ~ spaces).void

  val digit19: P[Char] = P.charIn('1' to '9')
  val digit09: P[Char] = P.charIn('0' to '9')

  /** This parser allows _ between any two digits to allow literals such as:
    * 1_000_000
    *
    * It will also parse terrible examples like: 1_0_0_0_0_0_0 but I think
    * banning things like that shouldn't be done by the parser
    */
  val positiveIntegerString: P[String] = {
    val rest = (P.char('_').?.with1 ~ digit09).rep0
    val nonZero: P[Unit] = (digit19 ~ rest).void

    P.char('0').orElse(nonZero).string
  }

  val integerString: P[String] =
    (P.charIn("+-").?.with1 *> positiveIntegerString).string

  // all two character base 10 BigIntegers
  private val smallBase10: Map[String, BigInteger] = {
    val signs = "+" :: "-" :: Nil

    ((0 to 99).iterator.map { i =>
      (i.toString, BigInteger.valueOf(i.toLong))
    } ++
      (0 to 9).iterator.flatMap { i =>
        signs.map { sign =>
          if (sign == "-") {
            (s"-$i", BigInteger.valueOf(-i.toLong))
          } else (s"+$i", BigInteger.valueOf(i.toLong))
        }
      }).toMap
  }
  val integerWithBase: P[(BigInteger, Int)] = {
    val binDigit = P.charIn(('0' to '1'))
    val octDigit = P.charIn(('0' to '7'))
    val hexDigit = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val under = P.char('_')

    def rest(d: P[Char]): P[String] =
      d.repSep(sep = under.?).string

    def base(n: Int, str: String, d: P[Char]) =
      (P.string(str.toLowerCase) | P.string(str)) *>
        (under.?.with1 *> rest(d))
          .map((_, n))

    val not10 = base(2, "0B", binDigit) |
      base(8, "0O", octDigit) |
      base(16, "0X", hexDigit)

    val pos = not10 | positiveIntegerString.map((_, 10))

    (P.charIn("+-").?.string.with1 ~ pos)
      .map { case (sign, (str, base)) =>
        val noUnder =
          if (str.indexOf("_") >= 0) (sign + str.filter(_ != '_'))
          else (sign + str)

        if ((base == 10) && (noUnder.length <= 2)) (smallBase10(noUnder), 10)
        else (new BigInteger(noUnder, base), base)
      }
  }

  object JsonNumber {

    /** from: https://tools.ietf.org/html/rfc4627 number = [ minus ] int [ frac
      * ] [ exp ] decimal-point = %x2E ; . digit1-9 = %x31-39 ; 1-9 e = %x65 /
      * %x45 ; e E exp = e [ minus / plus ] 1*DIGIT frac = decimal-point 1*DIGIT
      * int = zero / ( digit1-9 *DIGIT ) minus = %x2D ; - plus = %x2B ; + zero =
      * %x30 ; 0
      */
    val digits: P0[Unit] = digit09.rep0.void
    val digits1: P[Unit] = digit09.rep.void
    val int: P[Unit] = P.char('0') <+> (digit19 ~ digits).void
    val frac: P[Any] = P.char('.') ~ digits1
    val exp: P[Unit] = (P.charIn("eE") ~ P.charIn("+-").? ~ digits1).void

    val parser: P[String] =
      (P.char('-').?.with1 ~ int ~ frac.? ~ exp.?).string

    // this gives you the individual parts of a floating point string
    case class Parts(
        negative: Boolean,
        leftOfPoint: String,
        floatingPart: String,
        exp: String
    ) {
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
      case None     => Nil
      case Some(ne) => ne.toList
    }

  /** Parse python-like dicts: delimited by curlies "{" "}" and keys separated
    * by colon
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
          case (a, None)    => a
          case (a, Some(f)) => f(a)
        }

    def nonEmptyListOfWsSep(
        ws: P0[Unit],
        sep: P0[Unit],
        allowTrailing: Boolean
    ): P[NonEmptyList[T]] = {
      val wsSep = (ws.soft ~ sep ~ ws).void
      val trail =
        if (allowTrailing) (ws.soft ~ sep).?.void
        else P.unit

      P.repSep(item, min = 1, sep = wsSep) <* trail
    }

    def bracketed[A, B](left: P0[A], right: P0[B]): P[T] =
      left.with1 *> item <* right

    def nonEmptyListSyntax: P[NonEmptyList[T]] = {
      val ws = maybeSpacesAndCommentLines
      val ts = nonEmptyListOfWs(ws)
      (P.char('[') ~ ws) *> ts <* (ws ~ P.char(']'))
    }

    def listSyntax: P[List[T]] = {
      val ws = maybeSpacesAndCommentLines
      val lst = nonEmptyListToList(nonEmptyListOfWs(ws))

      (P.char('[') ~ ws) *> lst <* (ws ~ P.char(']'))
    }

    def region: P[(Region, T)] =
      (P.index.with1 ~ item ~ P.index)
        .map { case ((s, t), e) => (Region(s, e), t) }

    def parensCut: P[T] =
      parens(item)

    def parensLines1Cut: P[NonEmptyList[T]] =
      parens(
        item.nonEmptyListOfWs(maybeSpacesAndCommentLines),
        maybeSpacesAndCommentLines
      )

    def parensLines0Cut: P[List[T]] =
      parens(
        nonEmptyListToList(item.nonEmptyListOfWs(maybeSpacesAndCommentLines)),
        maybeSpacesAndCommentLines
      )

    /** either: a, b, c, .. or (a, b, c, ) where we allow newlines: return true
      * if we do have parens
      */
    def itemsMaybeParens: P[(Boolean, NonEmptyList[T])] = {
      val withP = item.parensLines1Cut.map((true, _))
      val noP = item.nonEmptyListOfWs(maybeSpace).map((false, _))
      withP.orElse(noP)
    }

    /** Parse a python-like tuple or a parens
      */
    def tupleOrParens: P[Either[T, List[T]]] =
      parens {
        tupleOrParens0.?.map {
          case None           => Right(Nil)
          case Some(Left(t))  => Left(t)
          case Some(Right(l)) => Right(l.toList)
        }
      }

    def tupleOrParens0: P[Either[T, NonEmptyList[T]]] = {
      val ws = maybeSpacesAndCommentLines
      val sep = (ws.soft ~ P.char(',') ~ ws).void
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

  private def parens[A](pa: P0[A]): P[A] =
    parens(pa, maybeSpacesAndLines)

  def parens[A](pa: P0[A], ws: P0[Any]): P[A] =
    (P.char('(') ~ ws) *> pa <* (ws ~ P.char(')'))

  val newline: P[Unit] = P.char('\n')
  val termination: P0[Unit] = lineTermination
  val toEOL: P0[Unit] = maybeSpace *> termination
  val toEOL1: P[Unit] = maybeSpace.with1 *> newline

  def optionParse[A](pa: P0[A], str: String): Option[A] =
    pa.parseAll(str).toOption

  def unsafeParse[A](pa: P0[A], str: String): A =
    pa.parseAll(str) match {
      case Right(a)  => a
      case Left(err) =>
        sys.error(show"$err")
    }

  sealed abstract class MaybeTupleOrParens[A]
  object MaybeTupleOrParens {
    sealed abstract class NotBare[A] extends MaybeTupleOrParens[A]
    case class Bare[A](bare: A) extends MaybeTupleOrParens[A]
    case class Tuple[A](bare: List[A]) extends NotBare[A]
    case class Parens[A](bare: A) extends NotBare[A]

    def tupleOrParens[A](p: P[A]): P[NotBare[A]] =
      p.tupleOrParens.map {
        case Right(tup)   => Tuple(tup)
        case Left(parens) => Parens(parens)
      }

    def parser[A](p: P[A]): P[MaybeTupleOrParens[A]] =
      tupleOrParens(p) | p.map(Bare(_))
  }

  def argFromParser[A](
      p: P0[A],
      defmeta: String,
      typeName: String,
      suggestion: String
  ): Argument[A] =
    new Argument[A] {
      def defaultMetavar: String = defmeta
      def read(string: String): ValidatedNel[String, A] =
        p.parseAll(string) match {
          case Right(a) => Validated.valid(a)
          case _        =>
            val sugSpace = if (suggestion.nonEmpty) s" $suggestion" else ""
            Validated.invalidNel(
              s"could not parse $string as a $typeName." + sugSpace
            )
        }
    }
}
