package org.bykn.bosatsu

import org.typelevel.paiges.{Doc, Document}
import cats.parse.{Parser => P, Parser1 => P1, Numbers}

sealed abstract class SExpr[+A] {
  def tag: A
}

object SExpr {
  sealed abstract class BracketKind(val start: Char, val end: Char)
  object BracketKind {
    case object Parens extends BracketKind('(', ')')
    case object Square extends BracketKind('[', ']')
    case object Curly extends BracketKind('{', '}')
  }

  trait Tagger[+A] {
    def apply[B](p: P1[B]): P1[(B, A)]
  }

  case class Sym[A](asString: String, tag: A) extends SExpr[A]
  case class Integer[A](toBigInt: BigInt, tag: A) extends SExpr[A]
  case class Floating[A](toBigDecimal: BigDecimal, tag: A) extends SExpr[A]
  case class Str[A](asString: String, tag: A) extends SExpr[A]
  case class Repeated[A](kind: BracketKind, items: List[SExpr[A]], tag: A) extends SExpr[A]

  def parser[A](tagger: Tagger[A]): P1[SExpr[A]] = cats.Defer[P1].fix[SExpr[A]] { rec =>
    val comment = (P.char(';') ~ P.until(P.char('\n')) ~ P.char('\n')).void

    val ws = P.charIn(" \t\r\n").orElse1[Any](comment).rep1.void

    val frac: P[Any] = P.char('.') ~ Numbers.digits
    val exp: P[Unit] = (P.charIn("eE") ~ P.charIn("+-").? ~ Numbers.digit.rep1).void

    val num = tagger(Numbers.signedIntString ~ (frac.? ~ exp.?).string)
      .map {
        case ((i, ""), tag) => Integer(BigInt(i), tag)
        case ((i, f), tag) =>
          try Floating(BigDecimal(i + f), tag)
          catch {
            case _: NumberFormatException =>
              throw new NumberFormatException(s"could not parse: $i$f as a BigDecimal")
          }
      }

    def repeated(b: BracketKind): P1[Repeated[A]] =
      tagger(P.char(b.start) *> ws.? *> P.repSep(rec, sep = ws, min = 0) <* ws.? <* P.char(b.end))
        .map { case (as, tag) => Repeated(b, as, tag) }

    val rep = List(BracketKind.Parens, BracketKind.Square, BracketKind.Curly).map(repeated)

    val symStr: P1[String] = symStrUtil.escapedToken.orElse1 {
      val mustEscape = symStrUtil.decodeTable.values.toSet
      P.charWhere { s => !mustEscape(s) }
    }.repAs1

    val sym = tagger(symStr).map { case (s, t) => Sym(s, t) }

    val str = tagger(JsonStringUtil.strParser).map { case (s, t) => Str(s, t) }

    P.oneOf1(num :: str :: sym :: rep)
  }

  private val symStrUtil = new GenericStringUtil(
    Map(
      (' ', ' '),
      ('(', '('),
      ('[', '['),
      ('{', '{'),
      (')', ')'),
      (']', ']'),
      ('}', '}'),
      (';', ';'),
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar), // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t'),
      ('\n', '\n'),
      ('\r', '\r'),
      ('\t', '\t'),
      ))

  implicit def documentSExpr[A]: Document[SExpr[A]] =
    new Document[SExpr[A]] {
     def document(s: SExpr[A]): Doc =
       s match {
         case Sym(str, _) => Doc.text(symStrUtil.escape(' ', str))
         case Integer(bi, _) => Doc.str(bi)
         case Floating(bd, _) =>
           val str = bd.toString
           // this sucks, but fine for now
           if (Numbers.signedIntString.parseAll(str).isRight) Doc.text(str + ".0")
           else Doc.text(str)
         case Str(content, _) =>
           Doc.char('"') + (Doc.text(JsonStringUtil.escape('"', content)) + Doc.char('"'))
         case Repeated(k, ss, _) =>
           val middle = Doc.intercalate(Doc.line, ss.map(document(_))).nested(4).grouped
           Doc.char(k.start) + (middle + Doc.char(k.end))
       }
    }
}
