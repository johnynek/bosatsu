package org.bykn.bosatsu

import cats.Order
import cats.parse.{Parser => P, Parser0 => P0}
import org.typelevel.paiges.{Doc, Document}
import scala.annotation.tailrec

sealed abstract class Kind {
  def toDoc: Doc = Kind.toDoc(this)

  def toArgs: List[Kind.Arg] = {
    @tailrec
    def loop(k: Kind, acc: List[Kind.Arg]): List[Kind.Arg] =
      k match {
        case Kind.Type => acc.reverse
        case Kind.Cons(arg, rest) =>
          loop(rest, arg :: acc)
      }

    loop(this, Nil)
  }

  def withVar(v: Variance): Kind.Arg = Kind.Arg(v, this)
  def in: Kind.Arg = withVar(Variance.in)
  def co: Kind.Arg = withVar(Variance.co)
  def contra: Kind.Arg = withVar(Variance.contra)
  def phantom: Kind.Arg = withVar(Variance.phantom)

  def isType: Boolean = this == Kind.Type

  // is order == 1, this is called a "generic" type in some language
  @tailrec
  final def isOrder1: Boolean =
    this match {
      case Kind.Type => false
      case Kind.Cons(Kind.Arg(_, in), rest) =>
        in.isType && (rest.isType || rest.isOrder1)
    }
  // is order 2 or more
  @tailrec
  final def isHigherOrder: Boolean =
    this match {
      case Kind.Type => false
      case Kind.Cons(Kind.Arg(_, in), rest) =>
        (!in.isType) || rest.isHigherOrder
    }

  def order: Int =
    this match {
      case Kind.Type => 0
      case Kind.Cons(Kind.Arg(_, i), o) =>
        scala.math.max(i.order + 1, o.order)
    }

}

object Kind {
  case class Arg(variance: Variance, kind: Kind) {
    def returns(kindRes: Kind): Kind = Cons(this, kindRes)
  }

  case object Type extends Kind
  // Int => *: Type
  // enum List[a: +*]=> + -> *: Cons(+Type, Type)
  // struct Wrapper[f: +(* -> *), a](unwrap: f[a])
  // struct Monad[f: (* -> *) -> *] => (* -> *) -> *: Cons(Cons(Type, Type), Type)
  // Map[k: *, v: *] => * -> * -> *: Cons(Type, Cons(Type, Type))
  // Function[-A, +B] => -* -> +* -> *
  case class Cons(arg: Arg, result: Kind) extends Kind

  def apply(args: Arg*): Kind =
    args.foldRight(Type: Kind)(_.returns(_))

  private[this] val phantomStr = "👻"
  private[this] val ghostDoc = Doc.text(phantomStr)
  private def varDoc(v: Variance): Doc =
    v match {
      case Variance.Covariant     => Doc.char('+')
      case Variance.Contravariant => Doc.char('-')
      case Variance.Invariant     => Doc.empty
      case Variance.Phantom       => ghostDoc
    }

  private[this] val arrow = Doc.text(" -> ")
  private[this] def par(d: Doc): Doc =
    Doc.char('(') + (d + Doc.char(')'))

  def toDoc(k: Kind): Doc =
    k match {
      case Type                          => Doc.char('*')
      case Cons(Arg(variance, arg), res) =>
        // to get associativity right, need parens
        val argDoc = if (arg.isType) Doc.char('*') else par(toDoc(arg))
        varDoc(variance) + argDoc + arrow + toDoc(res)
    }

  implicit val documentKind: Document[Kind] =
    Document(toDoc(_))

  val parser: P[Kind] = P.recursive[Kind] { recurse =>
    val varianceParser: P[Variance] =
      P.fromStringMap(
        Map(
          "+" -> Variance.co,
          "-" -> Variance.contra,
          phantomStr -> Variance.phantom
        )
      )

    val ws: P0[Unit] = Parser.maybeSpacesAndLines

    val pType: P[Type.type] = P.char('*').as(Type)
    val parens: P[Kind] = P.char('(') *> (ws *> recurse <* ws <* P.char(')'))

    // a -> b -> c needs to be parsed as a -> (b -> c)
    val arg = pType | parens
    val rhs = P.string("->") *> (ws *> recurse)

    // if we see variance, we know we have a Cons
    val varCase: P[Cons] =
      (varianceParser ~ (arg ~ (ws *> rhs))).map { case (v, (i, o)) =>
        Cons(Arg(v, i), o)
      }
    // with no var, we optionally have -> after
    val noVar: P[Kind] = (arg ~ (ws.soft *> rhs).?).map {
      case (k, None)    => k
      case (i, Some(o)) => Cons(i.in, o)
    }

    varCase | noVar
  }

  implicit val orderKind: Order[Kind] =
    new Order[Kind] {
      val ordVar = Order[Variance]
      def compare(left: Kind, right: Kind): Int =
        (left, right) match {
          case (Type, Type) => 0
          case (Type, _) => -1
          case (Cons(_, _), Type) => 1
          case (Cons(al, kl), Cons(ar, kr)) =>
            val cv = ordVar.compare(al.variance, ar.variance)
            if (cv != 0) cv
            else {
              val ca = compare(al.kind, ar.kind)
              if (ca != 0) ca
              else compare(kl, kr)
            }
        }
    }
  
  implicit val orderingKind: Ordering[Kind] = orderKind.toOrdering
}
