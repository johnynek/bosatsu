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

  def withVar(v: Variance): Kind.Arg = {
    if (isType && (v == Variance.in)) Kind.invariantTypeArg
    else Kind.Arg(v, this)
  }

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

  val invariantTypeArg: Arg = Arg(Variance.in, Type)

  def apply(args: Arg*): Kind =
    args.foldRight(Type: Kind)(_.returns(_))

  /** Ignoring variances do these two Kinds have the same shape
    */
  def shapeMatch(k1: Kind, k2: Kind): Boolean =
    (k1, k2) match {
      case (Type, Type) => true
      case (Cons(Arg(_, a), b), Cons(Arg(_, c), d)) =>
        shapeMatch(a, c) && shapeMatch(b, d)
      case _ => false
    }

  // Can we use the right Kind in place of a left
  // put another way, can we "widen" k2 into k1
  def leftSubsumesRight(k1: Kind, k2: Kind): Boolean =
    (k1, k2) match {
      case (Type, Type)                                   => true
      case (Cons(Arg(v1, a1), b1), Cons(Arg(v2, a2), b2)) =>
        // since kind itself is contravariant in the argument to the function
        // we switch the order of the check in the a2 and a1
        ((v1 + v2) == v1) && leftSubsumesRight(a2, a1) && leftSubsumesRight(
          b1,
          b2
        )
      case _ => false
    }

  // allSubKinds(k).forall(leftSubsumesRight(k, _))
  def allSubKinds(k: Kind): LazyList[Kind] =
    k match {
      case Type => Type #:: LazyList.empty[Kind]
      case Cons(Arg(v, a), b) =>
        for {
          a1 <- allSuperKinds(a)
          b1 <- allSubKinds(b)
          v1 <- Variance.all
          if (v + v1) == v
        } yield Cons(Arg(v1, a1), b1)
    }

  // allSuperKinds(k).forall(leftSubsumesRight(_, k))
  def allSuperKinds(k: Kind): LazyList[Kind] =
    k match {
      case Type => Type #:: LazyList.empty[Kind]
      case Cons(Arg(v, a), b) =>
        for {
          a1 <- allSubKinds(a)
          b1 <- allSuperKinds(b)
          v1 <- Variance.all
          if (v + v1) == v1
        } yield Cons(Arg(v1, a1), b1)
    }

  def allKinds: LazyList[Kind] = {
    // (0, 0), (0, 1), (1, 0), (0, 2), (1, 1), (2, 0), (0, 3), (1, 2), (2, 1), (3, 0), ...
    def diagonal[A](items: LazyList[A]): LazyList[(A, A)] =
      LazyList
        .from(1)
        .flatMap { diag =>
          // get O(1) indexing into this chunk
          val thisDiag = items.take(diag).to(scala.collection.mutable.Buffer)
          (0 until diag).iterator.map { i =>
            (thisDiag(i), thisDiag(diag - 1 - i))
          }
        }

    // don't make the outer a lazy val or it can never be GC
    lazy val res: LazyList[Kind] = Type #:: (for {
      (k1, k2) <- diagonal(res)
      v <- Variance.all
    } yield Cons(Arg(v, k1), k2))

    res
  }

  private[this] val phantomStr = "👻"
  private[this] val ghostDoc = Doc.text(phantomStr)

  private[this] val arrow = Doc.text(" -> ")
  private[this] def par(d: Doc): Doc =
    Doc.char('(') + (d + Doc.char(')'))

  def argDoc(a: Arg): Doc =
    varDoc(a.variance) + (a.kind match {
      case Type => toDoc(Type)
      case cons =>
        // to get associativity right, need parens
        val inner = toDoc(cons)
        if (a.variance != Variance.in) par(inner) else inner
    })

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

  def varDoc(v: Variance): Doc =
    v match {
      case Variance.Covariant     => Doc.char('+')
      case Variance.Contravariant => Doc.char('-')
      case Variance.Invariant     => Doc.empty
      case Variance.Phantom       => ghostDoc
    }

  val varianceParser: P[Variance] =
    P.fromStringMap(
      Map(
        "+" -> Variance.co,
        "-" -> Variance.contra,
        phantomStr -> Variance.phantom
      )
    )

  val parser: P[Kind] = P.recursive[Kind] { recurse =>
    val ws: P0[Unit] = Parser.maybeSpacesAndLines

    val pType: P[Type.type] = P.char('*').as(Type)

    // a -> b -> c needs to be parsed as a -> (b -> c)
    val arg = pType | Parser.parens(recurse, ws)

    // if we see variance, we know we have a Cons
    val kindArg: P[Arg] =
      (varianceParser ~ arg).map { case (v, i) => Arg(v, i) }

    val rhs = P.string("->") *> (ws *> recurse)
    val varCase: P[Cons] =
      (kindArg ~ (ws *> rhs)).map { case (a, o) =>
        Cons(a, o)
      }
    // with no var, we optionally have -> after
    val noVar: P[Kind] = (arg ~ (ws.soft *> rhs).?).map {
      case (k, None)    => k
      case (i, Some(o)) => Cons(i.in, o)
    }

    varCase | noVar
  }

  // When a kind appears in a struct/enum type parameter list we allow an outer variance
  val paramKindParser: P[Kind.Arg] = {
    // a -> b -> c needs to be parsed as a -> (b -> c)
    val ws = Parser.maybeSpacesAndLines
    val arg = P.char('*').as(Type) | Parser.parens(parser, ws)

    // variance binds tighter than ->
    val kindArg: P[Arg] =
      (varianceParser.orElse(P.pure(Variance.in)).with1 ~ arg).map {
        case (v, i) => Arg(v, i)
      }

    val rhs = P.string("->") *> (ws *> parser)
    (kindArg ~ (ws.soft *> rhs).?).map {
      case (k, None)    => k
      case (i, Some(o)) => Arg(Variance.in, Cons(i, o))
    }
  }

  implicit val orderKind: Order[Kind] =
    new Order[Kind] {
      val ordVar = Order[Variance]
      def compare(left: Kind, right: Kind): Int =
        (left, right) match {
          case (Type, Type)       => 0
          case (Type, _)          => -1
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
