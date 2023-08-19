package org.bykn.bosatsu

import cats.data.{Chain, Validated, ValidatedNec}
import scala.collection.immutable.SortedSet

import cats.syntax.all._

object NatConstraint {
  sealed trait Var {
    type V <: Value
    def name: String
    def castV(v: Value): Option[V]
  }
  object Var {
    type Aux[V1 <: Value] = Var { type V = V1 }
  }

  sealed abstract class Issue
  object Issue {
    case class TypeMismatch(thisVar: Var, value: Value) extends Issue
    case class UnboundVar(v: Var) extends Issue
  }

  sealed abstract class Scope {
    def get(v: Var): Either[Value, Option[(v.V, Scope)]]
    def updated[V1 <: Value](v: Var { type V = V1 }, value: V1): Scope
  }

  object Scope {
    private case class Impl(toMap: Map[String, (Value, Scope)]) extends Scope {
      def get(thisVar: Var): Either[Value, Option[(thisVar.V, Scope)]] =
        toMap.get(thisVar.name) match {
          case Some((v, s)) =>
            val maybeV: Option[thisVar.V] = thisVar.castV(v)
            maybeV match {
              case Some(v1) => Right(Some((v1, s)))
              case None => Left(v)
            }
          case None => Right(None)
        }

      def updated[V1 <: Value](v: Var { type V = V1 }, value: V1): Scope =
        Impl(toMap.updated(v.name, (value, this)))
    }

    val empty: Scope = Impl(Map.empty)
  }

  sealed abstract class Unsat[A]
  object Unsat {
    case class FromIssue[A](toIssue: Issue) extends Unsat[A]
  }

  sealed abstract class Value {
    def freeVars: Set[Var]
  }

  sealed abstract class Bool extends Value {
    def &(that: Bool): Bool = Bool.And(this, that)
    def |(that: Bool): Bool = Bool.Or(this, that)
    def unary_! : Bool = Bool.Not(this)
    def ===(that: Bool): Bool =
      (this & that) | !(this | that)

    def freeVars: Set[Var] =
      Bool.freeVars(this, Set.empty).iterator.toSet
  }

  sealed abstract class Nat extends Value {
    def +(that: Nat): Nat = Nat.Sum(this, that)
    def *(that: Nat): Nat = Nat.Prod(this, that)
    def <(that: Nat): Bool = Bool.LessThan(this, that)
    def <=(that: Nat): Bool = Bool.LessThanEquals(this, that)
    def ===(that: Nat): Bool = Bool.Equals(this, that)

    def >(that: Nat): Bool = that < this
    def >=(that: Nat): Bool = that <= this

    def freeVars: Set[Var] =
      Nat.freeVars(this, Set.empty).iterator.toSet
  }

  sealed abstract class BuildVar[V1 <: Value] {
    def forName(name: String): V1 with Var { type V = V1 }
  }
  object BuildVar {
    implicit val buildVarBool: BuildVar[Bool] =
      new BuildVar[Bool] {
        def forName(name: String) = Bool.VarBool(name)
      }
    implicit val buildVarNat: BuildVar[Nat] =
      new BuildVar[Nat] {
        def forName(name: String) = Nat.VarNat(name)
      }

    def forName[V1 <: Value](name: String)(implicit bv: BuildVar[V1]): V1 with Var { type V = V1 } =
      bv.forName(name)
  }

  object Bool {
    def apply(b: Boolean): Bool = if (b) True else False

    def let[V <: Value: BuildVar](name: String, b: V)(fn: V => Bool): Bool = {
      val thisVar: V with Var.Aux[V] = BuildVar.forName[V](name)
      Let[V](thisVar, b, fn(thisVar))
    }

    case object True extends Bool
    case object False extends Bool
    case class VarBool(name: String) extends Bool with Var {
      type V = Bool
      def castV(v: Value): Option[Bool] =
        v match {
          case b: Bool => Some(b)
          case _ => None
        }
    }
    case class And(left: Bool, right: Bool) extends Bool
    case class Or(left: Bool, right: Bool) extends Bool
    case class Not(arg: Bool) extends Bool
    case class LessThan(left: Nat, right: Nat) extends Bool
    case class LessThanEquals(left:  Nat, right: Nat) extends Bool
    case class Equals(left: Nat, right: Nat) extends Bool
    case class Let[V1 <: Value](v: Var.Aux[V1], value: V1, in: Bool) extends Bool {
      def update(s: Scope): Scope = s.updated[V1](v, value)
    }

    def freeVars(b: Bool, bound: Set[String]): Chain[Var] =
      b match {
        case True | False => Chain.empty
        case v@VarBool(name) =>
          if (bound.contains(name)) Chain.empty
          else Chain.one(v)
        case And(left, right) =>
          freeVars(left, bound) ++ freeVars(right, bound)
        case Or(left, right) =>
          freeVars(left, bound) ++ freeVars(right, bound)
        case Not(arg) => freeVars(arg, bound)
        case LessThan(left, right) =>
          Nat.freeVars(left, bound) ++ Nat.freeVars(right, bound)
        case LessThanEquals(left, right) =>
          Nat.freeVars(left, bound) ++ Nat.freeVars(right, bound)
        case Equals(left, right) =>
          Nat.freeVars(left, bound) ++ Nat.freeVars(right, bound)
        case Let(name, _, in) =>
          freeVars(in, bound + name.name)
      }

    def eval(b: Bool, scope: Scope): ValidatedNec[Issue, Boolean] =
      b match {
        case True => Validated.valid(true)
        case False => Validated.valid(false)
        case v@VarBool(_) =>
          scope.get(v) match {
            case Right(Some((b, s1))) => eval(b, s1)
            case Right(None) => Validated.invalidNec(Issue.UnboundVar(v))
            case Left(n) => Validated.invalidNec(Issue.TypeMismatch(v, n))
          }
        case And(left, right) =>
          (eval(left, scope), eval(right, scope)).mapN(_ && _)
        case Or(left, right) =>
          (eval(left, scope), eval(right, scope)).mapN(_ || _)
        case Not(arg) =>
          eval(arg, scope).map(b => !b)
        case LessThan(left, right) =>
          (Nat.eval(left, scope), Nat.eval(right, scope)).mapN(_ < _)
        case LessThanEquals(left, right) =>
          (Nat.eval(left, scope), Nat.eval(right, scope)).mapN(_ <= _)
        case Equals(left, right) =>
          (Nat.eval(left, scope), Nat.eval(right, scope)).mapN(_ == _)
        case l @ Let(_, _, in) =>
          eval(in, l.update(scope))
      }
  }

  object Nat {
    def apply(i: Int): Nat = {
      require(i >= 0, s"require i >= 0: $i")
      FromBigInt(BigInt(i))
    }

    case class FromBigInt(toBigInt: BigInt) extends Nat {
      require(toBigInt >= 0, s"require non-negative numbers, found: $toBigInt")
    }
    case class VarNat(name: String) extends Nat with Var {
      type V = Nat
      def castV(v: Value): Option[Nat] =
        v match {
          case n: Nat => Some(n)
          case _ => None
        }
    }

    case class Sum(left: Nat, right: Nat) extends Nat
    case class Prod(left: Nat, right: Nat) extends Nat
    case class Let[V1 <: Value](v: Var.Aux[V1], value: V1, in: Nat) extends Nat {
      def update(s: Scope): Scope = s.updated[V1](v, value)
    }

    def let[V <: Value: BuildVar](name: String, b: V)(fn: V => Nat): Nat = {
      val thisVar = BuildVar.forName[V](name)
      Let(thisVar, b, fn(thisVar))
    }

    def freeVars(b: Nat, bound: Set[String]): Chain[Var] =
      b match {
        case FromBigInt(_) => Chain.empty
        case v@VarNat(name) =>
          if (bound.contains(name)) Chain.empty
          else Chain.one(v)
        case Sum(left, right) =>
          freeVars(left, bound) ++ freeVars(right, bound)
        case Prod(left, right) =>
          freeVars(left, bound) ++ freeVars(right, bound)
        case Let(name, _, in) => 
          freeVars(in, bound + name.name)
      }

    def eval(b: Nat, scope: Scope): ValidatedNec[Issue, BigInt] =
      b match {
        case FromBigInt(b) => Validated.valid(b)
        case variable@VarNat(_) =>
          scope.get(variable) match {
            case Right(Some((n, s1))) => eval(n, s1)
            case Right(None) => Validated.invalidNec(Issue.UnboundVar(variable))
            case Left(v) => Validated.invalidNec(Issue.TypeMismatch(variable, v))
          }
        case Sum(left, right) =>
          (eval(left, scope), eval(right, scope)).mapN(_ + _)
        case Prod(left, right) =>
          (eval(left, scope), eval(right, scope)).mapN(_ * _)
        case l: Let[v] =>
          eval(l.in, scope.updated[v](l.v, l.value))
      }
  }

  sealed abstract class Binding {
    type V <: Value
    def variable: Var.Aux[V]
    def value: V
  }
  object Binding {
    def apply[V1 <: Value](vari: Var.Aux[V1], valu: V1): Binding =
      new Binding {
        type V = V1
        def variable = vari
        def value = valu
      }
  }

  sealed abstract class NatRange  {
    def &(that: NatRange): NatRange
    def contains(bi: BigInt): Boolean
    def containsAll(that: NatRange): Boolean =
      (this & that) == that

    def +(that: NatRange): NatRange
    def *(that: NatRange): NatRange
  }
  sealed abstract class NonEmptyNatRange extends NatRange
  object NatRange {
    def empty: NatRange = Empty
    val everything: NonEmptyNatRange = LowerBounded(BigInt(0))
    def one(b: BigInt): NonEmptyNatRange = Bounded(b, b + 1)

    case object Empty extends NatRange {
      def &(that: NatRange) = this
      def contains(bi: BigInt): Boolean = false
      def +(that: NatRange): NatRange = Empty
      def *(that: NatRange): NatRange = Empty
    }
    case class LowerBounded(inclusiveLower: BigInt) extends NonEmptyNatRange {
      require(inclusiveLower >= 0)
      def &(that: NatRange): NatRange =
        that match {
          case Empty => Empty
          case b @ Bounded(low, up) =>
            if (up <= inclusiveLower) Empty
            else if (inclusiveLower <= low) b
            else Bounded(inclusiveLower, up)
          case LowerBounded(inLow) => LowerBounded(inclusiveLower max inLow)
        }
      def contains(bi: BigInt): Boolean = inclusiveLower <= bi
      def +(that: NatRange): NatRange =
        that match {
          case Empty => Empty
          case LowerBounded(low) => LowerBounded(inclusiveLower + low)
          case Bounded(low, _) => LowerBounded(inclusiveLower + low)
        }
      def *(that: NatRange): NatRange =
        that match {
          case Empty => Empty
          case LowerBounded(low) => LowerBounded(inclusiveLower * low)
          case b@Bounded(low, _) =>
            if (b.isZero) b
            else LowerBounded(inclusiveLower * low)
        }
    }
    case class Bounded(inclusiveLower: BigInt, exclusiveUpper: BigInt) extends NonEmptyNatRange {
      require(inclusiveLower >= 0)
      require(inclusiveLower < exclusiveUpper)

      def isZero: Boolean = (inclusiveLower == BigInt(0)) && (exclusiveUpper == BigInt(1))

      def &(that: NatRange): NatRange =
        that match {
          case Empty => Empty
          case lb@LowerBounded(_) => lb & this
          case Bounded(il, xu) =>
            if (xu <= inclusiveLower || exclusiveUpper <= inclusiveLower) Empty
            else Bounded(il max inclusiveLower, xu min exclusiveUpper)
        }

      def contains(bi: BigInt): Boolean = (inclusiveLower <= bi) && (bi < exclusiveUpper)

      def +(that: NatRange): NatRange =
        that match {
          case Empty => Empty
          case LowerBounded(low) => LowerBounded(inclusiveLower + low)
          case b@Bounded(low, up) =>
            if (b.isZero) this
            else if (isZero) that
            else Bounded(low + inclusiveLower, up + exclusiveUpper - 1)
        }
      def *(that: NatRange): NatRange =
        that match {
          case Empty => Empty
          case LowerBounded(low) => LowerBounded(inclusiveLower * low)
          case Bounded(low, up) =>
            Bounded(low * inclusiveLower, (up - 1) * (exclusiveUpper - 1) + 1)
        }
    }

    implicit val natRangeOrd: Ordering[NatRange] =
      new Ordering[NatRange] {
        def compare(x: NatRange, y: NatRange): Int =
          x match {
            case LowerBounded(inclusiveLower) =>
              y match {
                case LowerBounded(thatLow) => inclusiveLower.compare(thatLow)
                case Bounded(low, _) =>
                  val c = inclusiveLower.compare(low)
                  if (c == 0) {
                    // but infinite ranges after
                    1
                  }
                  else c
                case Empty => 1
              }
            case Bounded(low, up) =>
              y match {
                case LowerBounded(ylow) =>
                  val c = low.compare(ylow)
                  if (c == 0) -1 // infinite ranges > finite ranges at the same spot 
                  else c
                case Bounded(ylow, yup) =>
                  val clow = low.compare(ylow)
                  if (clow != 0) clow
                  else {
                    up.compare(yup)
                  }
                case Empty => 1
              }
            case Empty => if (y == Empty) 0 else -1
          }
      }

    implicit val neNatRangeOrd: Ordering[NonEmptyNatRange] =
      new Ordering[NonEmptyNatRange] {
        def compare(x: NonEmptyNatRange, y: NonEmptyNatRange): Int = natRangeOrd.compare(x, y)
      }
  }

  case class NatSet(toSortedSet: SortedSet[NonEmptyNatRange]) {
    def &(that: NatSet): NatSet = {
      // leverage sorting
      val left = toSortedSet.iterator.buffered
      val right = that.toSortedSet.iterator.buffered
      val bldr = SortedSet.newBuilder[NonEmptyNatRange]
      while(left.hasNext && right.hasNext) {
        val c = Ordering[NatRange].compare(left.head, right.head)
        if (c < 0) {
          // left < right
          val thisIntr = left.head & right.head
        }
        else if (c > 0) {
          // right < left
        }
        else {
          // left == right
          val item = left.next()
          bldr += item
          right.next()
        }
      }
      NatSet(bldr.result())
    }
  }

  object NatSet {
    def fromNatRange(nr: NatRange): NatSet =
      nr match {
        case NatRange.Empty => empty
        case nr: NonEmptyNatRange => NatSet(SortedSet(nr))
      }
    def one(bi: BigInt): NatSet = fromNatRange(NatRange.one(bi))
    val empty: NatSet = NatSet(SortedSet.empty)
    val everything: NatSet = fromNatRange(NatRange.everything)
  }

  def sat[A](constraint: Bool): ValidatedNec[Unsat[A], LazyList[Scope]] = {
    val frees = constraint.freeVars
    // we need to find bindings for all of these
    // trivial approach is create all possible values, set them up as a scope, and filter
    // to see if any satisfy... this is too trivial since it will never terminate
    // for Nats
    ???
  }

  object Princess {
    import ap.api.SimpleAPI
    import ap.types.Sort
    import ap.parser.{IFunction, IVariable}
    import ap.basetypes.IdealInt

    /*
    enum Vec[s: Nat, a]:
      VecEmpty where s == 0
      VecCons(forall b. (forall s0: Nat where s0 + 1 == s. (a, Vec[s0, a]) -> b) -> b)

    def cons[s: Nat, a](a, v: Vec[s, a]) -> Vec[s + 1, a]:
      VecCons(fn -> fn(a, v))

    def cat[s1: Nat, s2: Nat, a](v1: Vec[s1, a], v2: Vec[s2, a]) -> Vec[s1 + s2, a]:
      match v1:
        case VecEmpty:
          # we know s1 == 0 here
          v2
        case VecCons(on_cons):
          match v2:
            case VecEmpty:
              # here we know that s2 == 0
              v1
            case _:
              on_cons((head, tail) -> cons(head, cat(tail, v2)))


    def cat_cons[s1: Nat, s2: Nat, a](item: a, v1: Vec[s1, a], v2: Vec[s2, a]) -> Vec[s1 + s2 + 1, a]:
      vc = cat(v1, v2)
      cons(a, vc)
    */
    SimpleAPI.withProver { api =>
      val x = api.createConstant(Sort.Nat)
      val y = api.createConstant(Sort.Nat)
      val z = api.createConstant(Sort.Nat)
      api.makeExistential(List(x, y))
      val f1 = (x + y + IdealInt.ONE * 3) === z
      val f2 = z === (y + x + IdealInt.ONE * 3)
      api.addAssertion(f1)
      api.addAssertion(f2)
      println(api.checkSat(true))
    }
  }
}