package org.bykn.bosatsu

import cats.data.{Chain, Validated, ValidatedNec}

import cats.syntax.all._

object NatConstraint {
  sealed trait Var {
    def name: String
  }

  sealed abstract class Issue
  object Issue {
    case class TypeMismatch(thisVar: Var, value: Value) extends Issue
    case class UnboundVar(v: Var) extends Issue
  }

  sealed abstract class Scope {
    def get(name: String): Option[(Value, Scope)]
    def updated(name: String, value: Value): Scope
  }

  object Scope {
    private case class Impl(toMap: Map[String, (Value, Scope)]) extends Scope {
      def get(name: String): Option[(Value, Scope)] = toMap.get(name)
      def updated(name: String, value: Value) =
        Impl(toMap.updated(name, (value, this)))
    }

    val empty: Scope = Impl(Map.empty)
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

  sealed abstract class BuildVar[V <: Value] {
    def forName(name: String): V
  }
  object BuildVar {
    implicit val buildVarBool: BuildVar[Bool] =
      new BuildVar[Bool] {
        def forName(name: String): Bool = Bool.VarBool(name)
      }
    implicit val buildVarNat: BuildVar[Nat] =
      new BuildVar[Nat] {
        def forName(name: String): Nat = Nat.VarNat(name)
      }

    def forName[V <: Value](name: String)(implicit bv: BuildVar[V]): V =
      bv.forName(name)
  }

  object Bool {
    def apply(b: Boolean): Bool = if (b) True else False

    def let[V <: Value: BuildVar](name: String, b: V)(fn: V => Bool): Bool =
      Let(name, b, fn(BuildVar.forName(name)))

    case object True extends Bool
    case object False extends Bool
    case class VarBool(name: String) extends Bool with Var
    case class And(left: Bool, right: Bool) extends Bool
    case class Or(left: Bool, right: Bool) extends Bool
    case class Not(arg: Bool) extends Bool
    case class LessThan(left: Nat, right: Nat) extends Bool
    case class LessThanEquals(left:  Nat, right: Nat) extends Bool
    case class Equals(left: Nat, right: Nat) extends Bool
    case class Let(name: String, value: Value, in: Bool) extends Bool

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
          freeVars(in, bound + name)
      }

    def eval(b: Bool, scope: Scope): ValidatedNec[Issue, Boolean] =
      b match {
        case True => Validated.valid(true)
        case False => Validated.valid(false)
        case v@VarBool(name) =>
          scope.get(name) match {
            case Some((b: Bool, s1)) => eval(b, s1)
            case Some((n: Nat, _)) => Validated.invalidNec(Issue.TypeMismatch(v, n))
            case None => Validated.invalidNec(Issue.UnboundVar(v))
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
        case Let(name, value, in) =>
          eval(in, scope.updated(name, value))
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
    case class VarNat(name: String) extends Nat with Var

    case class Sum(left: Nat, right: Nat) extends Nat
    case class Prod(left: Nat, right: Nat) extends Nat
    case class Let(name: String, value: Value, in: Nat) extends Nat

    def let[V <: Value: BuildVar](name: String, b: V)(fn: V => Nat): Nat =
      Let(name, b, fn(BuildVar.forName(name)))

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
          freeVars(in, bound + name)
      }

    def eval(b: Nat, scope: Scope): ValidatedNec[Issue, BigInt] =
      b match {
        case FromBigInt(b) => Validated.valid(b)
        case v@VarNat(name) =>
          scope.get(name) match {
            case Some((n: Nat, s1)) => eval(n, s1)
            case Some((b: Bool, _)) => Validated.invalidNec(Issue.TypeMismatch(v, b))
            case None => Validated.invalidNec(Issue.UnboundVar(v))
          }
        case Sum(left, right) =>
          (eval(left, scope), eval(right, scope)).mapN(_ + _)
        case Prod(left, right) =>
          (eval(left, scope), eval(right, scope)).mapN(_ * _)
        case Let(name, value, in) => 
          eval(in, scope.updated(name, value))
      }
  }
}