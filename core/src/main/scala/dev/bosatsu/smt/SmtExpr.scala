package dev.bosatsu.smt

sealed trait SmtExpr[S <: SmtSort] derives CanEqual
object SmtExpr {
  type IntExpr = SmtExpr[SmtSort.IntSort]
  type BoolExpr = SmtExpr[SmtSort.BoolSort]

  final case class IntConst(value: BigInt) extends SmtExpr[SmtSort.IntSort]
  final case class BoolConst private (value: Boolean)
      extends SmtExpr[SmtSort.BoolSort]
  object BoolConst {
    val True: BoolConst = new BoolConst(true)
    val False: BoolConst = new BoolConst(false)
    inline def apply(b: Boolean): BoolConst = if (b) True else False
  }

  final case class Var[S <: SmtSort](name: String) extends SmtExpr[S]
  final case class App[S <: SmtSort](name: String, args: Vector[SmtExpr[?]])
      extends SmtExpr[S]

  final case class Ite[S <: SmtSort](
      cond: BoolExpr,
      ifTrue: SmtExpr[S],
      ifFalse: SmtExpr[S]
  ) extends SmtExpr[S]

  // Int built-ins
  final case class Add(args: Vector[IntExpr]) extends SmtExpr[SmtSort.IntSort]
  final case class Sub(args: Vector[IntExpr]) extends SmtExpr[SmtSort.IntSort]
  final case class Mul(args: Vector[IntExpr]) extends SmtExpr[SmtSort.IntSort]
  final case class Div(num: IntExpr, den: IntExpr) extends SmtExpr[SmtSort.IntSort]
  final case class Mod(num: IntExpr, den: IntExpr) extends SmtExpr[SmtSort.IntSort]

  // Int comparisons
  final case class Lt(left: IntExpr, right: IntExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class Lte(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Gt(left: IntExpr, right: IntExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class Gte(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class EqInt(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class EqBool(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]

  // Bool built-ins
  final case class Not(expr: BoolExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class And(args: Vector[BoolExpr]) extends SmtExpr[SmtSort.BoolSort]
  final case class Or(args: Vector[BoolExpr]) extends SmtExpr[SmtSort.BoolSort]
  final case class Xor(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Implies(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]

  private def mkAnd(args: Vector[BoolExpr]): BoolExpr =
    args.size match {
      case 0 => BoolConst.True
      case 1 => args.head
      case _ => And(args)
    }

  private def mkOr(args: Vector[BoolExpr]): BoolExpr =
    args.size match {
      case 0 => BoolConst.False
      case 1 => args.head
      case _ => Or(args)
    }

  private def simplifyBoolExpr(expr: BoolExpr): BoolExpr =
    expr match {
      case BoolConst(_) | EqInt(_, _) | Lt(_, _) |
          Lte(_, _) | Gt(_, _) | Gte(_, _) =>
        expr
      case Not(in) =>
        simplifyBoolExpr(in) match {
          case BoolConst(value) => BoolConst(!value)
          case Not(in1)         => in1
          case other            => Not(other)
        }
      case And(args) =>
        val simp = args.map(simplifyBoolExpr)
        if (simp.contains(BoolConst.False)) BoolConst.False
        else mkAnd(simp.filterNot(_ == BoolConst.True))
      case Or(args)  =>
        val simp = args.map(simplifyBoolExpr)
        if (simp.contains(BoolConst.True)) BoolConst.True
        else mkOr(simp.filterNot(_ == BoolConst.False))
      case Ite(cond, ifTrue, ifFalse) =>
        val c = simplifyBoolExpr(cond)
        val t = simplifyBoolExpr(ifTrue)
        val f = simplifyBoolExpr(ifFalse)
        (c, t, f) match {
          case (_, BoolConst.True, BoolConst.False) =>
            c
          case (_, BoolConst.False, BoolConst.True) =>
            simplifyBoolExpr(Not(c))
          case (BoolConst.True, _, _) =>
            t
          case (BoolConst.False, _, _) =>
            f
          case _ if t == f =>
            t
          case _ =>
            Ite(c, t, f)
        }
      case _ =>
        expr
    }

  private def asNegativeConstMul(expr: IntExpr): Option[BigInt] =
    expr match {
      case Mul(args) =>
        args.toList match {
          case IntConst(negOne) :: IntConst(n) :: Nil
              if (negOne == BigInt(-1)) && (n > 0) =>
            Some(n)
          case IntConst(n) :: IntConst(negOne) :: Nil
              if (negOne == BigInt(-1)) && (n > 0) =>
            Some(n)
          case _ =>
            None
        }
      case _ =>
        None
    }

  private def asNegativeConst(expr: IntExpr): Option[BigInt] =
    expr match {
      case IntConst(n) if n < 0 =>
        Some(-n)
      case _ =>
        None
    }

  private def asSubByPositiveConst(expr: IntExpr): Option[(IntExpr, BigInt)] =
    expr match {
      case Add(args) =>
        args.toList match {
          case left :: right :: Nil =>
            asNegativeConstMul(right)
              .orElse(asNegativeConst(right))
              .map((left, _))
              .orElse(
                asNegativeConstMul(left)
                  .orElse(asNegativeConst(left))
                  .map((right, _))
              )
          case _ =>
            None
        }
      case Sub(args) =>
        args.toList match {
          case left :: IntConst(n) :: Nil if n > 0 =>
            Some((left, n))
          case _ =>
            None
        }
      case _ =>
        None
    }

  private def asNegatedTermForNormalization(expr: IntExpr): Option[IntExpr] =
    expr match {
      case Mul(args) =>
        args.toList match {
          case IntConst(negOne) :: term :: Nil if negOne == BigInt(-1) =>
            Some(term)
          case term :: IntConst(negOne) :: Nil if negOne == BigInt(-1) =>
            Some(term)
          case _ =>
            None
        }
      case IntConst(n) if n < 0 =>
        Some(IntConst(-n))
      case _ =>
        None
    }

  def normalizeIntForSolver(expr: IntExpr): IntExpr = {
    val normalized: IntExpr =
      expr match {
        case Add(args) =>
          Add(args.map(normalizeIntForSolver))
        case Sub(args) =>
          Sub(args.map(normalizeIntForSolver))
        case Mul(args) =>
          Mul(args.map(normalizeIntForSolver))
        case Div(num, den) =>
          Div(
            normalizeIntForSolver(num),
            normalizeIntForSolver(den)
          )
        case Mod(num, den) =>
          Mod(
            normalizeIntForSolver(num),
            normalizeIntForSolver(den)
          )
        case Ite(cond, ifTrue, ifFalse) =>
          Ite(
            normalizeBoolForSolver(cond),
            normalizeIntForSolver(ifTrue),
            normalizeIntForSolver(ifFalse)
          )
        case other =>
          other
      }

    normalized match {
      case Add(args) =>
        args.toList match {
          case left :: right :: Nil =>
            asNegatedTermForNormalization(right)
              .map(term => Sub(Vector(left, normalizeIntForSolver(term))))
              .orElse(
                asNegatedTermForNormalization(left)
                  .map(term => Sub(Vector(right, normalizeIntForSolver(term))))
              )
              .getOrElse(normalized)
          case _ =>
            normalized
        }
      case _ =>
        normalized
    }
  }

  def normalizeBoolForSolver(expr: BoolExpr): BoolExpr =
    expr match {
      case EqInt(left, right) =>
        EqInt(
          normalizeIntForSolver(left),
          normalizeIntForSolver(right)
        )
      case EqBool(left, right) =>
        EqBool(
          normalizeBoolForSolver(left),
          normalizeBoolForSolver(right)
        )
      case Lt(left, right) =>
        Lt(
          normalizeIntForSolver(left),
          normalizeIntForSolver(right)
        )
      case Lte(left, right) =>
        Lte(
          normalizeIntForSolver(left),
          normalizeIntForSolver(right)
        )
      case Gt(left, right) =>
        Gt(
          normalizeIntForSolver(left),
          normalizeIntForSolver(right)
        )
      case Gte(left, right) =>
        Gte(
          normalizeIntForSolver(left),
          normalizeIntForSolver(right)
        )
      case Not(inner) =>
        Not(normalizeBoolForSolver(inner))
      case And(args) =>
        And(args.map(normalizeBoolForSolver))
      case Or(args)  =>
        Or(args.map(normalizeBoolForSolver))
      case Xor(left, right) =>
        Xor(
          normalizeBoolForSolver(left),
          normalizeBoolForSolver(right)
        )
      case Implies(left, right) =>
        Implies(
          normalizeBoolForSolver(left),
          normalizeBoolForSolver(right)
        )
      case Ite(cond, ifTrue, ifFalse) =>
        Ite(
          normalizeBoolForSolver(cond),
          normalizeBoolForSolver(ifTrue),
          normalizeBoolForSolver(ifFalse)
        )
      case other =>
        other
    }

  def pathImplies(goal: BoolExpr, facts0: Iterable[BoolExpr]): Boolean = {
    val goal1 = simplifyBoolExpr(goal)
    val facts = facts0.iterator.map(simplifyBoolExpr).toVector

    def hasFact(p: BoolExpr => Boolean): Boolean =
      facts.exists(p)

    def hasGte(base: IntExpr, lower: BigInt): Boolean =
      hasFact {
        case Gte(l, IntConst(c)) => (l == base) && (c >= lower)
        case Gt(l, IntConst(c))  => (l == base) && (c >= (lower - 1))
        case EqInt(l, IntConst(c)) => (l == base) && (c >= lower)
        case _                    => false
      }

    facts.contains(goal1) ||
    (goal1 match {
      case Gte(left, right) =>
        hasFact {
          case Gt(l1, r1)    => (l1 == left) && (r1 == right)
          case EqInt(l1, r1) => (l1 == left) && (r1 == right)
          case _             => false
        } ||
          (right match {
            case IntConst(zero) if zero == BigInt(0) =>
              asSubByPositiveConst(left).exists { case (base, by) =>
                hasGte(base, by)
              }
            case _ =>
              false
          })
      case Lte(left, right) =>
        hasFact {
          case Lt(l1, r1)    => (l1 == left) && (r1 == right)
          case EqInt(l1, r1) => (l1 == left) && (r1 == right)
          case _             => false
        }
      case Lt(left, right)  =>
        asSubByPositiveConst(left).exists { case (base, by) =>
          (base == right) && (by > 0)
        }
      case _ =>
        false
    })
  }
}
