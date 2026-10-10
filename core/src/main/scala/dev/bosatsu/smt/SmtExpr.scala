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
  final case class Div(num: IntExpr, den: IntExpr)
      extends SmtExpr[SmtSort.IntSort]
  final case class Mod(num: IntExpr, den: IntExpr)
      extends SmtExpr[SmtSort.IntSort]

  // Int comparisons
  final case class Lt(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Lte(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Gt(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Gte(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class EqInt(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class EqBool(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]

  // Bool built-ins
  final case class Not(expr: BoolExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class And private (args: Vector[BoolExpr])
      extends SmtExpr[SmtSort.BoolSort]
  object And {
    def apply(args: Vector[BoolExpr]): BoolExpr =
      args.size match {
        case 0 => BoolConst.True
        case 1 => args.head
        case _ => new And(args)
      }

    def apply(first: BoolExpr, second: BoolExpr, rest: BoolExpr*): And =
      new And((first :: second :: rest.toList).toVector)

    def unapply(and: And): Some[Vector[BoolExpr]] =
      Some(and.args)
  }

  final case class Or private (args: Vector[BoolExpr])
      extends SmtExpr[SmtSort.BoolSort]
  object Or {
    def apply(args: Vector[BoolExpr]): BoolExpr =
      args.size match {
        case 0 => BoolConst.False
        case 1 => args.head
        case _ => new Or(args)
      }

    def apply(first: BoolExpr, second: BoolExpr, rest: BoolExpr*): Or =
      new Or((first :: second :: rest.toList).toVector)

    def unapply(or: Or): Some[Vector[BoolExpr]] =
      Some(or.args)
  }

  final case class Xor(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Implies(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]

  private def simplifyBoolExpr(expr: BoolExpr): BoolExpr =
    expr match {
      case BoolConst(_) | EqInt(_, _) | Lt(_, _) | Lte(_, _) | Gt(_, _) |
          Gte(_, _) =>
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
        else And(simp.filterNot(_ == BoolConst.True))
      case Or(args) =>
        val simp = args.map(simplifyBoolExpr)
        if (simp.contains(BoolConst.True)) BoolConst.True
        else Or(simp.filterNot(_ == BoolConst.False))
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

  private def asNegatedTerm(expr: IntExpr): Option[IntExpr] =
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

  private def asSubByTerm(expr: IntExpr): Option[(IntExpr, IntExpr)] =
    expr match {
      case Add(args) =>
        args.toList match {
          case left :: right :: Nil =>
            asNegatedTerm(right)
              .map((left, _))
              .orElse(asNegatedTerm(left).map((right, _)))
          case _ =>
            None
        }
      case Sub(args) =>
        args.toList match {
          case left :: right :: Nil =>
            Some((left, right))
          case _ =>
            None
        }
      case _ =>
        None
    }

  private enum CompareRel derives CanEqual {
    case Lt, Lte, Gt, Gte, Eq
  }

  final private case class CompareFact(
      left: IntExpr,
      rel: CompareRel,
      right: IntExpr
  ) derives CanEqual

  private def directCompareFact(expr: BoolExpr): Option[CompareFact] =
    expr match {
      case Lt(left, right)    => Some(CompareFact(left, CompareRel.Lt, right))
      case Lte(left, right)   => Some(CompareFact(left, CompareRel.Lte, right))
      case Gt(left, right)    => Some(CompareFact(left, CompareRel.Gt, right))
      case Gte(left, right)   => Some(CompareFact(left, CompareRel.Gte, right))
      case EqInt(left, right) => Some(CompareFact(left, CompareRel.Eq, right))
      case _                  => None
    }

  private def flipCompareRel(rel: CompareRel): CompareRel =
    rel match {
      case CompareRel.Lt  => CompareRel.Gt
      case CompareRel.Lte => CompareRel.Gte
      case CompareRel.Gt  => CompareRel.Lt
      case CompareRel.Gte => CompareRel.Lte
      case CompareRel.Eq  => CompareRel.Eq
    }

  private def compareRelImplies(
      found: CompareRel,
      target: CompareRel
  ): Boolean =
    (found, target) match {
      case (a, b) if a == b                => true
      case (CompareRel.Gt, CompareRel.Gte) => true
      case (CompareRel.Lt, CompareRel.Lte) => true
      case (CompareRel.Eq, CompareRel.Gte) => true
      case (CompareRel.Eq, CompareRel.Lte) => true
      case _                               => false
    }

  private def negateCompareFact(fact: CompareFact): Option[CompareFact] =
    fact.rel match {
      case CompareRel.Lt  => Some(fact.copy(rel = CompareRel.Gte))
      case CompareRel.Lte => Some(fact.copy(rel = CompareRel.Gt))
      case CompareRel.Gt  => Some(fact.copy(rel = CompareRel.Lte))
      case CompareRel.Gte => Some(fact.copy(rel = CompareRel.Lt))
      case CompareRel.Eq  => None
    }

  private def eqMatchesSides(
      eqLeft: IntExpr,
      eqRight: IntExpr,
      left: IntExpr,
      right: IntExpr
  ): Boolean =
    ((eqLeft == left) && (eqRight == right)) ||
      ((eqLeft == right) && (eqRight == left))

  private def orCompareFact(args: Vector[BoolExpr]): Option[CompareFact] =
    args.toList match {
      case leftExpr :: rightExpr :: Nil =>
        (directCompareFact(leftExpr), directCompareFact(rightExpr)) match {
          case (
                Some(CompareFact(left, CompareRel.Lt, right)),
                Some(CompareFact(eqLeft, CompareRel.Eq, eqRight))
              ) if eqMatchesSides(eqLeft, eqRight, left, right) =>
            Some(CompareFact(left, CompareRel.Lte, right))
          case (
                Some(CompareFact(eqLeft, CompareRel.Eq, eqRight)),
                Some(CompareFact(left, CompareRel.Lt, right))
              ) if eqMatchesSides(eqLeft, eqRight, left, right) =>
            Some(CompareFact(left, CompareRel.Lte, right))
          case (
                Some(CompareFact(left, CompareRel.Gt, right)),
                Some(CompareFact(eqLeft, CompareRel.Eq, eqRight))
              ) if eqMatchesSides(eqLeft, eqRight, left, right) =>
            Some(CompareFact(left, CompareRel.Gte, right))
          case (
                Some(CompareFact(eqLeft, CompareRel.Eq, eqRight)),
                Some(CompareFact(left, CompareRel.Gt, right))
              ) if eqMatchesSides(eqLeft, eqRight, left, right) =>
            Some(CompareFact(left, CompareRel.Gte, right))
          case _ =>
            None
        }
      case _ =>
        None
    }

  private def compareFact(expr: BoolExpr): Option[CompareFact] =
    directCompareFact(expr)
      .orElse(
        expr match {
          case Not(inner) =>
            compareFact(inner).flatMap(negateCompareFact)
          case Or(args) =>
            orCompareFact(args)
          case _ =>
            None
        }
      )

  private def flattenConjuncts(expr: BoolExpr): Vector[BoolExpr] =
    expr match {
      case And(args) =>
        args.iterator.flatMap(flattenConjuncts).toVector
      case other =>
        Vector(other)
    }

  private def lowerBoundFromCompareFact(
      expr: IntExpr,
      fact: CompareFact
  ): Option[BigInt] = {
    def lowerBoundFromRel(rel: CompareRel, c: BigInt): Option[BigInt] =
      rel match {
        case CompareRel.Gte => Some(c)
        case CompareRel.Gt  => Some(c + 1)
        case CompareRel.Eq  => Some(c)
        case _              => None
      }

    fact match {
      case CompareFact(left, rel, IntConst(c)) if left == expr =>
        lowerBoundFromRel(rel, c)
      case CompareFact(IntConst(c), rel, right) if right == expr =>
        lowerBoundFromRel(flipCompareRel(rel), c)
      case _ =>
        None
    }
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
            asNegatedTerm(right)
              .map(term => Sub(Vector(left, normalizeIntForSolver(term))))
              .orElse(
                asNegatedTerm(left)
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
      case Or(args) =>
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
    val flatFacts = facts.flatMap(flattenConjuncts)
    val compareFacts = flatFacts.flatMap(compareFact)

    def hasComparison(left: IntExpr, rel: CompareRel, right: IntExpr): Boolean =
      compareFacts.exists { fact =>
        val orientedRelOpt =
          if ((fact.left == left) && (fact.right == right)) Some(fact.rel)
          else if ((fact.left == right) && (fact.right == left)) {
            Some(flipCompareRel(fact.rel))
          } else None
        orientedRelOpt.exists(compareRelImplies(_, rel))
      }

    def hasLowerBound(expr: IntExpr, lower: BigInt): Boolean =
      compareFacts.exists { fact =>
        lowerBoundFromCompareFact(expr, fact).exists(_ >= lower)
      }

    def isStrictlyPositive(expr: IntExpr): Boolean =
      hasLowerBound(expr, BigInt(1))

    facts.contains(goal1) ||
    flatFacts.contains(goal1) ||
    compareFact(goal1).exists(g => hasComparison(g.left, g.rel, g.right)) ||
    (goal1 match {
      case Gte(left, right) =>
        hasComparison(left, CompareRel.Gte, right) ||
        (right match {
          case IntConst(zero) if zero == BigInt(0) =>
            asSubByTerm(left).exists { case (base, by) =>
              hasComparison(base, CompareRel.Gte, by) ||
              (by match {
                case IntConst(c) => hasLowerBound(base, c)
                case _           => false
              })
            }
          case _ =>
            false
        })
      case Lte(left, right) =>
        hasComparison(left, CompareRel.Lte, right)
      case Lt(left, right) =>
        hasComparison(left, CompareRel.Lt, right) ||
        asSubByTerm(left).exists { case (base, by) =>
          (base == right) &&
          (by match {
            case IntConst(c) => c > 0
            case _           => isStrictlyPositive(by)
          })
        }
      case _ =>
        false
    })
  }
}
