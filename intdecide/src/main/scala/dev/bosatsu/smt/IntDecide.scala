package dev.bosatsu.smt

import cats.syntax.all.*
import scala.collection.immutable.{IntMap, SortedMap}
import scala.collection.mutable

import SmtExpr.*

/** Decides whether facts imply a goal over the integers, for the Int
  * obligations of the recursion check, without an external solver.
  *
  * Terms mean what they mean in the SMT-LIB that `SmtToSExpr` renders:
  * `div` and `mod` by a nonzero constant `k` are Euclidean (`n == k * q + r`
  * with `0 <= r < |k|`), `+`, `-` and `*` with one argument are that
  * argument, and Int and Bool variables are separate even when they share a
  * name. Bosatsu's own `div` and `mod_Int` are built from these by
  * `SmtExpr.bosatsuDiv` and `bosatsuMod`.
  *
  * The fragment it decides is linear integer arithmetic with booleans: Int
  * constants and variables, `+`, `-`, multiplication by a constant, `div`
  * and `mod` by a nonzero constant, `ite`, comparisons, Bool constants and
  * variables, and every connective. It works in three steps:
  *
  *   1. `SmtExpr.pathImplies` (with `LinearBounds`) proves the common goals
  *      quickly.
  *   2. Otherwise the facts and the negated goal are put in negation normal
  *      form over linear constraints. Each `div` or `mod` by a constant `k`
  *      becomes a fresh quotient `q` and remainder `r` with
  *      `n == k * q + r` and `0 <= r <= |k| - 1`, and each Int `ite` a fresh
  *      variable equal to one branch or the other.
  *   3. A search looks for values making all of that true. Constraints that
  *      must hold go to the Omega test (`Omega`), which is exact over the
  *      integers. A disjunction is only split when the values found so far
  *      make it false, so disjunctions that don't matter cost nothing.
  *
  * No values means the goal is proved. Values found are checked on the
  * facts and goal themselves before they are reported as a counterexample.
  *
  * Terms outside the fragment (products of variables, division by a variable
  * or by zero, uninterpreted functions) are read as unknown integers or
  * booleans, the same one for the same term. That keeps every proof sound,
  * since it only forgets what the term is, but values found that way may not
  * be a real counterexample; those that aren't give `Unknown`.
  *
  * The work is bounded by a limit: reaching it gives `Unknown`, which the
  * recursion check reports as a failure, as it did for z3's `unknown`.
  */
object IntDecide {

  /** A value for every variable of the facts and goal (those only inside an
    * uninterpreted function's arguments aside).
    */
  final case class Model(
      ints: SortedMap[String, BigInt],
      bools: SortedMap[String, Boolean]
  ) derives CanEqual

  enum Reason derives CanEqual {

    /** The work limit was reached. */
    case WorkLimit

    /** Values were found that make the facts true and the goal false when
      * the terms outside the fragment are read as unknowns, but not for the
      * terms themselves.
      */
    case OutsideFragment

    /** Values were found for terms in the fragment that don't make the facts
      * true and the goal false. This is a bug in this procedure, never a
      * property of the goal; it is reported rather than thrown so the
      * compiler still explains what it couldn't prove.
      */
    case UnverifiedCounterexample

    def message: String =
      this match {
        case WorkLimit                =>
          "the Int decision procedure reached its work limit"
        case OutsideFragment          =>
          "the obligation depends on terms outside linear integer arithmetic, such as a product of variables or division by a variable"
        case UnverifiedCounterexample =>
          "internal error: the Int decision procedure found values that do not refute the obligation"
      }
  }

  enum Result derives CanEqual {
    case Proved
    case Refuted(counterexample: Model)
    case Unknown(reason: Reason)
  }

  /** About 100 times the most work any obligation from test_workspace or the
    * recursion check's tests needs (under 2,000), and about 0.2 seconds on a
    * laptop when a call runs out.
    */
  val DefaultWorkLimit: Long = 200000L

  /** Whether the facts imply the goal.
    *
    * Laws, for every goal, facts and limit:
    *   - if the result is `Proved`, the goal holds at every value of the
    *     variables where every fact holds, under every interpretation of
    *     division by zero
    *   - if the result is `Refuted(m)`, every fact holds at `m` and the goal
    *     doesn't, under every interpretation of division by zero
    *   - for facts and a goal in the fragment, with a limit large enough, the
    *     result is never `Unknown`; so then it is `Proved` exactly when the
    *     goal follows
    */
  def implies(
      goal: BoolExpr,
      facts: Iterable[BoolExpr],
      workLimit: Long
  ): Result = {
    val factList = facts.toVector
    if (SmtExpr.pathImplies(goal, factList)) Result.Proved
    else decide(goal, factList, workLimit)
  }

  private def decide(
      goal: BoolExpr,
      facts: Vector[BoolExpr],
      workLimit: Long
  ): Result = {
    val budget = Omega.Budget(workLimit)
    val tr = new Translator(budget)
    // numbered in order of appearance, including those translating drops,
    // such as the condition of an ite with equal branches
    facts.foreach(tr.registerBool)
    tr.registerBool(goal)
    val parts = facts.map(tr.bool(_, positive = true)) :+ tr.bool(goal, positive = false)
    // the definitions of the fresh variables, built while translating
    val formula = Nnf.all(tr.definitions ::: parts.toList)
    if (budget.exhausted) Result.Unknown(Reason.WorkLimit)
    else
      new Search(budget).run(formula) match {
        case Search.Outcome.Unsat               => Result.Proved
        case Search.Outcome.GaveUp              => Result.Unknown(Reason.WorkLimit)
        case Search.Outcome.Sat(ints, bools) =>
          val model = tr.model(ints, bools)
          if (refutes(model, goal, facts)) Result.Refuted(model)
          else if (tr.abstracted) Result.Unknown(Reason.OutsideFragment)
          else Result.Unknown(Reason.UnverifiedCounterexample)
      }
  }

  // True when every fact holds at the model and the goal doesn't, with
  // nothing depending on division by zero or an uninterpreted function.
  private def refutes(model: Model, goal: BoolExpr, facts: Vector[BoolExpr]): Boolean =
    facts.forall(evalBool(_, model).contains(true)) && evalBool(goal, model).contains(false)

  // Euclidean division for d != 0: n == d * q + r with 0 <= r < |d|.
  private def euclid(n: BigInt, d: BigInt): (BigInt, BigInt) = {
    val r = n.mod(d.abs)
    ((n - r) / d, r)
  }

  /** The value of `e` at the model with SMT-LIB semantics, or None if it
    * depends on division by zero, an uninterpreted function, or `+`, `-` or
    * `*` with no arguments.
    *
    * Bosatsu's `n.div(0) == 0` and `n.mod_Int(0) == n` aren't SMT-LIB's, which
    * leaves `div` and `mod` by zero unspecified, and they don't need to be
    * here: `SmtExpr.bosatsuDiv` and `bosatsuMod` test the divisor for zero
    * with an `ite` and give those values themselves, so the `Div` and `Mod`
    * they build are only reached with a nonzero divisor. A `Div` or `Mod` by
    * zero can only come from a term built some other way, and since its value
    * isn't defined, it has none here.
    */
  private[smt] def evalInt(e: IntExpr, model: Model): Option[BigInt] =
    e match {
      case IntConst(n)   => Some(n)
      case Var(name)     => Some(model.ints.getOrElse(name, BigInt(0)))
      case Add(args)     =>
        if (args.isEmpty) None else args.traverse(evalInt(_, model)).map(_.sum)
      case Sub(args)     =>
        if (args.isEmpty) None
        else args.traverse(evalInt(_, model)).map(vs => vs.tail.foldLeft(vs.head)(_ - _))
      case Mul(args)     =>
        if (args.isEmpty) None else args.traverse(evalInt(_, model)).map(_.product)
      case Div(n, d)     =>
        (evalInt(n, model), evalInt(d, model)).tupled.flatMap { case (nv, dv) =>
          if (dv == 0) None else Some(euclid(nv, dv)._1)
        }
      case Mod(n, d)     =>
        (evalInt(n, model), evalInt(d, model)).tupled.flatMap { case (nv, dv) =>
          if (dv == 0) None else Some(euclid(nv, dv)._2)
        }
      case Ite(c, t, f)  =>
        evalBool(c, model).flatMap(b => evalInt(if (b) t else f, model))
      case App(_, _)     => None
    }

  /** The value of `e` at the model with SMT-LIB semantics; None as for
    * `evalInt`.
    */
  private[smt] def evalBool(e: BoolExpr, model: Model): Option[Boolean] = {
    def cmp(l: IntExpr, r: IntExpr)(fn: (BigInt, BigInt) => Boolean): Option[Boolean] =
      (evalInt(l, model), evalInt(r, model)).mapN(fn)
    def both(l: BoolExpr, r: BoolExpr)(fn: (Boolean, Boolean) => Boolean): Option[Boolean] =
      (evalBool(l, model), evalBool(r, model)).mapN(fn)
    def bools(args: Vector[BoolExpr]): Option[Vector[Boolean]] =
      args.traverse(evalBool(_, model))

    e match {
      case BoolConst(b)  => Some(b)
      case Var(name)     => Some(model.bools.getOrElse(name, false))
      case Lt(l, r)      => cmp(l, r)(_ < _)
      case Lte(l, r)     => cmp(l, r)(_ <= _)
      case Gt(l, r)      => cmp(l, r)(_ > _)
      case Gte(l, r)     => cmp(l, r)(_ >= _)
      case EqInt(l, r)   => cmp(l, r)(_ == _)
      case EqBool(l, r)  => both(l, r)(_ == _)
      case Xor(l, r)     => both(l, r)(_ != _)
      case Implies(l, r) => both(l, r)((a, b) => !a || b)
      case Not(in)       => evalBool(in, model).map(!_)
      case And(args)     => bools(args).map(_.forall(identity))
      case Or(args)      => bools(args).map(_.exists(identity))
      case Ite(c, t, f)  => evalBool(c, model).flatMap(b => evalBool(if (b) t else f, model))
      case App(_, _)     => None
    }
  }

  /** A formula in negation normal form over linear constraints and Bool
    * variables (numbered by Int, apart from the Int variables).
    */
  private enum Nnf derives CanEqual {
    case Atom(constraint: Omega.Constraint)
    case Lit(boolVar: Int, value: Boolean)
    // true when every part is; All(Nil) is true
    case All(parts: List[Nnf])
    // true when some part is; AnyOf(Nil) is false
    case AnyOf(parts: List[Nnf])
  }

  private object Nnf {
    val True: Nnf = All(Nil)
    val False: Nnf = AnyOf(Nil)

    def all(parts: List[Nnf]): Nnf = {
      val flat = parts.flatMap {
        case All(ps) => ps
        case other   => other :: Nil
      }
      if (flat.contains(False)) False
      else
        flat match {
          case one :: Nil => one
          case _          => All(flat)
        }
    }

    def anyOf(parts: List[Nnf]): Nnf = {
      val flat = parts.flatMap {
        case AnyOf(ps) => ps
        case other     => other :: Nil
      }
      if (flat.contains(True)) True
      else
        flat match {
          case one :: Nil => one
          case _          => AnyOf(flat)
        }
    }

    def geq(f: Omega.Form): Nnf =
      if (f.isConstant) { if (f.const >= 0) True else False }
      else Atom(Omega.Constraint.Geq(f))

    def eq(f: Omega.Form): Nnf =
      if (f.isConstant) { if (f.const == 0) True else False }
      else Atom(Omega.Constraint.Eq(f))
  }

  /** Translates facts and goal into Nnf, numbering variables as it goes.
    * One instance per call of `decide`, so its mutable state never escapes.
    */
  private final class Translator(budget: Omega.Budget) {
    private val intIds = mutable.LinkedHashMap.empty[String, Int]
    private val boolIds = mutable.LinkedHashMap.empty[String, Int]
    private var nextInt = 0
    private var nextBool = 0
    // the same term always gets the same fresh variable
    private val divMods = mutable.HashMap.empty[(Omega.Form, BigInt), (Int, Int)]
    private val iteVars = mutable.HashMap.empty[IntExpr, Int]
    private val opaqueInts = mutable.HashMap.empty[IntExpr, Int]
    private val opaqueBools = mutable.HashMap.empty[BoolExpr, Int]
    private var defs = List.empty[Nnf]
    private var readUnknowns = false

    /** True once a term outside the fragment was read as an unknown. */
    def abstracted: Boolean = readUnknowns

    /** What the fresh variables mean, in the order they were made. */
    def definitions: List[Nnf] = defs.reverse

    private def freshInt(): Int = {
      val v = nextInt
      nextInt += 1
      v
    }

    private def freshBool(): Int = {
      val v = nextBool
      nextBool += 1
      v
    }

    private def intVar(name: String): Int =
      intIds.get(name) match {
        case Some(v) => v
        case None    =>
          val v = freshInt()
          intIds.update(name, v)
          v
      }

    private def boolVar(name: String): Int =
      boolIds.get(name) match {
        case Some(v) => v
        case None    =>
          val v = freshBool()
          boolIds.update(name, v)
          v
      }

    /** Numbers the variables of e, those only inside an uninterpreted
      * function's arguments aside: their sorts aren't known.
      */
    def registerInt(e: IntExpr): Unit = {
      val _ = budget.spend(1)
      e match {
        case IntConst(_)   => ()
        case Var(name)     => val _ = intVar(name)
        case Add(args)     => args.foreach(registerInt)
        case Sub(args)     => args.foreach(registerInt)
        case Mul(args)     => args.foreach(registerInt)
        case Div(n, d)     => registerInt(n); registerInt(d)
        case Mod(n, d)     => registerInt(n); registerInt(d)
        case Ite(c, t, f)  => registerBool(c); registerInt(t); registerInt(f)
        case App(_, _)     => ()
      }
    }

    def registerBool(e: BoolExpr): Unit = {
      val _ = budget.spend(1)
      e match {
        case BoolConst(_)  => ()
        case Var(name)     => val _ = boolVar(name)
        case Not(in)       => registerBool(in)
        case And(args)     => args.foreach(registerBool)
        case Or(args)      => args.foreach(registerBool)
        case Implies(l, r) => registerBool(l); registerBool(r)
        case EqBool(l, r)  => registerBool(l); registerBool(r)
        case Xor(l, r)     => registerBool(l); registerBool(r)
        case Ite(c, t, f)  => registerBool(c); registerBool(t); registerBool(f)
        case Lt(l, r)      => registerInt(l); registerInt(r)
        case Lte(l, r)     => registerInt(l); registerInt(r)
        case Gt(l, r)      => registerInt(l); registerInt(r)
        case Gte(l, r)     => registerInt(l); registerInt(r)
        case EqInt(l, r)   => registerInt(l); registerInt(r)
        case App(_, _)     => ()
      }
    }

    // An unknown integer standing for a term outside the fragment.
    private def opaqueInt(e: IntExpr): Omega.Form = {
      readUnknowns = true
      val v = opaqueInts.get(e) match {
        case Some(v) => v
        case None    =>
          val v = freshInt()
          opaqueInts.update(e, v)
          v
      }
      Omega.Form.variable(v)
    }

    private def opaqueBool(e: BoolExpr): Int = {
      readUnknowns = true
      opaqueBools.get(e) match {
        case Some(v) => v
        case None    =>
          val v = freshBool()
          opaqueBools.update(e, v)
          v
      }
    }

    // The quotient and remainder of n by k != 0.
    private def divMod(n: Omega.Form, k: BigInt): (Int, Int) =
      divMods.get((n, k)) match {
        case Some(qr) => qr
        case None     =>
          val q = freshInt()
          val r = freshInt()
          val rf = Omega.Form.variable(r)
          // n == k * q + r and 0 <= r <= |k| - 1
          defs = Nnf.geq((-rf).plus(k.abs - 1)) :: Nnf.geq(rf) ::
            Nnf.eq(n - (Omega.Form.variable(q) * k) - rf) :: defs
          divMods.update((n, k), (q, r))
          (q, r)
      }

    private def nonzeroConstant(f: Omega.Form): Option[BigInt] =
      if (f.isConstant && (f.const != 0)) Some(f.const) else None

    def int(e: IntExpr): Omega.Form = {
      val _ = budget.spend(1)
      e match {
        case IntConst(n)                 => Omega.Form.constant(n)
        case Var(name)                   => Omega.Form.variable(intVar(name))
        case Add(args) if args.nonEmpty  =>
          args.foldLeft(Omega.Form.zero)(_ + int(_))
        case Sub(args) if args.nonEmpty  =>
          // one argument renders as that argument, not its negation
          args.tail.foldLeft(int(args.head))(_ - int(_))
        case Mul(args) if args.nonEmpty  =>
          val forms = args.map(int)
          val (consts, terms) = forms.partition(_.isConstant)
          val k = consts.foldLeft(BigInt(1))(_ * _.const)
          terms match {
            case Vector()  => Omega.Form.constant(k)
            case Vector(t) => t * k
            case _         => opaqueInt(e)
          }
        case Div(n, d)                   =>
          val nf = int(n)
          nonzeroConstant(int(d)) match {
            case Some(k) => Omega.Form.variable(divMod(nf, k)._1)
            case None    => opaqueInt(e)
          }
        case Mod(n, d)                   =>
          val nf = int(n)
          nonzeroConstant(int(d)) match {
            case Some(k) => Omega.Form.variable(divMod(nf, k)._2)
            case None    => opaqueInt(e)
          }
        case Ite(c, t, f)                =>
          val tf = int(t)
          val ff = int(f)
          if (tf == ff) tf
          else
            iteVars.get(e) match {
              case Some(v) => Omega.Form.variable(v)
              case None    =>
                val v = freshInt()
                val vf = Omega.Form.variable(v)
                defs = Nnf.anyOf(
                  Nnf.all(bool(c, positive = true) :: Nnf.eq(vf - tf) :: Nil) ::
                    Nnf.all(bool(c, positive = false) :: Nnf.eq(vf - ff) :: Nil) ::
                    Nil
                ) :: defs
                iteVars.update(e, v)
                vf
            }
        case Add(_) | Sub(_) | Mul(_) | App(_, _) =>
          // no arguments, which is not valid SMT-LIB, or an uninterpreted
          // function
          opaqueInt(e)
      }
    }

    /** `e` when positive, else its negation, in negation normal form. */
    def bool(e: BoolExpr, positive: Boolean): Nnf =
      if (!budget.spend(1)) Nnf.False // the result is thrown away
      else {
        def both(l: BoolExpr, lp: Boolean, r: BoolExpr, rp: Boolean): Nnf =
          Nnf.all(bool(l, lp) :: bool(r, rp) :: Nil)
        // over the integers, not (l <= r) is r + 1 <= l
        def lte(l: IntExpr, r: IntExpr, pos: Boolean): Nnf =
          if (pos) Nnf.geq(int(r) - int(l))
          else Nnf.geq(int(l) - int(r) - Omega.Form.constant(BigInt(1)))

        e match {
          case BoolConst(b)  => if (b == positive) Nnf.True else Nnf.False
          case Var(name)     => Nnf.Lit(boolVar(name), positive)
          case Not(in)       => bool(in, !positive)
          case And(args)     =>
            val parts = args.toList.map(bool(_, positive))
            if (positive) Nnf.all(parts) else Nnf.anyOf(parts)
          case Or(args)      =>
            val parts = args.toList.map(bool(_, positive))
            if (positive) Nnf.anyOf(parts) else Nnf.all(parts)
          case Implies(l, r) =>
            if (positive) Nnf.anyOf(bool(l, false) :: bool(r, true) :: Nil)
            else both(l, true, r, false)
          case EqBool(l, r)  =>
            if (positive)
              Nnf.anyOf(both(l, true, r, true) :: both(l, false, r, false) :: Nil)
            else
              Nnf.anyOf(both(l, true, r, false) :: both(l, false, r, true) :: Nil)
          case Xor(l, r)     => bool(EqBool(l, r), !positive)
          case Ite(c, t, f)  =>
            Nnf.anyOf(both(c, true, t, positive) :: both(c, false, f, positive) :: Nil)
          case Lt(l, r)      => lte(Add(Vector(l, IntConst(BigInt(1)))), r, positive)
          case Lte(l, r)     => lte(l, r, positive)
          case Gt(l, r)      => lte(Add(Vector(r, IntConst(BigInt(1)))), l, positive)
          case Gte(l, r)     => lte(r, l, positive)
          case EqInt(l, r)   =>
            val diff = int(l) - int(r)
            if (positive) Nnf.eq(diff)
            else
              Nnf.anyOf(
                Nnf.geq(diff.plus(BigInt(-1))) :: Nnf.geq((-diff).plus(BigInt(-1))) :: Nil
              )
          case App(_, _)     => Nnf.Lit(opaqueBool(e), positive)
        }
      }

    /** The named variables at the values found; those not found are 0 or
      * false, which the search also assumed.
      */
    def model(ints: IntMap[BigInt], bools: IntMap[Boolean]): Model =
      Model(
        SortedMap.from(intIds.iterator.map { case (name, v) =>
          (name, ints.getOrElse(v, BigInt(0)))
        }),
        SortedMap.from(boolIds.iterator.map { case (name, v) =>
          (name, bools.getOrElse(v, false))
        })
      )
  }

  private object Search {
    enum Outcome derives CanEqual {
      // values for the Int variables of the constraints and the Bool
      // variables that were assigned; any others are 0 and false
      case Sat(ints: IntMap[BigInt], bools: IntMap[Boolean])
      case Unsat
      case GaveUp
    }
  }

  /** Looks for values making an Nnf true. A node holds the constraints and
    * Bool literals that must hold, and the disjunctions still open. It asks
    * Omega for values meeting the constraints, and only splits a disjunction
    * those values (with unassigned Bools false) make false: if they make every
    * open disjunction true they are an answer.
    */
  private final class Search(budget: Omega.Budget) {
    import Search.Outcome

    def run(f: Nnf): Outcome = node(f :: Nil, Vector.empty, IntMap.empty, Vector.empty)

    private def holds(f: Nnf, ints: IntMap[BigInt], bools: IntMap[Boolean]): Boolean = {
      val _ = budget.spend(1)
      f match {
        case Nnf.Atom(c)       => c.holds(v => ints.getOrElse(v, BigInt(0)))
        case Nnf.Lit(v, value) => bools.getOrElse(v, false) == value
        case Nnf.All(ps)       => ps.forall(holds(_, ints, bools))
        case Nnf.AnyOf(ps)     => ps.exists(holds(_, ints, bools))
      }
    }

    // Moves what must hold out of todo, then drops the disjuncts that the
    // Bool literals rule out, repeating while that leaves one. None if the
    // literals contradict each other or a disjunction is left empty.
    @annotation.tailrec
    private def absorb(
        todo: List[Nnf],
        atoms: Vector[Omega.Constraint],
        bools: IntMap[Boolean],
        disjs: Vector[List[Nnf]]
    ): Option[(Vector[Omega.Constraint], IntMap[Boolean], Vector[List[Nnf]])] =
      todo match {
        case Nnf.Atom(c) :: rest          => absorb(rest, atoms :+ c, bools, disjs)
        case Nnf.Lit(v, value) :: rest    =>
          bools.get(v) match {
            case Some(other) if other != value => None
            case _                             => absorb(rest, atoms, bools.updated(v, value), disjs)
          }
        case Nnf.All(ps) :: rest          => absorb(ps ::: rest, atoms, bools, disjs)
        case Nnf.AnyOf(Nil) :: _          => None
        case Nnf.AnyOf(p :: Nil) :: rest  => absorb(p :: rest, atoms, bools, disjs)
        case Nnf.AnyOf(ps) :: rest        => absorb(rest, atoms, bools, disjs :+ ps)
        case Nil                          =>
          def ruledOut(p: Nnf): Boolean =
            p match {
              case Nnf.Lit(v, value) => bools.get(v).contains(!value)
              case _                 => false
            }
          def satisfied(p: Nnf): Boolean =
            p match {
              case Nnf.Lit(v, value) => bools.get(v).contains(value)
              case _                 => false
            }
          val open = disjs.filterNot(_.exists(satisfied)).map(_.filterNot(ruledOut))
          open.indexWhere(_.lengthCompare(1) <= 0) match {
            case -1 => Some((atoms, bools, open))
            case i  =>
              // empty is a conflict (AnyOf(Nil)), one must hold
              absorb(Nnf.AnyOf(open(i)) :: Nil, atoms, bools, open.patch(i, Nil, 1))
          }
      }

    private def node(
        todo: List[Nnf],
        atoms0: Vector[Omega.Constraint],
        bools0: IntMap[Boolean],
        disjs0: Vector[List[Nnf]]
    ): Outcome =
      if (!budget.spend(1)) Outcome.GaveUp
      else
        absorb(todo, atoms0, bools0, disjs0) match {
          case None                         => Outcome.Unsat
          case Some((atoms, bools, disjs)) =>
            Omega.solve(atoms.toList, budget) match {
              case Omega.Result.Unsat    => Outcome.Unsat
              case Omega.Result.GaveUp   => Outcome.GaveUp
              case Omega.Result.Sat(ints) =>
                disjs.indexWhere(d => !d.exists(holds(_, ints, bools))) match {
                  case -1 =>
                    if (budget.exhausted) Outcome.GaveUp
                    else Outcome.Sat(ints, bools)
                  case i  =>
                    val rest = disjs.patch(i, Nil, 1)
                    // the first answer wins; GaveUp only if none is found
                    disjs(i).foldLeft(Outcome.Unsat: Outcome) { (acc, p) =>
                      acc match {
                        case Outcome.Sat(_, _) => acc
                        case _                 =>
                          node(p :: Nil, atoms, bools, rest) match {
                            case sat @ Outcome.Sat(_, _) => sat
                            case Outcome.GaveUp          => Outcome.GaveUp
                            case Outcome.Unsat           => acc
                          }
                      }
                    }
                }
            }
        }
  }
}
