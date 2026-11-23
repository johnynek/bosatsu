package dev.bosatsu

import cats.data.{Ior, NonEmptyList}
import dev.bosatsu.rankn.TypeEnv
import dev.bosatsu.smt.{LinearBoundsGens, SmtExpr, SmtSort}
import org.scalacheck.{Gen, Prop, Shrink}
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import scala.concurrent.duration.DurationInt

import Identifier.Bindable
import LinearBoundsGens.{Env, byZeroChoices, evalBool, evalInt}
import TypedExprRecursionCheck.{LeafPathFacts, LoweredDefBody, LoweredTerm}

/** The recursion check proves Int recursion terminates by lowering Bosatsu
  * Int, Bool and Comparison expressions to SMT-LIB terms. If a term means
  * something other than the expression it came from, the check can accept a
  * def that does not terminate.
  *
  * These properties generate well-typed Bosatsu expressions over a few
  * arguments, using the constructs the lowering understands, typecheck them,
  * lower them with the real lowering, and check that every term the lowering
  * returns evaluates, with SMT-LIB semantics, to what Bosatsu's evaluator
  * computes.
  *
  * The check also assumes path facts on the way to a recursive call: that a
  * branch's pattern and guard matched, and that the earlier branches did not.
  * If one of those is false when Bosatsu takes the branch, the check can
  * accept a def that does not terminate, so a property checks those too.
  */
class TypedExprRecursionLoweringTest extends munit.ScalaCheckSuite {
  import TypedExprRecursionLoweringTest.*

  override val munitTimeout = 10.minutes

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isJvm) 500 else 30)
      .withMaxDiscardRatio(10)

  private val pack = PackageName.parts("LoweringProp")
  private val targetName: Bindable = Identifier.Name("target")

  // A program typechecked against Predef's interface, with its lets as the
  // recursion check sees them (inferBodyUnopt runs the check and does not
  // normalize), and a package map to evaluate it with. normalizedPm has the
  // lets after loop lowering and normalization, as inferBody gives them to
  // `tool eval` and codegen; it is only made when used.
  private final class Compiled(
      val fullTypeEnv: TypeEnv[Kind.Arg],
      val lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
      val pm: PackageMap.Typed[Declaration],
      normalize: => PackageMap.Typed[Declaration]
  ) {
    lazy val normalizedPm: PackageMap.Typed[Declaration] = normalize
  }

  private def compile(source: String): Either[String, Compiled] = {
    val result = Package.inferBodyUnopt(
      pack,
      TestUtils.predefResolvedImport :: Nil,
      Nil,
      TestUtils.statementsOf(source)
    )
    val compiled = result.map { case (fullTypeEnv, program) =>
      def packageMap(typed: Package.Typed[Declaration]): PackageMap.Typed[Declaration] =
        PackageMap.empty + typed + PackageMap.predefInferred
      def normalized = TypedExprNormalization.normalizeProgram(
        pack,
        fullTypeEnv,
        TypedExprLoopRecurLowering.lowerProgram(program)
      )
      new Compiled(
        fullTypeEnv,
        program.lets,
        packageMap(Package(pack, Nil, Nil, (program, ImportMap.empty))),
        packageMap(Package(pack, Nil, Nil, (normalized, ImportMap.empty)))
      )
    }
    compiled match {
      // postponable errors, such as unused bindings and unreachable
      // branches, still give a program to run
      case Ior.Right(c) => Right(c)
      case Ior.Both(errs, c) if errs.forall(runnable) =>
        Right(c)
      case Ior.Both(errs, _) => Left(messages(source, errs))
      case Ior.Left(errs)    => Left(messages(source, errs))
    }
  }

  // A shadowing binding that changes type is postponable, but Bosatsu does
  // not run such a program, and the normalized program can then compute
  // something else.
  private def runnable(err: PackageError): Boolean =
    err match {
      case _: PackageError.ShadowedBindingTypeError => false
      case other                                    => PackageError.isPostponable(other)
    }

  private def messages(source: String, errs: NonEmptyList[PackageError]): String = {
    val sm = Map(pack -> (LocationMap(source), "<generated>"))
    errs.toList
      .map(_.message(sm, LocationMap.Colorize.None))
      .mkString("\n-----\n")
  }

  private def lowerTarget(c: Compiled): Option[LoweredDefBody] =
    TypedExprRecursionCheck.lowerDefBodyForTest(
      pack,
      c.fullTypeEnv,
      c.lets,
      targetName
    )

  private def leafPathFacts(c: Compiled): Option[LeafPathFacts] =
    TypedExprRecursionCheck.leafPathFactsForTest(
      pack,
      c.fullTypeEnv,
      c.lets,
      targetName
    )

  private def runTarget(c: Compiled): Args => Value =
    runTargetIn(c.pm)

  private def runTargetIn(pm: PackageMap.Typed[Declaration]): Args => Value = {
    val (fnEval, _) = Evaluation(pm, Predef.jvmExternals)
      .evaluateName(pack, targetName)
      .getOrElse(sys.error("target not found"))
    val fn = fnEval.value
    args =>
      fn.applyAll(
        NonEmptyList.of(
          Value.VInt(args.x),
          Value.VInt(args.y),
          Value.VInt(args.z),
          if (args.p) Value.True else Value.False,
          Value.Comparison.fromInt(args.q)
        )
      )
  }

  // Every way the lowered term and its path facts disagree with Bosatsu on
  // argss, under each SMT-LIB reading of division by zero. Left when they use
  // a symbol that is not an argument, so their value is not determined.
  private def mismatches(
      lowered: LoweredDefBody,
      run: Args => Value,
      argss: List[Args]
  ): Either[String, List[String]] = {
    val syms =
      ArgSymbols(lowered.intArgs, lowered.boolArgs, lowered.comparisonArgs)
    val argSyms = syms.all

    lowered.term match {
      case None       => Right(Nil)
      case Some(term) =>
        val used = termVars(term) ++ lowered.pathFacts.flatMap(vars(_))
        if (!used.subsetOf(argSyms))
          Left(s"open term, free: ${(used -- argSyms).toList.sorted.mkString(", ")}")
        else
          Right(argss.flatMap { args =>
            val actual = run(args)
            byZeroChoices.zipWithIndex.flatMap { case (bz, bzIdx) =>
              val env = syms.env(args, bz)
              val badFacts =
                lowered.pathFacts.filterNot(evalBool(_, env)).map { f =>
                  s"$args byZero#$bzIdx: path fact is false: $f"
                }
              val got = evalTerm(term, env)
              val expected = expectedValue(term, actual)
              val gotNormalized = evalNormalized(term, env)
              val badValue =
                if ((got == expected) && (gotNormalized == expected)) Nil
                else
                  List(
                    s"$args byZero#$bzIdx: Bosatsu gives $actual, SMT term gives $got (normalized: $gotNormalized)"
                  )
              badFacts.toList ::: badValue
            }
          })
    }
  }

  private def check(prog: Program, argss: List[Args]): Prop = {
    val source = prog.render
    compile(source) match {
      case Left(_) =>
        // the generator only makes programs that compile (the coverage test
        // below checks), but shrinking can break one
        Prop.undecided
      case Right(c) =>
        val run = runTarget(c)
        lowerTarget(c) match {
          case None =>
            Prop.falsified :| s"no target def:\n$source"
          case Some(lowered) =>
            mismatches(lowered, run, argss) match {
              case Left(_) =>
                // A fresh symbol stands for a value that did not lower, so
                // the term only has to agree for some value of it, which
                // this does not search for. The coverage test below checks
                // that most terms are closed.
                Prop.passed
              case Right(Nil) =>
                Prop.passed
              case Right(bad) =>
                Prop.falsified :| (
                  s"source:\n$source\n\nterm: ${lowered.term}\npath facts: ${lowered.pathFacts}\n\n" +
                    bad.take(10).mkString("\n")
                )
            }
        }
    }
  }

  property("every lowered Int, Bool and Comparison term evaluates to Bosatsu's value") {
    given Shrink[Program] = Shrink.withLazyList(shrinkProgram)
    // a failure reports the failing arguments, so they need no shrinking
    given Shrink[List[Args]] = Shrink.withLazyList(_ => LazyList.empty)
    forAll(programGen, argsListGen)(check)
  }

  // The recursion check reasons about the program as type checking leaves
  // it, but `tool eval`, `tool test` and codegen run it after loop lowering
  // and normalization. If those change what it computes, a def the check
  // accepted can do something else when run, even fail to terminate.
  private def checkNormalized(prog: Program, argss: List[Args]): Prop = {
    val source = prog.render
    compile(source) match {
      case Left(_) =>
        Prop.undecided
      case Right(c) =>
        val run = runTarget(c)
        val runNormalized = runTargetIn(c.normalizedPm)
        argss.flatMap { args =>
          val v = run(args)
          val vn = runNormalized(args)
          if (v == vn) Nil
          else List(s"$args: Bosatsu gives $v, the normalized program gives $vn")
        } match {
          case Nil => Prop.passed
          case bad =>
            Prop.falsified :| (s"source:\n$source\n\n" + bad.take(10).mkString("\n"))
        }
    }
  }

  property("loop lowering and normalization do not change what a program computes") {
    given Shrink[Program] = Shrink.withLazyList(shrinkProgram)
    given Shrink[List[Args]] = Shrink.withLazyList(_ => LazyList.empty)
    forAll(programGen, argsListGen)(checkNormalized)
  }

  // Every way the path facts at the leaf Bosatsu returns are false on argss.
  // Facts over symbols other than the arguments are about values that did
  // not lower, which this does not search for.
  private def falsePathFacts(
      facts: LeafPathFacts,
      run: Args => Value,
      argss: List[Args]
  ): List[String] = {
    val syms = ArgSymbols(facts.intArgs, facts.boolArgs, facts.comparisonArgs)
    argss.flatMap { args =>
      run(args) match {
        case Value.VInt(leaf) =>
          facts.leaves.get(BigInt(leaf)) match {
            case None =>
              List(s"$args: Bosatsu returns leaf $leaf, which the walk did not reach")
            case Some(pathFacts) =>
              val closed = pathFacts.filter(vars(_).subsetOf(syms.all))
              byZeroChoices.zipWithIndex.flatMap { case (bz, bzIdx) =>
                val env = syms.env(args, bz)
                closed.filterNot(evalBool(_, env)).map { f =>
                  s"$args byZero#$bzIdx: leaf $leaf: path fact is false: $f"
                }
              }
          }
        case other =>
          List(s"$args: not an Int leaf: $other")
      }
    }
  }

  private def checkPathFacts(prog: Program, argss: List[Args]): Prop = {
    val source = prog.render
    compile(source) match {
      case Left(_) =>
        Prop.undecided
      case Right(c) =>
        leafPathFacts(c) match {
          case None =>
            Prop.falsified :| s"no target def:\n$source"
          case Some(facts) =>
            falsePathFacts(facts, runTarget(c), argss) match {
              case Nil => Prop.passed
              case bad =>
                Prop.falsified :| (
                  s"source:\n$source\n\nargument symbols: ${facts.intArgs} ${facts.boolArgs} ${facts.comparisonArgs}\n" +
                    bad.take(10).mkString("\n")
                )
            }
        }
    }
  }

  property("the path facts at a leaf hold whenever Bosatsu reaches it") {
    // shrinks stay bodies of lets and matches, with their leaves renumbered
    given Shrink[Program] = Shrink.withLazyList { prog =>
      shrinkProgram(prog)
        .filter(p => isPathTree(p.body))
        .map(p => p.copy(body = numberLeaves(p.body)))
        .filter(_ != prog)
    }
    given Shrink[List[Args]] = Shrink.withLazyList(_ => LazyList.empty)
    forAll(pathProgramGen, argsListGen)(checkPathFacts)
  }

  private val gridArgs: List[Args] =
    for {
      x <- List(-2, -1, 0, 1, 3)
      y <- List(-1, 0, 2)
      z <- List(0, 1)
      p <- List(true, false)
      q <- List(-1, 0, 1)
    } yield Args(BigInt(x), BigInt(y), BigInt(z), p, q)

  // closed: the term should only use the arguments, else it should use a
  // fresh symbol for a value that does not lower
  private def regression(name: String, source: String, closed: Boolean)(using
      munit.Location
  ): Unit =
    test(name) {
      compile(source) match {
        case Left(err) => fail(err)
        case Right(c) =>
          val lowered = lowerTarget(c).getOrElse(fail("no target def"))
          assert(lowered.term.nonEmpty, "expected a term")
          mismatches(lowered, runTarget(c), gridArgs) match {
            case Left(open) =>
              assert(!closed, s"expected a term over the arguments, got an $open")
            case Right(bad) =>
              assert(closed, s"expected a fresh symbol, got ${lowered.term}")
              assertEquals(bad, Nil)
          }
      }
    }

  private val opaqueDef = """
def opaque(a: Int)(b: Int) -> Int:
  add(b, a)
"""

  // Small cases of the bugs the property found. Each lowered to a term over
  // the arguments with a different value.

  // was ite(not(x == 1 or x == 2) and x == 1, 10, ...): a branch's condition
  // also required the later branches to miss
  regression(
    "overlapping branches take the first match",
    """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match x:
    case 1: 10
    case 1 | 2: 20
    case _: 30
""",
    closed = true
  )

  // the guard's y was the argument y, not x + 1
  regression(
    "a guard sees the names its pattern binds",
    """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match add(x, 1):
    case y if eq_Int(y, 0): 1
    case _: 2
""",
    closed = true
  )

  // the first branch's z was the last branch's z, 0
  regression(
    "a branch's names do not reach the other branches",
    """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match 0:
    case b: z
    case _ as z: 0
""",
    closed = true
  )

  // the second z was the let's z, 0
  regression(
    "a let's name does not reach the rest of the expression",
    """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  add((
      z = 0
      z
    ), z)
""",
    closed = true
  )

  // Helpers: a value alias and a forwarding def are Predef aliases, a def
  // that swaps its arguments or does more is inlined. The property covers
  // these too, but needs the right helper to be generated and called.
  regression(
    "helpers lower to what they compute",
    """
helper0 = sub

def helper1(a: Int, b: Int) -> Int:
  sub(a, b)

def helper2(a: Int, b: Int) -> Int:
  sub(b, a)

def helper3(a: Int, b: Int) -> Int:
  add(mul(a, 2), b)

def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  add(add(helper0(x, y), mul(helper1(y, z), 10)), add(mul(helper2(x, z), 100), mul(helper3(x, y), 1000)))
""",
    closed = true
  )

  // A `matches` guard's pattern matches its argument in the branch's scope,
  // and its names are bound for the inner guard and the body. This was an
  // open term, over a fresh symbol for x, before.
  regression(
    "a matches guard binds its names to its argument",
    """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match y:
    case _ if (add(x, 1) matches x if eq_Int(x, 0)): x
    case _: 5
""",
    closed = true
  )

  // The property reaches these only through its rarer opaque calls and tuple
  // matches, so not on every run. Each lowered to a closed term, over the
  // outer binding the name shadows.

  // the let's x kept the argument x
  regression(
    "a binding that does not lower gets a fresh symbol",
    opaqueDef + """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  x = opaque(x)(1)
  x
""",
    closed = false
  )

  // the pattern's x was the argument x, not y
  regression(
    "a name inside a struct pattern is not the outer name",
    """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match (y, x):
    case (x, _): x
""",
    closed = false
  )

  // the path facts at the branch were not(m < 1), about the outer m, but
  // were read as not(x < 1), so target(0, 5, ...) reached a leaf whose fact
  // was false
  test("an earlier guard about an outer name the pattern rebinds gives no fact") {
    val source = """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match x:
    case _ if cmp_Int(y, 1) matches LT: 0
    case y: 1
"""
    compile(source) match {
      case Left(err) => fail(err)
      case Right(c) =>
        val facts = leafPathFacts(c).getOrElse(fail("no target def"))
        assertEquals(falsePathFacts(facts, runTarget(c), gridArgs), Nil)
    }
  }

  test("the generated path programs usually check some facts") {
    val params = Gen.Parameters.default.withSize(100)
    val seed = Seed(7L)
    val progs =
      (0 until (if (Platform.isJvm) 60 else 10)).toList.flatMap { i =>
        pathProgramGen(params, seed.reseed(i.toLong))
      }
    val checked = progs.count { prog =>
      val source = prog.render
      compile(source) match {
        case Left(err) => fail(s"generated program does not compile:\n$source\n$err")
        case Right(c) =>
          val facts = leafPathFacts(c).getOrElse(fail("no target def"))
          val syms = ArgSymbols(facts.intArgs, facts.boolArgs, facts.comparisonArgs)
          facts.leaves.valuesIterator.exists(_.exists(vars(_).subsetOf(syms.all)))
      }
    }
    assert(
      checked * 10 >= progs.size * 6,
      s"only $checked of ${progs.size} programs have a leaf with a fact over the arguments"
    )
  }

  // Bosatsu's semantics give -1: the first branch's y is not in scope in the
  // second. Normalizing it used to give 2 (EvaluationTest has more of these).
  test("programs are evaluated with a guarded branch's names out of the next branch") {
    val source = """
def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> Int:
  match x:
    case y if eq_Int(0, y): 0
    case _: y
"""
    compile(source) match {
      case Left(err) => fail(err)
      case Right(c) =>
        val args = Args(BigInt(2), BigInt(-1), BigInt(0), false, 0)
        List(runTarget(c), runTargetIn(c.normalizedPm)).foreach { run =>
          run(args) match {
            case Value.VInt(v) => assertEquals(BigInt(v), BigInt(-1))
            case other         => fail(s"not an Int: $other")
          }
        }
    }
  }

  test("the generated programs usually lower to closed terms") {
    val params = Gen.Parameters.default.withSize(100)
    val seed = Seed(42L)
    val progs =
      (0 until (if (Platform.isJvm) 60 else 10)).toList.flatMap { i =>
        programGen(params, seed.reseed(i.toLong))
      }
    val outcomes = progs.map { prog =>
      val source = prog.render
      compile(source) match {
        case Left(err) => fail(s"generated program does not compile:\n$source\n$err")
        case Right(c) =>
          lowerTarget(c) match {
            case Some(lowered) if lowered.term.nonEmpty =>
              mismatches(lowered, runTarget(c), Nil) match {
                case Left(_)  => "open"
                case Right(_) => "closed"
              }
            case _ => "none"
          }
      }
    }
    val closed = outcomes.count(_ == "closed")
    assert(
      closed * 10 >= outcomes.size * 6,
      s"only $closed of ${outcomes.size} lowered to closed terms: ${outcomes.groupBy(identity).view.mapValues(_.size).toMap}"
    )
  }
}

object TypedExprRecursionLoweringTest {
  import SmtExpr.*

  // ---------------------------------------------------------------------------
  // Evaluating terms

  def vars[S <: SmtSort](e: SmtExpr[S]): Set[String] =
    e match {
      case IntConst(_) | BoolConst(_) => Set.empty
      case Var(n)                      => Set(n)
      case Ite(c, a, b)                => vars(c) ++ vars(a) ++ vars(b)
      case Add(args)                   => args.iterator.flatMap(vars(_)).toSet
      case Sub(args)                   => args.iterator.flatMap(vars(_)).toSet
      case Mul(args)                   => args.iterator.flatMap(vars(_)).toSet
      case Div(a, b)                   => vars(a) ++ vars(b)
      case Mod(a, b)                   => vars(a) ++ vars(b)
      case Lt(a, b)                    => vars(a) ++ vars(b)
      case Lte(a, b)                   => vars(a) ++ vars(b)
      case Gt(a, b)                    => vars(a) ++ vars(b)
      case Gte(a, b)                   => vars(a) ++ vars(b)
      case EqInt(a, b)                 => vars(a) ++ vars(b)
      case EqBool(a, b)                => vars(a) ++ vars(b)
      case Not(a)                      => vars(a)
      case And(args)                   => args.iterator.flatMap(vars(_)).toSet
      case Or(args)                    => args.iterator.flatMap(vars(_)).toSet
      case Xor(a, b)                   => vars(a) ++ vars(b)
      case Implies(a, b)               => vars(a) ++ vars(b)
      case other                       => sys.error(s"unexpected SMT term: $other")
    }

  def termVars(t: LoweredTerm): Set[String] =
    t match {
      case LoweredTerm.IntTerm(e)        => vars(e)
      case LoweredTerm.BoolTerm(e)       => vars(e)
      case LoweredTerm.ComparisonTerm(e) => vars(e)
    }

  // Int and Comparison terms evaluate to an Int, Bool terms to 0 or 1
  def evalTerm(t: LoweredTerm, env: Env): BigInt =
    t match {
      case LoweredTerm.IntTerm(e)        => evalInt(e, env)
      case LoweredTerm.BoolTerm(e)       => if (evalBool(e, env)) BigInt(1) else BigInt(0)
      case LoweredTerm.ComparisonTerm(e) => evalInt(e, env)
    }

  // the same, after the normalization the term gets before it goes to z3
  def evalNormalized(t: LoweredTerm, env: Env): BigInt =
    t match {
      case LoweredTerm.IntTerm(e)        => evalInt(normalizeIntForSolver(e), env)
      case LoweredTerm.BoolTerm(e)       =>
        if (evalBool(normalizeBoolForSolver(e), env)) BigInt(1) else BigInt(0)
      case LoweredTerm.ComparisonTerm(e) => evalInt(normalizeIntForSolver(e), env)
    }

  def expectedValue(t: LoweredTerm, v: Value): BigInt =
    (t, v) match {
      case (LoweredTerm.IntTerm(_), Value.VInt(i)) => BigInt(i)
      case (LoweredTerm.BoolTerm(_), s: Value.SumValue) =>
        // False is variant 0 and True is variant 1
        BigInt(s.variant)
      case (LoweredTerm.ComparisonTerm(_), s: Value.SumValue) =>
        // LT, EQ and GT are variants 0, 1 and 2
        BigInt(s.variant - 1)
      case other =>
        sys.error(s"unexpected value: $other")
    }

  // The SMT symbols bound to the target's arguments x, y, z, p and q
  final case class ArgSymbols(x: String, y: String, z: String, p: String, q: String) {
    def all: Set[String] = Set(x, y, z, p, q)

    def env(args: Args, byZero: BigInt => BigInt): Env =
      Env(
        Map(x -> args.x, y -> args.y, z -> args.z, q -> BigInt(args.q)),
        Map(p -> args.p),
        byZero
      )
  }
  object ArgSymbols {
    def apply(
        intArgs: Map[Bindable, IntExpr],
        boolArgs: Map[Bindable, BoolExpr],
        comparisonArgs: Map[Bindable, IntExpr]
    ): ArgSymbols = {
      def symbol[S <: SmtSort](m: Map[Bindable, SmtExpr[S]], n: String): String =
        m.get(Identifier.Name(n)) match {
          case Some(Var(sym)) => sym
          case other          => sys.error(s"argument $n is not bound to a symbol: $other")
        }
      ArgSymbols(
        symbol(intArgs, "x"),
        symbol(intArgs, "y"),
        symbol(intArgs, "z"),
        symbol(boolArgs, "p"),
        symbol(comparisonArgs, "q")
      )
    }
  }

  // ---------------------------------------------------------------------------
  // Programs

  enum Sort derives CanEqual {
    case IntS, BoolS, CmpS

    def typeName: String =
      this match {
        case IntS  => "Int"
        case BoolS => "Bool"
        case CmpS  => "Comparison"
      }
  }
  import Sort.*

  // -1, 0 and 1 for LT, EQ and GT
  def cmpName(c: Int): String =
    if (c < 0) "LT" else if (c > 0) "GT" else "EQ"

  enum Pat derives CanEqual {
    case Wild
    case Bind(name: String)
    case IntP(value: BigInt)
    case BoolP(value: Boolean)
    case CmpP(value: Int)
    // two or more literal patterns
    case Union(items: List[Pat])
    case Named(inner: Pat, name: String)

    def names: List[String] =
      this match {
        case Bind(n)        => n :: Nil
        case Named(p, n)    => n :: p.names
        case Union(ps)      => ps.flatMap(_.names)
        case _              => Nil
      }

    def isCatchAll: Boolean =
      this match {
        case Wild | Bind(_) => true
        case Named(p, _)    => p.isCatchAll
        case _              => false
      }

    def render: String =
      this match {
        case Wild        => "_"
        case Bind(n)     => n
        case IntP(v)     => v.toString
        case BoolP(b)    => if (b) "True" else "False"
        case CmpP(c)     => cmpName(c)
        case Union(ps)   => ps.map(_.render).mkString("(", " | ", ")")
        case Named(p, n) => s"${p.render} as $n"
      }
  }

  enum Guard derives CanEqual {
    case BoolG(cond: Ex)
    // `if arg matches pat if inner`, binding the names in pat
    case MatchG(arg: Ex, argSort: Sort, pat: Pat, inner: Option[Ex])
  }

  final case class Branch(pat: Pat, guard: Option[Guard], body: Ex)

  enum Ex derives CanEqual {
    case IntLit(value: BigInt)
    case BoolLit(value: Boolean)
    case CmpLit(value: Int)
    case Ref(name: String)
    case Call(fn: String, args: List[Ex])
    // elif renders an If in the else branch as `elif`
    case If(cond: Ex, ifTrue: Ex, ifFalse: Ex, elif: Boolean)
    // `if arg matches pat if guard: ifTrue else: ifFalse`
    case IfMatches(arg: Ex, pat: Pat, guard: Option[Ex], ifTrue: Ex, ifFalse: Ex)
    case Match(arg: Ex, branches: List[Branch])
    // `arg matches pat if guard`, a Bool
    case Matches(arg: Ex, pat: Pat, guard: Option[Ex])
    case Let(name: String, value: Ex, body: Ex)
    // `opaque(a)(b)`, an Int the lowering does not see into
    case Opaque(a: Ex, b: Ex)
    // `match (first, second): case (firstPat, secondPat): body`, which binds
    // names inside a struct
    case TupleMatch(
        first: Ex,
        firstSort: Sort,
        second: Ex,
        secondSort: Sort,
        firstPat: Pat,
        secondPat: Pat,
        body: Ex
    )
  }
  import Ex.*

  // An extra top level def the target can call. With a forwarding body of
  // `fn(a, b)`, it is an alias of the Predef function fn; otherwise the
  // lowering inlines its body.
  final case class Helper(
      name: String,
      params: List[(String, Sort)],
      result: Sort,
      body: HelperBody
  )
  enum HelperBody derives CanEqual {
    // `name = fn`
    case ValueAlias(fn: String)
    case Body(ex: Ex)
  }

  final case class Program(helpers: List[Helper], result: Sort, body: Ex)
      derives CanEqual {
    def render: String = {
      val helperLines = helpers.flatMap { h =>
        h.body match {
          case HelperBody.ValueAlias(fn) =>
            s"${h.name} = $fn" :: "" :: Nil
          case HelperBody.Body(ex) =>
            val params =
              h.params.map { case (n, s) => s"$n: ${s.typeName}" }.mkString(", ")
            s"def ${h.name}($params) -> ${h.result.typeName}:" ::
              (renderBlock(ex, "  ") :+ "")
        }
      }
      // two argument groups, so the lowering does not inline it
      val opaqueLines =
        "def opaque(a: Int)(b: Int) -> Int:" :: "  add(b, a)" :: "" :: Nil
      val targetLines =
        s"def target(x: Int, y: Int, z: Int, p: Bool, q: Comparison) -> ${result.typeName}:" ::
          renderBlock(body, "  ")
      (opaqueLines ::: helperLines ::: targetLines)
        .mkString("", "\n", "\n")
    }
  }

  private def isBlock(e: Ex): Boolean =
    e match {
      case If(_, _, _, _) | IfMatches(_, _, _, _, _) | Match(_, _) | Let(_, _, _) |
          TupleMatch(_, _, _, _, _, _, _) =>
        true
      case _                                                                     => false
    }

  def renderBlock(e: Ex, ind: String): List[String] = {
    val inner = ind + "  "
    e match {
      case Let(n, v, body) =>
        s"$ind$n = ${renderInline(v, ind)}" :: renderBlock(body, ind)
      case Match(arg, branches) =>
        s"${ind}match ${renderInline(arg, ind)}:" :: branches.flatMap { b =>
          val guard = b.guard.fold("")(g => " if " + renderGuard(g, inner))
          s"${inner}case ${b.pat.render}$guard:" :: renderBlock(b.body, inner + "  ")
        }
      case If(c, t, f, elif) =>
        val head = s"${ind}if ${renderInline(c, ind)}:" :: renderBlock(t, inner)
        def elses(f: Ex): List[String] =
          f match {
            case If(c1, t1, f1, _) if elif =>
              (s"${ind}elif ${renderInline(c1, ind)}:" :: renderBlock(t1, inner)) ::: elses(f1)
            case _ =>
              s"${ind}else:" :: renderBlock(f, inner)
          }
        head ::: elses(f)
      case TupleMatch(a, _, b, _, pa, pb, body) =>
        s"${ind}match (${renderInline(a, ind)}, ${renderInline(b, ind)}):" ::
          s"${inner}case (${pa.render}, ${pb.render}):" :: renderBlock(body, inner + "  ")
      case IfMatches(arg, pat, guard, t, f) =>
        val g = guard.fold("")(g => " if " + renderInline(g, ind))
        (s"${ind}if ${renderInline(arg, ind)} matches ${pat.render}$g:" :: renderBlock(t, inner)) :::
          (s"${ind}else:" :: renderBlock(f, inner))
      case other =>
        (ind + renderInline(other, ind)) :: Nil
    }
  }

  def renderGuard(g: Guard, ind: String): String =
    g match {
      case Guard.BoolG(c)                  => renderInline(c, ind)
      case Guard.MatchG(arg, _, pat, inner) =>
        val in = inner.fold("")(i => " if " + renderInline(i, ind))
        s"(${renderInline(arg, ind)} matches ${pat.render}$in)"
    }

  // a single line, except blocks in parentheses whose lines are indented
  // past ind
  def renderInline(e: Ex, ind: String): String =
    e match {
      case IntLit(v)          => v.toString
      case BoolLit(b)         => if (b) "True" else "False"
      case CmpLit(c)          => cmpName(c)
      case Ref(n)             => n
      case Call(fn, args)     => args.map(renderInline(_, ind)).mkString(s"$fn(", ", ", ")")
      case Opaque(a, b)       => s"opaque(${renderInline(a, ind)})(${renderInline(b, ind)})"
      case Matches(a, p, g)   =>
        val gs = g.fold("")(g => " if " + renderInline(g, ind))
        s"(${renderInline(a, ind)} matches ${p.render}$gs)"
      case block if isBlock(block) =>
        val inner = ind + "    "
        renderBlock(block, inner).mkString("(\n", "\n", s"\n$ind)")
      case other =>
        sys.error(s"unreachable: $other")
    }

  // ---------------------------------------------------------------------------
  // Shrinking: smaller programs that may not compile, which the property
  // treats as undecided

  def sortOf(e: Ex, scope: Scope, helpers: List[Helper]): Sort =
    e match {
      case IntLit(_) | Opaque(_, _)     => IntS
      case BoolLit(_) | Matches(_, _, _) => BoolS
      case CmpLit(_)                    => CmpS
      case Ref(n)                       => scope.getOrElse(n, IntS)
      case Call(fn, _)                  =>
        predefFns
          .collectFirst { case (`fn`, _, res) => res }
          .orElse(helpers.collectFirst { case h if h.name == fn => h.result })
          .getOrElse(IntS)
      case If(_, t, _, _)               => sortOf(t, scope, helpers)
      case IfMatches(_, _, _, _, f)     => sortOf(f, scope, helpers)
      case Match(arg, branches)         =>
        val last = branches.last
        val s = sortOf(arg, scope, helpers)
        sortOf(last.body, scope ++ last.pat.names.map(_ -> s), helpers)
      case Let(n, v, body)              =>
        sortOf(body, scope + (n -> sortOf(v, scope, helpers)), helpers)
      case TupleMatch(_, s1, _, s2, p1, p2, body) =>
        sortOf(body, scope ++ p1.names.map(_ -> s1) ++ p2.names.map(_ -> s2), helpers)
    }

  def simplestLit(s: Sort): Ex =
    s match {
      case IntS  => IntLit(BigInt(0))
      case BoolS => BoolLit(false)
      case CmpS  => CmpLit(0)
    }

  def shrinkPat(p: Pat): LazyList[Pat] =
    p match {
      case Pat.Named(inner, _)  => LazyList(inner)
      case Pat.Union(h :: _)    => LazyList(h)
      case Pat.IntP(v) if v != 0 => LazyList(Pat.IntP(BigInt(0)))
      case _                    => LazyList.empty
    }

  def shrinkGuard(g: Guard, scope: Scope, helpers: List[Helper]): LazyList[Guard] =
    g match {
      case Guard.BoolG(c) => shrinkEx(c, scope, helpers).map(Guard.BoolG(_))
      case Guard.MatchG(arg, s, pat, inner) =>
        val scope1 = scope ++ pat.names.map(_ -> s)
        inner.fold(LazyList.empty[Guard])(_ => LazyList(Guard.MatchG(arg, s, pat, None))) #:::
          shrinkPat(pat).map(Guard.MatchG(arg, s, _, inner)) #:::
          shrinkEx(arg, scope, helpers).map(Guard.MatchG(_, s, pat, inner)) #:::
          inner.fold(LazyList.empty[Guard])(i =>
            shrinkEx(i, scope1, helpers).map(i1 => Guard.MatchG(arg, s, pat, Some(i1)))
          )
    }

  def shrinkBranch(b: Branch, argSort: Sort, scope: Scope, helpers: List[Helper]): LazyList[Branch] = {
    val scope1 = scope ++ b.pat.names.map(_ -> argSort)
    val scope2 = b.guard match {
      case Some(Guard.MatchG(_, s, pat, _)) => scope1 ++ pat.names.map(_ -> s)
      case _                                => scope1
    }
    b.guard.fold(LazyList.empty[Branch])(_ => LazyList(b.copy(guard = None))) #:::
      shrinkPat(b.pat).map(p => b.copy(pat = p)) #:::
      b.guard.fold(LazyList.empty[Guard])(shrinkGuard(_, scope1, helpers)).map(g => b.copy(guard = Some(g))) #:::
      shrinkEx(b.body, scope2, helpers).map(e => b.copy(body = e))
  }

  // replace a list item with each of its shrinks
  def shrinkEach[A](as: List[A])(fn: A => LazyList[A]): LazyList[List[A]] =
    LazyList.from(as.indices).flatMap { i =>
      fn(as(i)).map(a1 => as.updated(i, a1))
    }

  def shrinkEx(e: Ex, scope: Scope, helpers: List[Helper]): LazyList[Ex] = {
    val sort = sortOf(e, scope, helpers)
    val lit = simplestLit(sort)
    def sameSort(cands: List[(Ex, Scope)]): LazyList[Ex] =
      LazyList.from(cands).collect {
        case (c, sc) if sortOf(c, sc, helpers) == sort => c
      }
    val toLit = if (e == lit) LazyList.empty else LazyList(lit)
    val smaller: LazyList[Ex] =
      e match {
        case IntLit(v) =>
          LazyList(v / 2).filter(v1 => (v1 != v) && (v1 != BigInt(0))).map(IntLit(_))
        case BoolLit(_) | CmpLit(_) | Ref(_) => LazyList.empty
        case Call(fn, args) =>
          sameSort(args.map(_ -> scope)) #:::
            shrinkEach(args)(shrinkEx(_, scope, helpers)).map(Call(fn, _))
        case If(c, t, f, elif) =>
          LazyList(t, f) #:::
            shrinkEx(c, scope, helpers).map(If(_, t, f, elif)) #:::
            shrinkEx(t, scope, helpers).map(If(c, _, f, elif)) #:::
            shrinkEx(f, scope, helpers).map(If(c, t, _, elif))
        case IfMatches(arg, pat, guard, t, f) =>
          val s = sortOf(arg, scope, helpers)
          val scope1 = scope ++ pat.names.map(_ -> s)
          LazyList(f, t) #:::
            guard.fold(LazyList.empty[Ex])(_ => LazyList(IfMatches(arg, pat, None, t, f))) #:::
            shrinkPat(pat).map(IfMatches(arg, _, guard, t, f)) #:::
            shrinkEx(arg, scope, helpers).map(IfMatches(_, pat, guard, t, f)) #:::
            guard.fold(LazyList.empty[Ex])(g =>
              shrinkEx(g, scope1, helpers).map(g1 => IfMatches(arg, pat, Some(g1), t, f))
            ) #:::
            shrinkEx(t, scope1, helpers).map(IfMatches(arg, pat, guard, _, f)) #:::
            shrinkEx(f, scope, helpers).map(IfMatches(arg, pat, guard, t, _))
        case Match(arg, branches) =>
          val s = sortOf(arg, scope, helpers)
          val bodies = branches.map(b => b.body -> (scope ++ b.pat.names.map(_ -> s)))
          // drop a branch other than the last, which keeps the match total
          val dropped =
            LazyList.from(branches.indices.init).map(i => Match(arg, branches.patch(i, Nil, 1)))
          sameSort(bodies) #::: dropped #:::
            shrinkEx(arg, scope, helpers).map(Match(_, branches)) #:::
            shrinkEach(branches)(shrinkBranch(_, s, scope, helpers)).map(Match(arg, _))
        case Matches(arg, pat, guard) =>
          val s = sortOf(arg, scope, helpers)
          guard.fold(LazyList.empty[Ex])(_ => LazyList(Matches(arg, pat, None))) #:::
            shrinkPat(pat).map(Matches(arg, _, guard)) #:::
            shrinkEx(arg, scope, helpers).map(Matches(_, pat, guard)) #:::
            guard.fold(LazyList.empty[Ex])(g =>
              shrinkEx(g, scope ++ pat.names.map(_ -> s), helpers).map(g1 => Matches(arg, pat, Some(g1)))
            )
        case Opaque(a, b) =>
          LazyList(a, b) #:::
            shrinkEx(a, scope, helpers).map(Opaque(_, b)) #:::
            shrinkEx(b, scope, helpers).map(Opaque(a, _))
        case TupleMatch(a, s1, b, s2, p1, p2, body) =>
          val scope1 = scope ++ p1.names.map(_ -> s1) ++ p2.names.map(_ -> s2)
          sameSort(List(body -> scope)) #:::
            shrinkEx(a, scope, helpers).map(TupleMatch(_, s1, b, s2, p1, p2, body)) #:::
            shrinkEx(b, scope, helpers).map(TupleMatch(a, s1, _, s2, p1, p2, body)) #:::
            shrinkEx(body, scope1, helpers).map(TupleMatch(a, s1, b, s2, p1, p2, _))
        case Let(n, v, body) =>
          val vs = sortOf(v, scope, helpers)
          val scope1 = scope + (n -> vs)
          sameSort(List(body -> scope, v -> scope)) #:::
            shrinkEx(v, scope, helpers).map(Let(n, _, body)) #:::
            shrinkEx(body, scope1, helpers).map(Let(n, v, _))
      }
    toLit #::: smaller
  }

  def shrinkProgram(p: Program): LazyList[Program] = {
    // drop helpers, then shrink the body
    val fewerHelpers =
      LazyList.from(p.helpers.indices).map(i => p.copy(helpers = p.helpers.patch(i, Nil, 1)))
    val bodies = shrinkEx(p.body, argScope, p.helpers).map(b => p.copy(body = b))
    fewerHelpers #::: bodies
  }

  // ---------------------------------------------------------------------------
  // Generators

  final case class Args(x: BigInt, y: BigInt, z: BigInt, p: Boolean, q: Int) {
    override def toString: String =
      s"target($x, $y, $z, ${if (p) "True" else "False"}, ${cmpName(q)})"
  }

  // mostly small values, so they hit literal patterns, plus some large ones
  val argIntGen: Gen[BigInt] =
    Gen.frequency(
      8 -> Gen.chooseNum(-4, 4).map(BigInt(_)),
      2 -> LinearBoundsGens.bigIntGen
    )

  val argsGen: Gen[Args] =
    for {
      x <- argIntGen
      y <- argIntGen
      z <- argIntGen
      p <- Gen.oneOf(true, false)
      q <- Gen.oneOf(-1, 0, 1)
    } yield Args(x, y, z, p, q)

  val argsListGen: Gen[List[Args]] =
    Gen.listOfN(16, argsGen).map(Args(0, 0, 0, false, 0) :: _)

  // binders shadow the arguments and each other
  val binderNames: Vector[String] = Vector("x", "y", "z", "p", "q", "a", "b", "c")

  val intLitGen: Gen[BigInt] =
    Gen.frequency(
      8 -> Gen.chooseNum(-4, 4).map(BigInt(_)),
      2 -> Gen.chooseNum(-100, 100).map(BigInt(_)),
      1 -> LinearBoundsGens.bigIntGen
    )

  // zero, negative and large divisors, as well as small positive ones
  val divisorLitGen: Gen[BigInt] =
    Gen.frequency(
      3 -> Gen.const(BigInt(0)),
      4 -> Gen.chooseNum(-5, -1).map(BigInt(_)),
      4 -> Gen.chooseNum(1, 5).map(BigInt(_)),
      1 -> LinearBoundsGens.bigIntGen
    )

  type Scope = Map[String, Sort]

  // recent: the names bound most recently, newest first. Uses favor them, so
  // that shadowing matters.
  // fallbacks: also use opaque calls and tuple matches, which lower to terms
  // over fresh symbols, or not at all.
  final case class Ctx(
      scope: Scope,
      recent: List[String],
      helpers: List[Helper],
      fallbacks: Boolean
  ) {
    def bind(names: List[String], sort: Sort): Ctx =
      copy(scope = scope ++ names.map(_ -> sort), recent = (names ::: recent).take(3))
    def namesOf(sort: Sort): List[String] =
      scope.iterator.collect { case (n, s) if s == sort => n }.toList.sorted
  }

  def litGen(sort: Sort): Gen[Ex] =
    sort match {
      case IntS  => intLitGen.map(IntLit(_))
      case BoolS => Gen.oneOf(true, false).map(BoolLit(_))
      case CmpS  => Gen.oneOf(-1, 0, 1).map(CmpLit(_))
    }

  def leafGen(sort: Sort, ctx: Ctx): Gen[Ex] =
    ctx.namesOf(sort) match {
      case Nil   => litGen(sort)
      case names =>
        val recent = ctx.recent.filter(names.contains)
        val recentGen =
          if (recent.isEmpty) Nil else List(3 -> Gen.oneOf(recent).map(Ref(_)))
        Gen.frequency(
          (1 -> litGen(sort)) :: (3 -> Gen.oneOf(names).map(Ref(_))) :: recentGen*
        )
    }

  val sortGen: Gen[Sort] = Gen.frequency(5 -> IntS, 3 -> BoolS, 2 -> CmpS)

  // (function, argument sorts, result sort)
  val predefFns: List[(String, List[Sort], Sort)] =
    List(
      ("add", List(IntS, IntS), IntS),
      ("sub", List(IntS, IntS), IntS),
      ("mul", List(IntS, IntS), IntS),
      ("div", List(IntS, IntS), IntS),
      ("mod_Int", List(IntS, IntS), IntS),
      ("eq_Int", List(IntS, IntS), BoolS),
      ("cmp_Int", List(IntS, IntS), CmpS),
      ("and", List(BoolS, BoolS), BoolS),
      ("or", List(BoolS, BoolS), BoolS),
      ("xor", List(BoolS, BoolS), BoolS),
      ("implies", List(BoolS, BoolS), BoolS),
      ("not", List(BoolS), BoolS)
    )

  def callGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] = {
    val predef = predefFns.filter(_._3 == sort)
    val helpers = ctx.helpers.collect {
      case h if h.result == sort => (h.name, h.params.map(_._2), h.result)
    }
    // helpers are few, so call them more often than any one Predef function
    val fnGen =
      if (helpers.isEmpty) Gen.oneOf(predef)
      else Gen.frequency(2 -> Gen.oneOf(predef), 1 -> Gen.oneOf(helpers))
    fnGen.flatMap { case (fn, argSorts, _) =>
      val divLike = (fn == "div") || (fn == "mod_Int")
      val argGens = argSorts.zipWithIndex.map { case (s, idx) =>
        if (divLike && (idx == 1))
          Gen.frequency(
            2 -> divisorLitGen.map(IntLit(_)),
            1 -> exGen(s, ctx, depth - 1)
          )
        else exGen(s, ctx, depth - 1)
      }
      Gen.sequence[List[Ex], Ex](argGens).map(Call(fn, _))
    }
  }

  def literalPatGen(sort: Sort): Gen[Pat] =
    sort match {
      case IntS  => Gen.chooseNum(-3, 3).map(i => Pat.IntP(BigInt(i)))
      case BoolS => Gen.oneOf(true, false).map(Pat.BoolP(_))
      case CmpS  => Gen.oneOf(-1, 0, 1).map(Pat.CmpP(_))
    }

  // usually a name already in scope, so the binding shadows it
  // a name only ever bound to a value of the sort
  def spareName(sort: Sort): String =
    sort match {
      case IntS  => "n0"
      case BoolS => "f0"
      case CmpS  => "c0"
    }

  // A name to bind a value of `sort` to, usually one in scope, which the
  // binding shadows. Bosatsu rejects a shadowing binding that changes the
  // type, so never a name in scope with another sort.
  def binderGen(ctx: Ctx, sort: Sort): Gen[String] = {
    val free = spareName(sort) :: binderNames.filterNot(ctx.scope.contains).toList
    ctx.namesOf(sort) match {
      case Nil     => Gen.oneOf(free)
      case inScope => Gen.frequency(2 -> Gen.oneOf(inScope), 1 -> Gen.oneOf(free))
    }
  }

  // a pattern that may not match
  def refutablePatGen(sort: Sort, ctx: Ctx): Gen[Pat] = {
    val lit = literalPatGen(sort)
    val union = Gen.listOfN(2, lit).map(Pat.Union(_))
    Gen.frequency(
      5 -> lit,
      2 -> union,
      2 -> Gen.zip(Gen.oneOf(lit, union), binderGen(ctx, sort)).map { case (p, n) =>
        Pat.Named(p, n)
      }
    )
  }

  // `x matches True | False` is an error, since it is always True
  def notTotal(p: Pat): Pat =
    p match {
      case Pat.Union(items) if items.toSet == Set(Pat.BoolP(true), Pat.BoolP(false)) =>
        items.head
      case Pat.Named(inner, n) => Pat.Named(notTotal(inner), n)
      case other               => other
    }

  // a pattern that always matches
  def catchAllGen(ctx: Ctx, sort: Sort): Gen[Pat] =
    Gen.frequency(
      3 -> Gen.const(Pat.Wild),
      3 -> binderGen(ctx, sort).map(Pat.Bind(_)),
      1 -> binderGen(ctx, sort).map(Pat.Named(Pat.Wild, _))
    )

  // Bosatsu drops a literal `if True` guard, which would make some
  // `matches` always true or binding, so a guard is never literally True
  def condGen(ctx: Ctx, depth: Int): Gen[Ex] =
    exGen(BoolS, ctx, depth).map {
      case BoolLit(true) => BoolLit(false)
      case other         => other
    }

  // `if x matches p:` binds the names in p in the if branch, even in
  // parentheses, so a plain Bool condition must not be a matches
  def plainCond(e: Ex): Ex =
    e match {
      case m @ Matches(_, _, _) => Call("and", List(BoolLit(true), m))
      case other                => other
    }

  // a test of a name, such as one a pattern just bound
  def nameTestGen(name: String, sort: Sort): Gen[Ex] = {
    val k = Gen.chooseNum(-3, 3).map(i => IntLit(BigInt(i)))
    val c = Gen.oneOf(-1, 0, 1).map(Pat.CmpP(_))
    sort match {
      case IntS =>
        Gen.oneOf(
          k.map(k => Call("eq_Int", List(Ref(name), k))),
          Gen.zip(k, c).map { case (k, c) =>
            plainCond(Matches(Call("cmp_Int", List(Ref(name), k)), c, None))
          }
        )
      case BoolS => Gen.oneOf(Ref(name), Call("not", List(Ref(name))))
      case CmpS  => c.map(c => plainCond(Matches(Ref(name), c, None)))
    }
  }

  def guardGen(ctx: Ctx, depth: Int): Gen[Option[Guard]] = {
    // usually about the names the branch's pattern binds
    val nameTests = ctx.recent.flatMap { n =>
      ctx.scope.get(n).map(s => 2 -> nameTestGen(n, s).map(c => Some(Guard.BoolG(c))))
    }
    Gen.frequency(
      (3 -> Gen.const(None)) :: (3 -> guardExprGen(ctx, depth)) :: nameTests.take(1)*
    )
  }

  def guardExprGen(ctx: Ctx, depth: Int): Gen[Option[Guard]] =
    Gen.frequency(
      3 -> condGen(ctx, depth - 1).map(c => Some(Guard.BoolG(plainCond(c)))),
      2 -> (for {
        s <- sortGen
        arg <- exGen(s, ctx, depth - 1)
        pat <- Gen.frequency(3 -> refutablePatGen(s, ctx), 1 -> catchAllGen(ctx, s))
        inner <- Gen.option(condGen(ctx.bind(pat.names, s), depth - 1))
      } yield Some(Guard.MatchG(arg, s, pat, inner)))
    )

  def guardNames(g: Option[Guard]): Ctx => Ctx =
    g match {
      case Some(Guard.MatchG(_, s, pat, _)) => _.bind(pat.names, s)
      case _                                => identity
    }

  def matchGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s <- sortGen
      arg <- exGen(s, ctx, depth - 1)
      n <- Gen.chooseNum(0, 3)
      init <- Gen.listOfN(
        n,
        for {
          pat <- Gen.frequency(4 -> refutablePatGen(s, ctx), 1 -> catchAllGen(ctx, s))
          guard <- guardGen(ctx.bind(pat.names, s), depth)
          body <- exGen(sort, guardNames(guard)(ctx.bind(pat.names, s)), depth - 1)
        } yield Branch(pat, guard, body)
      )
      last <- lastBranchesGen(s, sort, ctx, depth)
    } yield Match(arg, init ::: last)

  // unguarded branches that make the match total
  def lastBranchesGen(s: Sort, sort: Sort, ctx: Ctx, depth: Int): Gen[List[Branch]] = {
    val catchAll =
      for {
        pat <- catchAllGen(ctx, s)
        body <- exGen(sort, ctx.bind(pat.names, s), depth - 1)
      } yield List(Branch(pat, None, body))
    val pats: List[Pat] =
      s match {
        case IntS  => Nil
        case BoolS => List(Pat.BoolP(true), Pat.BoolP(false))
        case CmpS  => List(Pat.Union(List(Pat.CmpP(-1), Pat.CmpP(0))), Pat.CmpP(1))
      }
    val literals =
      Gen.sequence[List[Branch], Branch](pats.map { p =>
        exGen(sort, ctx, depth - 1).map(Branch(p, None, _))
      })
    if (pats.isEmpty) catchAll else Gen.oneOf(catchAll, literals)
  }

  def letGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s <- sortGen
      name <- binderGen(ctx, s)
      value <-
        if (ctx.fallbacks && (s == IntS))
          Gen.frequency(2 -> exGen(s, ctx, depth - 1), 1 -> opaqueGen(ctx, depth - 1))
        else exGen(s, ctx, depth - 1)
      body <- exGen(sort, ctx.bind(name :: Nil, s), depth - 1)
    } yield Let(name, value, body)

  def ifGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      c <- exGen(BoolS, ctx, depth - 1).map(plainCond)
      t <- exGen(sort, ctx, depth - 1)
      f <- exGen(sort, ctx, depth - 1)
      elif <- Gen.oneOf(true, false)
    } yield If(c, t, f, elif)

  def ifMatchesGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s <- sortGen
      arg <- exGen(s, ctx, depth - 1)
      pat <- refutablePatGen(s, ctx).map(notTotal)
      ctx1 = ctx.bind(pat.names, s)
      guard <- Gen.option(condGen(ctx1, depth - 1))
      t <- exGen(sort, ctx1, depth - 1)
      f <- exGen(sort, ctx, depth - 1)
    } yield IfMatches(arg, pat, guard, t, f)

  // `arg matches pat` must not bind names or always match, unless guarded
  def matchesGen(ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s <- sortGen
      arg <- exGen(s, ctx, depth - 1)
      guarded <- Gen.oneOf(true, false)
      res <-
        if (guarded)
          for {
            pat <- Gen.oneOf(refutablePatGen(s, ctx), catchAllGen(ctx, s))
            g <- condGen(ctx.bind(pat.names, s), depth - 1)
          } yield Matches(arg, pat, Some(g))
        else
          literalPatGen(s).map(Matches(arg, _, None))
    } yield res

  def opaqueGen(ctx: Ctx, depth: Int): Gen[Ex] =
    Gen.zip(exGen(IntS, ctx, depth - 1), exGen(IntS, ctx, depth - 1)).map {
      case (a, b) => Opaque(a, b)
    }

  def tupleMatchGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s1 <- sortGen
      s2 <- sortGen
      a <- exGen(s1, ctx, depth - 1)
      b <- exGen(s2, ctx, depth - 1)
      p1 <- catchAllGen(ctx, s1)
      // a pattern binds each name at most once
      p2 <- catchAllGen(ctx, s2).map { p =>
        if (p.names.exists(p1.names.contains)) Pat.Wild else p
      }
      body <- exGen(sort, ctx.bind(p1.names, s1).bind(p2.names, s2), depth - 1)
    } yield TupleMatch(a, s1, b, s2, p1, p2, body)

  def exGen(sort: Sort, ctx: Ctx, depth: Int): Gen[Ex] =
    if (depth <= 0) leafGen(sort, ctx)
    else {
      val common: List[(Int, Gen[Ex])] =
        List(
          2 -> leafGen(sort, ctx),
          6 -> callGen(sort, ctx, depth),
          2 -> ifGen(sort, ctx, depth),
          1 -> ifMatchesGen(sort, ctx, depth),
          3 -> matchGen(sort, ctx, depth),
          3 -> letGen(sort, ctx, depth)
        )
      val withFallbacks =
        if (!ctx.fallbacks) common
        else
          (2 -> tupleMatchGen(sort, ctx, depth)) :: (
            if (sort == IntS) (2 -> opaqueGen(ctx, depth)) :: common
            else common
          )
      val gens =
        if (sort == BoolS) (2 -> matchesGen(ctx, depth)) :: withFallbacks
        else withFallbacks
      Gen.lzy(Gen.frequency(gens*))
    }

  val argScope: Scope =
    Map("x" -> IntS, "y" -> IntS, "z" -> IntS, "p" -> BoolS, "q" -> CmpS)

  def helperGen(idx: Int, earlier: List[Helper]): Gen[Helper] = {
    val name = s"helper$idx"
    val forwarding: Gen[Helper] =
      Gen.oneOf(predefFns).flatMap { case (fn, argSorts, res) =>
        val params = argSorts.zip(List("a", "b"))
        Gen.frequency(
          2 -> Gen.const(
            Helper(name, params.map { case (s, n) => (n, s) }, res, HelperBody.ValueAlias(fn))
          ),
          // forwards its arguments, maybe in a different order
          3 -> Gen.oneOf(params.permutations.toList).map { perm =>
            Helper(
              name,
              params.map { case (s, n) => (n, s) },
              res,
              HelperBody.Body(Call(fn, perm.map { case (_, n) => Ref(n) }))
            )
          }
        )
      }
    val general: Gen[Helper] =
      for {
        nparams <- Gen.chooseNum(1, 2)
        sorts <- Gen.listOfN(nparams, sortGen)
        res <- sortGen
        params = sorts.zip(List("a", "b")).map { case (s, n) => (n, s) }
        body <- exGen(res, Ctx(params.toMap, Nil, earlier, fallbacks = false), 2)
      } yield Helper(name, params, res, HelperBody.Body(body))
    Gen.frequency(1 -> forwarding, 2 -> general)
  }

  val helpersGen: Gen[List[Helper]] =
    Gen.chooseNum(0, 3).flatMap { n =>
      (0 until n).foldLeft(Gen.const(List.empty[Helper])) { (accGen, idx) =>
        accGen.flatMap(acc => helperGen(idx, acc).map(acc :+ _))
      }
    }

  val programGen: Gen[Program] =
    for {
      helpers <- helpersGen
      res <- sortGen
      depth <- Gen.chooseNum(1, 4)
      fallbacks <- Gen.frequency(3 -> false, 1 -> true)
      body <- exGen(res, Ctx(argScope, Nil, helpers, fallbacks), depth)
    } yield Program(helpers, res, body)

  // ---------------------------------------------------------------------------
  // Path programs: an Int body of lets and matches whose leaves are distinct
  // Int literals, so the value Bosatsu returns names the leaf it reached.
  // Scrutinees, guards and let values are any expressions.

  def isPathTree(e: Ex): Boolean =
    e match {
      case IntLit(_)          => true
      case Let(_, _, body)    => isPathTree(body)
      case Match(_, branches) => branches.forall(b => isPathTree(b.body))
      case _                  => false
    }

  // number the leaves 0, 1, 2, ... in order
  def numberLeaves(e: Ex): Ex = {
    def go(e: Ex, next: Int): (Ex, Int) =
      e match {
        case Let(n, v, body) =>
          val (body1, next1) = go(body, next)
          (Let(n, v, body1), next1)
        case Match(arg, branches) =>
          val (revBranches, nextN) =
            branches.foldLeft((List.empty[Branch], next)) { case ((acc, n), b) =>
              val (body1, n1) = go(b.body, n)
              (b.copy(body = body1) :: acc, n1)
            }
          (Match(arg, revBranches.reverse), nextN)
        case _ =>
          (IntLit(BigInt(next)), next + 1)
      }
    go(e, 0)._1
  }

  private val leafPlaceholder: Ex = IntLit(BigInt(0))

  // small scrutinees, guards and values: the facts are the point
  private val pathExDepth = 2

  def pathGen(ctx: Ctx, depth: Int): Gen[Ex] =
    if (depth <= 0) Gen.const(leafPlaceholder)
    else
      Gen.lzy(
        Gen.frequency(
          1 -> Gen.const(leafPlaceholder),
          2 -> pathLetGen(ctx, depth),
          5 -> pathMatchGen(ctx, depth)
        )
      )

  def pathLetGen(ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s <- sortGen
      name <- binderGen(ctx, s)
      value <- exGen(s, ctx, pathExDepth)
      body <- pathGen(ctx.bind(name :: Nil, s), depth - 1)
    } yield Let(name, value, body)

  // Often a name in scope of the scrutinee's sort that earlier guards test
  // and later patterns rebind. An earlier guard is about the outer name, so
  // the fact that it was false must not become a fact about the new one.
  def shadowNameGen(ctx: Ctx, s: Sort): Gen[Option[String]] =
    ctx.namesOf(s) match {
      case Nil   => Gen.const(None)
      case names => Gen.frequency(1 -> Gen.const(None), 2 -> Gen.oneOf(names).map(Some(_)))
    }

  // a catch-all pattern, which subsumes the later branches, that may rebind
  // the shadow name
  def pathCatchAllGen(ctx: Ctx, sort: Sort, shadow: Option[String]): Gen[Pat] =
    shadow match {
      case None    => catchAllGen(ctx, sort)
      case Some(n) => Gen.frequency(2 -> catchAllGen(ctx, sort), 1 -> Gen.const(Pat.Bind(n)))
    }

  def pathMatchGen(ctx: Ctx, depth: Int): Gen[Ex] =
    for {
      s <- sortGen
      arg <- exGen(s, ctx, pathExDepth)
      shadow <- shadowNameGen(ctx, s)
      // guards test the most recent name, which is the shadow name when the
      // pattern binds none
      guardCtx = shadow.fold(ctx)(n => ctx.copy(recent = (n :: ctx.recent).take(3)))
      n <- Gen.chooseNum(1, 3)
      init <- Gen.listOfN(
        n,
        for {
          pat <- Gen.frequency(3 -> refutablePatGen(s, ctx), 2 -> pathCatchAllGen(ctx, s, shadow))
          guard <- guardGen(guardCtx.bind(pat.names, s), pathExDepth)
          body <- pathGen(guardNames(guard)(ctx.bind(pat.names, s)), depth - 1)
        } yield Branch(pat, guard, body)
      )
      last <- lastPathBranchesGen(s, ctx, shadow, depth)
    } yield Match(arg, init ::: last)

  // unguarded branches that make the match total
  def lastPathBranchesGen(
      s: Sort,
      ctx: Ctx,
      shadow: Option[String],
      depth: Int
  ): Gen[List[Branch]] = {
    val catchAll =
      for {
        pat <- pathCatchAllGen(ctx, s, shadow)
        body <- pathGen(ctx.bind(pat.names, s), depth - 1)
      } yield List(Branch(pat, None, body))
    val pats: List[Pat] =
      s match {
        case IntS  => Nil
        case BoolS => List(Pat.BoolP(true), Pat.BoolP(false))
        case CmpS  => List(Pat.Union(List(Pat.CmpP(-1), Pat.CmpP(0))), Pat.CmpP(1))
      }
    val literals =
      Gen.sequence[List[Branch], Branch](pats.map { p =>
        pathGen(ctx, depth - 1).map(Branch(p, None, _))
      })
    if (pats.isEmpty) catchAll else Gen.oneOf(catchAll, literals)
  }

  val pathProgramGen: Gen[Program] =
    for {
      helpers <- helpersGen
      depth <- Gen.chooseNum(1, 3)
      fallbacks <- Gen.frequency(3 -> false, 1 -> true)
      body <- pathGen(Ctx(argScope, Nil, helpers, fallbacks), depth)
    } yield Program(helpers, IntS, numberLeaves(body))
}
