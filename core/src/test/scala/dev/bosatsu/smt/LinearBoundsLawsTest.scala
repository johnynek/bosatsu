package dev.bosatsu.smt

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}

import LinearBounds.{Interval, Lin, Rel}

/** The laws in the doc comments of LinearBounds, one property (or a few) per
  * function or method, stated the same way.
  *
  * Lin laws are checked through `eval`, a homomorphism into integer
  * arithmetic, and by structural equality, which means equality of functions
  * because Lins are canonical. Interval laws are checked through `contains`,
  * a homomorphism into sets of integers: each operation must contain exactly
  * the right members, checked at probe points that decide set equality for
  * intervals (see LinearBoundsGens.probes).
  */
class LinearBoundsLawsTest extends munit.ScalaCheckSuite {
  import SmtExpr.*
  import LinearBoundsGens.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (dev.bosatsu.Platform.isJvm) 500 else 100)
      .withMaxDiscardRatio(20)

  private def all(props: Prop*): Prop = Prop.all(props*)

  // ---------------------------------------------------------------------------
  // floorDiv and ceilDiv

  property("floorDiv: k * q <= a < k * (q + 1)") {
    forAll(bigIntGen, positiveBigIntGen) { (a, k) =>
      val q = LinearBounds.floorDiv(a, k)
      ((k * q <= a) && (a < k * (q + 1))) :| s"a=$a k=$k q=$q"
    }
  }

  property("ceilDiv: k * (q - 1) < a <= k * q") {
    forAll(bigIntGen, positiveBigIntGen) { (a, k) =>
      val q = LinearBounds.ceilDiv(a, k)
      ((k * (q - 1) < a) && (a <= k * q)) :| s"a=$a k=$k q=$q"
    }
  }

  property("floorDiv agrees with SMT-LIB div for positive divisors") {
    forAll(bigIntGen, positiveBigIntGen) { (a, k) =>
      LinearBounds.floorDiv(a, k) == euclid(a, k)._1
    }
  }

  // ---------------------------------------------------------------------------
  // Lin

  property("Lin: every Lin is canonical, with no zero coefficients") {
    forAll(linGen) { l => l.coeffs.values.forall(_ != 0) }
  }

  property("Lin.of drops zero coefficients and evaluates its terms") {
    forAll(
      bigIntGen,
      Gen.mapOf(Gen.zip(Gen.oneOf(linVarNames), Gen.frequency(3 -> bigIntGen, 1 -> Gen.const(BigInt(0))))),
      linEnvGen
    ) { (c, m, env) =>
      val l = Lin.of(c, m)
      all(
        l.coeffs.values.forall(_ != 0) :| "canonical",
        (l.eval(env) == m.foldLeft(c) { case (acc, (v, k)) => acc + k * env(v) }) :| "eval"
      )
    }
  }

  property("Lin.of(a.const, a.coeffs) == a") {
    forAll(linGen) { a => Lin.of(a.const, a.coeffs) == a }
  }

  property("Lin.constant, Lin.variable and Lin.zero evaluate as defined") {
    forAll(bigIntGen, Gen.oneOf(linVarNames), linEnvGen) { (c, v, env) =>
      all(
        (Lin.constant(c).eval(env) == c) :| "constant",
        Lin.constant(c).coeffs.isEmpty :| "constant has no variables",
        (Lin.variable(v).eval(env) == env(v)) :| "variable",
        (Lin.zero.eval(env) == 0) :| "zero"
      )
    }
  }

  property("Lin: eval is a homomorphism for +, -, *, unary - and plus") {
    forAll(linGen, linGen, bigIntGen, linEnvGen) { (a, b, k, env) =>
      all(
        ((a + b).eval(env) == a.eval(env) + b.eval(env)) :| "+",
        ((a - b).eval(env) == a.eval(env) - b.eval(env)) :| "-",
        ((a * k).eval(env) == a.eval(env) * k) :| "*",
        ((-a).eval(env) == -a.eval(env)) :| "unary -",
        (a.plus(k).eval(env) == a.eval(env) + k) :| "plus"
      )
    }
  }

  property("Lin: + is associative and commutative, with identity zero and inverse -a") {
    forAll(linGen, linGen, linGen) { (a, b, c) =>
      all(
        ((a + b) + c == a + (b + c)) :| "associative",
        (a + b == b + a) :| "commutative",
        (a + Lin.zero == a) :| "right identity",
        (Lin.zero + a == a) :| "left identity",
        (a + (-a) == Lin.zero) :| "inverse",
        ((a + b) - b == a) :| "cancel"
      )
    }
  }

  property("Lin: * is a module action that distributes over +") {
    forAll(linGen, linGen, bigIntGen, bigIntGen) { (a, b, j, k) =>
      all(
        ((a * j) * k == a * (j * k)) :| "(a * j) * k == a * (j * k)",
        (a * BigInt(1) == a) :| "a * 1 == a",
        (a * BigInt(0) == Lin.zero) :| "a * 0 == zero",
        ((a + b) * k == a * k + b * k) :| "(a + b) * k == a * k + b * k",
        (a * (j + k) == a * j + a * k) :| "a * (j + k) == a * j + a * k"
      )
    }
  }

  property("Lin: -, unary - and plus are derived from + and *") {
    forAll(linGen, linGen, bigIntGen) { (a, b, k) =>
      all(
        (a - b == a + (-b)) :| "a - b == a + (-b)",
        (-a == a * BigInt(-1)) :| "-a == a * -1",
        (-(-a) == a) :| "-(-a) == a",
        (a - a == Lin.zero) :| "a - a == zero",
        (a.plus(k) == a + Lin.constant(k)) :| "a.plus(k) == a + constant(k)"
      )
    }
  }

  property("Lin: equal exactly when equal as functions") {
    forAll(linGen, linGen) { (a, b) =>
      val zeros = linVarNames.map(_ -> BigInt(0)).toMap
      val d = a - b
      if (a == b) (d == Lin.zero) :| "equal Lins have zero difference"
      else {
        // a nonzero canonical Lin is nonzero at the origin or at a unit
        // vector, so some env tells a and b apart
        val envs = zeros :: d.coeffs.keys.toList.map(v => zeros.updated(v, BigInt(1)))
        envs.exists(env => a.eval(env) != b.eval(env)) :| s"no env separates $a and $b"
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Interval

  // contains, checked at the probe points and random values
  private def intervalAndPointGen: Gen[(Interval, BigInt)] =
    intervalGen.flatMap(i => Gen.oneOf(Gen.oneOf(probes(i)), bigIntGen).map((i, _)))

  property("Interval.contains is lo <= x <= hi, inclusive, None unbounded") {
    forAll(intervalAndPointGen) { case (i, x) =>
      i.contains(x) == (i.lo.forall(_ <= x) && i.hi.forall(x <= _))
    }
  }

  property("Interval.isEmpty exactly when nothing is contained") {
    forAll(intervalGen) { i =>
      if (i.isEmpty) probes(i).forall(!i.contains(_)) :| "empty contains a probe"
      else
        all(
          i.lo.forall(i.contains) :| "contains lo",
          i.hi.forall(i.contains) :| "contains hi",
          probes(i).exists(i.contains) :| "nonempty contains no probe"
        )
    }
  }

  property("Interval.point, all and empty contain what they say") {
    forAll(intervalEndGen, bigIntGen) { (c, x) =>
      val pts = List(x, c, c - 1, c + 1)
      all(
        pts.forall(p => Interval.point(c).contains(p) == (p == c)) :| "point",
        pts.forall(Interval.all.contains) :| "all",
        pts.forall(p => !Interval.empty.contains(p)) :| "empty",
        Interval.empty.isEmpty :| "empty.isEmpty",
        !Interval.all.isEmpty :| "!all.isEmpty"
      )
    }
  }

  private val twoMembersGen: Gen[(Interval, BigInt, Interval, BigInt)] =
    for {
      a <- nonEmptyIntervalGen
      x <- memberGen(a)
      b <- nonEmptyIntervalGen
      y <- memberGen(b)
    } yield (a, x, b, y)

  property("Interval +: contains every sum of members") {
    forAll(twoMembersGen) { case (a, x, b, y) => (a + b).contains(x + y) }
  }

  // Is z == x + y for some x in a and y in b? The x that work form the
  // interval a meet (z - b), whose ends are among these candidates, so this
  // decides it.
  private def isSum(z: BigInt, a: Interval, b: Interval): Boolean = {
    val candidates =
      z :: a.lo.toList ::: a.hi.toList ::: b.lo.toList.map(z - _) ::: b.hi.toList.map(z - _)
    candidates.exists(x => a.contains(x) && b.contains(z - x))
  }

  property("Interval +: every member is a sum of members, and empty if either is empty") {
    forAll(intervalGen, intervalGen) { (a, b) =>
      val s = a + b
      // the true sums form an interval whose ends are sums of ends
      val ends = for {
        p <- a.lo.toList ::: a.hi.toList
        q <- b.lo.toList ::: b.hi.toList
      } yield p + q
      val pts = (probes(s, a, b) ::: ends.flatMap(e => List(e - 1, e, e + 1))).distinct
      all(
        pts.forall(z => s.contains(z) == isSum(z, a, b)) :| s"a + b == $s",
        (!(a.isEmpty || b.isEmpty) || s.isEmpty) :| "empty"
      )
    }
  }

  property("Interval unary -: (-a).contains(-x) == a.contains(x)") {
    forAll(intervalGen) { a =>
      val pts = probes(a, -a)
      all(
        pts.forall(x => (-a).contains(-x) == a.contains(x)) :| "negation",
        sameSet(-(-a), a) :| "-(-a)"
      )
    }
  }

  private val memberAndScaleGen: Gen[(Interval, BigInt, BigInt)] =
    for {
      a <- nonEmptyIntervalGen
      x <- memberGen(a)
      k <- Gen.frequency(4 -> Gen.chooseNum(-5, 5).map(BigInt(_)), 1 -> bigIntGen)
    } yield (a, x, k)

  property("Interval *: contains x * k for every member x") {
    forAll(memberAndScaleGen) { case (a, x, k) => (a * k).contains(x * k) }
  }

  property("Interval *: each bound is a bound of a times k, so nothing smaller works") {
    forAll(intervalGen, bigIntGen) { (a, k) =>
      val s = a * k
      val scaledEnds = (a.lo.toList ::: a.hi.toList).map(_ * k)
      if (a.isEmpty) s.isEmpty :| "empty * k"
      else if (k == 0) sameSet(s, Interval.point(BigInt(0))) :| "a * 0"
      else {
        // the side that comes from a.lo when k > 0, or from a.hi when k < 0
        val (fromLo, fromHi) = if (k > 0) (a.lo, a.hi) else (a.hi, a.lo)
        all(
          s.lo.forall(scaledEnds.contains) :| "lo is a scaled end",
          s.hi.forall(scaledEnds.contains) :| "hi is a scaled end",
          (s.lo.isEmpty == fromLo.isEmpty) :| "unbounded below exactly when a is on that side",
          (s.hi.isEmpty == fromHi.isEmpty) :| "unbounded above exactly when a is on that side"
        )
      }
    }
  }

  // Is z between two members of a or b? That's membership in the convex hull
  // of the union.
  private def inHull(z: BigInt, a: Interval, b: Interval): Boolean = {
    def hasAtMost(i: Interval) = !i.isEmpty && i.lo.forall(_ <= z)
    def hasAtLeast(i: Interval) = !i.isEmpty && i.hi.forall(_ >= z)
    (hasAtMost(a) || hasAtMost(b)) && (hasAtLeast(a) || hasAtLeast(b))
  }

  property("Interval join: exactly the convex hull of the union") {
    forAll(intervalGen, intervalGen) { (a, b) =>
      val j = a.join(b)
      val pts = probes(j, a, b)
      all(
        pts.forall(z => !(a.contains(z) || b.contains(z)) || j.contains(z)) :| "contains both",
        pts.forall(z => j.contains(z) == inHull(z, a, b)) :| s"hull, got $j"
      )
    }
  }

  property("Interval join: commutative, associative, idempotent, identity empty") {
    forAll(intervalGen, intervalGen, intervalGen) { (a, b, c) =>
      all(
        sameSet(a.join(b), b.join(a)) :| "commutative",
        sameSet(a.join(b).join(c), a.join(b.join(c))) :| "associative",
        sameSet(a.join(a), a) :| "idempotent",
        sameSet(Interval.empty.join(a), a) :| "left identity",
        sameSet(a.join(Interval.empty), a) :| "right identity"
      )
    }
  }

  property("Interval meet: (a meet b).contains(x) == (a.contains(x) && b.contains(x))") {
    forAll(intervalGen, intervalGen) { (a, b) =>
      val m = a.meet(b)
      probes(m, a, b).forall(x => m.contains(x) == (a.contains(x) && b.contains(x)))
    }
  }

  property("Interval meet: commutative, associative, idempotent, identity all; absorption") {
    forAll(intervalGen, intervalGen, intervalGen) { (a, b, c) =>
      all(
        sameSet(a.meet(b), b.meet(a)) :| "commutative",
        sameSet(a.meet(b).meet(c), a.meet(b.meet(c))) :| "associative",
        sameSet(a.meet(a), a) :| "idempotent",
        sameSet(Interval.all.meet(a), a) :| "identity",
        sameSet(a.meet(a.join(b)), a) :| "a meet (a join b) == a",
        sameSet(a.join(a.meet(b)), a) :| "a join (a meet b) == a"
      )
    }
  }

  property("Interval divFloor: contains floorDiv(x, k) for every member x") {
    forAll(nonEmptyIntervalGen.flatMap(a => memberGen(a).map((a, _))), positiveBigIntGen) {
      case ((a, x), k) => a.divFloor(k).contains(LinearBounds.floorDiv(x, k))
    }
  }

  property("Interval divFloor: each bound is floorDiv of a bound, empty if a is") {
    forAll(intervalGen, Gen.frequency(4 -> Gen.chooseNum(1, 6).map(BigInt(_)), 1 -> positiveBigIntGen)) {
      (a, k) =>
        val d = a.divFloor(k)
        if (a.isEmpty) d.isEmpty :| "empty"
        else
          all(
            (d.lo == a.lo.map(LinearBounds.floorDiv(_, k))) :| "lo",
            (d.hi == a.hi.map(LinearBounds.floorDiv(_, k))) :| "hi"
          )
    }
  }

  // ---------------------------------------------------------------------------
  // Box

  property("Box.interval is all for a variable with no interval") {
    forAll(boxGen) { b =>
      boxVarNames.forall(v => b.intervals.contains(v) || b.interval(v) == Interval.all)
    }
  }

  // envs on either side of the box's edges, one variable at a time, plus
  // anywhere near it
  private val boxAndEdgeEnvGen =
    boxGen.flatMap(b => Gen.frequency(3 -> envEdgeOfBoxGen(b), 1 -> envProbeBoxGen(b), 1 -> envNearBoxGen(b)).map((b, _)))

  property("Box.contains: every variable in its interval and not excluded") {
    forAll(boxAndEdgeEnvGen) { case (b, env) =>
      b.contains(env) ==
        (boxVarNames.forall(v => b.interval(v).contains(env(v))) &&
          boxVarNames.forall(v => !b.excluded.getOrElse(v, Set.empty[BigInt])(env(v))))
    }
  }

  property("tighten keeps exactly the points of the box") {
    forAll(boxAndEdgeEnvGen) {
      case (b, env) =>
        b.contains(env) == LinearBounds.tighten(b).exists(_.contains(env))
    }
  }

  property("addFact keeps every point of the box where the fact holds") {
    forAll(boxEnvGen, boxLinGen, relGen) { case ((b, env), lin, rel) =>
      rel.holds(lin.eval(env)) ==> LinearBounds.addFact(b, (lin, rel)).exists(_.contains(env))
    }
  }

  property("Rel.holds compares with 0") {
    forAll(bigIntGen) { x =>
      all(
        (Rel.Le.holds(x) == (x <= 0)) :| "Le",
        (Rel.Ge.holds(x) == (x >= 0)) :| "Ge",
        (Rel.Eq.holds(x) == (x == 0)) :| "Eq",
        (Rel.Ne.holds(x) == (x != 0)) :| "Ne"
      )
    }
  }

  // ---------------------------------------------------------------------------
  // Expressions

  private val smallEnvGen: Gen[Map[String, BigInt]] =
    Gen.listOfN(varNames.size, Gen.frequency(4 -> Gen.chooseNum(-8, 8).map(BigInt(_)), 1 -> bigIntGen))
      .map(vs => varNames.zip(vs).toMap)

  property("linear: the Lin it returns has the expression's value") {
    forAll(intExprGen(3, z3Safe = false), smallEnvGen) { (e, env) =>
      LinearBounds.linear(e) match {
        case Some(l) => Prop(l.eval(env) == evalInt(e, Env(env, Map.empty, byZeroChoices.head)))
        case None    => Prop.undecided
      }
    }
  }

  // booleans with mostly constant comparisons, so constantBool often answers
  private val mostlyConstantBoolGen: Gen[BoolExpr] =
    boolExprGen(3, Gen.frequency(4 -> constGen, 1 -> intExprGen(1, z3Safe = false)))

  property("constantBool: the value it returns holds at every env") {
    forAll(mostlyConstantBoolGen, smallEnvGen) { (p, env) =>
      LinearBounds.constantBool(p) match {
        case Some(v) => Prop(byZeroChoices.forall(bz => evalBool(p, Env(env, Map.empty, bz)) == v))
        case None    => Prop.undecided
      }
    }
  }

  private val anyBoolGen: Gen[BoolExpr] =
    Gen.oneOf(boolExprGen(3, intExprGen(2, z3Safe = false)), factGen(z3Safe = false), goalGen(z3Safe = false))

  property("atoms: a fact holds exactly when all its atoms do") {
    forAll(anyBoolGen, smallEnvGen) { (f, env) =>
      byZeroChoices.forall { bz =>
        val e = Env(env, Map.empty, bz)
        evalBool(f, e) == LinearBounds.atoms(f).forall(evalBool(_, e))
      }
    }
  }

  property("linearAtom: the atom holds exactly when rel holds of the Lin") {
    forAll(Gen.oneOf(singleVarFactGen, cmpGen(intExprGen(2, z3Safe = false), intExprGen(2, z3Safe = false)), anyBoolGen), smallEnvGen) {
      (a, env) =>
        LinearBounds.linearAtom(a) match {
          case Some((lin, rel)) =>
            Prop(evalBool(a, Env(env, Map.empty, byZeroChoices.head)) == rel.holds(lin.eval(env)))
          case None => Prop.undecided
        }
    }
  }

  private val smallGrid: List[Env] =
    for {
      x <- (-5 to 5).toList
      y <- (-5 to 5).toList
      z <- (-5 to 5).toList
      bz <- byZeroChoices.take(3)
    } yield Env(Map("x" -> BigInt(x), "y" -> BigInt(y), "z" -> BigInt(z)), Map.empty, bz)

  property("box: contains every env where all the facts hold, None only if none do") {
    forAll(factsGen(z3Safe = false)) { facts =>
      val bx = LinearBounds.box(facts)
      smallGrid.forall(env => !facts.forall(evalBool(_, env)) || bx.exists(_.contains(env.vars)))
    }
  }

  property("interval: contains the expression's value at every env in the box") {
    forAll(boxEnvGen, intExprGen(3, z3Safe = false)) { case ((b, env), e) =>
      val i = LinearBounds.interval(e, b)
      byZeroChoices.forall(bz => i.contains(evalInt(e, Env(env, Map.empty, bz))))
    }
  }

  property("mulInterval: contains every product of members, empty if either is") {
    forAll(twoMembersGen, intervalGen) { case ((a, x, b, y), e) =>
      all(
        LinearBounds.mulInterval(a, b).contains(x * y) :| "products",
        (!e.isEmpty || LinearBounds.mulInterval(e, a).isEmpty) :| "empty"
      )
    }
  }

  property("linInterval: contains lin's value at every env in the box") {
    forAll(boxEnvGen, boxLinGen) { case ((b, env), lin) =>
      LinearBounds.linInterval(lin, b).contains(lin.eval(env))
    }
  }

  property("bounds: every lower is <= the value and every upper >= it, in the box") {
    forAll(boxEnvGen, intExprGen(3, z3Safe = false)) { case ((b, env), e) =>
      val bs = LinearBounds.bounds(e, b)
      byZeroChoices.forall { bz =>
        val value = evalInt(e, Env(env, Map.empty, bz))
        bs.lowers.forall(_.eval(env) <= value) && bs.uppers.forall(value <= _.eval(env))
      }
    }
  }

  property("maxOver: an upper bound in the box, attained in its intervals") {
    forAll(boxEnvGen, boxLinGen) { case ((b, env), lin) =>
      LinearBounds.maxOver(lin, b) match {
        case Some(m) =>
          // the corner: each variable at the end that maximizes its term
          val corner = boxVarNames.map { v =>
            val c = lin.coeffs.getOrElse(v, BigInt(0))
            val i = b.interval(v)
            val end = if (c > 0) i.hi else if (c < 0) i.lo else None
            v -> end.orElse(i.lo).orElse(i.hi).getOrElse(BigInt(0))
          }.toMap
          all(
            (lin.eval(env) <= m) :| "upper bound",
            (lin.eval(corner) == m) :| "attained",
            (LinearBounds.linInterval(lin, b).hi == Some(m)) :| "agrees with linInterval"
          )
        case None => Prop.undecided
      }
    }
  }

  property("diffAtMost: the difference is at most the limit at every env in the box") {
    forAll(boxEnvGen, intExprGen(2, z3Safe = false), intExprGen(2, z3Safe = false), Gen.chooseNum(-3, 3)) {
      case ((b, env), a, c, limit) =>
        LinearBounds.diffAtMost(a, c, BigInt(limit), b) ==>
          byZeroChoices.forall { bz =>
            val e = Env(env, Map.empty, bz)
            evalInt(a, e) - evalInt(c, e) <= limit
          }
    }
  }

  property("proves: the goal holds at every env in the box") {
    forAll(boxEnvGen, goalGen(z3Safe = false)) { case ((b, env), goal) =>
      LinearBounds.proves(goal, b) ==>
        byZeroChoices.forall(bz => evalBool(goal, Env(env, Map.empty, bz)))
    }
  }
}
