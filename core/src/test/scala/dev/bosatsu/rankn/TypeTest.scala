package dev.bosatsu.rankn

import cats.data.NonEmptyList
import cats.syntax.all._
import dev.bosatsu.{Kind, TypedExpr}
import dev.bosatsu.hashing.{Algo, Hashable}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class TypeTest extends munit.ScalaCheckSuite {
  import NTypeGen.shrinkType

  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(1000)
  // PropertyCheckConfiguration(minSuccessful = 5)

  def parse(s: String): Type =
    Type.fullyResolvedParser.parseAll(s) match {
      case Right(t)  => t
      case Left(err) =>
        sys.error(
          s"failed to parse: <$s> at ${s.drop(err.failedAtOffset)}\n\n$err"
        )
    }

  test("free vars are not duplicated") {
    forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      val frees = Type.freeTyVars(ts)
      assertEquals(frees.distinct, frees)
    }
  }

  private val genTau: Gen[Type] =
    NTypeGen.genDepth03.suchThat(Type.Tau.isTau)

  private val genExistsVars: Gen[NonEmptyList[(Type.Var.Bound, Kind)]] =
    for {
      n <- Gen.choose(1, 4)
      vars <- Gen.listOfN(n, Gen.zip(NTypeGen.genBound, NTypeGen.genKind))
    } yield NonEmptyList.fromListUnsafe(vars)

  test("Type.exists preserves Tau when input is Tau") {
    forAll(genExistsVars, genTau) { (vars, in) =>
      val ex = Type.exists(vars, in)
      assert(Type.Tau.isTau(ex), s"exists($vars, $in) == $ex is not Tau")
    }
  }

  test("Type.apply1 preserves Tau when both args are Tau") {
    forAll(genTau, genTau) { (fn, arg) =>
      val app = Type.apply1(fn, arg)
      assert(Type.Tau.isTau(app), s"apply1($fn, $arg) == $app is not Tau")
    }
  }

  test("Type.apply1Rho agrees with Type.apply1 on rho inputs") {
    val genRho =
      Gen.choose(0, 3).flatMap(d => NTypeGen.genTypeRho(d, Some(NTypeGen.genConst)))

    forAll(genRho, NTypeGen.genDepth03) { (fn, arg) =>
      assertEquals(Type.apply1Rho(fn, arg), Type.apply1(fn, arg))
    }
  }

  private val genTauLeafOrApply: Gen[Type] =
    genTau.suchThat {
      case _: (Type.Leaf | Type.TyApply) => true
      case _                             => false
    }

  test("Tau.unapply recognizes tau and rejects forall") {
    forAll(genTau) { t =>
      val m = Type.Tau.unapply(t)
      assert(!m.isEmpty, s"Tau.unapply should match: $t")
      val asTau = m.get
      assert(Type.Tau.isTau(asTau), s"Tau.unapply get not Tau: $asTau")
    }

    val fa = Type.forAll(
      NonEmptyList.one((Type.Var.Bound("a"), Kind.Type)),
      Type.TyVar(Type.Var.Bound("a"))
    )
    assert(Type.Tau.unapply(fa).isEmpty, "Tau.unapply should reject ForAll")
  }

  test("TauApply apply/unapply round trip") {
    forAll(genTauLeafOrApply, genTau) { (fn0, arg0) =>
      val fn = Type.Tau.unapply(fn0).get
      val arg = Type.Tau.unapply(arg0).get
      val applied = Type.Tau.TauApply(fn, arg)

      assert(Type.Tau.isTau(applied), s"TauApply result not Tau: $applied")

      val m = Type.Tau.TauApply.unapply(applied)
      assert(!m.isEmpty, s"TauApply.unapply should match: $applied")
      val ta = m.get
      assertEquals(ta.on, fn)
      assertEquals(ta.arg, arg)
      assertEquals(
        ta.toTyApply,
        Type.TyApply(fn.asInstanceOf[Type.Leaf | Type.TyApply], arg)
      )
    }
  }

  test("TauApply unapply rejects non-apply tau") {
    val leaf = Type.Tau.unapply(Type.TyVar(Type.Var.Bound("a"))).get
    val m = Type.Tau.TauApply.unapply(leaf)
    assert(m.isEmpty, s"TauApply.unapply should be empty for: $leaf")
  }

  test("TauExists apply/unapply round trip") {
    forAll(genExistsVars, genTau) { (vars, in0) =>
      val in = Type.Tau.unapply(in0).get
      val ex = Type.Tau.TauExists(vars, in)
      val expected = Type.existsRho(vars, in)

      assert(Type.Tau.isTau(ex), s"TauExists result not Tau: $ex")
      assertEquals(ex.toExists, expected)
      assertEquals(ex.vars, expected.vars)
      assertEquals(ex.in.asInstanceOf[Type], expected.in: Type)

      val m = Type.Tau.TauExists.unapply(ex)
      assert(!m.isEmpty, s"TauExists.unapply should match: $ex")
      val ex1 = m.get
      assertEquals(ex1.vars, expected.vars)
      assertEquals(ex1.in.asInstanceOf[Type], expected.in: Type)
    }
  }

  test("TauExists unapply rejects non-exists tau") {
    val leaf = Type.Tau.unapply(Type.TyVar(Type.Var.Bound("b"))).get
    val m = Type.Tau.TauExists.unapply(leaf)
    assert(m.isEmpty, s"TauExists.unapply should be empty for: $leaf")
  }

  test("normalize preserves free vars") {
    forAll(NTypeGen.genDepth03) { ts =>
      val frees = Type.freeTyVars(ts :: Nil)
      val norm = Type.normalize(ts)
      val freeNorm = Type.freeTyVars(norm :: Nil)
      assertEquals(
        frees,
        freeNorm,
        s"${Type.typeParser.render(ts)} => ${Type.typeParser.render(norm)}"
      )
    }
  }

  test("Tuple unapply is the inverse of apply") {
    val prop = forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      Type.Tuple(ts) match {
        case Type.Tuple(ts1) => assertEquals(ts1, ts)
        case notTup          => fail(notTup.toString)
      }
    }

    assertEquals(Type.Tuple.unapply(parse("()")), Some(Nil))
    assertEquals(
      Type.Tuple.unapply(parse("(a, b, c)")),
      Some(List("a", "b", "c").map(parse))
    )
    prop
  }

  test("unapplyAll is the inverse of applyAll") {
    val prop = forAll(NTypeGen.genDepth03) { ts =>
      val (left, args) = Type.unapplyAll(ts)
      assertEquals(Type.applyAll(left, args), ts)
    }

    assertEquals(
      Type.unapplyAll(parse("foo[bar]")),
      (parse("foo"), List(parse("bar")))
    )
    prop
  }

  test("freeBoundVar doesn't change by applyAll") {
    forAll(NTypeGen.genDepth03, Gen.listOf(NTypeGen.genDepth03)) { (ts, args) =>
      val applied = Type.applyAll(ts, args)
      val free0 = Type.freeBoundTyVars(ts :: args)
      val free1 = Type.freeBoundTyVars(applied :: Nil)
      assert(
        free1.toSet === free0.toSet,
        s"applied = ${Type.typeParser.render(applied)}, (${Type.typeParser
            .render(ts)})[${args.iterator.map(Type.typeParser.render(_)).mkString(", ")}]})"
      )
    }
  }

  test("substituteVar avoids capture") {
    import Type.Var.Bound
    val a = Bound("a")
    val b = Bound("b")
    val t =
      Type.forAll(
        NonEmptyList.of((a, Kind.Type)),
        Type.TyVar(b)
      )
    val sub = Type.substituteVar(t, Map[Type.Var, Type](b -> Type.TyVar(a)))

    assertEquals(Type.freeBoundTyVars(sub :: Nil), List(a))
  }

  test("types are well ordered") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03, NTypeGen.genDepth03) {
      dev.bosatsu.OrderingLaws.law(_, _, _)
    }
  }

  test("sameAs is transitive") {
    val g = NTypeGen.genDepth03
    forAll(g, g, g) { (a, b, c) =>
      if (a.sameAs(b)) {
        assertEquals(a.sameAs(c), b.sameAs(c))
      }
    }
  }

  test("a.sameAs(b) == (normalize(a) == normalize(b))") {
    val g = NTypeGen.genDepth03
    forAll(g, g) { (a, b) =>
      assertEquals(a.sameAs(b), (Type.normalize(a) == Type.normalize(b)))
    }
  }

  test("sameAs if and only if Blake3 hashes are equal") {
    val g = NTypeGen.genDepth03
    forAll(g, g) { (a, b) =>
      val ah = Hashable.hash(Algo.blake3Algo, a).hash
      val bh = Hashable.hash(Algo.blake3Algo, b).hash
      assertEquals(
        a.sameAs(b),
        ah.hex == bh.hex,
        s"sameAs/hash mismatch for ${Type.typeParser.render(a)} and ${Type.typeParser.render(b)} with hashes ${ah.hex} / ${bh.hex}"
      )
    }
  }

  test("Blake3 hash for exists type is stable (simple golden)") {
    val t = parse("exists a. a -> a")
    val tRenamed = parse("exists x. x -> x")
    val expected =
      "de88397010f29295c2c7a3adb9200b709a08aea28f853dab0359c6132925bec0"
    val hashT = Hashable.hash(Algo.blake3Algo, t).hash.hex
    val hashTRenamed = Hashable.hash(Algo.blake3Algo, tRenamed).hash.hex

    assertEquals(hashT, expected)
    assertEquals(hashTRenamed, expected)
  }

  test("Blake3 hash for exists type is stable (nested quantifier golden)") {
    val t = parse("forall z. exists a, b. (a -> z) -> (b -> z)")
    val tRenamed = parse("forall q. exists y, x. (y -> q) -> (x -> q)")
    val expected =
      "40c1750a4f97e5d050f510e7d6aef4a1187c43ae331c62fad9e938f831e52d22"
    val hashT = Hashable.hash(Algo.blake3Algo, t).hash.hex
    val hashTRenamed = Hashable.hash(Algo.blake3Algo, tRenamed).hash.hex

    assertEquals(hashT, expected)
    assertEquals(hashTRenamed, expected)
  }

  test("same as doesn't care about quant var order") {
    assert(parse("forall a, b. a -> b").sameAs(parse("forall b, a. a -> b")))
    assert(parse("exists a, b. a -> b").sameAs(parse("exists b, a. a -> b")))
  }

  test("normalization never throws") {
    val prop = forAll(NTypeGen.genDepth03) { t =>
      assert(t.sameAs(Type.normalize(t)))
    }

    {
      import Type._
      import Var.Bound
      import dev.bosatsu.Variance._
      import dev.bosatsu.Kind.{Arg, Cons, Type => KType}

      val qt1 = Type.forAll(
        NonEmptyList(
          (
            Bound("qsnMgkhqY"),
            Cons(
              Arg(Covariant, Cons(Arg(Covariant, KType), KType)),
              Cons(Arg(Phantom, KType), KType)
            )
          ),
          List(
            (
              Bound("u"),
              Cons(
                Arg(Contravariant, KType),
                Cons(Arg(Invariant, KType), KType)
              )
            )
          )
        ),
        Type.exists(
          NonEmptyList(
            (
              Bound("nack"),
              Cons(
                Arg(Invariant, Cons(Arg(Phantom, KType), KType)),
                Cons(Arg(Phantom, KType), KType)
              )
            ),
            List(
              (
                Bound("u"),
                Cons(
                  Arg(Contravariant, Cons(Arg(Contravariant, KType), KType)),
                  Cons(Arg(Contravariant, KType), KType)
                )
              ),
              (
                Bound("vHxbikOne"),
                Cons(
                  Arg(Invariant, Cons(Arg(Covariant, KType), KType)),
                  Cons(Arg(Contravariant, KType), KType)
                )
              ),
              (
                Bound("jofpdjgp"),
                Cons(Arg(Covariant, Cons(Arg(Phantom, KType), KType)), KType)
              ),
              (
                Bound("r"),
                Cons(
                  Arg(Invariant, Cons(Arg(Covariant, KType), KType)),
                  Cons(Arg(Invariant, KType), KType)
                )
              )
            )
          ),
          TyVar(Bound("u"))
        )
      )

      val qt2 = Type.exists(
        NonEmptyList(
          (
            Bound("chajb"),
            Cons(
              Arg(Contravariant, Cons(Arg(Covariant, KType), KType)),
              Cons(Arg(Contravariant, KType), KType)
            )
          ),
          List(
            (
              Bound("e"),
              Cons(
                Arg(Invariant, Cons(Arg(Phantom, KType), KType)),
                Cons(Arg(Phantom, KType), Cons(Arg(Phantom, KType), KType))
              )
            ),
            (
              Bound("vg"),
              Cons(
                Arg(Phantom, Cons(Arg(Phantom, KType), KType)),
                Cons(Arg(Phantom, KType), KType)
              )
            ),
            (
              Bound("vvki"),
              Cons(
                Arg(
                  Contravariant,
                  Cons(Arg(Phantom, KType), Cons(Arg(Phantom, KType), KType))
                ),
                KType
              )
            ),
            (
              Bound("e"),
              Cons(
                Arg(Invariant, Cons(Arg(Invariant, KType), KType)),
                Cons(Arg(Phantom, KType), KType)
              )
            )
          )
        ),
        TyVar(Bound("e"))
      )

      val regressions: List[Type] =
        qt1 ::
          qt2 ::
          Nil

      regressions.foreach { t =>
        val normt = Type.normalize(t)
        def show(t: Type): String =
          Type.typeParser.render(t)

        val normt2 = Type.normalize(normt)
        assertEquals(
          normt,
          normt2,
          s"${show(normt)} normalizes to ${show(normt2)}"
        )
        assert(t.sameAs(normt), s"${show(t)}.sameAs(${show(normt)}) == false")
      }

      val (_, _, qt1In) = Type.splitQuantifiers(qt1)
      assertEquals(Type.freeBoundTyVars(qt1In :: Nil), List(Bound("u")))
    }
    prop
  }

  test("normalization is idempotent") {
    forAll(NTypeGen.genDepth03) { t =>
      val n1 = Type.normalize(t)
      val n2 = Type.normalize(n1)
      assertEquals(n2, n1)
    }
  }

  test("we can parse types") {
    def law(t: Type) = {
      val str = Type.fullyResolvedDocument.document(t).render(80)
      assertEquals(
        Type.fullyResolvedParser.parseAll(str),
        Right(t),
        s"$str != $t"
      )
    }

    val propRoundTrip = forAll(NTypeGen.genDepth03)(law(_))

    val propSkolem =
      forAll(NTypeGen.lowerIdent, Gen.choose(Long.MinValue, Long.MaxValue)) {
        (b, id) =>
          val str = "$" + b + "$" + id.toString
          val tpe = parse(str)
          law(tpe)
          tpe match {
            case Type.TyVar(Type.Var.Skolem(b1, k1, _, i1)) =>
              assertEquals((b1, k1, i1), (b, Kind.Type, id))
            case other => fail(other.toString)
          }
      }

    val propMeta = forAll { (l: Long) =>
      assertEquals(
        parse("?" + l.toString).asInstanceOf[Type.TyMeta].toMeta.id,
        l
      )
    }
    org.scalacheck.Prop.all(propRoundTrip, propSkolem, propMeta)
  }

  test("test all binders") {
    assertEquals(
      Type.allBinders.filter(_.name.startsWith("a")).take(100).map(_.name),
      ("a" #:: LazyList.iterate(0)(_ + 1).map(i => s"a$i")).take(100)
    )
  }

  test("tyVarBinders is identity for Bound") {
    forAll(Gen.listOf(NTypeGen.lowerIdent), NTypeGen.genDepth03) { (vs, t) =>
      val vsD = vs.distinct
      val bs = vsD.map(Type.Var.Bound(_))
      val fa = Type.forAll(bs.map((_, Kind.Type)), t)
      val binders = Type.tyVarBinders(fa :: Nil)
      assert(bs.toSet.subsetOf(binders), s"bs = $bs, binders = $binders")
    }
  }

  test("if Type.hasNoVars then freeVars is empty") {
    forAll(NTypeGen.genDepth03) { t =>
      assertEquals(Type.hasNoVars(t), Type.freeTyVars(t :: Nil).isEmpty)
    }
  }

  test("if Type.hasNoUnboundVars then freeBoundTyVars is empty") {
    forAll(NTypeGen.genDepth03) { t =>
      assertEquals(
        Type.hasNoUnboundVars(t),
        Type.freeBoundTyVars(t :: Nil).isEmpty
      )
    }
  }

  test("hasNoVars fully recurses") {
    def allTypesIn(t: Type): List[Type] =
      t match {
        case f @ Type.ForAll(bounds, in) =>
          // filter bounds out, since they are shadowed
          val boundSet = bounds.toList.iterator.map(_._1).toSet[Type.Var]
          f :: (allTypesIn(in).filterNot { it =>
            val frees = Type.freeTyVars(it :: Nil).toSet
            // if we intersect, this is not a legit type to consider
            (boundSet & frees).nonEmpty
          })
        case t @ Type.TyApply(a, b) => t :: allTypesIn(a) ::: allTypesIn(b)
        case other                  => other :: Nil
      }

    def law(t: Type) = {
      val allT = allTypesIn(t)
      val hnv = Type.hasNoVars(t)

      if (hnv) assert(allT.forall(Type.hasNoVars), "hasNoVars == true")
      else assert(allT.exists(t => !Type.hasNoVars(t)), "hasNoVars == false")
    }

    val prop = forAll(NTypeGen.genDepth03)(law)

    val pastFails =
      List(
        Type.forAll(
          NonEmptyList.of(
            (Type.Var.Bound("x"), Kind.Type),
            (Type.Var.Bound("ogtumm"), Kind.Type),
            (Type.Var.Bound("t"), Kind.Type)
          ),
          Type.TyVar(Type.Var.Bound("x"))
        ),
        Type.forAll(
          NonEmptyList.of((Type.Var.Bound("a"), Kind.Type)),
          Type.TyVar(Type.Var.Bound("a"))
        )
      )

    pastFails.foreach(law)
    prop
  }

  test("Type.freeTyVars is empty for ForAll fully bound") {
    val fa = parse("forall a. a")
    assert(Type.freeTyVars(fa :: Nil).isEmpty)

    val fb = parse("forall a. a -> Bosatsu/Predef::Int")
    assert(Type.freeTyVars(fb :: Nil).isEmpty)
  }

  test("Type.freeTyVars is left to right for functions") {
    val ba = Type.Var.Bound("a")
    val bb = Type.Var.Bound("b")

    val ta = Type.TyVar(ba)
    val tb = Type.TyVar(bb)
    val fb = Type.Fun(NonEmptyList.one(ta), tb)
    assertEquals(Type.freeTyVars(fb :: ta :: Nil), List(ba, bb))
    assertEquals(Type.freeTyVars(fb :: tb :: Nil), List(ba, bb))
  }

  test("types are in order in freeTyVars") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03) { (t1, t2) =>
      val left = Type.freeTyVars(t1 :: Nil)
      val right = Type.freeTyVars(t2 :: Nil)
      val both = Type.freeTyVars(t1 :: t2 :: Nil)

      assertEquals((left.toSet | right.toSet), both.toSet)
      assertEquals(left ::: (right.filterNot(left.toSet)), both)
    }
  }

  def genSubs(depth: Int): Gen[Map[Type.Var, Type]] = {
    val pair = Gen.zip(
      NTypeGen.genBound,
      NTypeGen.genDepth(depth, Some(NTypeGen.genConst))
    )
    Gen.mapOf(pair)
  }

  test("substitute is a no-op if none of the substitutions are free") {
    forAll(NTypeGen.genDepth03, genSubs(3)) { (t, subs) =>
      val subs1 = subs -- Type.freeBoundTyVars(t :: Nil)
      assertEquals(Type.substituteVar(t, subs1), t)
    }
  }

  test("substitute is idempotent") {
    def law(t: Type, subs0: Map[Type.Var, Type]) = {
      /*
       * subs is only idempotent if none of
       * the free variables are also keys to the substitution
       */
      val allFrees = Type.freeTyVars(subs0.values.toList).toSet
      val subs = subs0.filterNot { case (k, _) => allFrees(k) }
      val t1 = Type.substituteVar(t, subs)
      val t2 = Type.substituteVar(t1, subs)
      assertEquals(t2, t1)
    }

    val prop = forAll(NTypeGen.genDepth03, genSubs(3))(law)

    val ba: Type.Var = Type.Var.Bound("a")

    val pastFails = List(
      // this is an illegitmate substitution, but the law needs to be robust to it
      (Type.TyVar(ba), Map(ba -> Type.TyApply(Type.TyVar(ba), Type.TyVar(ba))))
    )

    pastFails.foreach { case (t, s) => law(t, s) }
    prop
  }

  test("after substitution, none of the keys are free") {
    def law(t: Type, subs: Map[Type.Var, Type]) = {
      // don't substitute back onto the keys
      val subs1 = subs.filter { case (_, v) =>
        (Type.freeBoundTyVars(v :: Nil).toSet & subs.keySet).isEmpty
      }
      // now subs1 has keys that can be completely removed, so
      // after substitution, those keys should be gone
      val t1 = Type.substituteVar(t, subs1)
      assertEquals(
        (Type.freeBoundTyVars(t1 :: Nil).toSet & subs1.keySet),
        Set.empty
      )
    }

    forAll(NTypeGen.genDepth03, genSubs(3))(law)
  }

  test("we can substitute to get an instantiation") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03) { (t1, t2) =>
      t1 match {
        case Type.ForAll(fas, t) =>
          Type.instantiate(fas.iterator.toMap, t, t2, Map.empty) match {
            case Some((frees, subs)) =>
              val t3 = Type.substituteVar(
                t,
                subs.iterator.map { case (k, (_, v)) => (k, v) }.toMap
              )

              val t4 = Type.substituteVar(
                t3,
                frees.iterator.map { case (v1, (_, v2)) =>
                  (v1, Type.TyVar(v2))
                }.toMap
              )

              val t5 = Type.quantify(
                forallList = frees.iterator.map { case (_, tup) =>
                  tup.swap
                }.toList,
                existList = Nil,
                t4
              )

              assert(t5.sameAs(t2))
            case None =>
              ()
          }
        case _ => ()
      }
    }
  }

  test("some example instantiations") {
    def check(forall: String, matches: String, subs: List[(String, String)]) = {
      val Type.ForAll(fas, t) = parse(forall).runtimeChecked
      val targ = parse(matches)
      Type.instantiate(fas.iterator.toMap, t, targ, Map.empty) match {
        case Some((_, subMap)) =>
          assertEquals(subMap.size, subs.size)
          subs.foreach { case (k, v) =>
            val Type.TyVar(b: Type.Var.Bound) = parse(k).runtimeChecked
            assertEquals(subMap(b)._2, parse(v))
          }
        case None =>
          fail(s"could not instantitate: $forall to $matches")
      }
    }

    def noSub(forall: String, matches: String) = {
      val Type.ForAll(fas, t) = parse(forall).runtimeChecked
      val targ = parse(matches)
      val res = Type.instantiate(fas.iterator.toMap, t, targ, Map.empty)
      assertEquals(res, None)
    }

    check(
      "forall a. a",
      "Bosatsu/Predef::Int",
      List("a" -> "Bosatsu/Predef::Int")
    )
    check(
      "forall a. a -> a",
      "Bosatsu/Predef::Int -> Bosatsu/Predef::Int",
      List("a" -> "Bosatsu/Predef::Int")
    )
    check(
      "forall a. a -> Bosatsu/Predef::Foo[a]",
      "Bosatsu/Predef::Int -> Bosatsu/Predef::Foo[Bosatsu/Predef::Int]",
      List("a" -> "Bosatsu/Predef::Int")
    )
    check(
      "forall a. Bosatsu/Predef::Option[a]",
      "Bosatsu/Predef::Option[Bosatsu/Predef::Int]",
      List("a" -> "Bosatsu/Predef::Int")
    )

    check("forall a. a", "forall a. a", List("a" -> "forall a. a"))

    check(
      "forall a, b. a -> b",
      "forall c. c -> Bosatsu/Predef::Int",
      List("b" -> "Bosatsu/Predef::Int")
    )

    check(
      "forall a, b. T::Cont[a, b]",
      "forall a. T::Cont[a, T::Foo]",
      List("b" -> "T::Foo")
    )

    noSub("forall a, b. T::Cont[a, b]", "forall a: * -> *. T::Cont[a, T::Foo]")
    noSub("forall a. T::Box[a]", "forall a. T::Box[T::Opt[a]]")

    check("forall a. T::Foo[a, a]", "forall b. T::Foo[b, b]", Nil)
  }

  test("instantiate handles rhs forall shadowing") {
    // Regression guard: instantiate should alpha-rename RHS forall binders
    // that collide with already-in-scope binders instead of failing.
    def ok(from: String, matches: String) = {
      val Type.ForAll(fas, t) = parse(from).runtimeChecked
      val targ = parse(matches)
      val res = Type.instantiate(fas.iterator.toMap, t, targ, Map.empty)
      assert(res.nonEmpty, s"could not instantiate: $from to $matches")
    }

    ok(
      "forall b. Bosatsu/Predef::Option[b]",
      "forall a. forall a. Bosatsu/Predef::Option[a]"
    )

    ok(
      "forall b, c. (Bosatsu/Predef::Option[b], Bosatsu/Predef::Option[c])",
      "forall a. (Bosatsu/Predef::Option[a], forall a. Bosatsu/Predef::Option[a])"
    )
  }

  test("instantiate handles rhs forall shadowing inside tuple") {
    // Regression guard: shadowing inside nested forall in tuple position.
    val Type.ForAll(fas, t) = parse("forall b, c. (b, c)").runtimeChecked
    val targ = parse("forall a. (a, forall a. a)")
    val res = Type.instantiate(fas.iterator.toMap, t, targ, Map.empty)
    assert(res.nonEmpty, s"could not instantiate: $t to $targ")
  }

  test("Fun(ts, r) and Fun.unapply are inverses") {
    val genArgs = for {
      cnt <- Gen.choose(0, Type.FnType.MaxSize - 1)
      head <- NTypeGen.genDepth03
      tail <- Gen.listOfN(cnt, NTypeGen.genDepth03)
    } yield NonEmptyList(head, tail)

    forAll(genArgs, NTypeGen.genDepth03) { (args, res) =>
      val fnType = Type.Fun(args, res)
      fnType match {
        case Type.Fun(args1, res1) =>
          assertEquals(args1, args)
          assertEquals(res1, res)
        case _ =>
          fail(s"fnType didn't match Fun")
      }
    }
  }

  test("Quantification.concat is associative") {
    forAll(NTypeGen.genQuant, NTypeGen.genQuant, NTypeGen.genQuant) {
      (a, b, c) =>
        assertEquals(a.concat(b).concat(c), a.concat(b.concat(c)))
    }
  }

  test("Quantification.toLists/fromList identity") {
    forAll(NTypeGen.genQuant) { q =>
      assertEquals(
        TypedExpr.Quantification.fromLists(q.forallList, q.existList),
        Some(q)
      )
    }
  }

  test("genQuantifiers is non-empty and distinct") {
    forAll(NTypeGen.genQuantifiers(NTypeGen.genBound)) { qs =>
      val vars = qs.toList.map(_._1)
      assert(vars.nonEmpty)
      assertEquals(vars.distinct, vars)
    }
  }

  test("genForAll and genExists always use generated binders") {
    val genForAll =
      Gen.choose(1, 3).flatMap(d => NTypeGen.genForAll(d, Some(NTypeGen.genConst)))
    val genExists =
      Gen.choose(1, 3).flatMap(d => NTypeGen.genExists(d, Some(NTypeGen.genConst)))

    forAll(genForAll) { fa =>
      val free = Type.freeBoundTyVars(fa.in :: Nil).toSet
      assert(
        fa.vars.toList.forall { case (b, _) => free(b) },
        s"forall binders should all be free in body: $fa"
      )
    }

    forAll(genExists) { ex =>
      val free = Type.freeBoundTyVars(ex.in :: Nil).toSet
      assert(
        ex.vars.toList.forall { case (b, _) => free(b) },
        s"exists binders should all be free in body: $ex"
      )
    }
  }

  test("Type.forAll and Type.exists bind vars selected from free vars") {
    val genTypeWithFreeVars = NTypeGen.genDepth03.map { t =>
      NonEmptyList.fromList(Type.freeBoundTyVars(t :: Nil)) match {
        case Some(free) => (t, free)
        case None       =>
          val b = Type.Var.Bound("_forced_free")
          (Type.apply1(t, Type.TyVar(b)), NonEmptyList.one(b))
      }
    }

    val gen = for {
      (t, free) <- genTypeWithFreeVars
      qs <- NTypeGen.genQuantifiers(Gen.oneOf(free.toList))
    } yield (t, qs)

    forAll(gen) { case (t, qs) =>
      val qset = qs.toList.iterator.map(_._1).toSet
      val free0 = Type.freeBoundTyVars(t :: Nil).toSet
      val expected = free0 -- qset

      val f1 = Type.freeBoundTyVars(Type.forAll(qs.toList, t) :: Nil).toSet
      val e1 = Type.freeBoundTyVars(Type.exists(qs.toList, t) :: Nil).toSet

      assertEquals(f1, expected)
      assertEquals(e1, expected)
    }
  }

  test("Type.forAll and Type.exists can be no-ops with independent quantifiers") {
    val unused = Type.Var.Bound("_unused_quantifier")
    val genUnused = NTypeGen.genQuantifiers(Gen.const(unused))

    forAll(NTypeGen.genDepth03, genUnused) { (t, qs) =>
      val free0 = Type.freeBoundTyVars(t :: Nil).toSet

      val f1 = Type.freeBoundTyVars(Type.forAll(qs.toList, t) :: Nil).toSet
      val e1 = Type.freeBoundTyVars(Type.exists(qs.toList, t) :: Nil).toSet

      assertEquals(f1, free0)
      assertEquals(e1, free0)
    }
  }

  test("unexists/exists | unforall/forall iso") {
    forAll(NTypeGen.genDepth03) {
      case t @ Type.Exists(ps, in) =>
        assertEquals(Type.exists(ps, in), t)
      case t @ Type.ForAll(ps, in) =>
        assertEquals(Type.forAll(ps, in), t)
      case _ => ()
    }
  }
  test("exists -> unexists") {
    forAll(NTypeGen.genQuantArgs, NTypeGen.genRootType(None)) { (args, t) =>
      Type.exists(args, t) match {
        case Type.Exists(ps, in) =>
          assertEquals(ps.toList, args)
          assertEquals(in, t)
        case notExists =>
          assert(args.isEmpty)
          assertEquals(notExists, t)
      }
    }
  }
  test("forall -> unforall") {
    forAll(NTypeGen.genQuantArgs, NTypeGen.genRootType(None)) { (args, t) =>
      Type.forAll(args, t) match {
        case Type.ForAll(ps, in) =>
          assertEquals(ps.toList, args)
          assertEquals(in, t)
        case notExists =>
          assert(args.isEmpty)
          assertEquals(notExists, t)
      }
    }
  }

  test("tuple syntax looks right") {
    val code = Type.allTupleCode
    (1 to 32).foreach { i =>
      assert(code.contains(s"struct Tuple$i["))
      assert(code.contains(s"Tuple$i(),"))
    }
    List(0, 33).foreach { i =>
      assert(!code.contains(s"struct Tuple$i["))
      assert(!code.contains(s"Tuple$i(),"))
    }
  }

  test("allConsts never throws") {
    forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      assert(Type.allConsts(ts) ne null)
    }
  }

  test("allConsts laws") {
    forAll(NTypeGen.genDepth03) { t =>
      import Type._

      val consts = allConsts(t :: Nil)
      t match {
        case tyc @ TyConst(_) =>
          assertEquals(consts, (tyc :: Nil))
        case (TyVar(_) | TyMeta(_)) =>
          assertEquals(consts, Nil)
        case TyApply(left, right) =>
          assertEquals(consts, Type.allConsts(left :: right :: Nil))
        case Type.ForAll(_, in) =>
          assertEquals(consts, allConsts(in :: Nil))
        case Type.Exists(_, in) =>
          assertEquals(consts, allConsts(in :: Nil))
      }
    }
  }
  test("some example Fun.SimpleUniversal") {
    def check(fn: String, expect: Option[String]) =
      parse(fn) match {
        case Type.Fun.SimpleUniversal((u, args, res)) =>
          val resTpe = Type.forAll(u, Type.Fun(args, res))

          expect match {
            case None =>
              fail(s"$fn resulted in ${Type.typeParser.render(resTpe)}")
            case Some(exTpe) =>
              val exT = parse(exTpe)
              assert(resTpe.sameAs(exT), s"${resTpe}.sameAs($exT) == false")
          }
        case _ =>
          expect match {
            case None        => ()
            case Some(exTpe) =>
              fail(s"$fn is not SimpleUniversal but expected: $exTpe")
          }
      }

    check("forall a. a -> a", Some("forall a. a -> a"))
    check(
      "forall a. a -> Foo::Option[a]",
      Some("forall a. a -> Foo::Option[a]")
    )
    check("forall a. a -> (forall b. b)", Some("forall a, b. a -> b"))
    check(
      "forall a. a -> (forall b. Foo::Option[b])",
      Some("forall a, b. a -> Foo::Option[b]")
    )
    check("forall a. a -> (forall a. a)", Some("forall a, b. a -> b"))
    check(
      "forall a. a -> (forall a, c. a -> c)",
      Some("forall a, b, c. a -> (b -> c)")
    )
  }
}
