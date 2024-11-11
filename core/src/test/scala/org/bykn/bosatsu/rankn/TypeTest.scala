package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.bykn.bosatsu.Kind
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite

class TypeTest extends AnyFunSuite {
  import NTypeGen.shrinkType

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 1000)
  // PropertyCheckConfiguration(minSuccessful = 5)

  def parse(s: String): Type =
    Type.fullyResolvedParser.parseAll(s) match {
      case Right(t) => t
      case Left(err) =>
        sys.error(
          s"failed to parse: <$s> at ${s.drop(err.failedAtOffset)}\n\n$err"
        )
    }

  test("free vars are not duplicated") {
    forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      val frees = Type.freeTyVars(ts)
      assert(frees.distinct == frees)
    }
  }

  test("normalize preserves free vars") {
    forAll(NTypeGen.genDepth03) { ts =>
      val frees = Type.freeTyVars(ts :: Nil)
      val norm = Type.normalize(ts)
      val freeNorm = Type.freeTyVars(norm :: Nil)
      assert(
        frees == freeNorm,
        s"${Type.typeParser.render(ts)} => ${Type.typeParser.render(norm)}"
      )
    }
  }

  test("Tuple unapply is the inverse of apply") {
    forAll(Gen.listOf(NTypeGen.genDepth03)) { ts =>
      Type.Tuple(ts) match {
        case Type.Tuple(ts1) => assert(ts1 == ts)
        case notTup          => fail(notTup.toString)
      }
    }

    assert(Type.Tuple.unapply(parse("()")) == Some(Nil))
    assert(
      Type.Tuple.unapply(parse("(a, b, c)")) ==
        Some(List("a", "b", "c").map(parse))
    )
  }

  test("unapplyAll is the inverse of applyAll") {
    forAll(NTypeGen.genDepth03) { ts =>
      val (left, args) = Type.unapplyAll(ts)
      assert(Type.applyAll(left, args) == ts)
    }

    assert(
      Type.unapplyAll(parse("foo[bar]")) ==
        (parse("foo"), List(parse("bar")))
    )
  }

  test("freeBoundVar doesn't change by applyAll") {
    forAll(NTypeGen.genDepth03, Gen.listOf(NTypeGen.genDepth03)) { (ts, args) =>
      val applied = Type.applyAll(ts, args)
      val free0 = Type.freeBoundTyVars(ts :: args)
      val free1 = Type.freeBoundTyVars(applied :: Nil)
      assert(
        free1.toSet == free0.toSet,
        s"applied = ${Type.typeParser.render(applied)}, (${Type.typeParser
            .render(ts)})[${args.iterator.map(Type.typeParser.render(_)).mkString(", ")}]})"
      )
    }
  }

  test("types are well ordered") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03, NTypeGen.genDepth03) {
      org.bykn.bosatsu.OrderingLaws.law(_, _, _)
    }
  }

  test("sameAs is transitive") {
    val g = NTypeGen.genDepth03
    forAll(g, g, g) { (a, b, c) =>
      if (a.sameAs(b)) {
        assert(a.sameAs(c) == b.sameAs(c))
      }
    }
  }

  test("a.sameAs(b) == (normalize(a) == normalize(b))") {
    val g = NTypeGen.genDepth03
    forAll(g, g) { (a, b) =>
      assert(a.sameAs(b) == (Type.normalize(a) == Type.normalize(b)))
    }
  }

  test("normalization never throws") {
    forAll(NTypeGen.genDepth03) { t =>
      assert(t.sameAs(Type.normalize(t)))
    }

    {
      import Type._
      import Var.Bound
      import org.bykn.bosatsu.Variance._
      import org.bykn.bosatsu.Kind.{Arg, Cons, Type => KType}

      val qt1 = Quantified(
        Quantification.Dual(
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
          )
        ),
        TyVar(Bound("u"))
      )

      val qt2 = Quantified(
        Quantification.Exists(
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
        assert(normt == normt2, s"${show(normt)} normalizes to ${show(normt2)}")
        assert(t.sameAs(normt), s"${show(t)}.sameAs(${show(normt)}) == false")
      }

      assert(Type.freeBoundTyVars(qt1.in :: Nil) == List(Bound("u")))
    }
  }

  test("normalization is idempotent") {
    forAll(NTypeGen.genDepth03) { t =>
      val n1 = Type.normalize(t)
      val n2 = Type.normalize(n1)
      assert(n2 == n1)
    }
  }

  test("we can parse types") {
    def law(t: Type) = {
      val str = Type.fullyResolvedDocument.document(t).render(80)
      assert(Type.fullyResolvedParser.parseAll(str) == Right(t), s"$str != $t")
    }

    forAll(NTypeGen.genDepth03)(law(_))

    forAll(NTypeGen.lowerIdent, Gen.choose(Long.MinValue, Long.MaxValue)) {
      (b, id) =>
        val str = "$" + b + "$" + id.toString
        val tpe = parse(str)
        law(tpe)
        tpe match {
          case Type.TyVar(Type.Var.Skolem(b1, k1, _, i1)) =>
            assert((b1, k1, i1) === (b, Kind.Type, id))
          case other => fail(other.toString)
        }
    }

    forAll { (l: Long) =>
      assert(parse("?" + l.toString).asInstanceOf[Type.TyMeta].toMeta.id == l)
    }
  }

  test("test all binders") {
    assert(
      Type.allBinders.filter(_.name.startsWith("a")).take(100).map(_.name) ==
        ("a" #:: Stream.iterate(0)(_ + 1).map(i => s"a$i")).take(100)
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
      assert(Type.hasNoVars(t) == Type.freeTyVars(t :: Nil).isEmpty)
    }
  }

  test("if Type.hasNoUnboundVars then freeBoundTyVars is empty") {
    forAll(NTypeGen.genDepth03) { t =>
      assert(Type.hasNoUnboundVars(t) == Type.freeBoundTyVars(t :: Nil).isEmpty)
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

    forAll(NTypeGen.genDepth03)(law)

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
    assert(Type.freeTyVars(fb :: ta :: Nil) == List(ba, bb))
    assert(Type.freeTyVars(fb :: tb :: Nil) == List(ba, bb))
  }

  test("types are in order in freeTyVars") {
    forAll(NTypeGen.genDepth03, NTypeGen.genDepth03) { (t1, t2) =>
      val left = Type.freeTyVars(t1 :: Nil)
      val right = Type.freeTyVars(t2 :: Nil)
      val both = Type.freeTyVars(t1 :: t2 :: Nil)

      assert((left.toSet | right.toSet) == both.toSet)
      assert(left ::: (right.filterNot(left.toSet)) == both)
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
      assert(Type.substituteVar(t, subs1) == t)
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
      assert(t2 == t1)
    }

    forAll(NTypeGen.genDepth03, genSubs(3))(law)

    val ba: Type.Var = Type.Var.Bound("a")

    val pastFails = List(
      // this is an illegitmate substitution, but the law needs to be robust to it
      (Type.TyVar(ba), Map(ba -> Type.TyApply(Type.TyVar(ba), Type.TyVar(ba))))
    )

    pastFails.foreach { case (t, s) => law(t, s) }
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
      assert(
        (Type.freeBoundTyVars(t1 :: Nil).toSet & subs1.keySet) == Set.empty
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
      val Type.ForAll(fas, t) = parse(forall): @unchecked
      val targ = parse(matches)
      Type.instantiate(fas.iterator.toMap, t, targ, Map.empty) match {
        case Some((_, subMap)) =>
          assert(subMap.size == subs.size)
          subs.foreach { case (k, v) =>
            val Type.TyVar(b: Type.Var.Bound) = parse(k): @unchecked
            assert(subMap(b)._2 == parse(v))
          }
        case None =>
          fail(s"could not instantitate: $forall to $matches")
      }
    }

    def noSub(forall: String, matches: String) = {
      val Type.ForAll(fas, t) = parse(forall): @unchecked
      val targ = parse(matches)
      val res = Type.instantiate(fas.iterator.toMap, t, targ, Map.empty)
      assert(res == None)
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
          assert(args1 == args)
          assert(res1 == res)
        case _ =>
          fail(s"fnType didn't match Fun")
      }
    }
  }

  test("Quantification.concat is associative") {
    forAll(NTypeGen.genQuant, NTypeGen.genQuant, NTypeGen.genQuant) {
      (a, b, c) =>
        assert(a.concat(b).concat(c) == a.concat(b.concat(c)))
    }
  }

  test("Quantification.toLists/fromList identity") {
    forAll(NTypeGen.genQuant) { q =>
      assert(
        Type.Quantification.fromLists(q.forallList, q.existList) == Some(q)
      )
    }
  }

  test("unexists/exists | unforall/forall iso") {
    forAll(NTypeGen.genDepth03) {
      case t @ Type.Exists(ps, in) =>
        assert(Type.exists(ps, in) == t)
      case t @ Type.ForAll(ps, in) =>
        assert(Type.forAll(ps, in) == t)
      case _ => ()
    }
  }
  test("exists -> unexists") {
    forAll(NTypeGen.genQuantArgs, NTypeGen.genRootType(None)) { (args, t) =>
      Type.exists(args, t) match {
        case Type.Exists(ps, in) =>
          assert(ps.toList == args)
          assert(in == t)
        case notExists =>
          assert(args.isEmpty)
          assert(notExists == t)
      }
    }
  }
  test("forall -> unforall") {
    forAll(NTypeGen.genQuantArgs, NTypeGen.genRootType(None)) { (args, t) =>
      Type.forAll(args, t) match {
        case Type.ForAll(ps, in) =>
          assert(ps.toList == args)
          assert(in == t)
        case notExists =>
          assert(args.isEmpty)
          assert(notExists == t)
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
          assert(consts == (tyc :: Nil))
        case (TyVar(_) | TyMeta(_)) =>
          assert(consts == Nil)
        case TyApply(left, right) =>
          assert(consts == Type.allConsts(left :: right :: Nil))
        case Quantified(_, in) =>
          assert(consts == allConsts(in :: Nil))
      }
    }
  }
  test("some example Fun.SimpleUniversal") {
    def check(fn: String, expect: Option[String]) =
      parse(fn) match {
        case Type.Fun.SimpleUniversal((u, args, res)) =>
          val resTpe = Type.Quantified(
            Type.Quantification.ForAll(u),
            Type.Fun(args, res)
          )

          expect match {
            case None =>
              fail(s"$fn resulted in ${Type.typeParser.render(resTpe)}")
            case Some(exTpe) =>
              val exT = parse(exTpe)
              assert(resTpe.sameAs(exT), s"${resTpe}.sameAs($exT) == false")
          }
        case _ =>
          expect match {
            case None => succeed
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
