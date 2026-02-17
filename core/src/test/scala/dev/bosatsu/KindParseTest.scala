package dev.bosatsu

import cats.kernel.Eq
import cats.syntax.eq._
import dev.bosatsu.hashing.{Algo, Hashable}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import rankn.NTypeGen.{genKind, genKindArg => genArg, shrinkKind}

class KindParseTest extends ParserTestBase {
  given Eq[Kind] = Eq.fromUniversalEquals

  override protected def minSuccessfulTests: Int =
    if (Platform.isScalaJvm) 500 else 10

  def show(k: Kind): String = Kind.toDoc(k).render(80)

  test("we can parse everything we generate") {
    forAll(genKind)(law(Kind.parser))
  }

  test("equal Blake3 hashes imply kind equality") {
    forAll(genKind, genKind) { (a, b) =>
      val ah = Hashable.hash(Algo.blake3Algo, a).hash
      val bh = Hashable.hash(Algo.blake3Algo, b).hash
      if (ah.hex == bh.hex) {
        assert(
          a === b,
          s"expected equal kinds for equal hashes: ${show(a)} and ${show(b)}"
        )
      }
    }
  }

  test("test some examples") {
    import Kind.Type

    parseTestAll(Kind.parser, "*", Type)

    val kind1 = Kind(Type.in)
    parseTestAll(Kind.parser, "(*) -> *", kind1)
    parseTestAll(Kind.parser, "(* -> *)", kind1)
    parseTestAll(Kind.parser, "* -> (*)", kind1)

    parseTestAll(Kind.parser, "* -> *", kind1)
    parseTestAll(Kind.parser, "*->*", kind1)
    parseTestAll(Kind.parser, "+*->*", Kind(Type.co))
    parseTestAll(Kind.parser, "-* -> *", Kind(Type.contra))
    parseTestAll(Kind.parser, "👻* -> *", Kind(Type.phantom))

    parseTestAll(Kind.parser, "(* -> *) -> *", Kind(Kind(Type.in).in))
    parseTestAll(Kind.parser, "+(* -> *) -> *", Kind(Kind(Type.in).co))
    parseTestAll(Kind.parser, "* -> * -> *", Kind(Type.in, Type.in))
    parseTestAll(Kind.parser, "-* -> +* -> *", Kind(Type.contra, Type.co))
    parseTestAll(Kind.parser, "* -> (* -> *)", Kind(Type.in, Type.in))
  }

  test("order is consistent with isType") {
    forAll(genKind) { k =>
      assertEquals((k.order == 0), k.isType)
    }
  }

  test("order is consistent with isOrder1") {
    forAll(genKind) { k =>
      assertEquals((k.order == 1), k.isOrder1)
    }
  }

  test("order is consistent with isHigherKind") {
    forAll(genKind) { k =>
      assertEquals((k.order > 1), k.isHigherOrder)
    }
  }

  test("order is non-negative") {
    forAll(genKind) { k =>
      assert(k.order >= 0)
      assert(k.isType || k.isOrder1 || k.isHigherOrder)
    }
  }

  test("order composes correctly") {
    forAll(genKind, genKind) { (k0, k1) =>
      val ord0 = k0.order
      val ord1 = k1.order
      val ord2 = Kind.Cons(k0.in, k1).order
      assertEquals(ord2, scala.math.max(ord0 + 1, ord1))
    }
  }

  test("toArgs and Kind.apply are inverses") {
    val propKind = forAll(genKind) { k =>
      assertEquals(Kind(k.toArgs*), k)
    }

    val propArgs = forAll(Gen.listOf(genArg)) { args =>
      assertEquals(Kind(args*).toArgs, args)
    }
    org.scalacheck.Prop.all(propKind, propArgs)
  }

  test("example orders") {
    def check(str: String, ord: Int) =
      Kind.parser.parseAll(str) match {
        case Right(k)  => assertEquals(k.order, ord)
        case Left(err) => fail(err.toString)
      }

    check("*", 0)

    check("* -> *", 1)
    check("* -> * -> *", 1)
    check("* -> * -> * -> *", 1)

    check("(* -> *) -> *", 2)
    check("(* -> *) -> (* -> *) -> *", 2)
  }

  test("Kind.shapeMatch(a, b) == Kind.shapeMatch(b, a)") {
    forAll(genKind, genKind) { (a, b) =>
      val m = Kind.shapeMatch(a, b)
      assertEquals(m, Kind.shapeMatch(b, a))
      if (m) {
        assertEquals(a.order, b.order)
      }
      assert(Kind.shapeMatch(a, a))
    }
  }
  test("if Kind.leftSubsumesRight(a, b) then shapeMatch(a, b)") {
    forAll(genKind, genKind) { (a, b) =>
      if (Kind.leftSubsumesRight(a, b)) {
        assert(Kind.shapeMatch(a, b))
      }

      assert(Kind.leftSubsumesRight(a, a))
    }
  }

  test("Kind.allSubkinds(k).forall(Kind.leftSubsumesRight(k, _))") {
    def law(k1: Kind, k2: Kind) =
      if (Kind.allSubKindsSize(k1) < (1 << 12)) {
        // allSubKinds is not stack safe currently
        val subs = Kind.allSubKinds(k1)
        if (Kind.leftSubsumesRight(k1, k2)) {
          assert(subs.indexOf(k2) >= 0)
        }
        assert(subs.indexOf(k1) >= 0)
        subs.foreach { k =>
          assert(Kind.leftSubsumesRight(k1, k))
          assert(Kind.shapeMatch(k, k1))
          assert(Kind.shapeMatch(k1, k))
          assertEquals(k.order, k1.order)
        }
      }

    forAll(genKind, genKind)((k1, k2) => law(k1, k2))
  }

  test("Kind.allSuperkinds(k).forall(Kind.leftSubsumesRight(_, k))") {
    forAll(genKind, genKind) { (k1, k2) =>
      if (Kind.allSuperKindsSize(k1) < (1 << 14)) {
        // allSuperKinds is not stack safe currently
        val sups = Kind.allSuperKinds(k1)
        if (Kind.leftSubsumesRight(k2, k1)) {
          assert(sups.indexOf(k2) >= 0)
        }
        assert(sups.indexOf(k1) >= 0)
        sups.foreach { k =>
          assert(Kind.leftSubsumesRight(k, k1))
          assert(Kind.shapeMatch(k, k1))
          assert(Kind.shapeMatch(k1, k))
          assertEquals(k.order, k1.order)
        }
      }
    }
  }
  test("Kind.allSubKinds(k).size == allSubKindsSize(k)") {
    forAll(genKind) { k =>
      val size = Kind.allSubKindsSize(k)
      if (size < (1 << 14))
        assertEquals(size, Kind.allSubKinds(k).size.toLong)
    }
  }

  test("Kind.allSuperKinds(k).size == allSuperKindsSize(k)") {
    forAll(genKind) { k =>
      val size = Kind.allSuperKindsSize(k)
      if (size < (1 << 14))
        assertEquals(size, Kind.allSuperKinds(k).size.toLong)
    }
  }

  test("we enumerate from most to superkinds") {
    def law(k: Kind) = {
      val ks = Kind.allSuperKinds(k).take(20).toList
      ks.sliding(2).foreach {
        case Seq(k1, k2) =>
          assert(
            Kind.allSuperKindsSize(k1) >= Kind.allSuperKindsSize(k2),
            s"\n\nk = ${show(k)}\nk1 = ${show(k1)}\nk2 = ${show(k2)}\nsups = ${ks
                .map(show)}\nidx = ${ks.indexOf(k1)}"
          )
        case _ => ()
      }
    }

    val regressions = {
      import Variance._
      import Kind._

      // -* -> (?* -> *)
      Cons(Arg(Contravariant, Type), Cons(Arg(Phantom, Type), Type)) ::
        Nil
    }

    regressions.foreach(law(_))
    forAll(genKind)(law(_))
  }

  test("we enumerate from most to subkinds") {
    def law(k: Kind) = {
      val ks = Kind.allSubKinds(k).take(20).toList
      ks.sliding(2).foreach {
        case Seq(k1, k2) =>
          assert(
            Kind.allSubKindsSize(k1) >= Kind.allSubKindsSize(k2),
            s"\n\nk = ${show(k)}\nk1 = ${show(k1)}\nk2 = ${show(k2)}\nsubs = ${ks
                .map(show)}\nidx = ${ks.indexOf(k1)}"
          )
        case _ => ()
      }
    }

    val regressions =
      Nil

    regressions.foreach(law(_))
    forAll(genKind)(law(_))
  }

  test("test diagonal utility function") {
    forAll { (lst: List[Int]) =>
      val diags = Kind.diagonal(lst.to(LazyList)).toList
      val lenLst = lst.length
      val expectSize = lenLst * (lenLst + 1) / 2
      assertEquals(diags.length, expectSize)
      // expensive version
      val lstIdx = lst.zipWithIndex
      val prod = for {
        (a, aidx) <- lstIdx
        (b, bidx) <- lstIdx
        if aidx + bidx < lenLst
      } yield (a, b)
      assertEquals(diags.sorted, prod.sorted)
    }
  }

  test("test sortmerge") {
    forAll { (l1: List[Int], l2: List[Int]) =>
      val ll1 = l1.sorted.to(LazyList)
      val ll2 = l2.sorted.to(LazyList)
      assertEquals(Kind.sortMerge(ll1, ll2).toList, (l1 ::: l2).sorted)
    }
  }

  test("sortMerge is lazy and doesn't blow the stack") {
    val pairs = Kind.sortMerge(LazyList.from(0), LazyList.from(0))
    val idx = 1000000
    assertEquals(pairs.drop(idx).head, (idx / 2))
  }

  test("product sort test") {
    def prodSort(l1: LazyList[Short], l2: LazyList[Short]): LazyList[Long] =
      // we know that a1, a2, ...
      // and b1, b2 ...
      // with a2 <= a1 and b2 <= b1
      // we know a1 * b1 <= a2 * b2 but we don't know if a1 * b2 <= a2 * b1 or not
      (l1, l2) match {
        case (a1 #:: as, b1 #:: bs) =>
          val aL = a1.toLong
          val bL = b1.toLong
          val prod = aL * bL
          val uppers = bs.map(_.toLong * aL)
          val lowers = as.map(_.toLong * bL)
          val square = prodSort(as, bs)
          prod #:: Kind.sortMerge(uppers, Kind.sortMerge(lowers, square))
        case _ =>
          // at least one is empty
          LazyList.empty
      }

    forAll { (l1: List[Short], l2: List[Short]) =>
      prodSort(
        l1.filter(_ >= 0).sorted.to(LazyList),
        l2.filter(_ >= 0).sorted.to(LazyList)
      ).sliding(2).foreach {
        case Seq(a, b) => assert(a <= b)
        case _         => ()
      }
    }
  }

  test("some subsume examples") {
    def check(str1: String, str2: String, matches: Boolean = true) =
      (Kind.parser.parseAll(str1), Kind.parser.parseAll(str2)) match {
        case (Right(k1), Right(k2)) =>
          assertEquals(Kind.leftSubsumesRight(k1, k2), matches)
        case err => fail(err.toString)
      }

    check("* -> *", "+* -> *")
    check("* -> *", "-* -> *")
    check("* -> *", Kind(Kind.Type.phantom).toDoc.render(80))

    check("+* -> *", Kind(Kind.Type.phantom).toDoc.render(80))
    check("-* -> *", Kind(Kind.Type.phantom).toDoc.render(80))
    // this is tricky:
    // if left is like trait Foo[G[+_]]
    // and right is like trait Bar[G[_]]
    // can we use the kind of Bar where we require kind of Foo?
    // yes, because we will only pass covariant G's to Bar, but Bar
    // doesn't care if they are covariant.
    check("(+* -> *) -> *", "(* -> *) -> *")

    check("(* -> *) -> *", "(+* -> *) -> *", false)
    check("(* -> *) -> *", "(-* -> *) -> *", false)

    check("+* -> *", "-* -> *", false)
    check("-* -> *", "+* -> *", false)
    // Some scala agreement
    // if we try to pass a kind, it has to be a subkind to accept
    // (+* -> *) -> *
    trait CoMonad[F[+_]]
    // (-* -> *) -> *
    trait ContraMonad[F[-_]]
    // (* -> *) -> *
    trait Monad[F[_]]
    // ((+* -> *) -> *) -> *
    trait CoMonadTrans[F[_[+_]]]
    trait ContraMonadTrans[F[_[-_]]]
    // ((* -> *) -> *) -> *
    trait MonadTrans[F[_[_]]]

    val _ = new MonadTrans[Monad] {}

    // this does not compile
    // new MonadTrans[CoMonad] {}
    check("(* -> *) -> *", "(+* -> *) -> *", false)
    // this does not compile
    // new MonadTrans[ContraMonad] {}
    check("(* -> *) -> *", "(-* -> *) -> *", false)

    val _ = new CoMonadTrans[CoMonad] {}
    // CoMonadTrans accepts Monad
    val _ = new CoMonadTrans[Monad] {}
    check("(+* -> *) -> *", "(* -> *) -> *")
    val _ = new ContraMonadTrans[Monad] {}
    check("(-* -> *) -> *", "(* -> *) -> *")
    val _ = new ContraMonadTrans[ContraMonad] {}
    check("(-* -> *) -> *", "(-* -> *) -> *")
    // this does not compile
    // new CoMonadTrans[ContraMonad] {}
    check("(+* -> *) -> *", "(-* -> *) -> *", false)
  }

  test("we can find all kinds in the allKinds list") {
    // cache this
    val all = Kind.allKinds

    // if order gets too crazy we will blow up the memory
    assertEquals(all.take(10000).toSet.size, 10000)

    def check(k: Kind) = assert(all.indexOf(k) >= 0)

    import Kind.Type
    check(Kind())
    check(Kind(Type.withVar(Variance.phantom)))
    check(Kind(Type.withVar(Variance.co)))
    check(Kind(Type.withVar(Variance.contra)))
    check(Kind(Type.withVar(Variance.in)))
    check(Kind(Type.withVar(Variance.co), Type.withVar(Variance.co)))
  }

  test("we can parse Kind.Arg") {
    forAll(genArg) { a =>
      Kind.paramKindParser.parseAll(Kind.argDoc(a).render(80)) match {
        case Right(a1) =>
          assertEquals(a1, a)
        case err => fail(err.toString)
      }
    }
  }

  test("Kind order is lawful") {
    forAll(genKind, genKind, genKind)(OrderingLaws.forOrder(_, _, _))
  }

  test("kindToLong can invert") {
    forAll(genKind) { k =>
      Kind.kindToLong(k) match {
        case Some(idx) =>
          assertEquals(Kind.longToKind(idx), Some(k))
        case None => ()
      }
    }

    assertEquals(Kind.kindToLong(Kind.Type), Some(0L))
    assertEquals(Kind.kindToLong(Kind(Kind.Type.in)), Some(11L))
    assertEquals(Kind.kindToLong(Kind(Kind.Type.co)), Some(3L))
    assertEquals(Kind.kindToLong(Kind(Kind.Type.contra)), Some(9L))
    assertEquals(Kind.kindToLong(Kind(Kind.Type.phantom)), Some(1L))

    // small kinds have small codes
    Kind.allKinds.take(21).foreach { k =>
      // these can all be encoded in 1 byte in proto
      assert(Kind.kindToLong(k).get < 0x7fL)
    }
    Kind.allKinds.take(217).foreach { k =>
      // these can all be encoded in 2 byte in proto
      assert(Kind.kindToLong(k).get < 0x7fffL)
    }
  }

  test("interleave and uninterleave -> inverses") {
    forAll { (l: Long) =>
      val res = Kind.uninterleave(l)
      val high = (res >>> 32).toInt
      val low = (res & 0xffffffffL).toInt
      assertEquals(
        Kind.interleave(high, low),
        l,
        s"res = $res low = $low, high = $high"
      )
    }

    forAll { (low: Int, high: Int) =>
      val long = Kind.interleave(high, low)
      val res = Kind.uninterleave(long)
      val high1 = (res >>> 32).toInt
      val low1 = (res & 0xffffffffL).toInt
      assertEquals(
        (high, low),
        (high1, low1),
        s"interleave($low, $high) = $long uninterleave($long) = $res"
      )
    }
  }
}
