package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eq
import dev.bosatsu.rankn.Type
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.util.{Failure, Success, Try}
import cats.implicits._

import Identifier.Constructor

class ProtoConverterTest extends munit.ScalaCheckSuite with ParTest {
  private given Eq[Package.Interface] =
    // Safe: Package.Interface is immutable and uses structural equals.
    Eq.fromUniversalEquals
  private given Eq[Package.Typed[Unit]] =
    // Safe: Package.Typed[Unit] is immutable and uses structural equals.
    Eq.fromUniversalEquals
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 100 else 10
    )

  def law[A: Eq, B](a: A, fn: A => Try[B], gn: B => Try[A]) = {
    val maybeProto = fn(a)
    assert(maybeProto.isSuccess, maybeProto.toString)
    val proto = maybeProto.get

    val orig = gn(proto) match {
      case Success(o)   => o
      case Failure(err) =>
        err.printStackTrace
        fail(s"expected to deserialize: $err")
        sys.error(s"could not deserialize: $err")
    }

    lazy val diffIdx =
      a.toString
        .zip(orig.toString)
        .zipWithIndex
        .dropWhile { case ((a, b), _) => a == b }
        .headOption
        .map(_._2)
        .getOrElse(0)

    val context = 1000
    assert(
      Eq[A].eqv(a, orig),
      s"${a.toString.drop(diffIdx - context / 2).take(context)} != ${orig.toString.drop(diffIdx - context / 2).take(context)}"
    )
    // assert(Eq[A].eqv(a, orig), s"$a\n\n!=\n\n$orig")
  }

  def tabLaw[A: Eq, B](
      f: A => ProtoConverter.Tab[B]
  )(g: (ProtoConverter.SerState, B) => ProtoConverter.DTab[A]) = { (a: A) =>
    f(a).run(ProtoConverter.SerState.empty) match {
      case Success((ss, b)) =>
        val ds = ProtoConverter.DecodeState.init(ss.strings.inOrder)
        g(ss, b).run(ds) match {
          case Success(finalA) =>
            assert(Eq[A].eqv(a, finalA), s"$a\n\nnot equalto\n\n$finalA")
          case Failure(err) =>
            fail(s"on decode $b (from $a), got: ${err.toString}")
        }
      case Failure(err) =>
        fail(s"on encode $a, got: ${err.toString}")
    }
  }

  test("we can roundtrip types through proto") {
    val testFn = tabLaw(ProtoConverter.typeToProto(_: Type)) { (ss, idx) =>
      ProtoConverter.buildTypes(ss.types.inOrder).map(_(idx - 1))
    }

    forAll(rankn.NTypeGen.genDepth03)(testFn)
  }

  test("we can roundtrip patterns through proto") {
    val testFn = tabLaw(
      ProtoConverter.patternToProto(
        _: Pattern[(PackageName, Constructor), Type]
      )
    ) { (ss, idx) =>
      for {
        tps <- ProtoConverter.buildTypes(ss.types.inOrder)
        pats = ProtoConverter.buildPatterns(ss.patterns.inOrder).map(_(idx - 1))
        res <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
      } yield res
    }

    forAll(Generators.genCompiledPattern(5))(testFn)
  }

  test("we can roundtrip TypedExpr through proto") {
    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Unit])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          expr = ProtoConverter
            .buildExprs(ss.expressions.inOrder)
            .map(_(idx - 1))
          res <- expr.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    forAll(
      Generators.genTypedExpr(Gen.const(()), 4, rankn.NTypeGen.genDepth03)
    )(testFn)
  }

  test("we can roundtrip interface through proto") {
    forAll(Generators.interfaceGen) { iface =>
      law(
        iface,
        ProtoConverter.interfaceToProto,
        ProtoConverter.interfaceFromProto
      )
    }
  }

  val sortedEq: Eq[List[Package.Interface]] =
    new Eq[List[Package.Interface]] {
      def eqv(l: List[Package.Interface], r: List[Package.Interface]) =
        // we are only sorting the left because we expect the right
        // to come out sorted
        l.sortBy(_.name.asString) === r
    }

  test("we can roundtrip interfaces through proto") {
    forAll(Generators.smallDistinctByList(Generators.interfaceGen)(_.name)) {
      ifaces =>
        law(
          ifaces,
          ProtoConverter.interfacesToProto[List],
          ProtoConverter.interfacesFromProto
        )(using sortedEq)
    }
  }

  test("we can roundtrip interfaces from full packages through proto") {
    forAll(Generators.genPackage(Gen.const(()), 10)) { packMap =>
      val ifaces = packMap.iterator.map { case (_, p) =>
        Package.interfaceOf(p)
      }.toList
      law(
        ifaces,
        ProtoConverter.interfacesToProto[List],
        ProtoConverter.interfacesFromProto
      )(using sortedEq)
    }
  }

  test("test some hand written packages") {
    def ser(p: List[Package.Typed[Unit]]): Try[List[proto.Package]] =
      p.traverse(ProtoConverter.packageToProto)
    def deser(ps: List[proto.Package]): Try[List[Package.Typed[Unit]]] =
      ProtoConverter.packagesFromProto(Nil, ps).map { case (_, p) =>
        p.sortBy(_.name)
      }

    val tf = Package.typedFunctor
    TestUtils.testInferred(
      List(
        """package Foo

export bar

bar = 1
"""
      ),
      "Foo",
      (packs, _) =>
        law(
          packs.toMap.values.toList.sortBy(_.name).map { pt =>
            Package.setProgramFrom(tf.void(pt), ())
          },
          ser,
          deser
        )
    )
  }

  test("we can roundtrip packages through proto") {
    forAll(Generators.genPackage(Gen.const(()), 10)) { packMap =>
      def ser(p: List[Package.Typed[Unit]]): Try[List[proto.Package]] =
        p.traverse(ProtoConverter.packageToProto)
      def deser(ps: List[proto.Package]): Try[List[Package.Typed[Unit]]] =
        ProtoConverter.packagesFromProto(Nil, ps).map { case (_, p) =>
          p.sortBy(_.name)
        }

      val packList = packMap.toList.sortBy(_._1).map(_._2)
      law(packList, ser, deser)
    }
  }

  test("packagesFromProto accepts interface/package name overlap") {
    forAll(Generators.genPackage(Gen.const(()), 5)) { packMap =>
      val packs = packMap.values.toList
      val ifaces = packs.map(Package.interfaceOf(_))
      val res = for {
        ifacesProto <- ifaces.traverse(ProtoConverter.interfaceToProto)
        packsProto <- packs.traverse(ProtoConverter.packageToProto)
        decoded <- ProtoConverter.packagesFromProto(ifacesProto, packsProto)
      } yield decoded

      assert(res.isSuccess, res.toString)
    }
  }

}
