package dev.bosatsu.codegen.python

import cats.data.NonEmptyList
import dev.bosatsu.Generators.bindIdentGen
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.{
  CompileOptions,
  Identifier,
  Lit,
  LocationMap,
  Matchless,
  Package,
  PackageMap,
  PackageName,
  Par,
  Parser,
  TestUtils
}
import dev.bosatsu.codegen.CompilationNamespace
import dev.bosatsu.graph.Toposort
import org.scalacheck.Prop.forAll

class PythonGenTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(5000)
  // PropertyCheckConfiguration(minSuccessful = 500)

  val PythonName = "[_A-Za-z][_A-Za-z0-9]*".r.pattern

  private def normalizeGeneratedTemps(code: String): String = {
    val tempName = "___[A-Za-z]\\d+".r
    val mapping = scala.collection.mutable.LinkedHashMap.empty[String, String]
    var next = 0
    tempName.replaceAllIn(
      code,
      m =>
        mapping.getOrElseUpdate(
          m.matched.nn, {
            val nm = s"___v$next"
            next = next + 1
            nm
          }
        )
    )
  }

  private def typeCheckPackages(srcs: List[String]): PackageMap.Typed[Any] = {
    val parsed =
      srcs.zipWithIndex.map { case (src, idx) =>
        val pack = Parser.unsafeParse(Package.parser(None), src)
        ((s"test_$idx", LocationMap(src)), pack)
      }
    val nel = NonEmptyList.fromList(parsed).getOrElse(fail("expected sources"))
    Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, Nil, "<test>", CompileOptions.Default)
        .strictToValidated
        .fold(errs => fail(errs.toList.mkString("\n")), identity)
    }
  }

  test("all escapes are valid python identifiers") {
    forAll(bindIdentGen) { b =>
      val str = PythonGen.escape(b).name

      assert(
        PythonName.matcher(str).matches(),
        s"escaped: ${b.sourceCodeRepr} to $str"
      )
    }
  }

  test("we can parse an example externals file") {
    val extstr = """
      { IO: { foo: bar.baz, quux: quux.quux_impl }, Bop: { foo: collections.queue } }
    """
    assertEquals(
      PythonGen.externalParser.parseAll(extstr).map(_ => ()),
      Right(())
    )
  }

  test("we can parse an example evals file") {
    val str = """
      {
        IO::IO: IOPy.run_io,
        Build/Foo::Bar: BuildImpl.run_build,
      }
    """
    assertEquals(
      PythonGen.evaluatorParser.parseAll(str).map(_ => ()),
      Right(())
    )
  }

  test("top-level lets around lambda avoid closure tuples") {
    val src =
      """|enum Nat:
         |  Zero
         |  Succ(pred: Nat)
         |
         |def count_chars(s):
         |  def loop(s, acc: Nat):
         |    recur s:
         |      case "": acc
         |      case "$.{_}${tail}": loop(tail, Succ(acc))
         |
         |  loop(s, Zero)
         |""".stripMargin

    TestUtils.checkPackageMap(src) { pm =>
      Par.withEC {
        val rendered = PythonGen.renderSource(pm, Map.empty, Map.empty)
        val doc = rendered(())(TestUtils.testPackage)._2
        val code = doc.render(120)

        val expected =
          "\n\n" +
            """def count_chars(___bs0):
    ___a4 = ___bs0
    ___a6 = 0
    ___a1 = 1
    ___t1 = True
    while ___t1:
        if ___a4 == "":
            ___a1 = 0
            ___a2 = ___a6
        else:
            ___a4 = ___a4[1:]
            ___a6 = ___a6 + 1
        ___t1 = ___a1 == 1
    return ___a2"""
        assertEquals(
          normalizeGeneratedTemps(code),
          normalizeGeneratedTemps(expected)
        )
      }
    }
  }

  test("CheckVariantSet guards compile to direct Python membership comparisons") {
    val famArities = 0 :: 0 :: 0 :: 0 :: 0 :: Nil
    val arg = Identifier.Name("v")
    val body = Matchless.If(
      Matchless.CheckVariantSet(
        Matchless.Local(arg),
        NonEmptyList.of(0, 1),
        0,
        famArities
      ),
      Matchless.Literal(Lit(1)),
      Matchless.If(
        Matchless.CheckVariantSet(
          Matchless.Local(arg),
          NonEmptyList.of(3, 4),
          0,
          famArities
        ),
        Matchless.Literal(Lit(2)),
        Matchless.If(
          Matchless.CheckVariantSet(
            Matchless.Local(arg),
            NonEmptyList.of(1, 3),
            0,
            famArities
          ),
          Matchless.Literal(Lit(3)),
          Matchless.If(
            Matchless.CheckVariantSet(
              Matchless.Local(arg),
              NonEmptyList.of(0, 2, 4),
              0,
              famArities
            ),
            Matchless.Literal(Lit(4)),
            Matchless.If(
              Matchless.CheckVariantSet(
                Matchless.Local(arg),
                NonEmptyList.of(0, 1, 2, 3, 4),
                0,
                famArities
              ),
              Matchless.Literal(Lit(5)),
              Matchless.Literal(Lit(0))
            )
          )
        )
      )
    )
    val mainExpr: Matchless.Expr[Unit] =
      Matchless.Lambda(Nil, None, NonEmptyList.one(arg), body)
    val pn = PackageName.parts("Test", "GuardCoverage")
    val ns: CompilationNamespace[Unit] = new CompilationNamespace[Unit] {
      implicit val keyOrder: Ordering[Unit] = new Ordering[Unit] {
        def compare(x: Unit, y: Unit): Int = 0
      }
      val keyShow: cats.Show[Unit] = cats.Show.show(_ => "root")
      def identOf(k: Unit, p: PackageName): NonEmptyList[String] = p.parts
      def depFor(src: Unit, p: PackageName): Unit = ()
      def rootKey: Unit = ()
      val topoSort: Toposort.Result[(Unit, PackageName)] =
        Toposort.Success(Vector(NonEmptyList.one(((), pn))))
      val compiled = scala.collection.immutable.SortedMap(
        () -> Map(pn -> List((Identifier.Name("main"), mainExpr)))
      )
      def exportedValues(
          packageName: PackageName
      ): Option[Map[Identifier.Bindable, dev.bosatsu.rankn.Type]] =
        None
      def exportedTestEntry(
          packageName: PackageName,
          bindable: Identifier.Bindable
      ): Option[dev.bosatsu.Package.TestEntry[Any]] =
        None
      val testEntries = Map.empty
      def mainValues(
          mainTypeFn: dev.bosatsu.rankn.Type => Boolean
      ): Map[PackageName, (Identifier.Bindable, dev.bosatsu.rankn.Type)] =
        Map.empty
      val externals = scala.collection.immutable.SortedMap(
        () -> Map.empty[PackageName, List[
          (Identifier.Bindable, dev.bosatsu.rankn.Type)
        ]]
      )
      def treeShake(
          roots: Set[(PackageName, Identifier)]
      ): CompilationNamespace[Unit] = this
      def rootPackages =
        scala.collection.immutable.SortedSet(pn)
    }
    Par.withEC {
      val rendered = PythonGen.renderAll(ns, Map.empty, Map.empty)
      val doc = rendered(())(pn)._2
      val code = doc.render(120)

      assert(code.contains("< 2"), code)
      assert(code.contains("not"), code)
      assert(code.contains("< 3"), code)
      assert(code.contains("== 1"), code)
      assert(code.contains("== 3"), code)
      assert(code.contains("!= 1"), code)
      assert(code.contains("!= 3"), code)
      assert(code.contains(" or "), code)
      assert(code.contains(" and "), code)
    }
  }

  test("wide integer literal matches use ordered comparisons in Python") {
    TestUtils.checkPackageMap("""
def classify_int(n):
  match n:
    case 1: 10
    case 2: 20
    case 3: 30
    case 4: 40
    case 5: 50
    case _: 0

main = classify_int
""") { pm =>
      Par.withEC {
        val rendered = PythonGen.renderSource(pm, Map.empty, Map.empty)
        val doc = rendered(())(TestUtils.testPackage)._2
        val code = doc.render(120)

        assert(code.contains(" < "), code)
        assert(code.contains("not"), code)
      }
    }
  }

  test("wide char literal matches use ordered comparisons in Python") {
    TestUtils.checkPackageMap("""
def classify_char(ch):
  match ch:
    case .'a': 1
    case .'b': 2
    case .'c': 3
    case .'d': 4
    case .'e': 5
    case _: 0

main = classify_char
""") { pm =>
      Par.withEC {
        val rendered = PythonGen.renderSource(pm, Map.empty, Map.empty)
        val doc = rendered(())(TestUtils.testPackage)._2
        val code = doc.render(120)

        assert(code.contains(" < "), code)
        assert(code.contains("not"), code)
      }
    }
  }

  test("SwitchVariant compiles through toIfElse in Python") {
    val famArities = 0 :: 0 :: 1 :: 0 :: 0 :: Nil
    val arg = Identifier.Name("v")
    val body: Matchless.Expr[Unit] =
      Matchless.SwitchVariant(
        Matchless.Local(arg),
        famArities,
        NonEmptyList.of(
          0 -> Matchless.Literal(Lit(1)),
          1 -> Matchless.Literal(Lit(2)),
          3 -> Matchless.Literal(Lit(3)),
          4 -> Matchless.Literal(Lit(4))
        ),
        Some(Matchless.Literal(Lit(0)))
      )
    val mainExpr: Matchless.Expr[Unit] =
      Matchless.Lambda(Nil, None, NonEmptyList.one(arg), body)
    val pn = PackageName.parts("Test", "SwitchCoverage")
    val ns: CompilationNamespace[Unit] = new CompilationNamespace[Unit] {
      implicit val keyOrder: Ordering[Unit] = new Ordering[Unit] {
        def compare(x: Unit, y: Unit): Int = 0
      }
      val keyShow: cats.Show[Unit] = cats.Show.show(_ => "root")
      def identOf(k: Unit, p: PackageName): NonEmptyList[String] = p.parts
      def depFor(src: Unit, p: PackageName): Unit = ()
      def rootKey: Unit = ()
      val topoSort: Toposort.Result[(Unit, PackageName)] =
        Toposort.Success(Vector(NonEmptyList.one(((), pn))))
      val compiled = scala.collection.immutable.SortedMap(
        () -> Map(pn -> List((Identifier.Name("main"), mainExpr)))
      )
      def exportedValues(
          packageName: PackageName
      ): Option[Map[Identifier.Bindable, dev.bosatsu.rankn.Type]] =
        None
      def exportedTestEntry(
          packageName: PackageName,
          bindable: Identifier.Bindable
      ): Option[dev.bosatsu.Package.TestEntry[Any]] =
        None
      val testEntries = Map.empty
      def mainValues(
          mainTypeFn: dev.bosatsu.rankn.Type => Boolean
      ): Map[PackageName, (Identifier.Bindable, dev.bosatsu.rankn.Type)] =
        Map.empty
      val externals = scala.collection.immutable.SortedMap(
        () -> Map.empty[PackageName, List[
          (Identifier.Bindable, dev.bosatsu.rankn.Type)
        ]]
      )
      def treeShake(
          roots: Set[(PackageName, Identifier)]
      ): CompilationNamespace[Unit] = this
      def rootPackages =
        scala.collection.immutable.SortedSet(pn)
    }
    Par.withEC {
      val rendered = PythonGen.renderAll(ns, Map.empty, Map.empty)
      val doc = rendered(())(pn)._2
      val code = doc.render(120)

      assert(code.contains("[0] < 2"), code)
      assert(code.contains("[0] < 1"), code)
      assert(code.contains("[0] == 2"), code)
      assert(code.contains("[0] == 3"), code)
      assert(!code.contains("elif"), code)
    }
  }

  test("float32 conversion externals render in python codegen") {
    val src =
      """package Test
        |
        |from Bosatsu/Num/Float64 import (
        |  float32_bits_to_Float64,
        |  float64_to_float32_bits,
        |)
        |
        |def roundtrip(bits):
        |  float64_to_float32_bits(float32_bits_to_Float64(bits))
        |
        |main = roundtrip
        |""".stripMargin

    val floatPkg =
      """package Bosatsu/Num/Float64
        |
        |from Bosatsu/Predef import Float64
        |
        |export (
        |  float32_bits_to_Float64,
        |  float64_to_float32_bits,
        |)
        |
        |external def float32_bits_to_Float64(x: Int) -> Float64
        |external def float64_to_float32_bits(x: Float64) -> Int
        |""".stripMargin

    val pm = typeCheckPackages(List(floatPkg, src))
    Par.withEC {
      val rendered = PythonGen.renderSource(pm, Map.empty, Map.empty)
      val doc = rendered(())(PackageName.parts("Test"))._2
      val code = doc.render(120)

      assert(code.contains("import struct as"), code)
      assert(code.contains(".pack(\">I\""), code)
      assert(code.contains(".pack(\">f\""), code)
      assert(code.contains(".unpack(\">I\""), code)
      assert(code.contains(".unpack(\">f\""), code)
    }
  }

}
