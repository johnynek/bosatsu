package dev.bosatsu

import cats.Order
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension

class MatchlessInterfaceTest extends munit.FunSuite {

  private def typeCheck(
      src: String,
      ifaces: List[Package.Interface]
  ): PackageMap.Inferred = {
    val pack = Parser.unsafeParse(Package.parser(None), src)
    val nel = NonEmptyList.one((("test", LocationMap(src)), pack))
    Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, ifaces, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          identity
        )
    }
  }

  test("Matchless can compile constructors from imported interfaces") {
    val natSrc =
      """package Bosatsu/Num/Nat
        |
        |export Nat()
        |
        |enum Nat: Zero, Succ(prev: Nat)
        |""".stripMargin

    val natPm = typeCheck(natSrc, Nil)
    val natPack =
      natPm.toMap(PackageName.parts("Bosatsu", "Num", "Nat"))
    val natIface = Package.interfaceOf(natPack)

    val fibSrc =
      """package My/Fib
        |
        |from Bosatsu/Num/Nat import Nat, Zero, Succ
        |
        |def pred_or_zero(n: Nat) -> Nat:
        |  match n:
        |    case Zero: Zero
        |    case Succ(n): n
        |""".stripMargin

    val fibPm = typeCheck(fibSrc, natIface :: Nil)

    Par.withEC {
      given Order[Unit] = Order.fromOrdering
      val compiled = MatchlessFromTypedExpr.compile((), fibPm)
      assert(compiled.contains(PackageName.parts("My", "Fib")))
    }
  }
}
