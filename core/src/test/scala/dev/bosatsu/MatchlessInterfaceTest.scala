package dev.bosatsu

import cats.Order
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.codegen.CompilationSource

class MatchlessInterfaceTest extends munit.FunSuite {

  private def typeCheck(
      src: String,
      ifaces: List[Package.Interface]
  ): PackageMap.Compiled = {
    val pack = Parser.unsafeParse(Package.parser, src)
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

  private def containsGlobal(
      expr: Matchless.Expr[Unit],
      pack: PackageName,
      name: Identifier.Bindable
  ): Boolean =
    expr match {
      case Matchless.Global(_, `pack`, `name`) =>
        true
      case Matchless.Lambda(captures, _, _, body) =>
        captures.exists(containsGlobal(_, pack, name)) ||
          containsGlobal(body, pack, name)
      case Matchless.WhileExpr(cond, effectExpr, _) =>
        containsGlobal(cond, pack, name) || containsGlobal(effectExpr, pack, name)
      case Matchless.App(fn, args) =>
        containsGlobal(fn, pack, name) || args.exists(containsGlobal(_, pack, name))
      case Matchless.Let(_, value, in) =>
        containsGlobal(value, pack, name) || containsGlobal(in, pack, name)
      case Matchless.LetMut(_, in) =>
        containsGlobal(in, pack, name)
      case Matchless.If(cond, thenExpr, elseExpr) =>
        containsGlobal(cond, pack, name) ||
          containsGlobal(thenExpr, pack, name) ||
          containsGlobal(elseExpr, pack, name)
      case Matchless.SwitchVariant(on, _, cases, default) =>
        containsGlobal(on, pack, name) ||
          cases.exists { case (_, branch) => containsGlobal(branch, pack, name) } ||
          default.exists(containsGlobal(_, pack, name))
      case Matchless.Always(cond, thenExpr) =>
        containsGlobal(cond, pack, name) || containsGlobal(thenExpr, pack, name)
      case Matchless.PrevNat(of) =>
        containsGlobal(of, pack, name)
      case ge: Matchless.GetEnumElement[?] =>
        containsGlobal(ge.arg, pack, name)
      case gs: Matchless.GetStructElement[?] =>
        containsGlobal(gs.arg, pack, name)
      case _ =>
        false
    }

  private def containsGlobal(
      boolExpr: Matchless.BoolExpr[Unit],
      pack: PackageName,
      name: Identifier.Bindable
  ): Boolean =
    boolExpr match {
      case Matchless.CompareLit(arg, _, _) =>
        containsGlobal(arg, pack, name)
      case Matchless.CompareInt(left, _, right) =>
        containsGlobal(left, pack, name) || containsGlobal(right, pack, name)
      case Matchless.CompareInt64(left, _, right) =>
        containsGlobal(left, pack, name) || containsGlobal(right, pack, name)
      case Matchless.CompareFloat64(left, _, right) =>
        containsGlobal(left, pack, name) || containsGlobal(right, pack, name)
      case Matchless.EqualsNat(arg, _) =>
        containsGlobal(arg, pack, name)
      case Matchless.And(left, right) =>
        containsGlobal(left, pack, name) || containsGlobal(right, pack, name)
      case Matchless.CheckVariant(arg, _, _, _) =>
        containsGlobal(arg, pack, name)
      case Matchless.CheckVariantSet(arg, _, _, _) =>
        containsGlobal(arg, pack, name)
      case Matchless.SetMut(_, value) =>
        containsGlobal(value, pack, name)
      case Matchless.LetBool(_, value, in) =>
        containsGlobal(value, pack, name) || containsGlobal(in, pack, name)
      case Matchless.LetMutBool(_, in) =>
        containsGlobal(in, pack, name)
      case Matchless.TrueConst =>
        false
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
      val compiled =
        MatchlessFromTypedExpr.compile((), fibPm, Matchless.LocalPassOptions.Default)
      assert(compiled.contains(PackageName.parts("My", "Fib")))
    }
  }

  test("optimized Matchless only inlines imported helpers when implementations are available") {
    val helperPack = PackageName.parts("Helper", "Bool")
    val callerPack = PackageName.parts("Caller", "Bool")
    val choose = Identifier.Name("choose")
    val use = Identifier.Name("use")

    val helperSrc =
      """package Helper/Bool
        |
        |export choose
        |
        |def choose(flag: Bool, on_true: Int) -> Int:
        |  if flag:
        |    on_true
        |  else:
        |    0
        |""".stripMargin

    val helperPm = typeCheck(helperSrc, Nil)
    val helperIface = Package.interfaceOf(helperPm.toMap(helperPack))

    val callerSrc =
      """package Caller/Bool
        |
        |from Helper/Bool import choose
        |
        |def use(i: Int) -> Int:
        |  choose(False, i)
        |""".stripMargin

    val callerPm = typeCheck(callerSrc, helperIface :: Nil)
    val combinedPm = helperPm ++ callerPm.toMap.values

    Par.withEC {
      val optimizedWithImpl = CompilationSource.namespace(combinedPm).compiled(())
      val optimizedIfaceOnly = CompilationSource.namespace(callerPm).compiled(())

      assertEquals(
        containsGlobal(
          optimizedWithImpl(callerPack).toMap.apply(use),
          helperPack,
          choose
        ),
        false
      )
      assertEquals(
        containsGlobal(
          optimizedIfaceOnly(callerPack).toMap.apply(use),
          helperPack,
          choose
        ),
        true
      )
      Matchless.recoverTopLevelLambda(optimizedWithImpl(callerPack).toMap.apply(use)) match {
        case Matchless.Lambda(Nil, None, _, Matchless.Literal(lit)) =>
          assertEquals(lit, Lit.fromInt(0))
        case other =>
          fail(s"expected inlined helper to reduce to a constant-zero lambda, found: $other")
      }
    }
  }
}
