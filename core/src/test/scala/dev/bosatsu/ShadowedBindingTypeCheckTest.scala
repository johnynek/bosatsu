package dev.bosatsu

import cats.data.Validated

import Identifier.Bindable
import ShadowedBindingTypeCheck.BindingSite
import TestUtils.{checkEnvExpr, testPackage}

class ShadowedBindingTypeCheckTest extends munit.FunSuite {

  private def normalizeSource(source: String): String = {
    val lines = source.linesIterator.toList
    val nonEmpty = lines.filter(_.trim.nonEmpty)
    val sharedIndent =
      nonEmpty
        .map(_.takeWhile(_.isWhitespace).length)
        .minOption
        .getOrElse(0)

    lines
      .map { line =>
        if (line.length >= sharedIndent) line.drop(sharedIndent)
        else line.trim
      }
      .mkString("\n")
      .trim
  }

  private def checkSource(
      source: String
  ): ShadowedBindingTypeCheck.Res[Unit] =
    checkEnvExpr(normalizeSource(source)) { (_, lets) =>
      ShadowedBindingTypeCheck.checkLets(testPackage, lets)
    }

  private def positiveCheck(source: String): Unit =
    checkSource(source) match {
      case Validated.Valid(()) => ()
      case Validated.Invalid(errs) =>
        val all = errs.toNonEmptyList.toList
        fail(s"expected shadow check to pass, got errors: $all")
    }

  private def renderError(
      source: String,
      err: ShadowedBindingTypeCheck.Error
  ): String = {
    val normalized = normalizeSource(source)
    val sourceMap = Map(testPackage -> (LocationMap(normalized), "<test>"))
    PackageError
      .ShadowedBindingTypeError(testPackage, err)
      .message(sourceMap, LocationMap.Colorize.None)
  }

  private def negativeCheck(
      source: String
  )(assertErr: (ShadowedBindingTypeCheck.Error, String) => Unit): Unit =
    checkSource(source) match {
      case Validated.Valid(()) =>
        fail("expected shadow check to fail")
      case Validated.Invalid(errs) =>
        errs.toNonEmptyList.toList match {
          case err :: Nil =>
            assertErr(err, renderError(source, err))
          case many =>
            fail(s"expected one shadow error, got ${many.length}: $many")
        }
    }

  private def bindable(name: String): Bindable = Identifier.Name(name)

  test("top-level shadowing by locals is allowed") {
    positiveCheck(
      """
      x = 1
      main = (
        x = "two"
        x
      )
      """
    )
  }

  test("let shadow with same type passes") {
    positiveCheck(
      """
      main = (
        x = 1
        x = 2
        x
      )
      """
    )
  }

  test("let shadow with different type fails") {
    negativeCheck(
      """
      main = (
        x = 1
        x = "two"
        x
      )
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("x"))
      assertEquals(err.previous.site, BindingSite.LetBinding)
      assertEquals(err.current.site, BindingSite.LetBinding)
      assert(msg.contains("previous type: Int"), msg)
      assert(msg.contains("current type: String"), msg)
    }
  }

  test("lambda arg shadow with same type passes") {
    positiveCheck(
      """
      enum I: One, Two

      main = (
        fn = (x: I) -> (
          x = One
          x
        )
        fn(One)
      )
      """
    )
  }

  test("let shadowing lambda argument with a different type fails") {
    negativeCheck(
      """
      enum I: One, Two

      main = (
        fn = (x: I) -> (
          x = "two"
          x
        )
        fn(One)
      )
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("x"))
      assertEquals(err.current.site, BindingSite.LetBinding)
      assert(msg.contains("shadowed binding `x` changes type."), msg)
      assert(msg.contains("previous type: I"), msg)
      assert(msg.contains("current type: String"), msg)
    }
  }

  test("match pattern binder shadow different type fails") {
    negativeCheck(
      """
      struct Tup(a, b)

      main = (
        x = 1
        value = Tup("str", 1)
        match value:
          case Tup(x, _): x
      )
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("x"))
      assertEquals(err.previous.site, BindingSite.LetBinding)
      assertEquals(err.current.site, BindingSite.PatternBinding)
      assert(msg.contains("previous type: Int"), msg)
      assert(msg.contains("current type: String"), msg)
    }
  }

  test("pattern binding cannot shadow a lambda arg with a different type") {
    negativeCheck(
      """
      struct Tup(a, b)
      enum Either[a]: Left(a: a), Right(a: a)
      struct Foo

      def unwrap(e: exists a. Tup[Either[a], a -> Foo]):
        Tup(e, fn) = e
        a = match e:
          case Left(x): x
          case Right(y): y
        fn(a)

      main = 1
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("e"))
      assertEquals(err.current.site, BindingSite.PatternBinding)
      assert(msg.contains("shadowed binding `e` changes type."), msg)
    }
  }

  test("nested pattern binding cannot reuse the outer argument name with a new type") {
    negativeCheck(
      """
      struct Box[x: *](a: x)
      struct One
      enum Opt[a]: None, Some(a: a)

      y: forall a. Box[Opt[a]] = Box(None)
      def process(o: Box[Opt[One]]) -> One:
        match o:
          case Box(Some(o)): o
          case Box(None): One

      main = process(y)
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("o"))
      assertEquals(err.current.site, BindingSite.PatternBinding)
      assert(msg.contains("shadowed binding `o` changes type."), msg)
    }
  }

  test("nested lambda args can shadow outer lambda args with different types") {
    positiveCheck(
      """
      main = (
        cmp = (x, y) -> (
          inner = (x, b) -> x
          inner("s", y)
        )
        cmp(1, 1)
      )
      """
    )
  }

  test("inner lambda arg shadow from tuple to Int is allowed") {
    positiveCheck(
      """
      struct Tup(a, b)

      main = (
        outer = (x, y) -> (
          `&` = (x, b) -> x
          `&`(1, 2)
        )
        outer(Tup(1, 2), Tup(3, 4))
      )
      """
    )
  }

  // Lint-only policy note:
  // For an unannotated `def keep(x): ...`, the parameter `x` starts as a free type
  // variable during inference. Rebinding `x` in the body should be lint-illegal in
  // both cases below (`x = 2` and `x = "two"`), even if call sites later constrain
  // the function to an apparently matching concrete type.
  test("let shadowing def parameter with same-looking type still fails") {
    negativeCheck(
      """
      def keep(x):
        x = 2
        x

      main = keep(1)
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("x"))
      assertEquals(err.current.site, BindingSite.LetBinding)
      assert(msg.contains("shadowed binding `x` changes type."), msg)
    }
  }

  test("let shadowing def parameter with a different type fails") {
    negativeCheck(
      """
      def keep(x):
        x = "two"
        x

      main = keep(1)
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("x"))
      assertEquals(err.current.site, BindingSite.LetBinding)
      assert(msg.contains("shadowed binding `x` changes type."), msg)
      assert(msg.contains("previous type:"), msg)
      assert(msg.contains("current type: String"), msg)
    }
  }

  test("pattern binding shadowing def parameter with a different type fails") {
    negativeCheck(
      """
      struct Tup(a, b)

      def proj(x):
        match x:
          case Tup(x, _): x

      main = proj(Tup(1, 2))
      """
    ) { (err, msg) =>
      assertEquals(err.name, bindable("x"))
      assertEquals(err.current.site, BindingSite.PatternBinding)
      assert(msg.contains("shadowed binding `x` changes type."), msg)
    }
  }

  test("def parameter can shadow an outer local with a different type") {
    positiveCheck(
      """
      main = (
        x = 1
        def render(x): x
        render("ok")
      )
      """
    )
  }

  test("quantified alpha-equivalent shadows pass") {
    positiveCheck(
      """
      main = (
        id: forall a. a -> a = x -> x
        id: forall b. b -> b = id
        id
      )
      """
    )
  }

  test("quantified non-equivalent shadows fail and messages hide internal renames") {
    negativeCheck(
      """
      enum Box[a]: Box(value: a)

      main = (
        id: forall a. a -> a = x -> x
        id: forall a. a -> Box[a] = x -> Box(x)
        id
      )
      """
    ) { (_, msg) =>
      assert(!msg.contains("_shadow_t"), msg)
      assert(msg.contains("previous type: forall"), msg)
      assert(msg.contains("current type: forall"), msg)
    }
  }

  test("wildcard lambda args do not affect shadow checks") {
    positiveCheck(
      """
      main = (
        x = 1
        fn = _ -> "ok"
        fn(x)
      )
      """
    )
  }
}
