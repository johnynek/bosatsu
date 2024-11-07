package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import Code._

class CodeTest extends munit.FunSuite {

  def checkCode(c: Code, str: String)(implicit loc: munit.Location) = {
    assertEquals(toDoc(c).renderTrim(80), str)
  }

  test("basic renders") {
    checkCode(Ident("foo"), "foo")
    checkCode(Ident("foo") := Ident("bar"), "foo = bar;")
    checkCode(DeclareVar(Nil, TypeIdent.Named("Foo"), Ident("bar"), None), "Foo bar;")
    checkCode(DeclareVar(Nil, TypeIdent.Named("Foo"), Ident("bar"), Some(Ident("baz"))),
      "Foo bar = baz;")
    checkCode(DeclareVar(Nil, TypeIdent.Named("Foo").ptr, Ident("bar"), None), "Foo* bar;")
    checkCode(DeclareVar(Attr.Static :: Nil, TypeIdent.Named("Foo").ptr, Ident("bar"), None), "static Foo* bar;")

    checkCode(TypeIdent.Named("Foo").typedefAs(Ident("Baz")), "typedef Foo Baz;")
    checkCode(TypeIdent.Named("Foo").cast(Ident("Baz")), "(Foo)Baz")

    checkCode(DefineComplex(TypeIdent.StructType("Foo"),
      (TypeIdent.Named("int"), Ident("bar")) ::
      (TypeIdent.Named("float"), Ident("baz")) ::
      Nil
    ), "struct Foo {\n" +
      "    int bar;\n" +
      "    float baz;\n" +
      "};")

    checkCode(returnVoid, "return;")
    checkCode(Ident("bar").ret, "return bar;")
    checkCode(block(Ident("bar").ret, Ident("baz").ret), "{\n" +
      "    return bar;\n" +
      "    return baz;\n" +
      "}")

    checkCode(IfElse(NonEmptyList.of(
      Ident("foo") -> block(Ident("baz").ret),
      Ident("bar") -> block(Ident("quux").ret),
    ), None), "if (foo) {\n" +
      "    return baz;\n" +
      "}\n" +
      "else if (bar) {\n" +
      "    return quux;\n" +
      "}")

    checkCode(IfElse(NonEmptyList.of(
      Ident("bar") -> block(Ident("quux").ret),
    ), Some(block(returnVoid))), "if (bar) {\n" +
      "    return quux;\n" +
      "}\n" +
      "else {\n" +
      "    return;\n" +
      "}")

    checkCode(Ident("foo")(Ident("bar")), "foo(bar)")
    checkCode(Ident("foo")(Ident("bar"), Ident("baz")), "foo(bar, baz)")
    checkCode(Ident("foo")(), "foo()")
    checkCode(TypeIdent.Named("Foo").cast(Ident("foo"))(), "((Foo)foo)()")
  }
}