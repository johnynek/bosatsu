package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import org.bykn.bosatsu.codegen.Idents
import org.bykn.bosatsu.{PackageName, TestUtils, Identifier, Predef}
import Identifier.Name

class ClangGenTest extends munit.FunSuite {
  test("test some generation") {
    TestUtils.checkMatchless("""
x = 1    
""") { matchlessMap =>
  
      val predef_c = Code.Include(true, "bosatsu_predef.h")
      def predef(s: String) = {
        (PackageName.PredefName -> Name(s)) -> (predef_c,
          Code.Ident(Idents.escape("__bsts_predef_", s)))
      }

      val res = ClangGen.renderMain(
        sortedEnv = Vector(
          NonEmptyList.one(PackageName.PredefName -> matchlessMap(PackageName.PredefName)),
          NonEmptyList.one(TestUtils.testPackage -> matchlessMap(TestUtils.testPackage)),
        ),
        externals =
          Predef.jvmExternals.toMap.keys.iterator.map { case (_, n) => predef(n) }.toMap,
        value = (TestUtils.testPackage, Identifier.Name("x")),
        evaluator = (Code.Include(true, "eval.h"), Code.Ident("evaluator_run"))
      )

      res match {
        case Right(d) => assertEquals(d.render(80), "")
        case Left(e) => fail(e.toString)
      }
    }
  }
}