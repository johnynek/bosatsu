package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import org.bykn.bosatsu.{PackageName, PackageMap, TestUtils, Identifier, Predef}
import Identifier.Name
import org.bykn.bosatsu.MatchlessFromTypedExpr

import org.bykn.bosatsu.DirectEC.directEC

class ClangGenTest extends munit.FunSuite {
  val predef_c = Code.Include(true, "bosatsu_predef.h")

  def predef(s: String, arity: Int) =
    (PackageName.PredefName -> Name(s)) -> (predef_c,
      ClangGen.generatedName(PackageName.PredefName, Name(s)),
      arity)

  val jvmExternals = {
    val ext = Predef.jvmExternals.toMap.iterator.map { case ((_, n), ffi) => predef(n, ffi.arity) }
      .toMap[(PackageName, Identifier), (Code.Include, Code.Ident, Int)]
    
    { (pn: (PackageName, Identifier)) => ext.get(pn) }
  }

  def md5HashToHex(content: String): String = {
    val md = java.security.MessageDigest.getInstance("MD5")
    val digest = md.digest(content.getBytes("UTF-8"))
    digest.map("%02x".format(_)).mkString
  }
  def testFilesCompilesToHash(path0: String, paths: String*)(hashHex: String)(implicit loc: munit.Location) = {
    val pm: PackageMap.Typed[Any] = TestUtils.compileFile(path0, paths*)
    /*
    val exCode = ClangGen.generateExternalsStub(pm)
    println(exCode.render(80))
    sys.error("stop")
    */
    val matchlessMap = MatchlessFromTypedExpr.compile(pm)
    val topoSort = pm.topoSort.toSuccess.get
    val sortedEnv = cats.Functor[Vector].compose[NonEmptyList].map(topoSort) { pn =>
      (pn, matchlessMap(pn))  
    }

    val res = ClangGen.renderMain(
      sortedEnv = sortedEnv,
      externals = jvmExternals,
      value = (PackageName.PredefName, Identifier.Name("ignored")),
      evaluator = (Code.Include(true, "eval.h"), Code.Ident("evaluator_run"))
    )

    res match {
      case Right(d) =>
        val everything = d.render(80)
        val hashed = md5HashToHex(everything)
        assertEquals(hashed, hashHex, s"compilation didn't match. Compiled code:\n\n${"//" * 40}\n\n$everything")
      case Left(e) => fail(e.toString)
    }
  }

  test("test_workspace/Ackermann.bosatsu") {
    /* 
      To inspect the code, change the hash, and it will print the code out
     */
    testFilesCompilesToHash("test_workspace/Ackermann.bosatsu")(
      "46716ef3c97cf2a79bf17d4033d55854"
    )
  }
}