package org.bykn.bosatsu.codegen.clang

import cats.data.NonEmptyList
import org.bykn.bosatsu.{PackageName, PackageMap, TestUtils, Identifier}
import org.bykn.bosatsu.MatchlessFromTypedExpr

import org.bykn.bosatsu.DirectEC.directEC

class ClangGenTest extends munit.FunSuite {
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
      externals = ClangGen.ExternalResolver.FromJvmExternals,
      value = (PackageName.PredefName, Identifier.Name("ignored"))
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
      "ec2ce766943a4c2491cea6e288000294"
    )
  }
  test("test_workspace/Char.bosatsu") {
    /* 
      To inspect the code, change the hash, and it will print the code out
     */
    testFilesCompilesToHash("test_workspace/Char.bosatsu")(
      "87743537160db572063bc98fae5e5928"
    )
  }
  test("test_workspace/ListPat.bosatsu") {
    /* 
      To inspect the code, change the hash, and it will print the code out
     */
    testFilesCompilesToHash("test_workspace/ListPat.bosatsu")(
      "b4af2ec90fb1187574149b7c7fa4a283"
    )
  }
}