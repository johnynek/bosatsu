package dev.bosatsu.codegen.clang

import dev.bosatsu.{PackageName, PackageMap, Par, TestUtils, Identifier}

class ClangGenTest extends munit.FunSuite {
  def md5HashToHex(content: String): String = {
    val md = java.security.MessageDigest.getInstance("MD5")
    val digest = md.digest(content.getBytes("UTF-8"))
    digest.map("%02x".format(_)).mkString
  }
  def testFilesCompilesToHash(path0: String, paths: String*)(
      hashHex: String
  )(implicit loc: munit.Location) = Par.noParallelism {
    val pm: PackageMap.Typed[Any] = TestUtils.compileFile(path0, paths*)
    /*
    val exCode = ClangGen.generateExternalsStub(pm)
    println(exCode.render(80))
    sys.error("stop")
     */
    val res =
      ClangGen(pm).renderMain(
        PackageName.PredefName,
        Identifier.Name("range"),
        Code.Ident("run_main")
      )

    res match {
      case Right(d) =>
        val everything = d.render(80)
        val hashed = md5HashToHex(everything)
        assertEquals(
          hashed,
          hashHex,
          s"compilation didn't match. Compiled code:\n\n${"//" * 40}\n\n$everything"
        )
      case Left(e) => fail(e.toString)
    }
  }

  test("test_workspace/Ackermann.bosatsu") {
    /*
      To inspect the code, change the hash, and it will print the code out
     */
    testFilesCompilesToHash("test_workspace/Ackermann.bosatsu")(
      "9997231aa1202035f592de16fcc07006"
    )
  }
}
