package dev.bosatsu.codegen.clang

import dev.bosatsu.{PackageName, PackageMap, Par, TestUtils, Identifier}

class ClangGenTest extends munit.FunSuite {
  def md5HashToHex(content: String): String = {
    val md = java.security.MessageDigest.getInstance("MD5")
    val digest = md.digest(content.getBytes("UTF-8"))
    digest.map("%02x".format(_)).mkString
  }
  private val syntheticIdReplacements =
    List(
      raw"__bsts_a_\d+" -> "__bsts_a_N",
      raw"__bsts_l_[A-Za-z_]+\d+" -> "__bsts_l_IDN",
      raw"__bsts_t_[A-Za-z_]+\d+" -> "__bsts_t_IDN",
      raw"__bsts_b_[A-Za-z_]+\d+" -> "__bsts_b_IDN",
      raw"__bstsi_slot\[\d+\]" -> "__bstsi_slot[I]"
    )

  def normalizeGeneratedC(fragment: String): String =
    syntheticIdReplacements.foldLeft(fragment.replace("\r\n", "\n")) {
      case (acc, (pat, repl)) => acc.replaceAll(pat, repl)
    }

  def blockStartingWith(rendered: String, prefix: String): String =
    rendered
      .strip
      .split("\n\n")
      .find(_.startsWith(prefix))
      .getOrElse(fail(s"missing expected code block starting with: $prefix"))

  def testFilesCompilesToHash(
      path0: String,
      blockPrefixes: List[String],
      paths: String*
  )(
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
        val normalized = normalizeGeneratedC(
          blockPrefixes.iterator
            .map(blockStartingWith(everything, _))
            .mkString("", "\n\n", "\n")
        )
        val hashed = md5HashToHex(normalized)
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
    testFilesCompilesToHash(
      "test_workspace/Ackermann.bosatsu",
      List(
        "BValue ___bsts_g_Ackermann_l_ack(",
        "int main("
      )
    )(
      "64d9073ab16e7936fde71bf62db5e124"
    )
  }
}
