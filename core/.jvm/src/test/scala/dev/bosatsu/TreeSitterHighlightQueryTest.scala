package dev.bosatsu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class TreeSitterHighlightQueryTest extends munit.FunSuite {

  private def readHighlightQuery(): String = {
    val relativeToRepoRoot =
      Paths.get("syntax", "tree-sitter-bosatsu", "queries", "highlights.scm")
    val relativeToCoreDir =
      Paths.get("..", "syntax", "tree-sitter-bosatsu", "queries", "highlights.scm")

    val queryPath =
      if (Files.isRegularFile(relativeToRepoRoot)) relativeToRepoRoot
      else if (Files.isRegularFile(relativeToCoreDir)) relativeToCoreDir
      else fail("could not locate syntax/tree-sitter-bosatsu/queries/highlights.scm")

    Files.readString(queryPath, StandardCharsets.UTF_8)
  }

  test("highlights query marks package declaration keyword") {
    val query = readHighlightQuery()
    val expectedCapture =
      raw"""\(package_declaration\s+"package"\s+@keyword\)""".r

    assert(
      expectedCapture.findFirstIn(query).nonEmpty,
      "expected package declaration to capture \"package\" as @keyword"
    )
  }
}
