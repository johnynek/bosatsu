package dev.bosatsu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.scalacheck.Gen
import org.typelevel.paiges.Document

import scala.annotation.tailrec
import scala.io.{Codec, Source}

object TreeSitterDifferential {

  private final case class ParseResult(exitCode: Int, output: String) {
    def hasErrorOrMissing: Boolean =
      (exitCode != 0) || output.contains("ERROR") || output.contains("MISSING")
  }

  private val fixtureFiles: List[String] =
    List(
      "test_workspace/bo_test.bosatsu",
      "test_workspace/Option.bosatsu",
      "test_workspace/Foo.bosatsu"
    )

  private def builtInParses(source: String): Boolean =
    Parser.parse(Package.parser(None), source).isValid

  private def treeSitterParse(
      source: String,
      treeSitterBin: String,
      repoRoot: Path
  ): ParseResult = {
    val tmpFile =
      Files.createTempFile("bosatsu-tree-sitter-diff-", ".bosatsu")
    try {
      Files.writeString(tmpFile, source, StandardCharsets.UTF_8)
      val process =
        new ProcessBuilder(
          treeSitterBin,
          "parse",
          "--scope",
          "source.bosatsu",
          tmpFile.toString
        )
          .directory(repoRoot.toFile)
          .redirectErrorStream(true)
          .start()

      val output = {
        val src =
          Source
            .fromInputStream(process.getInputStream)(using Codec.UTF8)
        try src.mkString
        finally src.close()
      }
      val exitCode = process.waitFor()
      ParseResult(exitCode, output)
    } finally {
      Files.deleteIfExists(tmpFile): Unit
    }
  }

  private def loadFixtureSources(
      repoRoot: Path
  ): List[(String, String)] =
    fixtureFiles.map { rel =>
      val path = repoRoot.resolve(rel)
      val source = Files.readString(path, StandardCharsets.UTF_8)
      (rel, source)
    }

  private def generatedSources(sampleCount: Int): List[(String, String)] = {
    val generatedPackage: Gen[String] =
      Generators.genStatements(0, 2).map { stmts =>
        val parsedPack =
          Package.fromStatements(PackageName.parts("Diff", "Generated"), stmts)
        Document[Package.Parsed].document(parsedPack).render(80)
      }

    @tailrec
    def loop(
        idx: Int,
        accepted: List[(String, String)]
    ): List[(String, String)] =
      if (accepted.size >= sampleCount || idx > (sampleCount * 20)) {
        accepted.reverse
      } else {
        generatedPackage.sample match {
          case Some(src) =>
            loop(idx + 1, (s"generated-$idx", src) :: accepted)
          case None =>
            loop(idx + 1, accepted)
        }
      }

    loop(1, Nil)
  }

  private def malformedInputs: List[(String, String)] =
    List(
      (
        "negative-missing-list-close",
        "package Broken\nx = [1, 2\n"
      ),
      (
        "negative-missing-tuple-close",
        "package Broken\nx = (1, 2\n"
      ),
      (
        "negative-missing-record-close",
        "package Broken\nx = {a: 1\n"
      ),
      (
        "negative-nested-missing-close",
        "package Broken\nx = [Assertion(True, \"ok\")\n"
      )
    )

  private def failFrom(label: String, failures: List[(String, String)]): Unit = {
    val rendered =
      failures
        .take(5)
        .map { case (name, detail) =>
          s"- $name\n  ${detail.take(240).replace('\n', ' ')}"
        }
        .mkString("\n")

    val extra =
      if (failures.size > 5) s"\n...and ${failures.size - 5} more"
      else ""

    sys.error(s"$label\n$rendered$extra")
  }

  def main(args: Array[String]): Unit = {
    val repoRoot =
      Paths
        .get(sys.env.getOrElse("BOSATSU_REPO_ROOT", "."))
        .toAbsolutePath
        .normalize()

    val treeSitterBin = sys.env.getOrElse("TREE_SITTER_BIN", "tree-sitter")

    val fixtureSources = loadFixtureSources(repoRoot)
    val randomSources = generatedSources(sampleCount = 20)
    val positiveSources = fixtureSources ++ randomSources

    val positiveFailures =
      positiveSources.collect {
        case (name, source) if builtInParses(source) =>
          val parsed = treeSitterParse(source, treeSitterBin, repoRoot)
          if (parsed.hasErrorOrMissing)
            Some(name -> parsed.output)
          else None
      }.flatten

    if (positiveFailures.nonEmpty) {
      failFrom(
        "Tree-sitter produced ERROR/MISSING for built-in-valid input.",
        positiveFailures
      )
    }

    val malformed = malformedInputs

    val malformedRejectedByBuiltIn = malformed.filterNot { case (_, source) =>
      builtInParses(source)
    }

    if (malformedRejectedByBuiltIn.isEmpty) {
      sys.error(
        "No malformed inputs were rejected by the built-in parser; differential negative sanity was not exercised."
      )
    }

    val negativeFailures =
      malformedRejectedByBuiltIn.collect {
        case (name, source) =>
          val parsed = treeSitterParse(source, treeSitterBin, repoRoot)
          if (!parsed.hasErrorOrMissing)
            Some(name -> parsed.output)
          else None
      }.flatten

    if (negativeFailures.nonEmpty) {
      failFrom(
        "Tree-sitter accepted malformed input that built-in parser rejected.",
        negativeFailures
      )
    }

    println(
      s"tree-sitter differential checks passed (fixtures=${fixtureSources.size}, generated=${randomSources.size}, malformed=${malformedRejectedByBuiltIn.size})"
    )
  }
}
