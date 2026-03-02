package dev.bosatsu

import cats.data.Validated
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.scalacheck.Gen
import org.typelevel.paiges.Document

import scala.annotation.tailrec
import scala.io.{Codec, Source}
import scala.jdk.CollectionConverters._

object TreeSitterDifferential {

  private sealed trait BuiltInParse derives CanEqual
  private object BuiltInParse {
    case object Success extends BuiltInParse
    final case class Failure(offset: Int) extends BuiltInParse
  }

  private final case class ParseResult(
      exitCode: Int,
      output: String,
      firstErrorOffset: Option[Int]
  ) {
    def hasErrorOrMissing: Boolean =
      (exitCode != 0) || output.contains("ERROR") || output.contains("MISSING")
  }

  private val treeSitterPointRegex =
    raw"\[(\d+),\s*(\d+)\]".r

  private def builtInParse(source: String): BuiltInParse =
    Parser.parse(Package.parser(None), source) match {
      case Validated.Valid(_) =>
        BuiltInParse.Success
      case Validated.Invalid(errs) =>
        errs.head match {
          case Parser.Error.ParseFailure(position, _, _) =>
            BuiltInParse.Failure(position)
        }
    }

  private def lineColToOffset(source: String, row: Int, col: Int): Int = {
    val len = source.length
    var idx = 0
    var currentRow = 0

    while ((currentRow < row) && (idx < len)) {
      val next = source.indexOf('\n', idx)
      if (next < 0) {
        idx = len
        currentRow = row
      } else {
        idx = next + 1
        currentRow = currentRow + 1
      }
    }

    (idx + col).min(len)
  }

  private def firstTreeSitterErrorOffset(
      source: String,
      output: String
  ): Option[Int] = {
    val fromTaggedLine =
      output.linesIterator
        .filter(line => line.contains("ERROR") || line.contains("MISSING"))
        .flatMap { line =>
          treeSitterPointRegex.findFirstMatchIn(line).iterator.map { m =>
            val row = m.group(1).nn.toInt
            val col = m.group(2).nn.toInt
            lineColToOffset(source, row, col)
          }
        }
        .take(1)
        .toList
        .headOption

    fromTaggedLine.orElse {
      if (output.contains("ERROR") || output.contains("MISSING")) {
        treeSitterPointRegex.findFirstMatchIn(output).map { m =>
          val row = m.group(1).nn.toInt
          val col = m.group(2).nn.toInt
          lineColToOffset(source, row, col)
        }
      } else None
    }
  }

  private def treeSitterParse(
      source: String,
      treeSitterBin: String,
      repoRoot: Path
  ): ParseResult = {
    val tmpFile =
      Files.createTempFile("bosatsu-tree-sitter-diff-", ".bosatsu")
    try {
      Files.write(tmpFile, source.getBytes(StandardCharsets.UTF_8))
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
      val firstErrorOffset = firstTreeSitterErrorOffset(source, output)
      ParseResult(exitCode, output, firstErrorOffset)
    } finally {
      Files.deleteIfExists(tmpFile): Unit
    }
  }

  private def loadFixtureSources(
      repoRoot: Path
  ): List[(String, String)] = {
    val root = repoRoot.resolve("test_workspace")
    val walk = Files.walk(root)
    try {
      walk.iterator().asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".bosatsu"))
        .toList
        .sortBy(_.toString)
        .map { path =>
          val rel = repoRoot.relativize(path).toString.replace('\\', '/')
          val source = Files.readString(path, StandardCharsets.UTF_8)
          (rel, source)
        }
    } finally {
      walk.close()
    }
  }

  private def generatedSources(sampleCount: Int): List[(String, String)] = {
    val generatedPackage: Gen[(Int, Int, String)] =
      for {
        width <- Gen.choose(60, 200)
        statementCount <- Gen.choose(12, 40)
        statements <- Gen.listOfN(statementCount, Generators.genStatement(depth = 0))
      } yield {
        val parsedPack =
          Package.fromStatements(PackageName.parts("Diff", "Generated"), statements)
        val source = Document[Package.Parsed].document(parsedPack).render(width)
        (width, statementCount, source)
      }

    @tailrec
    def loop(
        idx: Int,
        accepted: List[(String, String)]
    ): List[(String, String)] =
      if (accepted.size >= sampleCount || idx > (sampleCount * 60)) {
        accepted.reverse
      } else {
        generatedPackage.sample match {
          case Some((width, statementCount, source)) =>
            val label =
              s"generated-$idx-width-$width-statements-$statementCount"
            loop(idx + 1, (label, source) :: accepted)
          case None =>
            loop(idx + 1, accepted)
        }
      }

    loop(1, Nil)
  }

  private def malformedFromRandomCuts(
      validSources: List[(String, String)],
      sampleCount: Int
  ): List[(String, String)] = {
    val cuttable = validSources.filter { case (_, source) => source.length > 1 }
    if (cuttable.isEmpty) Nil
    else {
      val randomCut: Gen[(String, String)] =
        Gen.oneOf(cuttable).flatMap { case (name, source) =>
          val len = source.length
          val minStart =
            if (len > 400) len / 4
            else if (len > 120) 40
            else 0
          for {
            start <- Gen.choose(minStart, len - 1)
            maxRemove = ((len - start) min 80) max 1
            remove <- Gen.choose(1, maxRemove)
            mutated = source.take(start) + source.drop(start + remove)
          } yield (s"negative-cut-$name-$start-$remove", mutated)
        }

      @tailrec
      def loop(
          idx: Int,
          accepted: List[(String, String)]
      ): List[(String, String)] =
        if (accepted.size >= sampleCount || idx > (sampleCount * 80)) {
          accepted.reverse
        } else {
          randomCut.sample match {
            case Some((name, source)) =>
              loop(idx + 1, (s"$name-$idx", source) :: accepted)
            case None =>
              loop(idx + 1, accepted)
          }
        }

      loop(1, Nil)
    }
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

  private def failuresNear(
      source: String,
      builtInOffset: Int,
      treeSitterOffset: Int
  ): Boolean = {
    val offsetDiff = math.abs(builtInOffset - treeSitterOffset)
    val lm = LocationMap(source)
    val rowDiff =
      (for {
        (builtInRow, _) <- lm.toLineCol(builtInOffset)
        (treeSitterRow, _) <- lm.toLineCol(treeSitterOffset)
      } yield math.abs(builtInRow - treeSitterRow)).getOrElse(Int.MaxValue)

    (offsetDiff <= 800) || (rowDiff <= 20)
  }

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
    val generatedCandidates = generatedSources(sampleCount = 1200)

    if (fixtureSources.isEmpty) {
      sys.error("No .bosatsu files found under test_workspace.")
    }

    if (generatedCandidates.size < 1200) {
      sys.error(
        s"Failed to generate required random sources: expected 1200, got ${generatedCandidates.size}."
      )
    }

    val fixtureEvaluations =
      fixtureSources.map { case (name, source) =>
        val builtIn = builtInParse(source)
        val parsed = treeSitterParse(source, treeSitterBin, repoRoot)
        (name, source, builtIn, parsed)
      }

    val fixtureBuiltInValid =
      fixtureEvaluations.flatMap {
        case (name, source, BuiltInParse.Success, _) =>
          Some((name, source))
        case _ =>
          None
      }

    val fixtureFailures =
      fixtureEvaluations.collect {
        case (name, _, BuiltInParse.Success, parsed) =>
          if (parsed.hasErrorOrMissing) Some(name -> parsed.output)
          else None
      }.flatten

    if (fixtureFailures.nonEmpty) {
      failFrom(
        "Fixture differential failure: tree-sitter produced ERROR/MISSING for built-in-valid fixture input.",
        fixtureFailures
      )
    }

    val generatedEvaluations =
      generatedCandidates.flatMap {
        case (name, source) =>
          builtInParse(source) match {
            case BuiltInParse.Success =>
              Some((name, source, treeSitterParse(source, treeSitterBin, repoRoot)))
            case BuiltInParse.Failure(_) =>
              None
          }
      }

    val generatedBuiltInValidAndTreeSitterValid =
      generatedEvaluations.collect {
        case (name, source, parsed) if !parsed.hasErrorOrMissing =>
          (name, source)
      }

    val generatedPositiveFailures =
      generatedEvaluations.collect {
        case (name, _, parsed) if parsed.hasErrorOrMissing =>
          (name, parsed.output)
      }

    if (generatedBuiltInValidAndTreeSitterValid.size < 1000) {
      failFrom(
        s"Unable to collect 1000 built-in-valid generated sources that tree-sitter parses cleanly (got ${generatedBuiltInValidAndTreeSitterValid.size} from ${generatedEvaluations.size} built-in-valid candidates).",
        generatedPositiveFailures
      )
    }

    val generatedSourcesList = generatedBuiltInValidAndTreeSitterValid.take(1000)

    val randomMalformed =
      malformedFromRandomCuts(
        fixtureBuiltInValid ++ generatedSourcesList,
        sampleCount = 300
      )
    val malformed = malformedInputs ++ randomMalformed

    val malformedRejectedByBuiltIn = malformed.collect {
      case (name, source) =>
        builtInParse(source) match {
          case BuiltInParse.Success =>
            None
          case BuiltInParse.Failure(offset) =>
            Some((name, source, offset))
        }
    }
    .flatten

    if (malformedRejectedByBuiltIn.isEmpty) {
      sys.error(
        "No malformed inputs were rejected by the built-in parser; differential negative sanity was not exercised."
      )
    }

    val malformedEvaluations =
      malformedRejectedByBuiltIn.map { case (name, source, builtInOffset) =>
        val parsed = treeSitterParse(source, treeSitterBin, repoRoot)
        (name, source, builtInOffset, parsed)
      }

    val sharedFailures =
      malformedEvaluations.collect {
        case (name, source, builtInOffset, parsed) if parsed.hasErrorOrMissing =>
          (name, source, builtInOffset, parsed)
      }

    if (sharedFailures.isEmpty) {
      sys.error(
        "No malformed random-cut inputs produced shared built-in/tree-sitter syntax failures; differential negative location checks were not exercised."
      )
    }

    val negativeLocationFailures =
      sharedFailures.collect {
        case (name, source, builtInOffset, parsed) =>
          parsed.firstErrorOffset match {
            case Some(treeSitterOffset) =>
              if (failuresNear(source, builtInOffset, treeSitterOffset)) None
              else {
                Some(
                  name ->
                    s"built-in failed at $builtInOffset, tree-sitter first ERROR/MISSING at $treeSitterOffset"
                )
              }
            case None =>
              Some(
                name ->
                  s"tree-sitter reported syntax failure but no ERROR/MISSING location was found in output: ${parsed.output.take(240).replace('\n', ' ')}"
              )
          }
      }.flatten

    val maxAllowedNearMismatches =
      ((sharedFailures.size / 10) max 5)

    if (negativeLocationFailures.size > maxAllowedNearMismatches) {
      failFrom(
        s"Tree-sitter and built-in parser failed far apart on malformed input (near-mismatch ${negativeLocationFailures.size} > allowed $maxAllowedNearMismatches).",
        negativeLocationFailures
      )
    }

    println(
      s"tree-sitter differential checks passed (fixtures=${fixtureSources.size}, generatedCandidates=${generatedCandidates.size}, generated=${generatedSourcesList.size}, malformed=${malformedRejectedByBuiltIn.size}, sharedMalformedFailures=${sharedFailures.size})"
    )
  }
}
