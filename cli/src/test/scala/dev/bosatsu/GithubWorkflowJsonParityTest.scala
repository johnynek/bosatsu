package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.tool.Output
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._
import scala.concurrent.duration._

// allow us to unsafeRunSync
import cats.effect.unsafe.implicits.global

class GithubWorkflowJsonParityTest extends munit.FunSuite {
  override def munitTimeout: Duration = 6.minutes

  private val yamlMapper = new ObjectMapper(new YAMLFactory())
  private val jsonMapper = new ObjectMapper()

  private val workflowCases: List[(String, String)] =
    List(
      ("ci.yml", "Bosatsu/Example/Json/Github/Workflows/Ci::workflow"),
      (
        "codecov_main.yml",
        "Bosatsu/Example/Json/Github/Workflows/CodecovMain::workflow"
      ),
      (
        "deploy_web.yml",
        "Bosatsu/Example/Json/Github/Workflows/DeployWeb::workflow"
      ),
      ("release.yml", "Bosatsu/Example/Json/Github/Workflows/Release::workflow")
    )

  private def run(args: String*): Output[Path] =
    PathModule.run(args.toList) match {
      case Left(h)   => fail(s"got help: $h on command: ${args.toList}")
      case Right(io) =>
        io.attempt
          .flatMap {
            case Right(out) =>
              PathModule.reportOutput(out).as(out)
            case Left(err) =>
              PathModule.reportException(err) *> IO.raiseError(err)
          }
          .unsafeRunSync()
    }

  override def beforeAll(): Unit = {
    run("lib", "fetch", "--name", "core_alpha")
    ()
  }

  // For this exercise we treat absent object fields and explicit null fields
  // as equivalent, which aligns with Option-backed JSON generation.
  private def equivalentJson(lhs: com.fasterxml.jackson.databind.JsonNode, rhs: com.fasterxml.jackson.databind.JsonNode): Boolean =
    if ((lhs eq null) || (rhs eq null)) (lhs eq rhs)
    else if (lhs.isObject && rhs.isObject) {
      val keys = lhs.fieldNames.asScala.toSet union rhs.fieldNames.asScala.toSet
      keys.forall { k =>
        val lv = lhs.get(k)
        val rv = rhs.get(k)
        if ((lv eq null) && (rv eq null)) true
        else if (lv eq null) rv.isNull
        else if (rv eq null) lv.isNull
        else equivalentJson(lv, rv)
      }
    } else if (lhs.isArray && rhs.isArray) {
      lhs.size == rhs.size &&
        (0 until lhs.size).forall(idx => equivalentJson(lhs.get(idx), rhs.get(idx)))
    } else lhs.equals(rhs)

  workflowCases.foreach { case (workflowFile, mainValue) =>
    test(s"lib json write matches .github/workflows/$workflowFile") {
      val out = run(
        "lib",
        "json",
        "write",
        "--name",
        "core_alpha",
        "--main",
        mainValue
      )

      val generatedJsonNode = out match {
        case Output.JsonOutput(json, _) =>
          jsonMapper.readTree(json.render)
        case other =>
          fail(s"expected JSON output, got: $other")
      }

      val workflowPath = Paths.get(".github", "workflows", workflowFile)
      val yamlJsonNode = yamlMapper.readTree(workflowPath.toFile)

      assert(
        equivalentJson(generatedJsonNode, yamlJsonNode),
        clues(
          s"workflow: $workflowFile, main: $mainValue",
          s"generated: ${generatedJsonNode.toPrettyString}",
          s"yaml: ${yamlJsonNode.toPrettyString}"
        )
      )
    }
  }
}
