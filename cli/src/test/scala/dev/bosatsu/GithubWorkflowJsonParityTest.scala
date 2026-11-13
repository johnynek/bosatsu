package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.tool.Output
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import java.nio.file.{Path, Paths}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

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
              IO.pure(out)
            case Left(err) =>
              PathModule.reportException(err) *> IO.raiseError(err)
          }
          .unsafeRunSync()
    }

  override def beforeAll(): Unit = {
    run("fetch", "--name", "core_alpha"): Unit
    ()
  }

  workflowCases.foreach { case (workflowFile, mainValue) =>
    test(s"json write matches .github/workflows/$workflowFile") {
      val out = run(
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

      assertEquals(
        generatedJsonNode,
        yamlJsonNode,
        clues(
          s"workflow: $workflowFile, main: $mainValue",
          s"generated: ${generatedJsonNode.toPrettyString}",
          s"yaml: ${yamlJsonNode.toPrettyString}"
        )
      )
    }
  }

  test(
    "ci workflow uses the current top-level fetch command for protobuf validation"
  ) {
    val out = run(
      "json",
      "write",
      "--name",
      "core_alpha",
      "--main",
      "Bosatsu/Example/Json/Github/Workflows/Ci::workflow"
    )

    val rendered = out match {
      case Output.JsonOutput(json, _) =>
        json.render
      case other =>
        fail(s"expected JSON output, got: $other")
    }

    assert(
      rendered.contains("./bosatsuj fetch --repo_root . --name core_alpha")
    )
    assert(
      !rendered.contains("./bosatsuj lib fetch --repo_root . --name core_alpha")
    )
  }

  test("ci workflow constrains native-image threads in the graal build step") {
    val out = run(
      "json",
      "write",
      "--name",
      "core_alpha",
      "--main",
      "Bosatsu/Example/Json/Github/Workflows/Ci::workflow"
    )

    val generatedJsonNode = out match {
      case Output.JsonOutput(json, _) =>
        jsonMapper.readTree(json.render)
      case other =>
        fail(s"expected JSON output, got: $other")
    }

    val buildNativeImageStep =
      generatedJsonNode
        .path("jobs")
        .path("buildWithGraal")
        .path("steps")
        .elements()
        .asScala
        .find(_.path("name").asText() == "build native image")
        .getOrElse(fail("missing build native image step"))

    assertEquals(
      buildNativeImageStep
        .path("env")
        .path("BOSATSU_NATIVE_IMAGE_THREADS")
        .asText(),
      "2"
    )
  }
}
