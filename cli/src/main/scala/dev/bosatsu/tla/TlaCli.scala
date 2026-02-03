package dev.bosatsu.tla

import cats.effect._
import cats.syntax.all._
import com.monovore.decline._
import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets
import scala.sys.process._
import scala.util.Try
import dev.bosatsu.service.{ServiceBuilder, ServiceAnalysis, BatchConfig}
import dev.bosatsu.Par

/**
 * CLI for TLA+ operations.
 *
 * Commands:
 *   generate  - Generate TLA+ specification from a Bosatsu file
 *   check     - Run TLC model checker on a specification
 *   race      - Analyze for race conditions
 */
object TlaCli {
  import TlaJson.given

  /**
   * CLI action trait.
   */
  sealed trait TlaAction {
    def run: IO[ExitCode]
  }

  /**
   * Generate TLA+ specification.
   */
  case class Generate(
    file: Path,
    output: Option[Path],
    instances: Int,
    invariant: Option[String]
  ) extends TlaAction {
    def run: IO[ExitCode] = IO {
      Par.withEC {

      val source = Files.readString(file)
      val sourceFile = file.getFileName.toString

      ServiceBuilder.compileHandlers(source, sourceFile) match {
        case Left(err) =>
          System.err.println(s"Compilation error: $err")
          ExitCode.Error

        case Right(handlers) if handlers.isEmpty =>
          System.err.println("No handlers found")
          ExitCode.Error

        case Right(handlers) =>
          // Use the first handler's analysis
          val handler = handlers.head
          val options = TlaOptions(
            invariant = invariant,
            checkDeadlock = true
          )

          val spec = TlaGen.generate(handler.analysis, options, instances)
          val tlaCode = spec.render

          output match {
            case Some(outPath) =>
              Files.writeString(outPath, tlaCode, StandardCharsets.UTF_8)
              println(s"TLA+ specification written to: $outPath")
            case None =>
              println(tlaCode)
          }

          ExitCode.Success
      }
      }
    }
  }

  /**
   * Run TLC model checker.
   */
  case class Check(
    file: Path,
    workers: Int,
    depth: Option[Int],
    timeout: Option[Int]
  ) extends TlaAction {
    def run: IO[ExitCode] = IO {
      val options = TlcOptions(
        workers = workers,
        checkDeadlock = true,
        depth = depth,
        timeout = timeout
      )

      TlcRunner.run(file, options) match {
        case result if result.skipped =>
          println(s"TLC skipped: ${result.skipReason.getOrElse("unknown reason")}")
          ExitCode.Success

        case result if result.success =>
          println("Model checking completed successfully!")
          result.statesGenerated.foreach(n => println(s"States generated: $n"))
          result.distinctStates.foreach(n => println(s"Distinct states: $n"))
          ExitCode.Success

        case result =>
          System.err.println("Model checking failed!")
          result.errorMessage.foreach(msg => System.err.println(s"Error: $msg"))

          if (result.invariantViolation) {
            System.err.println("Invariant violation detected!")
            println("\nError trace:")
            result.errorTrace.foreach { state =>
              println(s"  State ${state.stateNumber}: ${state.action}")
              println(s"    ${state.variables}")
            }
          }

          if (result.deadlock) {
            System.err.println("Deadlock detected!")
          }

          ExitCode.Error
      }
    }
  }

  /**
   * Analyze for race conditions.
   */
  case class Race(
    file: Path,
    instances: Int,
    invariant: Option[String]
  ) extends TlaAction {
    def run: IO[ExitCode] = IO {
      Par.withEC {

      val source = Files.readString(file)
      val sourceFile = file.getFileName.toString

      ServiceBuilder.compileHandlers(source, sourceFile) match {
        case Left(err) =>
          System.err.println(s"Compilation error: $err")
          ExitCode.Error

        case Right(handlers) if handlers.isEmpty =>
          System.err.println("No handlers found")
          ExitCode.Error

        case Right(handlers) =>
          val handler = handlers.head
          val inv = invariant.getOrElse("TRUE")

          println(s"Analyzing handler '${handler.name}' for race conditions...")
          println(s"  Instances: $instances")
          println(s"  Invariant: $inv")

          // Generate race detection spec
          val spec = TlaGen.generateRaceSpec(handler.analysis, instances, inv)

          // Write to temp file and run TLC
          val tempFile = Files.createTempFile("bosatsu_race_", ".tla")
          Files.writeString(tempFile, spec.render, StandardCharsets.UTF_8)

          val options = TlcOptions(
            workers = 1,
            checkDeadlock = true,
            depth = Some(100)
          )

          val result = TlcRunner.run(tempFile, options)

          // Clean up
          Files.deleteIfExists(tempFile)

          if (result.skipped) {
            println(s"TLC not available: ${result.skipReason.getOrElse("unknown")}")
            println("Install TLC to enable race condition analysis.")
            ExitCode.Success
          } else if (result.invariantViolation) {
            println("\n⚠️  Race condition detected!")
            println("\nViolating trace:")
            result.errorTrace.foreach { state =>
              println(s"  ${state.stateNumber}. ${state.action}")
            }
            ExitCode.Error
          } else if (result.success) {
            println("\n✓ No race conditions found")
            result.distinctStates.foreach(n => println(s"  Checked $n distinct states"))
            ExitCode.Success
          } else {
            System.err.println(s"Analysis failed: ${result.errorMessage.getOrElse("unknown error")}")
            ExitCode.Error
          }
      }
      }
    }
  }

  // CLI argument parsing

  private val fileArg = Opts.argument[Path]("file")

  private val outputOpt = Opts.option[Path](
    "output", "Output file", "o"
  ).orNone

  private val instancesOpt = Opts.option[Int](
    "instances", "Number of concurrent instances", "n"
  ).withDefault(1)

  private val workersOpt = Opts.option[Int](
    "workers", "Number of TLC workers", "w"
  ).withDefault(1)

  private val depthOpt = Opts.option[Int](
    "depth", "Maximum search depth"
  ).orNone

  private val timeoutOpt = Opts.option[Int](
    "timeout", "Timeout in milliseconds"
  ).orNone

  private val invariantOpt = Opts.option[String](
    "invariant", "TLA+ invariant to check", "i"
  ).orNone

  val generateCmd: Opts[TlaAction] = Opts.subcommand(
    "generate", "Generate TLA+ specification"
  ) {
    (fileArg, outputOpt, instancesOpt, invariantOpt).mapN(Generate.apply)
  }

  val checkCmd: Opts[TlaAction] = Opts.subcommand(
    "check", "Run TLC model checker"
  ) {
    (fileArg, workersOpt, depthOpt, timeoutOpt).mapN(Check.apply)
  }

  val raceCmd: Opts[TlaAction] = Opts.subcommand(
    "race", "Analyze for race conditions"
  ) {
    (fileArg, instancesOpt, invariantOpt).mapN(Race.apply)
  }

  val command: Command[TlaAction] = Command(
    "tla", "TLA+ formal verification tools"
  ) {
    generateCmd orElse checkCmd orElse raceCmd
  }

  /**
   * Parse and run CLI command.
   */
  def parse(args: List[String]): Either[Help, TlaAction] =
    command.parse(args)
}

/**
 * TLC model checker runner.
 */
object TlcRunner {

  /**
   * Find TLC on the system.
   */
  def findTlc(): Option[String] = {
    // Try common locations
    val candidates = List(
      "tlc2.TLC",  // Java classpath
      "/usr/local/bin/tlc",
      "/opt/homebrew/bin/tlc",
      System.getProperty("user.home") + "/bin/tlc"
    )

    // Check if TLC is available via java
    val javaCheck = Try {
      val result = "java -cp tla2tools.jar tlc2.TLC -h".!!
      "java -cp tla2tools.jar tlc2.TLC"
    }.toOption

    javaCheck.orElse {
      candidates.find { cmd =>
        Try(Paths.get(cmd).toFile.exists()).getOrElse(false)
      }
    }
  }

  /**
   * Run TLC on a specification file.
   */
  def run(specFile: Path, options: TlcOptions): TlcResult = {
    findTlc() match {
      case None =>
        TlcResult(
          success = false,
          skipped = true,
          skipReason = Some("TLC not found. Install TLA+ Toolbox or tla2tools.jar.")
        )

      case Some(tlcCmd) =>
        runTlc(tlcCmd, specFile, options)
    }
  }

  private def runTlc(tlcCmd: String, specFile: Path, options: TlcOptions): TlcResult = {
    val args = List.newBuilder[String]
    args ++= tlcCmd.split(" ")

    if (!options.checkDeadlock) {
      args += "-deadlock"
    }

    options.depth.foreach { d =>
      args += "-depth"
      args += d.toString
    }

    args += "-workers"
    args += options.workers.toString

    args += specFile.toAbsolutePath.toString

    Try {
      val process = args.result().mkString(" ")
      val output = new StringBuilder
      val errors = new StringBuilder

      val exitCode = Process(process).!(ProcessLogger(
        line => output.append(line).append("\n"),
        line => errors.append(line).append("\n")
      ))

      parseOutput(exitCode, output.toString, errors.toString)
    }.getOrElse {
      TlcResult(
        success = false,
        errorMessage = Some("Failed to run TLC"),
        skipped = true,
        skipReason = Some("TLC execution failed")
      )
    }
  }

  private def parseOutput(exitCode: Int, stdout: String, stderr: String): TlcResult = {
    val output = stdout + "\n" + stderr

    // Parse states generated
    val statesGenerated = """(\d+) states generated""".r
      .findFirstMatchIn(output)
      .map(_.group(1).toInt)

    val distinctStates = """(\d+) distinct states""".r
      .findFirstMatchIn(output)
      .map(_.group(1).toInt)

    // Check for violations
    val invariantViolation = output.contains("Invariant") && output.contains("violated")
    val deadlock = output.contains("deadlock")
    val syntaxError = output.contains("Syntax error") || output.contains("Parse Error")

    // Parse error trace
    val errorTrace = if (invariantViolation || deadlock) {
      parseErrorTrace(output)
    } else {
      Nil
    }

    val errorMessage = if (exitCode != 0 || invariantViolation || deadlock) {
      Some(extractErrorMessage(output))
    } else {
      None
    }

    TlcResult(
      success = exitCode == 0 && !invariantViolation && !deadlock && !syntaxError,
      invariantViolation = invariantViolation,
      deadlock = deadlock,
      syntaxError = syntaxError,
      errorMessage = errorMessage,
      statesGenerated = statesGenerated,
      distinctStates = distinctStates,
      errorTrace = errorTrace,
      rawOutput = Some(output)
    )
  }

  private def parseErrorTrace(output: String): List[TlcTraceState] = {
    val statePattern = """State (\d+):(.+?)(?=State \d+:|$)""".r
    statePattern.findAllMatchIn(output).map { m =>
      TlcTraceState(
        stateNumber = m.group(1).toInt,
        action = "unknown",
        variables = m.group(2).trim
      )
    }.toList
  }

  private def extractErrorMessage(output: String): String = {
    // Try to find the most relevant error message
    val lines = output.split("\n")
    lines.find(_.contains("Error:"))
      .orElse(lines.find(_.contains("violated")))
      .orElse(lines.find(_.contains("deadlock")))
      .getOrElse("Model checking failed")
  }
}
