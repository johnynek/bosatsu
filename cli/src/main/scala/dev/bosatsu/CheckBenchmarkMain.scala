package dev.bosatsu

import cats.effect.{ExitCode, IO, IOApp}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.tool.ExitCode as ToolExitCode

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration.DurationLong

object CheckBenchmarkMain extends IOApp {
  private val asyncProfilerPath =
    "~/Downloads/async-profiler-4.3-macos/bin/asprof"

  private sealed trait CacheMode derives CanEqual
  private object CacheMode {
    case object Default extends CacheMode
    case object NoCache extends CacheMode
    final case class Directory(path: Path) extends CacheMode
  }

  private final case class Config(
      repoRoot: Path,
      name: Option[String] = None,
      filters: List[String] = Nil,
      cacheMode: CacheMode = CacheMode.Default,
      loop: Boolean = true,
      warmupIterations: Int = 2,
      reportEvery: Int = 1,
      startupWaitMs: Long = 0L,
      color: Colorize = Colorize.None
  )

  private final case class PartialConfig(
      repoRoot: Option[Path] = None,
      name: Option[String] = None,
      filters: List[String] = Nil,
      cacheMode: CacheMode = CacheMode.Default,
      loop: Boolean = true,
      warmupIterations: Int = 2,
      reportEvery: Int = 1,
      startupWaitMs: Long = 0L,
      color: Colorize = Colorize.None
  )

  private final case class IterationResult(
      iteration: Long,
      elapsedNanos: Long,
      exitCode: ToolExitCode
  ) {
    val seconds: Double =
      elapsedNanos.toDouble / 1000000000.0d
  }

  private final case class BlockStats(
      iterations: Int,
      elapsedNanos: Long
  ) {
    val iterationsPerSecond: Double =
      if (elapsedNanos <= 0L) Double.PositiveInfinity
      else iterations.toDouble * 1000000000.0d / elapsedNanos.toDouble

    val seconds: Double =
      elapsedNanos.toDouble / 1000000000.0d
  }

  private sealed trait ParseResult derives CanEqual
  private object ParseResult {
    case object Help extends ParseResult
    final case class Invalid(message: String) extends ParseResult
    final case class Parsed(config: Config) extends ParseResult
  }

  private val usage: String =
    s"""Usage: ${CheckBenchmarkMain.getClass.getSimpleName.stripSuffix("$")} [options]
       |
       |Options:
       |  --repo-root <path>         Repo root to run `check` against. Required.
       |  --name <lib>               Library name. Optional when the target repo has only one lib.
       |  --filter <regex>           Package filter regex. Repeatable.
       |  --cache-dir <path>         Override compiled package cache directory.
       |  --no-cache                 Disable compiled package artifact cache.
       |  --loop                     Run forever (default) so async-profiler can attach.
       |  --once                     Run one measured block and exit.
       |  --warmup <n>               Warmup iterations before measurement. Default: 2.
       |  --report-every <n>         Checks per measured block. Default: 1.
       |  --startup-wait-ms <n>      Wait before warmup/measurement. Default: 0.
       |  --color <color>            Colorize mode passed to `check`: none, ansi or html. Default: none.
       |  --help                     Show this help.
       |""".stripMargin

  private def parseExistingDirectory(
      raw: String,
      flag: String
  ): Either[String, Path] = {
    val path = Paths.get(raw).toAbsolutePath.normalize
    if (Files.isDirectory(path)) Right(path)
    else Left(s"$flag requires an existing directory, received: $raw")
  }

  private def parsePath(raw: String): Path =
    Paths.get(raw).toAbsolutePath.normalize

  private def parseNonNegativeInt(
      raw: String,
      flag: String
  ): Either[String, Int] =
    try {
      val parsed = raw.toInt
      if (parsed >= 0) Right(parsed)
      else Left(s"$flag requires a non-negative integer, received: $raw")
    } catch {
      case _: NumberFormatException =>
        Left(s"$flag requires a non-negative integer, received: $raw")
    }

  private def parsePositiveInt(
      raw: String,
      flag: String
  ): Either[String, Int] =
    try {
      val parsed = raw.toInt
      if (parsed > 0) Right(parsed)
      else Left(s"$flag requires a positive integer, received: $raw")
    } catch {
      case _: NumberFormatException =>
        Left(s"$flag requires a positive integer, received: $raw")
    }

  private def parseNonNegativeLong(
      raw: String,
      flag: String
  ): Either[String, Long] =
    try {
      val parsed = raw.toLong
      if (parsed >= 0L) Right(parsed)
      else Left(s"$flag requires a non-negative integer, received: $raw")
    } catch {
      case _: NumberFormatException =>
        Left(s"$flag requires a non-negative integer, received: $raw")
    }

  private def parseColor(
      raw: String
  ): Either[String, Colorize] =
    raw.toLowerCase match {
      case "none" => Right(Colorize.None)
      case "ansi" => Right(Colorize.Console)
      case "html" => Right(Colorize.HmtlFont)
      case other  =>
        Left(s"--color expects one of: none, ansi, html. Received: $other")
    }

  private def colorArg(color: Colorize): String = {
    val ref = color.asInstanceOf[AnyRef]
    if (ref eq Colorize.None) "none"
    else if (ref eq Colorize.Console) "ansi"
    else if (ref eq Colorize.HmtlFont) "html"
    else "unknown"
  }

  private def profileStem(config: Config): String = {
    val base =
      config.name
        .orElse(Option(config.repoRoot.getFileName).map(_.toString))
        .getOrElse("bosatsu-check")

    val sanitized = base.replaceAll("[^A-Za-z0-9._-]+", "-")
    if (sanitized.nonEmpty) sanitized else "bosatsu-check"
  }

  private def asprofOutputPath(
      config: Config,
      suffix: String,
      extension: String
  ): String =
    s"/tmp/${profileStem(config)}-check-$suffix.$extension"

  private def parseArgs(args: List[String]): ParseResult = {
    import ParseResult._

    @annotation.tailrec
    def loop(rest: List[String], partial: PartialConfig): ParseResult =
      rest match {
        case Nil =>
          partial.repoRoot match {
            case Some(repoRoot) =>
              Parsed(
                Config(
                  repoRoot = repoRoot,
                  name = partial.name,
                  filters = partial.filters.reverse,
                  cacheMode = partial.cacheMode,
                  loop = partial.loop,
                  warmupIterations = partial.warmupIterations,
                  reportEvery = partial.reportEvery,
                  startupWaitMs = partial.startupWaitMs,
                  color = partial.color
                )
              )
            case None =>
              Invalid("missing required --repo-root <path>")
          }
        case "--help" :: _ =>
          Help
        case ("--repo-root" | "--repo_root") :: value :: tail =>
          parseExistingDirectory(value, "--repo-root") match {
            case Right(path) =>
              loop(tail, partial.copy(repoRoot = Some(path)))
            case Left(err) =>
              Invalid(err)
          }
        case "--name" :: value :: tail =>
          loop(tail, partial.copy(name = Some(value)))
        case "--filter" :: value :: tail =>
          loop(tail, partial.copy(filters = value :: partial.filters))
        case ("--cache-dir" | "--cache_dir") :: value :: tail =>
          partial.cacheMode match {
            case CacheMode.Default =>
              loop(
                tail,
                partial.copy(cacheMode = CacheMode.Directory(parsePath(value)))
              )
            case CacheMode.NoCache =>
              Invalid("cannot combine --cache-dir with --no-cache")
            case CacheMode.Directory(_) =>
              Invalid("--cache-dir may only be specified once")
          }
        case ("--no-cache" | "--no_cache") :: tail =>
          partial.cacheMode match {
            case CacheMode.Default =>
              loop(tail, partial.copy(cacheMode = CacheMode.NoCache))
            case CacheMode.NoCache =>
              Invalid("--no-cache may only be specified once")
            case CacheMode.Directory(_) =>
              Invalid("cannot combine --no-cache with --cache-dir")
          }
        case "--loop" :: tail =>
          loop(tail, partial.copy(loop = true))
        case "--once" :: tail =>
          loop(tail, partial.copy(loop = false))
        case "--warmup" :: value :: tail =>
          parseNonNegativeInt(value, "--warmup") match {
            case Right(n) =>
              loop(tail, partial.copy(warmupIterations = n))
            case Left(err) =>
              Invalid(err)
          }
        case "--report-every" :: value :: tail =>
          parsePositiveInt(value, "--report-every") match {
            case Right(n) =>
              loop(tail, partial.copy(reportEvery = n))
            case Left(err) =>
              Invalid(err)
          }
        case "--startup-wait-ms" :: value :: tail =>
          parseNonNegativeLong(value, "--startup-wait-ms") match {
            case Right(n) =>
              loop(tail, partial.copy(startupWaitMs = n))
            case Left(err) =>
              Invalid(err)
          }
        case "--color" :: value :: tail =>
          parseColor(value) match {
            case Right(color) =>
              loop(tail, partial.copy(color = color))
            case Left(err) =>
              Invalid(err)
          }
        case flag :: Nil if flag.startsWith("--") =>
          Invalid(s"missing value for $flag")
        case unknown :: _ =>
          Invalid(s"unknown argument: $unknown")
      }

    loop(args, PartialConfig())
  }

  private def benchmarkArgs(config: Config): List[String] = {
    val baseArgs =
      List(
        "check",
        "--repo_root",
        config.repoRoot.toString,
        "--color",
        colorArg(config.color)
      )

    val nameArgs =
      config.name.toList.flatMap(name => List("--name", name))

    val filterArgs =
      config.filters.flatMap(filter => List("--filter", filter))

    val cacheArgs =
      config.cacheMode match {
        case CacheMode.Default        => Nil
        case CacheMode.NoCache        => List("--no_cache")
        case CacheMode.Directory(dir) => List("--cache_dir", dir.toString)
      }

    baseArgs ::: nameArgs ::: filterArgs ::: cacheArgs
  }

  private def configSummary(config: Config): List[String] = {
    val cacheSummary =
      config.cacheMode match {
        case CacheMode.Default        => "default"
        case CacheMode.NoCache        => "disabled"
        case CacheMode.Directory(dir) => dir.toString
      }

    List(
      s"repoRoot=${config.repoRoot}",
      s"name=${config.name.getOrElse("(auto)")}",
      s"filters=${if (config.filters.isEmpty) "(none)" else config.filters.mkString(",")}",
      s"cache=$cacheSummary",
      s"mode=${if (config.loop) "loop" else "once"}",
      s"warmup=${config.warmupIterations}",
      s"reportEvery=${config.reportEvery}",
      s"startupWaitMs=${config.startupWaitMs}",
      s"color=${colorArg(config.color)}"
    )
  }

  private def runCheck(config: Config): IO[ToolExitCode] =
    IO.defer {
      PathModule.runAndReport(benchmarkArgs(config)) match {
        case Right(io) => io
        case Left(help) =>
          IO.blocking {
            System.err.println(help.toString)
            ToolExitCode.Error
          }
      }
    }

  private def runMeasuredIteration(
      config: Config,
      label: String,
      iteration: Long
  ): IO[IterationResult] =
    for {
      start <- IO.monotonic
      exitCode <- runCheck(config)
      end <- IO.monotonic
      nanos = (end - start).toNanos
      result = IterationResult(iteration, nanos, exitCode)
      _ <- IO.blocking {
        println(
          f"$label iteration=${result.iteration}%d time=${result.seconds}%.3fs exit=${result.exitCode.toInt}%d"
        )
      }
    } yield result

  private def runBlock(
      config: Config,
      label: String,
      startIteration: Long,
      iterations: Int
  ): IO[Either[ToolExitCode, BlockStats]] = {
    def loop(idx: Int, totalNanos: Long): IO[Either[ToolExitCode, BlockStats]] =
      if (idx >= iterations) IO.pure(Right(BlockStats(iterations, totalNanos)))
      else {
        val iterationNo = startIteration + idx.toLong
        runMeasuredIteration(config, label, iterationNo).flatMap { result =>
          result.exitCode match {
            case ToolExitCode.Success =>
              loop(idx + 1, totalNanos + result.elapsedNanos)
            case other =>
              IO.pure(Left(other))
          }
        }
      }

    loop(0, 0L)
  }

  private def printBanner(config: Config): IO[Unit] = {
    val pid = ProcessHandle.current().pid()
    val cpuProfile = asprofOutputPath(config, "cpu", "html")
    val allocProfile = asprofOutputPath(config, "alloc", "html")
    val wallProfile = asprofOutputPath(config, "wall", "html")
    val jfrProfile = asprofOutputPath(config, "all", "jfr")

    IO.blocking {
      println(s"CheckBenchmarkMain PID: $pid")
      configSummary(config).foreach(line => println(s"  $line"))
      println(s"  checkArgs=${benchmarkArgs(config).mkString(" ")}")
      println("Attach async-profiler with:")
      println(
        s"  $asyncProfilerPath -d 30 -e cpu -t -f $cpuProfile $pid"
      )
      println(
        s"  $asyncProfilerPath -d 30 -e alloc -t -f $allocProfile $pid"
      )
      println(
        s"  $asyncProfilerPath -d 30 --wall 1ms -t -f $wallProfile $pid"
      )
      println(
        s"  $asyncProfilerPath -d 30 --all -o jfr -f $jfrProfile $pid"
      )
    }
  }

  private def maybeWaitForProfiler(config: Config): IO[Unit] =
    if (config.startupWaitMs <= 0L) IO.unit
    else
      IO.blocking {
        println(
          s"Waiting ${config.startupWaitMs}ms before warmup/measurement so profiler can attach..."
        )
      } *> IO.sleep(config.startupWaitMs.millis)

  private def warmup(config: Config): IO[Either[ToolExitCode, Unit]] =
    if (config.warmupIterations <= 0) IO.pure(Right(()))
    else {
      IO.blocking {
        println(s"Starting warmup (${config.warmupIterations} iteration(s))...")
      } *> runBlock(config, "warmup", 1L, config.warmupIterations).map(_.map(_ => ()))
    }

  private def runOnce(config: Config): IO[ExitCode] =
    runBlock(
      config,
      "measured",
      config.warmupIterations.toLong + 1L,
      config.reportEvery
    ).flatMap {
      case Left(exitCode) =>
        IO.blocking {
          System.err.println(
            s"Measured run failed with exit code ${exitCode.toInt}."
          )
        }.as(ExitCode.Error)
      case Right(stats)   =>
        IO.blocking {
          println(
            f"measured iterations=${stats.iterations}%d totalTime=${stats.seconds}%.3fs checks/s=${stats.iterationsPerSecond}%.3f"
          )
        }.as(ExitCode.Success)
    }

  private def loopForever(config: Config): IO[ExitCode] = {
    def go(
        block: Long,
        nextIteration: Long,
        totalIterations: Long,
        totalElapsedNanos: Long
    ): IO[ExitCode] =
      runBlock(config, "measured", nextIteration, config.reportEvery).flatMap {
        case Left(exitCode) =>
          IO.blocking {
            System.err.println(
              s"Measured block $block failed with exit code ${exitCode.toInt}."
            )
          }.as(ExitCode.Error)
        case Right(stats)   =>
          val updatedTotalIterations = totalIterations + stats.iterations.toLong
          val updatedTotalElapsed = totalElapsedNanos + stats.elapsedNanos
          val totalChecksPerSecond =
            if (updatedTotalElapsed <= 0L) Double.PositiveInfinity
            else
              updatedTotalIterations.toDouble * 1000000000.0d / updatedTotalElapsed.toDouble

          IO.blocking {
            println(
              f"block=${block}%d blockIterations=${stats.iterations}%d blockTime=${stats.seconds}%.3fs blockChecks/s=${stats.iterationsPerSecond}%.3f totalIterations=${updatedTotalIterations}%d totalChecks/s=${totalChecksPerSecond}%.3f"
            )
          } *> go(
            block + 1L,
            nextIteration + stats.iterations.toLong,
            updatedTotalIterations,
            updatedTotalElapsed
          )
      }

    val firstMeasuredIteration = config.warmupIterations.toLong + 1L
    go(1L, firstMeasuredIteration, 0L, 0L)
  }

  private def execute(config: Config): IO[ExitCode] =
    for {
      _ <- printBanner(config)
      _ <- maybeWaitForProfiler(config)
      warmupResult <- warmup(config)
      exitCode <- warmupResult match {
        case Left(exitCode) =>
          IO.blocking {
            System.err.println(s"Warmup failed with exit code ${exitCode.toInt}.")
          }.as(ExitCode.Error)
        case Right(())      =>
          if (config.loop) loopForever(config)
          else runOnce(config)
      }
    } yield exitCode

  def run(args: List[String]): IO[ExitCode] =
    parseArgs(args) match {
      case ParseResult.Help =>
        IO.blocking {
          println(usage)
          ExitCode.Success
        }
      case ParseResult.Invalid(message) =>
        IO.blocking {
          System.err.println(message)
          System.err.println()
          System.err.println(usage)
          ExitCode.Error
        }
      case ParseResult.Parsed(config) =>
        execute(config)
    }
}
