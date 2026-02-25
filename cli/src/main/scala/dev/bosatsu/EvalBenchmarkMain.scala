package dev.bosatsu

import cats.data.NonEmptyList
import cats.effect.{ExitCode, IO, IOApp}
import java.math.BigInteger

object EvalBenchmarkMain extends IOApp {
  private val benchmarkSource: String =
    """package EvalBench
      |
      |def add3(a, b, c): add(add(a, b), c)
      |
      |def function_calls(n):
      |  def loop(items, acc):
      |    recur items:
      |      case []: acc
      |      case [i, *tail]:
      |        v0 = add3(acc, i, 1)
      |        v1 = add3(v0, i, 2)
      |        v2 = add3(v1, i, 3)
      |        loop(tail, v2)
      |
      |  loop(range(n), 0)
      |
      |def tail_loop(n):
      |  def loop(items, acc):
      |    recur items:
      |      case []: acc
      |      case [i, *tail]:
      |        loop(tail, add(acc, i))
      |
      |  loop(range(n), 0)
      |
      |def non_tail_recursion(n):
      |  def loop(items):
      |    recur items:
      |      case []: 0
      |      case [head, *tail]:
      |        add(head, loop(tail))
      |
      |  loop(range(n))
      |
      |def int_ops(n):
      |  def loop(items, acc):
      |    recur items:
      |      case []: acc
      |      case [i, *tail]:
      |        mixed0 = add(acc, mul(i, 3))
      |        mixed1 = xor_Int(mixed0, shift_left_Int(i, 1))
      |        mixed2 = and_Int(mixed1, 65535)
      |        mixed3 = mod_Int(add(mixed2, acc), 1000003)
      |        loop(tail, mixed3)
      |
      |  loop(range(n), 1)
      |
      |def string_ops(n, seed):
      |  def loop(items, s):
      |    recur items:
      |      case []: s
      |      case [_, *rest]:
      |        tailS = tail_or_empty_String(s)
      |        rotated = concat_String([tailS, "x"])
      |        part = match partition_String(rotated, "ab"):
      |          case Some((left, right)): concat_String([right, left])
      |          case None: rotated
      |        clipped = match partition_String(part, "xxxx"):
      |          case Some((left, _)): left
      |          case None: part
      |        loop(rest, clipped)
      |
      |  loop(range(n), seed)
      |
      |main = (function_calls, tail_loop, non_tail_recursion, int_ops, string_ops)
      |""".stripMargin

  private val benchmarkPackage: PackageName = PackageName.parts("EvalBench")

  private final case class Config(
      loop: Boolean = true,
      iterations: Int = 200,
      warmupIterations: Int = 100,
      work: Int = 400,
      nonTailDepth: Int = 220,
      reportEvery: Int = 200
  )

  private final case class BenchFns(
      functionCalls: Value,
      tailLoop: Value,
      nonTailRecursion: Value,
      intOps: Value,
      stringOps: Value
  )

  private final case class RunStats(iterations: Int, elapsedNanos: Long, digest: Long) {
    val itersPerSecond: Double =
      if (elapsedNanos <= 0L) Double.PositiveInfinity
      else iterations.toDouble * 1000000000.0d / elapsedNanos.toDouble

    val nanosPerIteration: Double =
      if (iterations <= 0) 0.0d
      else elapsedNanos.toDouble / iterations.toDouble
  }

  private val usage: String =
    s"""Usage: ${EvalBenchmarkMain.getClass.getSimpleName.stripSuffix("$")} [options]
       |
       |Options:
       |  --loop                      Run forever (default) so async-profiler can attach.
       |  --once                      Run once and exit.
       |  --iterations <n>            Iterations for --once mode. Default: 200.
       |  --warmup <n>                Warmup iterations before measurement. Default: 100.
       |  --work <n>                  Shared loop depth for function/tail/int/string benches. Default: 400.
       |  --non-tail-depth <n>        Recursion depth for non-tail recursion bench. Default: 220.
       |  --report-every <n>          Iterations per throughput report in --loop mode. Default: 200.
       |  --help                      Show this help.
       |""".stripMargin

  private def parsePositiveInt(raw: String, flag: String): Either[String, Int] =
    try {
      val parsed = raw.toInt
      if (parsed > 0) Right(parsed)
      else Left(s"$flag requires a positive integer, received: $raw")
    } catch {
      case _: NumberFormatException =>
        Left(s"$flag requires a positive integer, received: $raw")
    }

  private def parseArgs(args: List[String]): Either[String, Config] = {
    @annotation.tailrec
    def loop(rest: List[String], config: Config): Either[String, Config] =
      rest match {
        case Nil => Right(config)
        case "--help" :: _ => Left(usage)
        case "--loop" :: tail => loop(tail, config.copy(loop = true))
        case "--once" :: tail => loop(tail, config.copy(loop = false))
        case "--iterations" :: value :: tail =>
          parsePositiveInt(value, "--iterations") match {
            case Right(n) => loop(tail, config.copy(iterations = n))
            case Left(err) => Left(err)
          }
        case "--warmup" :: value :: tail =>
          parsePositiveInt(value, "--warmup") match {
            case Right(n) => loop(tail, config.copy(warmupIterations = n))
            case Left(err) => Left(err)
          }
        case "--work" :: value :: tail =>
          parsePositiveInt(value, "--work") match {
            case Right(n) => loop(tail, config.copy(work = n))
            case Left(err) => Left(err)
          }
        case "--non-tail-depth" :: value :: tail =>
          parsePositiveInt(value, "--non-tail-depth") match {
            case Right(n) => loop(tail, config.copy(nonTailDepth = n))
            case Left(err) => Left(err)
          }
        case "--report-every" :: value :: tail =>
          parsePositiveInt(value, "--report-every") match {
            case Right(n) => loop(tail, config.copy(reportEvery = n))
            case Left(err) => Left(err)
          }
        case flag :: Nil if flag.startsWith("--") =>
          Left(s"missing value for $flag")
        case unknown :: _ =>
          Left(s"unknown argument: $unknown")
      }

    loop(args, Config())
  }

  private def compileBenchmarkEvaluation(): Either[String, Evaluation[Declaration]] = {
    val parsed = Parser.unsafeParse(Package.parser(None), benchmarkSource)
    val locMap = LocationMap(benchmarkSource)

    val result = Par.noParallelism {
      PackageMap.typeCheckParsed(
        cats.data.NonEmptyList.one(("<eval-benchmark>", locMap) -> parsed),
        Nil,
        "<eval-benchmark>",
        CompileOptions.Default
      )
    }

    result.toEither.left.map { errs =>
      val srcMap = Map(parsed.name -> (locMap, "<eval-benchmark>"))
      errs.toList
        .map(_.message(srcMap, LocationMap.Colorize.None))
        .mkString("\n\n")
    }.map(pm => Evaluation(pm, Predef.jvmExternals))
  }

  private def expectFn(value: Value, idx: Int): Either[String, Value] =
    value match {
      case fn: Value.FnValue => Right(fn)
      case other =>
        Left(s"expected main tuple element $idx to be a function, found: $other")
    }

  private def buildBenchFns(): Either[String, BenchFns] =
    for {
      evaluation <- compileBenchmarkEvaluation()
      mainValue <- evaluation
        .evaluateMain(benchmarkPackage)
        .toRight("benchmark package did not produce a main value")
        .map(_._1.value)
      tuple <- mainValue match {
        case p: Value.ProductValue if p.values.length == 5 => Right(p.values)
        case other =>
          Left(
            s"benchmark main must be a 5-element tuple of functions, found: $other"
          )
      }
      functionCalls <- expectFn(tuple(0), 0)
      tailLoop <- expectFn(tuple(1), 1)
      nonTailRec <- expectFn(tuple(2), 2)
      intOps <- expectFn(tuple(3), 3)
      stringOps <- expectFn(tuple(4), 4)
    } yield BenchFns(functionCalls, tailLoop, nonTailRec, intOps, stringOps)

  private def call1(fn: Value, a: Value): Value =
    fn.applyAll(NonEmptyList.one(a))

  private def call2(fn: Value, a: Value, b: Value): Value =
    fn.applyAll(NonEmptyList(a, b :: Nil))

  private def asBigInteger(v: Value): BigInteger =
    v match {
      case Value.VInt(bi) => bi
      case other          =>
        // $COVERAGE-OFF$
        sys.error(s"expected Int value, received: $other")
      // $COVERAGE-ON$
    }

  private def asString(v: Value): String =
    v match {
      case Value.Str(s) => s
      case other        =>
        // $COVERAGE-OFF$
        sys.error(s"expected String value, received: $other")
      // $COVERAGE-ON$
    }

  private def runSingleIteration(fns: BenchFns, config: Config): Long = {
    val workload = Value.VInt(config.work)
    val nonTailDepth = Value.VInt(config.nonTailDepth)
    val seed = Value.Str("abcdefghijklmnopqrstuvwxyz0123456789")

    val fnCallRes = call1(fns.functionCalls, workload)
    val tailLoopRes = call1(fns.tailLoop, workload)
    val nonTailRes = call1(fns.nonTailRecursion, nonTailDepth)
    val intOpsRes = call1(fns.intOps, workload)
    val stringRes = call2(fns.stringOps, workload, seed)

    val sLen = asString(stringRes).length

    val mixed =
      asBigInteger(fnCallRes)
        .xor(asBigInteger(tailLoopRes))
        .xor(asBigInteger(nonTailRes))
        .xor(asBigInteger(intOpsRes))
        .xor(BigInteger.valueOf(sLen.toLong))

    mixed.longValue()
  }

  private def runIterations(fns: BenchFns, config: Config, iterations: Int): RunStats = {
    val start = System.nanoTime()
    var idx = 0
    var digest = 0L

    while (idx < iterations) {
      val value = runSingleIteration(fns, config)
      digest = (digest * 1664525L) + value + 1013904223L
      idx = idx + 1
    }

    RunStats(iterations, System.nanoTime() - start, digest)
  }

  private def printStats(prefix: String, stats: RunStats): Unit = {
    val sec = stats.elapsedNanos.toDouble / 1000000000.0d
    println(
      f"$prefix iterations=${stats.iterations}%d time=${sec}%.3fs iters/s=${stats.itersPerSecond}%.2f ns/iter=${stats.nanosPerIteration}%.0f digest=${stats.digest}%d"
    )
  }

  private def runOnce(config: Config, fns: BenchFns): IO[ExitCode] =
    IO.blocking {
      if (config.warmupIterations > 0) {
        val warmup = runIterations(fns, config, config.warmupIterations)
        printStats("warmup", warmup)
      }
      val measured = runIterations(fns, config, config.iterations)
      printStats("measured", measured)
      ExitCode.Success
    }

  private def loopForever(config: Config, fns: BenchFns): IO[ExitCode] = {
    val pid = ProcessHandle.current().pid()

    def go(
        block: Long,
        totalIterations: Long,
        totalElapsedNanos: Long,
        digest: Long
    ): IO[ExitCode] =
      IO.blocking(runIterations(fns, config, config.reportEvery)).flatMap { stats =>
        val nextBlock = block + 1L
        val nextTotalIterations = totalIterations + stats.iterations.toLong
        val nextElapsed = totalElapsedNanos + stats.elapsedNanos
        val nextDigest = digest ^ stats.digest
        val totalItersPerSec =
          if (nextElapsed <= 0L) Double.PositiveInfinity
          else nextTotalIterations.toDouble * 1000000000.0d / nextElapsed.toDouble

        IO.blocking {
          val sec = stats.elapsedNanos.toDouble / 1000000000.0d
          println(
            f"block=${nextBlock}%d blockIters=${stats.iterations}%d blockTime=${sec}%.3fs blockIters/s=${stats.itersPerSecond}%.2f totalIters=${nextTotalIterations}%d totalIters/s=${totalItersPerSec}%.2f digest=${nextDigest}%d"
          )
        } *> go(nextBlock, nextTotalIterations, nextElapsed, nextDigest)
      }

    IO.blocking {
      println(s"EvalBenchmarkMain PID: $pid")
      println(
        "Attach async-profiler with: ~/Downloads/async-profiler-4.3-macos/bin/asprof -d 15 -f /tmp/eval-benchmark.svg <pid>"
      )
      if (config.warmupIterations > 0) {
        val warmup = runIterations(fns, config, config.warmupIterations)
        printStats("warmup", warmup)
      }
    } *> go(0L, 0L, 0L, 0L)
  }

  def run(args: List[String]): IO[ExitCode] =
    parseArgs(args) match {
      case Left(msg) =>
        IO.blocking {
          System.err.println(msg)
          if (!msg.startsWith("Usage:")) {
            System.err.println()
            System.err.println(usage)
          }
          ExitCode.Error
        }
      case Right(config) =>
        buildBenchFns() match {
          case Left(err) =>
            IO.blocking {
              System.err.println(err)
              ExitCode.Error
            }
          case Right(fns) =>
            if (config.loop) loopForever(config, fns)
            else runOnce(config, fns)
        }
    }
}
