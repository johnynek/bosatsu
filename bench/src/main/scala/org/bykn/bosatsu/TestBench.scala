package org.bykn.bosatsu

import cats.data.Validated
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import cats.implicits._

@State(Scope.Thread)
class TestBench {

  private def prepPackages(packages: List[String], mainPackS: String): (PackageMap.Inferred, PackageName) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser, pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          p.showContext.foreach(System.err.println)
        }
        sys.error("failed to parse") //errs.toString)
    }

    PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths)) match {
      case (dups, Validated.Valid(packMap)) if dups.isEmpty =>
        (packMap, mainPack)
      case other => sys.error(s"expected clean compilation: $other")
    }
  }

  def gauss(n: Int) = prepPackages(
      List(s"""
package Gauss

gauss$n = range($n).foldLeft(0, add)
"""), "Gauss")

  val compiled0: (PackageMap.Inferred, PackageName) =
    gauss(10)

  @Benchmark def bench0(): Unit = {
    val ev = Evaluation(compiled0._1, Predef.jvmExternals)
    // run the evaluation
    val res = ev.evaluateLast(compiled0._2).get._1.value
    ()
  }

  val compiled1: (PackageMap.Inferred, PackageName) =
    gauss(20)

  @Benchmark def bench1(): Unit = {
    val ev = Evaluation(compiled1._1, Predef.jvmExternals)
    // run the evaluation
    val res = ev.evaluateLast(compiled1._2).get._1.value
    ()
  }
}
