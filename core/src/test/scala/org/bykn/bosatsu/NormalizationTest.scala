package org.bykn.bosatsu

import cats.data.{ Validated, ValidatedNel}
import fastparse.all._
import org.scalatest.FunSuite

object NormalizationHelpers {
  def normalizeExpr(expr: TypedExpr[Declaration]): TypedExpr[(Declaration, Normalization.NormalExpressionTag)] = {
    ???
  }

  def normalizeLet(inferredExpr: (String, RecursionKind, TypedExpr[Declaration])): (String, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)]) = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    (inferredExpr._1, inferredExpr._2, normalizeExpr(inferredExpr._3))
  }

  def normalizeProgram[T, S](inferredProgram: Program[T, TypedExpr[Declaration], S]): Program[T, TypedExpr[(Declaration, Normalization.NormalExpressionTag)], S] = {
    inferredProgram.copy(
      lets  = inferredProgram.lets.map(normalizeLet),
    )
  }

  def normalizePackage(pkg: Package.Inferred): Package.Normalized = {
    pkg.copy(
      program = normalizeProgram(pkg.program)
    )
  }

  def normalizePackageMap(pkgMap: PackageMap.Inferred): PackageMap.Normalized = {
    PackageMap(pkgMap.toMap.map { case (name, pkg) => {
      (name, normalizePackage(pkg))
    }})
  }
}

class NormalizationTest extends FunSuite {

  def resolveThenInfer(ps: Iterable[Package.Parsed]): ValidatedNel[PackageError, PackageMap.Inferred] =
    PackageMap.resolveThenInfer(ps.toList.map { p => ((), p) })._2

  def parse(s: String): Package.Parsed =
    Package.parser.parse(s) match {
      case Parsed.Success(p, idx) =>
        assert(idx == s.length)
        p
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $s: $exp at $idx with trace: ${extra.traced.trace}")
    }

//   test("package import normalization") {
//     val p1 = parse(
// """
// package Foo
// export [ main ]

// main = 1
// """)

//     val p2 = parse(
// """
// package Foo2
// import Foo [ main as mainFoo ]
// export [ main, ]

// main = mainFoo
// """)

//     val (_, validatedPackageMap) = PackageMap.resolveThenInfer(List(((), p1), ((), p2)))
//     val packageMap = validatedPackageMap match {
//       case Validated.Valid(rpm) => rpm
//       case Validated.Invalid(err) => fail(err.toString)
//     }

//     succeed
//   }


  test("simple package normalizes") {
    val p1 = parse(
"""
package Willem/Foo

struct Pair(first, second)

def bar(x):
  baz = \y -> Pair(x, y)
  baz(10)

main = bar(5)
""")

    val (_, validatedPackageMap) = PackageMap.resolveThenInfer(List(((), p1)))
    val packageMap = validatedPackageMap match {
      case Validated.Valid(rpm) => rpm
      case Validated.Invalid(err) => fail(err.toString)
    }

    // println("Input packageMap")
    // println(packageMap)

    val normalizedMap = NormalizationHelpers.normalizePackageMap(packageMap)

    // println("Output packageMap")
    // println(normalizedMap)

    succeed
  }
}
