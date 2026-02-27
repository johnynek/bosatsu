package dev.bosatsu

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import IorMethods.IorExtension
import org.scalacheck.Prop.forAll
import org.typelevel.paiges.Document

class TypedExprRecursionParitySeedRegressionTest
    extends munit.ScalaCheckSuite
    with ParTest {
  override def scalaCheckInitialSeed =
    "Fo4swOwXPC3xNI0E1pz2E4fwpHywwBXoLbvPIYU53bF="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(8)

  private def renderStatements(statements: List[Statement]): String =
    statements.map(Document[Statement].document(_).render(80)).mkString

  private def legacyPass(statements: List[Statement]): Boolean =
    statements.traverse_(LegacyDefRecursionCheck.checkStatement(_)).isValid

  private def typedPass(
      packageName: PackageName,
      statements: List[Statement],
      source: String
  ): Boolean = {
    val parsed = Package.fromStatements(packageName, statements)
    given cats.Show[String] = cats.Show.fromToString
    PackageMap
      .typeCheckParsed(
        NonEmptyList.one((("<generated>", LocationMap(source)), parsed)),
        Nil,
        "<predef>",
        CompileOptions.Default
      )
      .strictToValidated match {
      case Validated.Valid(_) =>
        true
      case Validated.Invalid(errs) =>
        val recursionErrors = errs.toList.collect {
          case re: PackageError.RecursionError => re
        }
        if (recursionErrors.nonEmpty && recursionErrors.size == errs.length) {
          false
        } else {
          val sourceMap = Map(packageName -> (LocationMap(source), "<generated>"))
          val msg = errs.toList
            .map(_.message(sourceMap, LocationMap.Colorize.None))
            .mkString("\n-----\n")
          fail(s"typed pipeline failed for non-recursion reason:\n$msg")
        }
    }
  }

  property("phase 3 seed repro: legacy vs typed recursion parity") {
    // Regression for issue #1801.
    forAll(WellTypedGen.wellTypedProgramGen(WellTypedGen.Config.phase3)) { program =>
      val source = renderStatements(program.statements)
      val legacy = legacyPass(program.statements)
      val typed = typedPass(program.packageName, program.statements, source)
      assertEquals(
        typed,
        legacy,
        s"legacy/typed recursion mismatch for source:\n$source"
      )
    }
  }
}
