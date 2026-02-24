package dev.bosatsu

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import IorMethods.IorExtension
import org.scalacheck.Prop.forAll
import org.typelevel.paiges.Document

class TypedExprRecursionParityTest extends munit.ScalaCheckSuite with ParTest {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 30 else 12
    )

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

  private def assertParity(cfg: WellTypedGen.Config) =
    forAll(WellTypedGen.wellTypedProgramGen(cfg)) { program =>
      val source = renderStatements(program.statements)
      val legacy = legacyPass(program.statements)
      val typed = typedPass(program.packageName, program.statements, source)
      assertEquals(
        typed,
        legacy,
        s"legacy/typed recursion mismatch for source:\n$source"
      )
    }

  property("phase 1: legacy vs typed recursion parity") {
    assertParity(WellTypedGen.Config.phase1)
  }

  property("phase 2: legacy vs typed recursion parity") {
    assertParity(WellTypedGen.Config.phase2)
  }

  property("phase 3: legacy vs typed recursion parity") {
    assertParity(WellTypedGen.Config.phase3)
  }

  property("phase 4: legacy vs typed recursion parity") {
    assertParity(WellTypedGen.Config.phase4)
  }
}
