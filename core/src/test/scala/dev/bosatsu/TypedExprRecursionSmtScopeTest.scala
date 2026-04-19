package dev.bosatsu

import cats.Show
import cats.data.{NonEmptyList, Validated}
import IorMethods.IorExtension
import org.scalacheck.Prop.forAll
import org.typelevel.paiges.Document

class TypedExprRecursionSmtScopeTest extends munit.ScalaCheckSuite with ParTest {
  override def scalaCheckTestParameters = {
    val base = super.scalaCheckTestParameters
    if (Platform.isScalaJvm) base.withMinSuccessfulTests(24)
    else base.withMinSuccessfulTests(1).withMaxSize(12)
  }

  private def renderStatements(statements: List[Statement]): String =
    statements.map(Document[Statement].document(_).render(80)).mkString

  private def recursionErrorsOf(
      packageName: PackageName,
      statements: List[Statement],
      source: String
  ): List[RecursionCheck.Error] = {
    val parsed = Package.fromStatements(packageName, statements)
    given Show[String] = Show.fromToString
    PackageMap
      .typeCheckParsed(
        NonEmptyList.one((("<generated>", LocationMap(source)), parsed)),
        Nil,
        "<predef>",
        CompileOptions.Default
      )
      .strictToValidated match {
      case Validated.Valid(_) =>
        Nil
      case Validated.Invalid(errs) =>
        val recursionErrors = errs.toList.collect {
          case PackageError.RecursionError(_, err) => err
        }
        val nonRecursion = errs.toList.filterNot {
          case PackageError.RecursionError(_, _) => true
          case _                                 => false
        }
        if (nonRecursion.nonEmpty) {
          val sourceMap = Map(packageName -> (LocationMap(source), "<generated>"))
          val msg = errs.toList
            .map(_.message(sourceMap, LocationMap.Colorize.None))
            .mkString("\n-----\n")
          fail(s"type check failed for non-recursion reason:\n$msg")
        }
        recursionErrors
    }
  }

  property("phase 3 generated programs do not emit undeclared SMT vars") {
    forAll(WellTypedGen.wellTypedProgramGen(WellTypedGen.Config.phase3)) { program =>
      val source = renderStatements(program.statements)
      val details = recursionErrorsOf(program.packageName, program.statements, source)
        .collect {
          case RecursionCheck.IntRecursionObligationFailed(_, _, _, _, _, Some(detail), _) =>
            detail
        }

      val bad = details.filter { detail =>
        detail.contains("internal SMT script uses undeclared variables") ||
        detail.contains("Trapped on unreachable instruction")
      }

      assert(
        bad.isEmpty,
        s"""unexpected SMT script issue in recursion detail for source:
           |$source
           |details:
           |${bad.mkString("\n---\n")}""".stripMargin
      )
    }
  }
}
