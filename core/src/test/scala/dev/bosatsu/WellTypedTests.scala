package dev.bosatsu

import cats.data.Ior
import cats.data.NonEmptyList
import cats.syntax.all._
import org.scalacheck.Prop.forAll
import org.typelevel.paiges.Document

class WellTypedTests extends munit.ScalaCheckSuite with ParTest {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 100 else 12
    )

  private def renderStatements(statements: List[Statement]): String =
    statements.map(Document[Statement].document(_).render(80)).mkString

  private def parseStatements(source: String): List[Statement] =
    Parser.parse(Statement.parser, source).toEither match {
      case Right((_, stmts)) => stmts
      case Left(errs)        =>
        val rendered = errs.toList
          .map(_.showContext(LocationMap.Colorize.None).render(100))
          .mkString("\n-----\n")
        fail(s"failed to parse generated source:\n$source\n\n$rendered")
    }

  private def assertSourceConverterSuccess(
      packageName: PackageName,
      statements: List[Statement],
      source: String
  ): Unit =
    SourceConverter.toProgram(packageName, Nil, statements) match {
      case Ior.Left(errs) =>
        val msg = errs.toList.mkString("\n")
        fail(s"source conversion failed for:\n$source\n\n$msg")
      case Ior.Both(errs, _) =>
        val msg = errs.toList.mkString("\n")
        fail(
          s"source conversion produced partial errors (Ior.Both) for:\n$source\n\n$msg"
        )
      case Ior.Right(_) =>
        ()
    }

  private def assertTypecheckSuccess(
      packageName: PackageName,
      statements: List[Statement],
      source: String
  ): Unit = {
    val parsed = Package.fromStatements(packageName, statements)
    given cats.Show[String] = cats.Show.fromToString
    val checked = PackageMap.typeCheckParsed(
      NonEmptyList.one((("<generated>", LocationMap(source)), parsed)),
      Nil,
      "<predef>",
      CompileOptions.Default
    )

    checked match {
      case Ior.Right(pm) =>
        TestUtils.assertPackageMapTypeConnections(pm, "well-typed optimized")
      case Ior.Both(errs, _) =>
        val sourceMap = Map(
          packageName -> (LocationMap(source), "<generated>"),
          PackageName.PredefName -> (LocationMap(Predef.predefString), "<predef>")
        )
        val msg = errs.toList
          .map(_.message(sourceMap, LocationMap.Colorize.None))
          .mkString("\n-----\n")
        fail(
          s"typecheck produced partial errors (Ior.Both) for:\n$source\n\n$msg"
        )
      case Ior.Left(errs)                =>
        val sourceMap = Map(
          packageName -> (LocationMap(source), "<generated>"),
          PackageName.PredefName -> (LocationMap(Predef.predefString), "<predef>")
        )
        val msg = errs.toList
          .map(_.message(sourceMap, LocationMap.Colorize.None))
          .mkString("\n-----\n")
        fail(s"typecheck failed for:\n$source\n\n$msg")
    }
  }

  private def assertPipeline(cfg: WellTypedGen.Config) =
    forAll(WellTypedGen.wellTypedProgramGen(cfg)) { program =>
      val source = renderStatements(program.statements)
      // Step 1: parse generated source
      val parsedStatements = parseStatements(source)
      // Step 2: source conversion must succeed for all generated programs
      assertSourceConverterSuccess(program.packageName, parsedStatements, source)
      // Step 3: package typechecking must succeed
      assertTypecheckSuccess(program.packageName, parsedStatements, source)
    }

  property("phase 1 generator: parse -> source-convert -> typecheck") {
    assertPipeline(WellTypedGen.Config.phase1)
  }

  property("phase 2 generator: parse -> source-convert -> typecheck") {
    assertPipeline(WellTypedGen.Config.phase2)
  }

  property("phase 3 generator: parse -> source-convert -> typecheck") {
    assertPipeline(WellTypedGen.Config.phase3)
  }

  property("phase 4 generator: parse -> source-convert -> typecheck") {
    assertPipeline(WellTypedGen.Config.phase4)
  }
}
