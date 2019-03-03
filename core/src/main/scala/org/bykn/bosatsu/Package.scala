package org.bykn.bosatsu

import cats.data.{ValidatedNel, Validated, NonEmptyList}
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import scala.collection.mutable.{Map => MMap}

import Parser.{spaces, maybeSpace, Combinators}
import rankn._

/**
 * Represents a package over its life-cycle: from parsed to resolved to inferred
 */
case class Package[A, B, C, D](
  name: PackageName,
  imports: List[Import[A, B]],
  exports: List[ExportedName[C]],
  program: D) {

  // TODO, this isn't great
  private lazy val importMap: ImportMap[A, B] =
    ImportMap.fromImports(imports)._2

  def localImport(n: String): Option[(A, ImportedName[B])] = importMap(n)

  def withImport(i: Import[A, B]): Package[A, B, C, D] =
    copy(imports = i :: imports)

  def mapProgram[D1](fn: D => D1): Package[A, B, C, D1] =
    Package(name, imports, exports, fn(program))
}

object Package {
  type FixPackage[B, C, D] = Fix[Lambda[a => Package[a, B, C, D]]]
  type PackageF[A, B, C] = Package[FixPackage[A, B, C], A, B, C]
  type PackageF2[A, B] = PackageF[A, A, B]
  type Parsed = Package[PackageName, Unit, Unit, Statement]
  type Resolved = FixPackage[Unit, Unit, (Statement, ImportMap[PackageName, Unit])]
  type Inferred = FixPackage[NonEmptyList[Referant[Variance]], Referant[Variance], Program[TypeEnv[Variance], TypedExpr[Declaration], Statement]]

  /**
   * build a Parsed Package from a Statement. This is useful for testing or
   * library usages.
   */
  def fromStatement(pn: PackageName, st: Statement): Package.Parsed =
    Package(pn, Nil, Nil, st)

  /** add a Fix wrapper
   *  it is combersome to write the correct type here
   */
  def asInferred(p: PackageF[NonEmptyList[Referant[Variance]], Referant[Variance], Program[TypeEnv[Variance], TypedExpr[Declaration], Statement]]): Inferred =
    Fix[Lambda[a =>
      Package[a, NonEmptyList[Referant[Variance]], Referant[Variance], Program[TypeEnv[Variance], TypedExpr[Declaration], Statement]]]](p)

  implicit val document: Document[Package[PackageName, Unit, Unit, Statement]] =
    Document.instance[Package.Parsed] { case Package(name, imports, exports, program) =>
      val p = Doc.text("package ") + Document[PackageName].document(name) + Doc.line
      val i = imports match {
        case Nil => Doc.empty
        case nonEmptyImports =>
          Doc.intercalate(Doc.line, nonEmptyImports.map(Document[Import[PackageName, Unit]].document _)) + Doc.line
      }
      val e = exports match {
        case Nil => Doc.empty
        case nonEmptyExports =>
          Doc.text("export ") + Doc.text("[ ") +
          Doc.intercalate(Doc.text(", "), nonEmptyExports.map(Document[ExportedName[Unit]].document _)) + Doc.text(" ]") + Doc.line
      }
      val b = Document[Statement].document(program)
      // add an extra line between each group
      Doc.intercalate(Doc.line, List(p, i, e, b))
    }

  val parser: P[Package[PackageName, Unit, Unit, Statement]] = {
    // TODO: support comments before the Statement
    val pname = Padding.parser(P("package" ~ spaces ~ PackageName.parser)).map(_.padded)
    val im = Padding.parser(Import.parser).map(_.padded).rep().map(_.toList)
    val ex = Padding.parser(P("export" ~ maybeSpace ~ ExportedName.parser.nonEmptyListSyntax)).map(_.padded)
    val body = Padding.parser(Statement.parser).map(_.padded)
    (pname ~ im ~ Parser.nonEmptyListToList(ex) ~ body).map { case (p, i, e, b) =>
      Package(p, i, e, b)
    }
  }

  /**
   * After having type checked the imports, we now type check the body
   * in order to type check the exports
   */
  def inferBody(
    p: PackageName,
    imps: List[Import[Package.Inferred, NonEmptyList[Referant[Variance]]]],
    stmt: Statement):
      ValidatedNel[PackageError, (TypeEnv[Variance], List[(String, TypedExpr[Declaration])])] = {

    val importedTypes: Map[String, (PackageName, String)] =
      Referant.importedTypes(imps)

    val resolveImportedCons: Map[String, (PackageName, ConstructorName)] =
      Referant.importedConsNames(imps)

    // here we make a pass to get all the local names
    val localDefs = Statement.definitionsOf(stmt)

    /*
     * We should probably error for non-predef name collisions.
     * Maybe we should even error even or predef collisions that
     * are not renamed
     */
    val localTypeNames = localDefs.map(_.name).toSet
    val localConstructors = localDefs.flatMap(_.constructors).toSet

    val typeCache: MMap[String, Type.Const] = MMap.empty
    val consCache: MMap[String, (PackageName, ConstructorName)] = MMap.empty

    val Program(parsedTypeEnv, lets, _) =
      Program.fromStatement(
        p,
        { s =>
          typeCache.getOrElseUpdate(s, {
            val (p1, s1) =
              if (localTypeNames(s)) (p, s)
              else importedTypes.getOrElse(s, (p, s))
            Type.Const.Defined(p1, s1)
          })
        }, // name to type
        { s =>
          consCache.getOrElseUpdate(s, {
            if (localConstructors(s)) (p, ConstructorName(s))
            else resolveImportedCons.getOrElse(s, (p, ConstructorName(s)))
          })
        }, // name to cons
        stmt)

    val importedTypeEnv = Referant.importedTypeEnv(imps)(_.unfix.name)

    val inferVarianceParsed: Either[PackageError, ParsedTypeEnv[Variance]] =
      VarianceFormula.solve(importedTypeEnv, parsedTypeEnv.allDefinedTypes)
        .map { infDTs => ParsedTypeEnv(infDTs, parsedTypeEnv.externalDefs) }
        .leftMap(PackageError.VarianceInferenceFailure(p, _))

    inferVarianceParsed.toValidatedNel.andThen { parsedTypeEnv =>
      /*
       * Check that the types defined here are not circular.
       */
      val circularCheck: ValidatedNel[PackageError, Unit] =
        TypeRecursionCheck.check(importedTypeEnv, parsedTypeEnv.allDefinedTypes)
          .leftMap { badPaths =>
            badPaths.map(PackageError.CircularType(p, _))
          }

      val typeEnv = TypeEnv.fromParsed(parsedTypeEnv)
      /*
      * These are values, including all constructor functions
      * that have been imported, this includes local external
      * defs
      */
      val importedValues: Map[String, Type] =
        Referant.importedValues(imps) ++ typeEnv.localValuesOf(p)

      val withFQN: Map[(Option[PackageName], String), Type] =
        (Referant.fullyQualifiedImportedValues(imps)(_.unfix.name)
          .iterator
          .map { case ((p, n), t) => ((Some(p), n), t) } ++
            importedValues.iterator.map { case (n, t) => ((None, n), t) }
          ).toMap

      val fullTypeEnv = importedTypeEnv ++ typeEnv
      val totalityCheck =
        lets
          .traverse { case (_, expr) => TotalityCheck(fullTypeEnv).checkExpr(expr) }
          .leftMap { errs => errs.map(PackageError.TotalityCheckError(p, _)) }

      val inferenceEither = Infer.typeCheckLets(lets)
        .runFully(withFQN,
          Referant.typeConstructors(imps) ++ typeEnv.typeConstructors
        )
        .map { lets => (typeEnv, lets) }
        .left
        .map(PackageError.TypeErrorIn(_, p))

      val inference = Validated.fromEither(inferenceEither).leftMap(NonEmptyList.of(_))

      circularCheck *> totalityCheck *> inference
    }
  }
}

