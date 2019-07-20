package org.bykn.bosatsu

import cats.Functor
import cats.data.{ValidatedNel, Validated, NonEmptyList}
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import scala.collection.mutable.{Map => MMap}
import scala.util.hashing.MurmurHash3

import rankn._
import Identifier.Constructor
import Parser.{spaces, maybeSpace, Combinators}

/**
 * Represents a package over its life-cycle: from parsed to resolved to inferred
 */
final case class Package[A, B, C, +D](
  name: PackageName,
  imports: List[Import[A, B]],
  exports: List[ExportedName[C]],
  program: D) {

  // It is really important to cache the hashcode and these large dags if
  // we use them as hash keys
  final override val hashCode: Int =
    MurmurHash3.productHash(this)

  override def equals(that: Any): Boolean =
    that match {
      case p: Package[_, _, _, _] =>
        (this eq p) || {
          (name == p.name) &&
          (imports == p.imports) &&
          (exports == p.exports) &&
          (program == p.program)
        }
      case _ => false
    }

  // TODO, this isn't great
  private lazy val importMap: ImportMap[A, B] =
    ImportMap.fromImports(imports)._2

  def localImport(n: Identifier): Option[(A, ImportedName[B])] = importMap(n)

  def withImport(i: Import[A, B]): Package[A, B, C, D] =
    copy(imports = i :: imports)

  def mapProgram[D1](fn: D => D1): Package[A, B, C, D1] =
    Package(name, imports, exports, fn(program))

  def replaceImports[A1, B1](newImports: List[Import[A1, B1]]): Package[A1, B1, C, D] =
    Package(name, newImports, exports, program)
}

object Package {
  type Interface = Package[Nothing, Nothing, Referant[Variance], Unit]
  /**
   * This is a package whose import type is Either:
   * 1 a package of the same kind
   * 2 an interface
   */
  type FixPackage[B, C, D] = Fix[Lambda[a => Either[Interface, Package[a, B, C, D]]]]
  type PackageF[A, B, C] = Either[Interface, Package[FixPackage[A, B, C], A, B, C]]
  type PackageF2[A, B] = PackageF[A, A, B]
  type Parsed = Package[PackageName, Unit, Unit, Statement]
  type Resolved = FixPackage[Unit, Unit, (Statement, ImportMap[PackageName, Unit])]
  type Typed[T] = Package[
    Interface,
    NonEmptyList[Referant[Variance]],
    Referant[Variance],
    Program[TypeEnv[Variance], TypedExpr[T], Any]]
  type Inferred = Typed[Declaration]

  val typedFunctor: Functor[Typed] =
    new Functor[Typed] {
      def map[A, B](fa: Typed[A])(fn: A => B): Typed[B] = {
        val mapLet = fa.program.lets.map { case (n, r, te) =>
          (n, r, Functor[TypedExpr].map(te)(fn))
        }
        fa.copy(program = fa.program.copy(lets = mapLet))
      }
    }

  def fix[A, B, C](p: PackageF[A, B, C]): FixPackage[A, B, C] =
    Fix[Lambda[a => Either[Interface, Package[a, A, B, C]]]](p)
  /**
   * build a Parsed Package from a Statement. This is useful for testing or
   * library usages.
   */
  def fromStatement(pn: PackageName, st: Statement): Package.Parsed =
    Package(pn, Nil, Nil, st)

  def interfaceOf[A](inferred: Typed[A]): Interface =
    inferred.mapProgram(_ => ()).replaceImports(Nil)

  def exportedTypeEnv(iface: Interface): TypeEnv[Variance] =
    Referant.exportedTypeEnv(iface.name, iface.exports)

  def setProgramFrom[A, B](t: Typed[A], newFrom: B): Typed[A] =
    t.copy(program = t.program.copy(from = newFrom))

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
    imps: List[Import[Package.Interface, NonEmptyList[Referant[Variance]]]],
    stmt: Statement):
      ValidatedNel[PackageError,
      Program[TypeEnv[Variance], TypedExpr[Declaration], Statement]] = {

    val importedTypes: Map[Identifier, (PackageName, TypeName)] =
      Referant.importedTypes(imps)

    val Program(parsedTypeEnv, lets, extDefs, _) = {

      val resolveImportedCons: Map[Identifier, (PackageName, Constructor)] =
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

      val typeCache: MMap[Constructor, Type.Const] = MMap.empty
      val consCache: MMap[Constructor, (PackageName, Constructor)] = MMap.empty

      val srcConv = new SourceConverter(
        { s =>
          typeCache.getOrElseUpdate(s, {
            val ts = TypeName(s)
            val (p1, s1) =
              if (localTypeNames(s)) (p, ts)
              else importedTypes.getOrElse(s, (p, ts))
            Type.Const.Defined(p1, s1)
          })
        }, // name to type
        { s =>
          consCache.getOrElseUpdate(s, {
            if (localConstructors(s)) (p, s)
            else resolveImportedCons.getOrElse(s, (p, s))
          })
        }) // name to cons

      Program.fromStatement(p, srcConv, stmt)
    }

    val importedTypeEnv = Referant.importedTypeEnv(imps)(_.name)

    val inferVarianceParsed: Either[PackageError, ParsedTypeEnv[Variance]] =
      VarianceFormula.solve(importedTypeEnv, parsedTypeEnv.allDefinedTypes)
        .map { infDTs => ParsedTypeEnv(infDTs, parsedTypeEnv.externalDefs) }
        .leftMap(PackageError.VarianceInferenceFailure(p, _))

    inferVarianceParsed.toValidatedNel.andThen { parsedTypeEnv =>
      /*
       * Check that the types defined here are not circular.
       */
      val circularCheck: ValidatedNel[PackageError, Unit] =
        TypeRecursionCheck.checkLegitRecursion(importedTypeEnv, parsedTypeEnv.allDefinedTypes)
          .leftMap { badPaths =>
            badPaths.map(PackageError.CircularType(p, _))
          }
      /*
       * Check that all recursion is allowable
       */
      val defRecursionCheck: ValidatedNel[PackageError, Unit] =
        DefRecursionCheck.checkStatement(stmt)
          .leftMap { badRecursions =>
            badRecursions.map(PackageError.RecursionError(p, _))
          }

      val typeEnv = TypeEnv.fromParsed(parsedTypeEnv)
      /*
      * These are values, including all constructor functions
      * that have been imported, this includes local external
      * defs
      */
      val importedValues: Map[Identifier, Type] =
        Referant.importedValues(imps) ++ typeEnv.localValuesOf(p)

      val withFQN: Map[(Option[PackageName], Identifier), Type] =
        (Referant.fullyQualifiedImportedValues(imps)(_.name)
          .iterator
          .map { case ((p, n), t) => ((Some(p), n), t) } ++
            importedValues.iterator.map { case (n, t) => ((None, n), t) }
          ).toMap

      val fullTypeEnv = importedTypeEnv ++ typeEnv
      val totalityCheck =
        lets
          .traverse { case (_, _, expr) => TotalityCheck(fullTypeEnv).checkExpr(expr) }
          .leftMap { errs => errs.map(PackageError.TotalityCheckError(p, _)) }

      val inferenceEither = Infer.typeCheckLets(lets)
        .runFully(withFQN,
          Referant.typeConstructors(imps) ++ typeEnv.typeConstructors
        )
        .map { lets => Program(typeEnv, lets, extDefs, stmt) }
        .left
        .map(PackageError.TypeErrorIn(_, p))

      val inference = Validated.fromEither(inferenceEither).leftMap(NonEmptyList.of(_))

      defRecursionCheck *> circularCheck *> totalityCheck *> inference
    }
  }
}

