package org.bykn.bosatsu

import cats.{Functor, Semigroup}
import cats.data.{Chain, Ior, ValidatedNel, Validated, NonEmptyList, Writer}
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import scala.util.hashing.MurmurHash3

import rankn._
import Parser.{spaces, maybeSpace, Combinators}

import FixType.Fix
import LocationMap.Colorize
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
  type Parsed = Package[PackageName, Unit, Unit, List[Statement]]
  type Resolved = FixPackage[Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]
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
    FixType.fix[Lambda[a => Either[Interface, Package[a, A, B, C]]]](p)

  def unfix[A, B, C](fp: FixPackage[A, B, C]): PackageF[A, B, C] =
    FixType.unfix[Lambda[a => Either[Interface, Package[a, A, B, C]]]](fp)
  /**
   * build a Parsed Package from a Statement. This is useful for testing or
   * library usages.
   */
  def fromStatements(pn: PackageName, stmts: List[Statement]): Package.Parsed =
    Package(pn, Nil, Nil, stmts)

  def interfaceOf[A](inferred: Typed[A]): Interface =
    inferred.mapProgram(_ => ()).replaceImports(Nil)

  def exportedTypeEnv(iface: Interface): TypeEnv[Variance] = {
    val packageName = iface.name
    iface.exports.foldLeft((TypeEnv.empty): TypeEnv[Variance]) { (te, exp) =>
      exp.tag match {
        case Referant.Value(t) =>
          te.addExternalValue(packageName, exp.name, t)
        case Referant.Constructor(n, dt, params, v) =>
          te.addConstructor(packageName, n, params, dt, v)
        case Referant.DefinedT(dt) =>
          te.addDefinedType(dt)
      }
    }
  }

  def setProgramFrom[A, B](t: Typed[A], newFrom: B): Typed[A] =
    t.copy(program = t.program.copy(from = newFrom))

  implicit val document: Document[Package[PackageName, Unit, Unit, List[Statement]]] =
    Document.instance[Package.Parsed] { case Package(name, imports, exports, statments) =>
      val p = Doc.text("package ") + Document[PackageName].document(name) + Doc.line
      val i = imports match {
        case Nil => Doc.empty
        case nonEmptyImports =>
          Doc.line +
            Doc.intercalate(Doc.line, nonEmptyImports.map(Document[Import[PackageName, Unit]].document _)) +
            Doc.line
      }
      val e = exports match {
        case Nil => Doc.empty
        case nonEmptyExports =>
          Doc.line +
            Doc.text("export ") +
            Doc.text("[ ") +
            Doc.intercalate(Doc.text(", "), nonEmptyExports.map(Document[ExportedName[Unit]].document _)) +
            Doc.text(" ]") +
            Doc.line
      }
      val b = statments.map(Document[Statement].document(_))
      Doc.intercalate(Doc.empty, p :: i :: e :: b)
    }

  def parser(defaultPack: Option[PackageName]): P[Package[PackageName, Unit, Unit, List[Statement]]] = {
    // TODO: support comments before the Statement
    val parsePack = Padding.parser(P("package" ~ spaces ~/ PackageName.parser ~ Parser.toEOL)).map(_.padded)
    val pname = defaultPack match {
      case None => parsePack
      case Some(p) => parsePack.?.map(_.getOrElse(p))
    }
    val im = Padding.parser(Import.parser ~ Parser.toEOL).map(_.padded).rep().map(_.toList)
    val ex = Padding.parser(P("export" ~ maybeSpace ~ ExportedName.parser.nonEmptyListSyntax ~ Parser.toEOL)).map(_.padded)
    val body = Statement.parser
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
    stmts: List[Statement]):
      ValidatedNel[PackageError,
      Program[TypeEnv[Variance], TypedExpr[Declaration], List[Statement]]] = {

    // here we make a pass to get all the local names
    val localDefs = Statement.definitionsOf(stmts)
    val optProg = SourceConverter(p, imps.map { i => i.copy(pack = i.pack.name) }, localDefs)
      .toProgram(stmts)
      .leftMap(_.map(PackageError.SourceConverterErrorIn(_, p): PackageError).toNonEmptyList)

    def andThen[A: Semigroup, B, C](ior: Ior[A, B])(fn: B => Validated[A, C]): Validated[A, C] =
      ior match {
        case Ior.Right(b) => fn(b)
        case Ior.Left(a) => Validated.Invalid(a)
        case Ior.Both(a, b) =>
          fn(b) match {
            case Validated.Valid(_) => Validated.Invalid(a)
            case Validated.Invalid(a1) => Validated.Invalid(Semigroup[A].combine(a, a1))
          }
      }

    andThen(optProg) {
      case Program((importedTypeEnv, parsedTypeEnv), lets, extDefs, _) =>
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
            stmts.traverse_(DefRecursionCheck.checkStatement(_))
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
            .map { lets =>
              val normalLets = lets.map { case (n, r, e) =>
                val norme = TypedExpr.normalize(e).getOrElse(e)
                (n, r, norme)
              }
              Program(typeEnv, normalLets, extDefs, stmts)
            }
            .left
            .map(PackageError.TypeErrorIn(_, p))

          val inference = Validated.fromEither(inferenceEither).leftMap(NonEmptyList.of(_))

          defRecursionCheck *> circularCheck *> totalityCheck *> inference
        }
    }
  }
}


sealed abstract class PackageError {
  def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize): String

  protected def getMapSrc(sourceMap: Map[PackageName, (LocationMap, String)], pack: PackageName): (LocationMap, String) =
    sourceMap.get(pack) match {
      case None => (LocationMap(""), "<unknown source>")
      case Some(found) => found
    }
}

object PackageError {
  def showTypes(pack: PackageName, tpes: List[Type]): Map[Type, String] =
    TypeRef.fromTypes(Some(pack), tpes).map { case (k, v) =>
      (k, v.toDoc.render(80))
    }.toMap

  def nearest[A](ident: Identifier, existing: Map[Identifier, A], count: Int): List[(Identifier, A)] =
    existing
      .iterator
      .map { case (i, a) =>
        val d = EditDistance(ident.asString.toIterable, i.asString.toIterable)
        (i, d, a)
      }
      .filter(_._2 < ident.asString.length) // don't show things that require total edits
      .toList
      .sortBy { case (_, d, _) => d }
      .distinct
      .take(count)
      .map { case (i, _, a) => (i, a) }

  case class UnknownExport[A](ex: ExportedName[A],
    in: PackageName,
    lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[Declaration])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, in)
      val header =
        s"in $sourceName unknown export ${ex.name}"
      val candidateMap: Map[Identifier, Region] =
        lets.map { case (n, _, expr) => (n, HasRegion.region(expr)) }.toMap
      val candidates =
        nearest(ex.name, candidateMap, 3)
          .map { case (n, r) =>
            val pos = lm.toLineCol(r.start).map { case (l, c) => s" at line: ${l + 1}, column: ${c + 1}" }.getOrElse("")
            s"${n.asString}$pos"
          }
      val candstr = candidates.mkString("\n\t", "\n\t", "\n")
      val suggestion =
        if (candidates.nonEmpty) "\n" + s"perhaps you meant:$candstr"
        else ""
      header + suggestion
    }
  }

  case class UnknownImportPackage[A, B, C](pack: PackageName, from: Package[PackageName, A, B, C]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (_, sourceName) = getMapSrc(sourceMap, from.name)
      s"in $sourceName package ${from.name.asString} imports unknown package ${pack.asString}"
    }
  }

  case class DuplicatedImport(duplicates: NonEmptyList[(PackageName, ImportedName[Unit])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) =
      duplicates.sortBy(_._2.localName)
        .toList
        .iterator
        .map { case (pack, imp) =>
          val (_, sourceName) = getMapSrc(sourceMap, pack)
          s"duplicate import in $sourceName package ${pack.asString} imports ${imp.originalName} as ${imp.localName}"
        }
        .mkString("\n")
  }

  // We could check if we forgot to export the name in the package and give that error
  case class UnknownImportName[A, B](
    in: PackageName,
    importing: Package.Inferred,
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
        val ls = importing
          .program
          .lets

        val (_, sourceName) = getMapSrc(sourceMap, in)
        val letMap = ls.iterator.map { case (n, _, _) => (n: Identifier, ()) }.toMap
        letMap
          .get(iname.originalName) match {
            case Some(_) =>
              s"in $sourceName package: ${importing.name.asString} has ${iname.originalName.sourceCodeRepr} but it is not exported. Add to exports"
            case None =>
              val near = nearest(iname.originalName, letMap, 3)
                .map { case (n, _) => n.sourceCodeRepr }
                .mkString(" Nearest: ", ", ", "")
              s"in $sourceName package: ${importing.name.asString} does not have name ${iname.originalName.sourceCodeRepr}.$near"
          }
      }
    }

  case class UnknownImportFromInterface[A, B](
    in: PackageName,
    importing: Package.Interface,
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {

        val (_, sourceName) = getMapSrc(sourceMap, in)
        val exportMap = importing.exports.map { e => (e.name, ()) }.toMap
        val near = nearest(iname.originalName, exportMap, 3)
          .map { case (n, _) => n.asString }
          .mkString(" Nearest: ", ", ", "")
        s"in $sourceName package: ${importing.name} does not have name ${iname.originalName}.$near"
      }
    }

  case class CircularDependency[A, B, C](from: Package[PackageName, A, B, C], path: NonEmptyList[PackageName]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val packs = from.name :: (path.toList)
      val msg = packs.map { p =>
        val (_, src) = getMapSrc(sourceMap, p)
        s"${p.asString} in $src"
      }
      val tab = "\n\t"
      s"circular package dependency:\n${msg.mkString(tab)}"
    }
  }

  case class CircularType[A](from: PackageName, path: NonEmptyList[rankn.DefinedType[A]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      s"circular types in ${from.asString} " + path.toList.reverse.map(_.name.ident.asString).mkString(" -> ")
    }
  }

  case class VarianceInferenceFailure(from: PackageName, failed: NonEmptyList[rankn.DefinedType[Unit]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      s"failed to infer variance in ${from.asString} of " + failed.toList.map(_.name.ident.asString).sorted.mkString(", ")
    }
  }

  case class TypeErrorIn(tpeErr: Infer.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val teMessage = tpeErr match {
        case Infer.Error.NotUnifiable(t0, t1, r0, r1) =>
          val context0 =
            if (r0 == r1) Doc.space // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0, 2, errColor).getOrElse(Doc.str(r0)) // we should highlight the whole region
              Doc.hardLine + m + Doc.hardLine
            }
          val context1 =
            lm.showRegion(r1, 2, errColor).getOrElse(Doc.str(r1)) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          val doc = Doc.text("type error: expected type ") + Doc.text(tmap(t0)) +
            context0 + Doc.text("to be the same as type ") + Doc.text(tmap(t1)) +
            Doc.hardLine + context1

          doc.render(80)
        case Infer.Error.VarNotInScope((pack, name), scope, region) =>
          val ctx = lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))
          val candidates: List[String] =
            nearest(name, scope.map { case ((_, n), _) => (n, ()) }, 3)
              .map { case (n, _) => n.asString }

          val cmessage =
            if (candidates.nonEmpty) candidates.mkString("\nClosest: ", ", ", ".\n")
            else ""
          val qname = "\"" + name.sourceCodeRepr + "\""
          (Doc.text("name ") + Doc.text(qname) + Doc.text(" unknown.") + Doc.text(cmessage) + Doc.hardLine +
            ctx).render(80)
        case Infer.Error.SubsumptionCheckFailure(t0, t1, r0, r1, tvs) =>
          val context0 =
            if (r0 == r1) Doc.space // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0, 2, errColor).getOrElse(Doc.str(r0)) // we should highlight the whole region
              Doc.hardLine + m + Doc.hardLine
            }
          val context1 =
            lm.showRegion(r1, 2, errColor).getOrElse(Doc.str(r1)) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          val doc = Doc.text("type ") + Doc.text(tmap(t0)) + context0 +
            Doc.text("does not subsume type ") + Doc.text(tmap(t1)) + Doc.hardLine +
            context1

          doc.render(80)
        case uc@Infer.Error.UnknownConstructor((p, n), region, _) =>
          val near = nearest(n, uc.knownConstructors.map { case (_, n) => (n, ()) }.toMap, 3)
            .map { case (n, _) => n.asString }

          val nearStr =
            if (near.isEmpty) ""
            else near.mkString(", nearest: ", ", ", "")

          val context =
            lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region)) // we should highlight the whole region

          val doc = Doc.text("unknown constructor ") + Doc.text(n.asString) +
            Doc.text(nearStr) + Doc.hardLine + context
          doc.render(80)
        case err => err.message
      }
      // TODO use the sourceMap/regiouns in Infer.Error
      s"in file: $sourceName, package ${pack.asString}, $teMessage"
    }
  }

  case class SourceConverterErrorIn(err: SourceConverter.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val msg = {
        val context =
          lm.showRegion(err.region, 2, errColor)
            .getOrElse(Doc.str(err.region)) // we should highlight the whole region

        Doc.text(err.message) + Doc.hardLine + context
      }
      val doc = Doc.text("in file: ") + Doc.text(sourceName) + Doc.text(", package ") + Doc.text(pack.asString) +
        Doc.text(", ") + msg

      doc.render(80)
    }
  }

  case class TotalityCheckError(pack: PackageName, err: TotalityCheck.ExprError[Declaration]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val region = err.matchExpr.tag.region
      val context1 =
        lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region)) // we should highlight the whole region
      val teMessage = err match {
        case TotalityCheck.NonTotalMatch(_, missing) =>
          val allTypes = missing.traverse(_.traverseType { t => Writer(Chain.one(t), ()) })
            .run._1.toList.distinct
          val showT = showTypes(pack, allTypes)

          val doc = Pattern.compiledDocument(Document.instance[Type] { t =>
            Doc.text(showT(t))
          })

          Doc.text("non-total match, missing: ") +
            (Doc.intercalate(Doc.char(',') + Doc.lineOrSpace,
              missing.toList.map(doc.document(_))))
        case TotalityCheck.InvalidPattern(_, err) =>
          import TotalityCheck._
          err match {
            case ArityMismatch((_, n), _, _, exp, found) =>
              Doc.text(s"arity mismatch: ${n.asString} expected $exp parameters, found $found")
            case UnknownConstructor((_, n), _, _) =>
              Doc.text(s"unknown constructor: ${n.asString}")
            case MultipleSplicesInPattern(_, _) =>
              Doc.text("multiple splices in pattern, only one per match allowed")
          }
      }
      // TODO use the sourceMap/regions in Infer.Error
      val doc = Doc.text(s"in file: $sourceName, package ${pack.asString}") + Doc.hardLine +
        context1 + Doc.hardLine + teMessage

      doc.render(80)
    }
  }

  case class RecursionError(pack: PackageName, err: DefRecursionCheck.RecursionError) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val ctx = lm.showRegion(err.region, 2, errColor)
        .getOrElse(Doc.str(err.region)) // we should highlight the whole region
      val errMessage = err.message
      // TODO use the sourceMap/regions in RecursionError
      val doc = Doc.text(s"in file: $sourceName, package ${pack.asString}, $errMessage") +
        Doc.hardLine + ctx + Doc.hardLine

      doc.render(80)
    }
  }
}
