package org.bykn.bosatsu

import cats.{Functor, Parallel}
import cats.data.{Ior, ValidatedNel, Validated, NonEmptyList}
import cats.implicits._
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{Doc, Document}
import scala.util.hashing.MurmurHash3

import rankn._
import Parser.{spaces, Combinators}

import FixType.Fix

/** Represents a package over its life-cycle: from parsed to resolved to
  * inferred
  */
final case class Package[A, B, C, +D](
    name: PackageName,
    imports: List[Import[A, B]],
    exports: List[ExportedName[C]],
    program: D
) {

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

  def replaceImports[A1, B1](
      newImports: List[Import[A1, B1]]
  ): Package[A1, B1, C, D] =
    Package(name, newImports, exports, program)
}

object Package {
  type Interface = Package[Nothing, Nothing, Referant[Kind.Arg], Unit]

  /** This is a package whose import type is Either: 1 a package of the same
    * kind 2 an interface
    */
  type FixPackage[B, C, D] = Fix[λ[a => Either[Interface, Package[a, B, C, D]]]]
  type PackageF[A, B, C] =
    Either[Interface, Package[FixPackage[A, B, C], A, B, C]]
  type PackageF2[A, B] = PackageF[A, A, B]
  type Parsed = Package[PackageName, Unit, Unit, List[Statement]]
  type Resolved =
    FixPackage[Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]
  type Typed[T] = Package[Interface, NonEmptyList[Referant[Kind.Arg]], Referant[
    Kind.Arg
  ], Program[TypeEnv[Kind.Arg], TypedExpr[T], Any]]
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

  /** Return the last binding in the file with the test type
    */
  def testValue[A](
      tp: Typed[A]
  ): Option[(Identifier.Bindable, RecursionKind, TypedExpr[A])] =
    tp.program.lets.filter { case (_, _, te) =>
      te.getType == Type.TestType
    }.lastOption

  /** Discard any top level values that are not referenced, exported, the final
    * test value, or the final expression
    *
    * This is used to remove private top levels that were inlined.
    */
  def discardUnused[A](tp: Typed[A]): Typed[A] = {
    val pinned: Set[Identifier] =
      tp.exports.iterator.map(_.name).toSet ++
        tp.program.lets.lastOption.map(_._1) ++
        testValue(tp).map(_._1)

    def topLevels(s: Set[(PackageName, Identifier)]): Set[Identifier] =
      s.collect { case (p, i) if p === tp.name => i }

    val letWithGlobals = tp.program.lets.map { case tup @ (_, _, te) =>
      (tup, topLevels(te.globals))
    }

    @annotation.tailrec
    def loop(reached: Set[Identifier]): Set[Identifier] = {
      val step = letWithGlobals
        .foldMap { case ((bn, _, _), tops) =>
          if (reached(bn)) tops else Set.empty[Identifier]
        }

      if (step.forall(reached)) reached
      else loop(step | reached)
    }

    val reached = loop(pinned)

    val reachedLets = letWithGlobals.collect {
      case (tup @ (bn, _, _), _) if reached(bn) => tup
    }
    tp.copy(program = tp.program.copy(lets = reachedLets))
  }

  def fix[A, B, C](p: PackageF[A, B, C]): FixPackage[A, B, C] =
    FixType.fix[λ[a => Either[Interface, Package[a, A, B, C]]]](p)

  def unfix[A, B, C](fp: FixPackage[A, B, C]): PackageF[A, B, C] =
    FixType.unfix[λ[a => Either[Interface, Package[a, A, B, C]]]](fp)

  /** build a Parsed Package from a Statement. This is useful for testing or
    * library usages.
    */
  def fromStatements(pn: PackageName, stmts: List[Statement]): Package.Parsed =
    Package(pn, Nil, Nil, stmts)

  def interfaceOf[A](inferred: Typed[A]): Interface =
    inferred.mapProgram(_ => ()).replaceImports(Nil)

  def setProgramFrom[A, B](t: Typed[A], newFrom: B): Typed[A] =
    t.copy(program = t.program.copy(from = newFrom))

  implicit val document
      : Document[Package[PackageName, Unit, Unit, List[Statement]]] =
    Document.instance[Package.Parsed] {
      case Package(name, imports, exports, statments) =>
        val p =
          Doc.text("package ") + Document[PackageName].document(name) + Doc.line
        val i = imports match {
          case Nil => Doc.empty
          case nonEmptyImports =>
            Doc.line +
              Doc.intercalate(
                Doc.line,
                nonEmptyImports.map(
                  Document[Import[PackageName, Unit]].document _
                )
              ) +
              Doc.line
        }
        val e = exports match {
          case Nil => Doc.empty
          case nonEmptyExports =>
            Doc.line +
              Doc.text("export ") +
              Doc.intercalate(
                Doc.text(", "),
                nonEmptyExports.map(Document[ExportedName[Unit]].document _)
              ) +
              Doc.line
        }
        val b = statments.map(Document[Statement].document(_))
        Doc.intercalate(Doc.empty, p :: i :: e :: b)
    }

  def parser(
      defaultPack: Option[PackageName]
  ): P0[Package[PackageName, Unit, Unit, List[Statement]]] = {
    // TODO: support comments before the Statement
    val parsePack = Padding
      .parser(
        (P.string("package")
          .soft ~ spaces) *> PackageName.parser <* Parser.toEOL
      )
      .map(_.padded)
    val pname: P0[PackageName] =
      defaultPack match {
        case None    => parsePack
        case Some(p) => parsePack.?.map(_.getOrElse(p))
      }

    val im = Padding.parser(Import.parser <* Parser.toEOL).map(_.padded).rep0
    val ex = Padding
      .parser(
        (P.string("export")
          .soft ~ spaces) *> ExportedName.parser.itemsMaybeParens
          .map(_._2) <* Parser.toEOL
      )
      .map(_.padded)
    val body: P0[List[Statement]] = Statement.parser
    (pname, im, Parser.nonEmptyListToList(ex), body)
      .mapN { (p, i, e, b) => Package(p, i, e, b) }
  }

  /** After having type checked the imports, we now type check the body in order
    * to type check the exports
    *
    * This is used by test code
    */
  def inferBody(
      p: PackageName,
      imps: List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]],
      stmts: List[Statement]
  ): Ior[NonEmptyList[PackageError], Program[TypeEnv[Kind.Arg], TypedExpr[
    Declaration
  ], List[Statement]]] =
    inferBodyUnopt(p, imps, stmts).map { case (fullTypeEnv, prog) =>
      TypedExprNormalization.normalizeProgram(p, fullTypeEnv, prog)
    }

  /** Infer the types but do not optimize/normalize the lets
    */
  def inferBodyUnopt(
      p: PackageName,
      imps: List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]],
      stmts: List[Statement]
  ): Ior[NonEmptyList[
    PackageError
  ], (TypeEnv[Kind.Arg], Program[TypeEnv[Kind.Arg], TypedExpr[Declaration], List[Statement]])] = {

    // here we make a pass to get all the local names
    val optProg = SourceConverter
      .toProgram(p, imps.map { i => i.copy(pack = i.pack.name) }, stmts)
      .leftMap(
        _.map(
          PackageError.SourceConverterErrorIn(_, p): PackageError
        ).toNonEmptyList
      )

    lazy val typeDefRegions: Map[Type.Const.Defined, Region] =
      stmts.iterator.collect { case tds: TypeDefinitionStatement =>
        Type.Const.Defined(p, TypeName(tds.name)) -> tds.region
      }.toMap

    optProg.flatMap {
      case Program((importedTypeEnv, parsedTypeEnv), lets, extDefs, _) =>
        val inferVarianceParsed
            : Ior[NonEmptyList[PackageError], ParsedTypeEnv[Kind.Arg]] =
          KindFormula
            .solveShapesAndKinds(
              importedTypeEnv,
              parsedTypeEnv.allDefinedTypes.reverse
            )
            .bimap(
              { necError =>
                necError
                  .map(PackageError.KindInferenceError(p, _, typeDefRegions))
                  .toNonEmptyList
              },
              { infDTs =>
                ParsedTypeEnv(infDTs, parsedTypeEnv.externalDefs)
              }
            )

        inferVarianceParsed.flatMap { parsedTypeEnv =>
          /*
           * Check that all recursion is allowable
           */
          val defRecursionCheck: ValidatedNel[PackageError, Unit] =
            stmts
              .traverse_(DefRecursionCheck.checkStatement(_))
              .leftMap { badRecursions =>
                badRecursions.map(PackageError.RecursionError(p, _))
              }

          val typeEnv: TypeEnv[Kind.Arg] = TypeEnv.fromParsed(parsedTypeEnv)

          /*
           * These are values, including all constructor functions
           * that have been imported, this includes local external
           * defs
           */
          val withFQN: Map[(Option[PackageName], Identifier), Type] = {
            val fqn =
              Referant
                .fullyQualifiedImportedValues(imps)(_.name)
                .iterator
                .map { case ((p, n), t) => ((Some(p), n), t) }

            // these are local construtors/externals
            val localDefined =
              typeEnv
                .localValuesOf(p)
                .iterator
                .map { case (n, t) => ((Some(p), n), t) }

            (fqn ++ localDefined).toMap
          }

          val fullTypeEnv = importedTypeEnv ++ typeEnv
          val totalityCheck =
            lets
              .traverse { case (_, _, expr) =>
                TotalityCheck(fullTypeEnv).checkExpr(expr)
              }
              .leftMap { errs =>
                errs.map(PackageError.TotalityCheckError(p, _))
              }

          val inferenceEither = Infer
            .typeCheckLets(p, lets)
            .runFully(
              withFQN,
              Referant.typeConstructors(imps) ++ typeEnv.typeConstructors,
              fullTypeEnv.toKindMap
            )
            .map { lets =>
              (fullTypeEnv, Program(typeEnv, lets, extDefs, stmts))
            }
            .left
            .map(PackageError.TypeErrorIn(_, p))

          val checkUnusedLets =
            lets
              .traverse_ { case (_, _, expr) =>
                UnusedLetCheck.check(expr)
              }
              .leftMap { errs =>
                NonEmptyList.one(
                  PackageError.UnusedLetError(p, errs.toNonEmptyList)
                )
              }

          /*
           * Checks accumulate errors, but have no return value:
           * warning: if we refactor this from validated, we need parMap on Ior to get this
           * error accumulation
           */
          val checks = List(
            defRecursionCheck,
            checkUnusedLets,
            totalityCheck
          ).sequence_

          val inference =
            Validated.fromEither(inferenceEither).leftMap(NonEmptyList.of(_))

          Parallel[Ior[NonEmptyList[PackageError], *]]
            .parProductR(checks.toIor)(inference.toIor)
        }
    }
  }

  def checkValuesHaveExportedTypes[V](
      pn: PackageName,
      exports: List[ExportedName[Referant[V]]]
  ): List[PackageError] = {
    val exportedTypes: List[DefinedType[V]] = exports.iterator
      .map(_.tag)
      .collect {
        case Referant.Constructor(dt, _) => dt
        case Referant.DefinedT(dt)       => dt
      }
      .toList
      .distinct

    val exportedTE = TypeEnv.fromDefinitions(exportedTypes)

    type Exp = ExportedName[Referant[V]]
    val usedTypes: Iterator[(Type.Const, Exp, Type)] = exports.iterator
      .flatMap { n =>
        n.tag match {
          case Referant.Value(t) => Iterator.single((t, n))
          case _                 => Iterator.empty
        }
      }
      .flatMap { case (t, n) => Type.constantsOf(t).map((_, n, t)) }
      .filter { case (Type.Const.Defined(p, _), _, _) => p === pn }

    def errorFor(t: (Type.Const, Exp, Type)): List[PackageError] =
      exportedTE.toDefinedType(t._1) match {
        case None =>
          if (Type.TyConst(t._1) != Type.FnType)
            PackageError.PrivateTypeEscape(t._2, t._3, pn, t._1) :: Nil
          else {
            // Fn is kind of a virtual type that is not defined as data or external
            Nil
          }
        case Some(_) => Nil
      }

    usedTypes.flatMap(errorFor).toList
  }

  /** The parsed representation of the predef.
    */
  val predefPackage: Package.Parsed =
    parser(None).parse(Predef.predefString) match {
      case Right((_, pack)) => pack
      case Left(err) =>
        val idx = err.failedAtOffset
        val lm = LocationMap(Predef.predefString)
        sys.error(s"couldn't parse predef: ${lm
            .showContext(idx, 2, LocationMap.Colorize.None)} with errs: ${err}")
    }
}
