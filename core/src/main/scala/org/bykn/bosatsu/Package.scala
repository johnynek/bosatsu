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

  type Header =
    (PackageName, List[Import[PackageName, Unit]], List[ExportedName[Unit]])

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

  def headerParser(defaultPack: Option[PackageName]): P0[Header] = {
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

    (pname, im, Parser.nonEmptyListToList(ex)).tupled
  }

  def parser(
      defaultPack: Option[PackageName]
  ): P0[Package[PackageName, Unit, Unit, List[Statement]]] = {
    val body: P0[List[Statement]] = Statement.parser
    (headerParser(defaultPack), body)
      .mapN { case ((p, i, e), b) => Package(p, i, e, b) }
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
      .toProgram(p, imps.map(i => i.copy(pack = i.pack.name)), stmts)
      .leftMap { scerrs =>
        scerrs.groupByNem(_.region)
          .transform { (region, errs) =>
            val uniqs = ListUtil.distinctByHashSet(errs.toNonEmptyList)
            PackageError.SourceConverterErrorsIn(region, uniqs, p): PackageError
          }
          .toNonEmptyList
      }

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
              necError =>
                necError
                  .map(PackageError.KindInferenceError(p, _, typeDefRegions))
                  .toNonEmptyList,
              infDTs =>
                ParsedTypeEnv(infDTs, parsedTypeEnv.externalDefs)
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

  /** The parsed representation of the predef.
    */
  lazy val predefPackage: Package.Parsed =
    parser(None).parse(Predef.predefString) match {
      case Right((_, pack)) =>
        // Make function defs:
        def paramType(n: Int) =
          (TypeRef.TypeVar(s"i$n"), Some(Kind.Arg(Variance.contra, Kind.Type)))
        def makeFns(
            n: Int,
            typeArgs: List[(TypeRef.TypeVar, Option[Kind.Arg])],
            acc: List[Statement.ExternalStruct]
        ): List[Statement.ExternalStruct] =
          if (n > Type.FnType.MaxSize) acc
          else {
            val fn = Statement.ExternalStruct(
              Identifier.Constructor(s"Fn$n"),
              typeArgs
            )(Region(0, 1))
            val acc1 = fn :: acc
            makeFns(n + 1, paramType(n) :: typeArgs, acc1)
          }

        val out = (TypeRef.TypeVar("z"), Some(Kind.Arg(Variance.co, Kind.Type)))
        val allFns = makeFns(1, paramType(0) :: out :: Nil, Nil).reverse
        val exported = allFns.map { extstr =>
          ExportedName.TypeName(extstr.name, ())
        }
        // Add functions into the predef
        pack.copy(
          exports = exported ::: pack.exports,
          program = allFns ::: pack.program
        )
      case Left(err) =>
        val idx = err.failedAtOffset
        val lm = LocationMap(Predef.predefString)
        val errorMsg =
          s"couldn't parse predef:\n\n${lm.showContext(idx, 2, LocationMap.Colorize.None)}\n\nwith errs: ${err}"
        System.err.println(errorMsg)
        sys.error(errorMsg)
    }

  implicit val documentPackage: Document[Package.Typed[Any]] =
    new Document[Package.Typed[Any]] {
      def document(pack: Typed[Any]): Doc =
        Doc.text("package: ") + Doc.text(pack.name.asString) + {
          val lines = Doc.hardLine
          val imps = Doc.text("imports: ") + Doc
            .intercalate(
              Doc.line,
              pack.imports.map { imp =>
                Doc.text(imp.pack.name.asString) + Doc.space + (Doc.char(
                  '['
                ) + Doc.line +
                  Doc.intercalate(
                    Doc.comma + Doc.line,
                    imp.items.toList.map { imp =>
                      Doc.text(imp.originalName.sourceCodeRepr)
                    }
                  ) + Doc.line + Doc.char(']')).grouped
              }
            )
            .nested(4)

          val exports = Doc.text("exports: ") + Doc
            .intercalate(
              Doc.line,
              pack.exports.map { exp =>
                Doc.text(exp.name.sourceCodeRepr)
              }
            )
            .grouped
            .nested(4)

          val tpes = Doc.text("types: ") + Doc
            .intercalate(
              Doc.comma + Doc.line,
              pack.program.types.definedTypes.toList.map { case (_, t) =>
                Doc.text(t.name.ident.sourceCodeRepr)
              }
            )
            .grouped
            .nested(4)

          val eqDoc = Doc.text(" = ")
          val exprs = Doc.intercalate(
            Doc.hardLine + Doc.hardLine,
            pack.program.lets.map { case (n, _, te) =>
              Doc.text(n.sourceCodeRepr) + eqDoc + te.repr
            }
          )

          val all = lines :: imps :: exports :: tpes :: exprs :: Nil

          Doc.intercalate(Doc.hardLine, all)
        }.nested(4)
    }

  implicit val documentInterface: Document[Interface] =
    new Document[Interface] {
      def document(iface: Interface): Doc =
        Doc.text("interface: ") + Doc.text(iface.name.asString) + {
          val lines = Doc.hardLine

          val exports = Doc.text("exports: ") + Doc
            .intercalate(
              Doc.line,
              iface.exports.map { exp =>
                Doc.text(exp.name.sourceCodeRepr)
              }
            )
            .grouped
            .nested(4)

          val all = lines :: exports :: Nil

          Doc.intercalate(Doc.hardLine, all)
        }.nested(4)
    }
}
