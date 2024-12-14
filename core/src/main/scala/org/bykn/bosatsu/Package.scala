package org.bykn.bosatsu

import cats.{Functor, Order, Parallel, Applicative}
import cats.data.{Ior, ValidatedNel, Validated, NonEmptyList}
import cats.syntax.all._
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

  def withImport(i: Import[A, B]): Package[A, B, C, D] =
    copy(imports = i :: imports)

  def mapProgram[D1](fn: D => D1): Package[A, B, C, D1] =
    Package(name, imports, exports, fn(program))

  def replaceImports[A1, B1](
      newImports: List[Import[A1, B1]]
  ): Package[A1, B1, C, D] =
    Package(name, newImports, exports, program)

  def getExport[T](i: ImportedName[T]): Option[NonEmptyList[ExportedName[C]]] = {
    val iname = i.originalName
    NonEmptyList.fromList(exports.filter(_.name == iname))
  }
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
  type ResolvedPackage =
    Package[Resolved, Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]

  type TypedProgram[T] = (
        Program[TypeEnv[Kind.Arg], TypedExpr[T], Any],
        ImportMap[Interface, NonEmptyList[Referant[Kind.Arg]]]
    )
  type Typed[T] = Package[
    Interface,
    NonEmptyList[Referant[Kind.Arg]],
    Referant[
      Kind.Arg
    ],
    TypedProgram[T]
  ]
  type Inferred = Typed[Declaration]

  type Header =
    (PackageName, List[Import[PackageName, Unit]], List[ExportedName[Unit]])

  val typedFunctor: Functor[Typed] =
    new Functor[Typed] {
      def map[A, B](fa: Typed[A])(fn: A => B): Typed[B] = {
        val mapLet = fa.lets.map { case (n, r, te) =>
          (n, r, Functor[TypedExpr].map(te)(fn))
        }
        val (prog, imap) = fa.program
        fa.copy(program = (prog.copy(lets = mapLet), imap))
      }
    }

  /** Return the last binding in the file with the test type
    */
  def testValue[A](
      tp: Typed[A]
  ): Option[(Identifier.Bindable, RecursionKind, TypedExpr[A])] =
    tp.lets.filter { case (_, _, te) =>
      te.getType == Type.TestType
    }.lastOption

  def mainValue[A](
      tp: Typed[A]
  ): Option[(Identifier.Bindable, RecursionKind, TypedExpr[A])] =
    tp.lets.lastOption

  /** Discard any top level values that are not referenced, exported, the final
    * test value, or the final expression
    *
    * This is used to remove private top levels that were inlined.
    */
  def discardUnused[A](tp: Typed[A]): Typed[A] = {
    val pinned: Set[Identifier] =
      tp.exports.iterator.map(_.name).toSet ++
        tp.lets.lastOption.map(_._1) ++
        testValue(tp).map(_._1)

    def topLevels(s: Set[(PackageName, Identifier)]): Set[Identifier] =
      s.collect { case (p, i) if p === tp.name => i }

    val letWithGlobals = tp.lets.map { case tup @ (_, _, te) =>
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
    tp.copy(program = (tp.program._1.copy(lets = reachedLets), tp.program._2))
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
    t.copy(program = (t.program._1.copy(from = newFrom), t.program._2))

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
    val spaceComment: P0[Unit] =
      (Parser.spaces.? ~ CommentStatement.commentPart.?).void

    val eol = spaceComment <* Parser.termination
    val parsePack = Padding
      .parser(
        (P.string("package").soft ~ spaces) *> PackageName.parser <* eol,
        spaceComment
      )
      .map(_.padded)
    val pname: P0[PackageName] =
      defaultPack match {
        case None    => parsePack
        case Some(p) => parsePack.?.map(_.getOrElse(p))
      }

    val im =
      Padding.parser(Import.parser <* eol, spaceComment).map(_.padded).rep0
    val ex = Padding
      .parser(
        (P.string("export")
          .soft ~ spaces) *> ExportedName.parser.itemsMaybeParens
          .map(_._2) <* eol,
        spaceComment
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
        scerrs
          .groupByNem(_.region)
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

    lazy val extDefRegions: Map[Identifier.Bindable, Region] =
      stmts.iterator.collect { case ed: Statement.ExternalDef =>
        ed.name -> ed.region
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
              infDTs => ParsedTypeEnv(infDTs, parsedTypeEnv.externalDefs)
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

          val theseExternals =
            parsedTypeEnv.externalDefs.collect {
              case (pack, b, t) if pack === p =>
                // by construction this has to have all the regions
                (b, (t, extDefRegions(b)))
            }.toMap

          val inferenceEither = Infer
            .typeCheckLets(p, lets, theseExternals)
            .runFully(
              withFQN,
              Referant.typeConstructors(imps) ++ typeEnv.typeConstructors,
              fullTypeEnv.toKindMap
            )
            .map { lets =>
              (fullTypeEnv, Program(typeEnv, lets, extDefs, stmts))
            }
            .left
            .map(PackageError.TypeErrorIn(_, p, lets, theseExternals))

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
              pack.types.definedTypes.toList.map { case (_, t) =>
                Doc.text(t.name.ident.sourceCodeRepr)
              }
            )
            .grouped
            .nested(4)

          val eqDoc = Doc.text(" = ")
          val exprs = Doc.intercalate(
            Doc.hardLine + Doc.hardLine,
            pack.lets.map { case (n, _, te) =>
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

  implicit class TypedMethods[A](private val pack: Typed[A]) extends AnyVal {
    def lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[A])] =
      pack.program._1.lets

    def types: TypeEnv[Kind.Arg] =
      pack.program._1.types

    def externalDefs: List[Identifier.Bindable] =
      pack.program._1.externalDefs

    def localImport(n: Identifier): Option[
      (Package.Interface, ImportedName[NonEmptyList[Referant[Kind.Arg]]])
    ] =
      pack.program._2(n)

    def filterLets(fn: Identifier => Boolean): Typed[A] = {
      val (prog, importMap) = pack.program
      val prog1 = prog.copy(lets = prog.lets.filter { case (b, _, _) => fn(b) })
      pack.copy(program = (prog1, importMap))
    }

    def getImport[B](
        inside: PackageName,
        i: ImportedName[B]
    ): Either[PackageError, ImportedName[NonEmptyList[Referant[Kind.Arg]]]] = {
      pack.getExport(i) match {
        case Some(exps) =>
          val bs = exps.map(_.tag)
          Right(i.map(_ => bs))
        case None =>
          Left(
            PackageError.UnknownImportName(
              inside,
              pack.name,
              pack.program._1.lets.iterator.map { case (n, _, _) =>
                (n: Identifier, ())
              }.toMap,
              i,
              pack.exports
            )
          )
        }
      }
  }

  implicit class IfaceMethods(private val iface: Interface) extends AnyVal {
    def getImportIface[A](
        inside: PackageName,
        i: ImportedName[A]
    ): Either[PackageError, ImportedName[NonEmptyList[Referant[Kind.Arg]]]] = {
      iface.getExport(i) match {
        case Some(exps) =>
          val bs = exps.map(_.tag)
          Right(i.map(_ => bs))
        case None =>
          Left(
            PackageError.UnknownImportFromInterface(
              inside,
              iface.name,
              iface.exports.map(_.name),
              i,
              iface.exports
            )
          )
      }
    }
  }

  implicit class ResolvedMethods(private val resolved: Resolved) extends AnyVal {
    def name: PackageName =
      FixType.unfix(resolved) match {
        case Left(iface) => iface.name
        case Right(pack) => pack.name
      }

    def importName[F[_], A](
      fromPackage: PackageName,
      item: ImportedName[Unit]
    )(recurse: ResolvedPackage => F[Typed[A]])(implicit F: Applicative[F]): F[Either[PackageError, (Package.Interface, ImportedName[NonEmptyList[Referant[Kind.Arg]]])]] =
      Package.unfix(resolved) match {
        case Right(p) =>
          /*
            * Here we have a source we need to fully resolve
            */
          recurse(p)
            .map { packF =>
              val packInterface = Package.interfaceOf(packF)
              packF.getImport(fromPackage, item)
                .map((packInterface, _))
            }
        case Left(iface) =>
          /*
            * this import is already an interface, we can stop here
            */
          // this is very fast and does not need to be done in a thread
          F.pure(iface.getImportIface(fromPackage, item)
              .map((iface, _)))
      }
  }

  def orderByName[A, B, C, D]: Order[Package[A, B, C, D]] =
    Order.by[Package[A, B, C, D], PackageName](_.name)
}
