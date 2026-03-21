package dev.bosatsu

import cats.{Applicative, Foldable, Functor, Order}
import cats.data.{Chain, Ior, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
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
    MurmurHash3.caseClassHash(this)

  override def equals(that: Any): Boolean =
    that match {
      case p: Package[_, _, _, _] =>
        (this eq p) || {
          name.equals(p.name) &&
          imports.equals(p.imports) &&
          exports.equals(p.exports) &&
          program.equals(p.program)
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

  def getExport[T](
      i: ImportedName[T]
  ): Option[NonEmptyList[ExportedName[C]]] = {
    val iname = i.originalName
    NonEmptyList.fromList(exports.filter(_.name == iname))
  }

  def visibleDepPackages[K](implicit ev: C <:< Referant[K]): List[PackageName] =
    exports
      .flatMap { en =>
        val ref = ev(en.tag)
        ref.depPackages
      }
      .distinct
      .sorted

  /** These are all the types that are exported with constructors deleted for
    * opaque types
    */
  def exportedTypeEnv[K](implicit ev: C <:< Referant[K]): TypeEnv[K] = {
    type ListEN[+Z] = List[ExportedName[Z]]
    val expRef: List[ExportedName[Referant[K]]] =
      ev.substituteCo[ListEN](exports)
    TypeEnv.fromDefinitions(
      expRef
        .groupByNel { e =>
          e.tag match {
            case Referant.DefinedT(dt)       => Some(dt.toTypeConst)
            case Referant.Constructor(dt, _) => Some(dt.toTypeConst)
            case Referant.Value(_)           => None
          }
        }
        .iterator
        .collect { case (Some(_), exps) =>
          val isOpaque = exps.map(_.tag).exists {
            case Referant.Constructor(_, _) => true
            case _                          => false
          }

          val dt = exps.head.tag match {
            case Referant.Constructor(dt, _) => dt
            case Referant.DefinedT(dt)       => dt
            case Referant.Value(_)           =>
              sys.error("impossible since we have Some(tc)")
          }

          if (isOpaque) dt.toOpaque
          else dt
        }
        .toList
    )
  }
}

object Package {
  type Interface = Package[Nothing, Nothing, Referant[Kind.Arg], Unit]

  /** This is a package whose import type is Either: 1 a package of the same
    * kind 2 an interface
    */
  type FixPackage[B, C, D] =
    Fix[[A] =>> Either[Interface, Package[A, B, C, D]]]
  type PackageF[A, B, C] =
    Either[Interface, Package[FixPackage[A, B, C], A, B, C]]
  type PackageF2[A, B] = PackageF[A, A, B]
  type Parsed = Package[PackageName, Unit, Unit, List[Statement]]
  type Resolved =
    FixPackage[Unit, Unit, (List[Statement], ImportMap[PackageName, Unit])]
  type ResolvedPackage =
    Package[
      Resolved,
      Unit,
      Unit,
      (List[Statement], ImportMap[PackageName, Unit])
    ]

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
  type Compiled = Typed[Region]

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

  def toCompiled[A: HasRegion](pack: Typed[A]): Compiled = {
    val withRegions = typedFunctor.map(pack)(tag => HasRegion.region(tag))
    // Serialized compiled artifacts never retain the parsed-source payload in
    // Program.from, so drop it before crossing the cache/compiler boundary.
    setProgramFrom(withRegions, ())
  }

  sealed trait TestEntry[+A] {
    def bindable: Identifier.Bindable
    def recursionKind: RecursionKind
    def expr: TypedExpr[A]
  }

  object TestEntry {
    final case class PlainTest[+A](
        bindable: Identifier.Bindable,
        recursionKind: RecursionKind,
        expr: TypedExpr[A]
    ) extends TestEntry[A]

    final case class ProgTest[+A](
        bindable: Identifier.Bindable,
        recursionKind: RecursionKind,
        expr: TypedExpr[A]
    ) extends TestEntry[A]
  }

  sealed trait TestDiscoveryError {
    def packageName: PackageName
  }

  object TestDiscoveryError {
    final case class PlainTestAfterProgTest(
        packageName: PackageName,
        progTest: Identifier.Bindable,
        plainTestsAfter: NonEmptyList[Identifier.Bindable]
    ) extends TestDiscoveryError
  }

  private def testEntryForLet[A](
      bindable: Identifier.Bindable,
      recursionKind: RecursionKind,
      expr: TypedExpr[A]
  ): Option[TestEntry[A]] =
    if (expr.getType.sameAs(Type.TestType)) {
      Some(TestEntry.PlainTest(bindable, recursionKind, expr))
    } else if (expr.getType.sameAs(Type.ProgTestType)) {
      Some(TestEntry.ProgTest(bindable, recursionKind, expr))
    } else None

  /** Return the selected test entry for a package.
    *
    * Discovery rules:
    *   1. If there are no ProgTest values, select the last plain Test.
    *   2. If there is at least one ProgTest, select the last ProgTest.
    *   3. If a plain Test appears after the selected ProgTest, report an
    *      ordering error.
    */
  def testEntry[A](
      tp: Typed[A]
  ): Either[TestDiscoveryError, Option[TestEntry[A]]] = {
    val indexedLets = tp.lets.zipWithIndex
    val plainTests = indexedLets.collect {
      case ((name, rec, te), idx) =>
        testEntryForLet(name, rec, te).collect {
          case plainTest @ TestEntry.PlainTest(_, _, _) => (idx, plainTest)
        }
    }
      .flatten
    val progTests = indexedLets.collect {
      case ((name, rec, te), idx) =>
        testEntryForLet(name, rec, te).collect {
          case progTest @ TestEntry.ProgTest(_, _, _) => (idx, progTest)
        }
    }
      .flatten

    progTests.lastOption match {
      case None =>
        Right(plainTests.lastOption.map(_._2))
      case Some((progIdx, progTest)) =>
        val plainAfter = plainTests.collect {
          case (idx, plain) if idx > progIdx => plain.bindable
        }
        NonEmptyList.fromList(plainAfter) match {
          case Some(plainTestsAfter) =>
            Left(
              TestDiscoveryError.PlainTestAfterProgTest(
                tp.name,
                progTest.bindable,
                plainTestsAfter
              )
            )
          case None =>
            Right(Some(progTest))
        }
    }
  }

  /** Return the selected plain Test binding in the file.
    */
  def testValue[A](
      tp: Typed[A]
  ): Option[(Identifier.Bindable, RecursionKind, TypedExpr[A])] =
    testEntry(tp).toOption.flatten.collect {
      case TestEntry.PlainTest(bindable, recursionKind, expr) =>
        (bindable, recursionKind, expr)
    }

  /** Return the selected top-level test value if this bindable has test type.
    */
  def testEntryForBindable[A](
      tp: Typed[A],
      bindable: Identifier.Bindable
  ): Option[TestEntry[A]] =
    tp.lets.collectFirstSome { case (name, recursionKind, expr) =>
      if (name == bindable) testEntryForLet(name, recursionKind, expr)
      else None
    }

  def testRootBindables[A](tp: Typed[A]): Set[Identifier.Bindable] =
    testEntry(tp) match {
      case Right(Some(entry)) =>
        Set(entry.bindable)
      case Right(None) =>
        Set.empty
      case Left(TestDiscoveryError.PlainTestAfterProgTest(_, prog, plainAfter)) =>
        plainAfter.toList.toSet + prog
    }

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
    val constructorDefaultHelpers: Set[Identifier.Bindable] =
      tp.exports.iterator
        .collect {
          case ExportedName.Constructor(
                _,
                Referant.Constructor(_, constructorFn)
              ) =>
            constructorFn.args.iterator.flatMap(_.defaultBinding)
        }
        .flatten
        .toSet

    val pinned: Set[Identifier] =
      tp.exports.iterator.map(_.name).toSet ++
        tp.lets.lastOption.map(_._1) ++
        testRootBindables(tp) ++
        constructorDefaultHelpers

    def topLevels(s: Set[(PackageName, Identifier)]): Set[Identifier] =
      s.collect { case (p, i) if p == tp.name => i }

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
    FixType.fix[[X] =>> Either[Interface, Package[X, A, B, C]]](p)

  def unfix[A, B, C](fp: FixPackage[A, B, C]): PackageF[A, B, C] =
    FixType.unfix[[X] =>> Either[Interface, Package[X, A, B, C]]](fp)

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
          case Nil             => Doc.empty
          case nonEmptyImports =>
            Doc.line +
              Doc.intercalate(
                Doc.line,
                nonEmptyImports.map(
                  Document[Import[PackageName, Unit]].document
                )
              ) +
              Doc.line
        }
        val e = exports match {
          case Nil             => Doc.empty
          case nonEmptyExports =>
            Doc.line +
              Doc.text("export ") +
              Doc.intercalate(
                Doc.text(", "),
                nonEmptyExports.map(Document[ExportedName[Unit]].document)
              ) +
              Doc.line
        }
        val b = statments.map(Document[Statement].document(_))
        Doc.intercalate(Doc.empty, p :: i :: e :: b)
    }

  def headerParser: P[Header] = {
    val spaceComment: P0[Unit] =
      (Parser.spaces.? ~ CommentStatement.commentPart.?).void

    val eol = spaceComment <* Parser.termination
    val parsePack = Padding
      .parser(
        (P.string("package").soft ~ spaces) *> PackageName.parser <* eol,
        spaceComment
      )
      .map(_.padded)
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

    ((parsePack ~ im) ~ Parser.nonEmptyListToList(ex)).map {
      case ((p, i), e) => (p, i, e)
    }
  }

  def parser: P0[Package[PackageName, Unit, Unit, List[Statement]]] = {
    val body: P0[List[Statement]] = Statement.parser
    (headerParser, body)
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
    inferBodyUnopt(p, imps, Nil, stmts).map { case (fullTypeEnv, prog) =>
      val lowered = TypedExprLoopRecurLowering.lowerProgram(prog)
      TypedExprNormalization.normalizeProgram(p, fullTypeEnv, lowered)
    }

  private def combineInferSingles(
      errs: NonEmptyList[Infer.Error.Single]
  ): Infer.Error =
    errs.tail.foldLeft(errs.head: Infer.Error) { (acc, next) =>
      Infer.Error.Combine(acc, next)
    }

  private type ParsedLet = (Identifier.Bindable, RecursionKind, Expr[Declaration])

  private case class RemovedKindRefs(
      pack: PackageName,
      removedTypeConsts: Set[Type.Const.Defined],
      removedConstructors: Set[Identifier.Constructor]
  ) {
    def isEmpty: Boolean =
      removedTypeConsts.isEmpty && removedConstructors.isEmpty

    def hasRemovedTypeConst(tpe: Type): Boolean =
      Type.allConsts(tpe :: Nil).exists { tyConst =>
        removedTypeConsts(tyConst.tpe.toDefined)
      }

    def hasRemovedPatternRef(
        pat: Pattern[(PackageName, Identifier.Constructor), Type]
    ): Boolean =
      pat match {
        case Pattern.WildCard | Pattern.Literal(_) | Pattern.Var(_) |
            Pattern.StrPat(_) =>
          false
        case Pattern.Named(_, inner) =>
          hasRemovedPatternRef(inner)
        case Pattern.ListPat(items)  =>
          items.exists {
            case Pattern.ListPart.Item(inner) =>
              hasRemovedPatternRef(inner)
            case Pattern.ListPart.WildList | Pattern.ListPart.NamedList(_) =>
              false
          }
        case Pattern.Annotation(inner, tpe) =>
          hasRemovedTypeConst(tpe) || hasRemovedPatternRef(inner)
        case Pattern.PositionalStruct((`pack`, ctor), params) =>
          removedConstructors(ctor) || params.exists(hasRemovedPatternRef)
        case Pattern.PositionalStruct(_, params) =>
          params.exists(hasRemovedPatternRef)
        case Pattern.Union(head, tail) =>
          hasRemovedPatternRef(head) || tail.exists(hasRemovedPatternRef)
      }

    def hasRemovedExprRef[A](expr: Expr[A]): Boolean =
      expr match {
        case Expr.Annotation(inner, tpe, _) =>
          hasRemovedTypeConst(tpe) || hasRemovedExprRef(inner)
        case Expr.Local(_, _) =>
          false
        case Expr.Generic(_, inner) =>
          hasRemovedExprRef(inner)
        case Expr.Global(`pack`, ctor: Identifier.Constructor, _) =>
          removedConstructors(ctor)
        case Expr.Global(_, _, _) =>
          false
        case Expr.App(fn, args, _) =>
          hasRemovedExprRef(fn) || args.exists(hasRemovedExprRef)
        case Expr.Lambda(args, inner, _) =>
          args.exists { case (_, ot) => ot.exists(hasRemovedTypeConst) } ||
            hasRemovedExprRef(inner)
        case Expr.Let(_, bound, in, _, _) =>
          hasRemovedExprRef(bound) || hasRemovedExprRef(in)
        case Expr.Literal(_, _) =>
          false
        case Expr.Match(arg, branches, _) =>
          hasRemovedExprRef(arg) ||
            branches.exists { branch =>
              hasRemovedPatternRef(branch.pattern) ||
              branch.guard.exists(hasRemovedExprRef) ||
              hasRemovedExprRef(branch.expr)
            }
      }
  }

  private def removedKindRefs(
      pack: PackageName,
      parsedTypeEnv0: ParsedTypeEnv[Option[Kind.Arg]],
      parsedTypeEnv: ParsedTypeEnv[Kind.Arg]
  ): RemovedKindRefs = {
    val parsedTypeConsts = parsedTypeEnv.allDefinedTypes.iterator
      .map(_.toTypeConst)
      .toSet
    val removedTypeDefs =
      parsedTypeEnv0.allDefinedTypes.filterNot { dt =>
        parsedTypeConsts(dt.toTypeConst)
      }
    val removedTypeConsts = removedTypeDefs.iterator.map(_.toTypeConst).toSet
    val removedConstructors = removedTypeDefs.iterator
      .flatMap(_.constructors.iterator.map(_.name))
      .toSet

    RemovedKindRefs(pack, removedTypeConsts, removedConstructors)
  }

  private def transitiveDependents(
      pack: PackageName,
      lets: List[ParsedLet],
      roots: Set[Identifier.Bindable]
  ): Set[Identifier.Bindable] =
    if (roots.isEmpty) Set.empty
    else {
      val dependents: Map[Identifier.Bindable, Set[Identifier.Bindable]] =
        lets.iterator
          .flatMap { case (dependent, _, expr) =>
            expr.globals.iterator.collect {
              case Expr.Global(`pack`, n, _) =>
                n.toBindable.map((_, dependent))
            }.flatten
          }
          .toList
          .groupBy(_._1)
          .iterator
          .map { case (dependency, pairs) =>
            (dependency, pairs.iterator.map(_._2).toSet)
          }
          .toMap

      val blocked = scala.collection.mutable.Set.empty[Identifier.Bindable]
      val queue = scala.collection.mutable.Queue.empty[Identifier.Bindable]

      roots.foreach { root =>
        blocked += root
        queue.enqueue(root)
      }

      while (queue.nonEmpty) {
        val next = queue.dequeue()
        dependents.getOrElse(next, Set.empty).foreach { dependent =>
          if (!blocked(dependent)) {
            blocked += dependent
            queue.enqueue(dependent)
          }
        }
      }

      blocked.toSet
    }

  private def letsForTypeChecking(
      pack: PackageName,
      parsedTypeEnv0: ParsedTypeEnv[Option[Kind.Arg]],
      parsedTypeEnv: ParsedTypeEnv[Kind.Arg],
      lets: List[ParsedLet]
  ): List[ParsedLet] = {
    val removed = removedKindRefs(pack, parsedTypeEnv0, parsedTypeEnv)
    if (removed.isEmpty) lets
    else {
      val roots = lets.iterator.collect {
        case (name, _, expr) if removed.hasRemovedExprRef(expr) =>
          name
      }.toSet
      val blocked = transitiveDependents(pack, lets, roots)
      if (blocked.isEmpty) lets
      else lets.filterNot { case (name, _, _) => blocked(name) }
    }
  }

  private def diagnosticsToIor[A](
      errors: List[PackageError],
      value: => A
  ): Ior[NonEmptyList[PackageError], A] =
    NonEmptyList.fromList(errors) match {
      case None =>
        Ior.right(value)
      case Some(errs) =>
        if (errs.forall(PackageError.isPostponable)) Ior.both(errs, value)
        else Ior.left(errs)
    }

  private def toErrs[A](v: ValidatedNel[A, Unit]): List[A] =
    v match {
      case Validated.Valid(_)     => Nil
      case Validated.Invalid(errs) => errs.toList
    }

  private def toErrsChain[A](v: ValidatedNec[A, Unit]): Chain[A] =
    v match {
      case Validated.Valid(_)     => Chain.empty
      case Validated.Invalid(errs) => errs.toChain
    }

  private def toIorPostponable[F[_]: Foldable, A](
      v: Validated[F[A], Unit]
  )(
      isPostponable: A => Boolean
  ): Ior[F[A], Unit] =
    v match {
      case Validated.Valid(unit)  => Ior.right(unit)
      case Validated.Invalid(errs) =>
        if (Foldable[F].forall(errs)(isPostponable)) Ior.both(errs, ())
        else Ior.left(errs)
    }

  /** Infer the types but do not optimize/normalize the lets. `exports` can be
    * provided to run pre-inference bindable checks from the expression DAG.
    */
  def inferBodyUnopt(
      p: PackageName,
      imps: List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]],
      exports: List[ExportedName[Unit]],
      stmts: List[Statement]
  ): Ior[
    NonEmptyList[
      PackageError
    ],
    (
        TypeEnv[Kind.Arg],
        Program[TypeEnv[Kind.Arg], TypedExpr[Declaration], List[Statement]]
    )
  ] = {

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
      case Program((importedTypeEnv, parsedTypeEnv0), lets, extDefs, _) =>
        val inferVarianceParsed
            : Ior[NonEmptyList[PackageError], ParsedTypeEnv[Kind.Arg]] =
          KindFormula
            .solveShapesAndKinds(
              importedTypeEnv,
              parsedTypeEnv0.allDefinedTypes.reverse
            )
            .bimap(
              necError =>
                necError
                  .map(PackageError.KindInferenceError(p, _, typeDefRegions))
                  .toNonEmptyList,
              infDTs => ParsedTypeEnv(infDTs, parsedTypeEnv0.externalDefs)
            )

        inferVarianceParsed.flatMap { parsedTypeEnv =>
          val letsForChecks =
            letsForTypeChecking(p, parsedTypeEnv0, parsedTypeEnv, lets)

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
          val dependencyTypeEnv: TypeEnv[Kind.Arg] =
            imps.foldMap(_.pack.exportedTypeEnv)

          val theseExternals =
            parsedTypeEnv.externalDefs.collect {
              case (pack, b, t) if pack == p =>
                // by construction this has to have all the regions
                (b, (t, extDefRegions(b)))
            }.toMap

          val letNameRegions: Map[Identifier.Bindable, Region] =
            stmts.iterator
              .collect { case vs: Statement.ValueStatement =>
                val stmtStart = vs.region.start
                vs.names.iterator.map { n =>
                  // top-level value statements begin with the bound name
                  (n, Region(stmtStart, stmtStart + n.sourceCodeRepr.length))
                }
              }
              .flatten
              .toMap

          val localTypeNames: Set[TypeName] =
            fullTypeEnv.definedTypes.keysIterator.collect { case (`p`, tn) =>
              tn
            }.toSet

          val (nameCheckErrorOpt, nameCheckResult) =
            NameCheck.checkLets(p, letsForChecks, withFQN)

          val nameCheckErrors: Ior[NonEmptyList[PackageError], Unit] =
            nameCheckErrorOpt match {
              case None           =>
                Ior.right(())
              case Some(nameErrs) =>
                val mergedError =
                  combineInferSingles(
                    nameErrs.toNonEmptyList.map(err => err: Infer.Error.Single)
                  )
                // Intentionally return Left (not Both): unresolved top-level names
                // mean we cannot safely expose a partial typed package API to
                // downstream packages. We still run inference on surviving lets
                // to accumulate additional diagnostics in this package.
                Ior.left(
                  NonEmptyList.one[PackageError](
                    PackageError.TypeErrorIn(
                      mergedError,
                      p,
                      letsForChecks,
                      theseExternals,
                      letNameRegions,
                      localTypeNames
                    )
                  )
                )
            }

          val exprDagBindableChecks: Ior[NonEmptyList[PackageError], Unit] =
            toIorPostponable(
              PackageCustoms
                .checkExprDagBindables(p, imps, exports, lets, extDefs)
            )(PackageError.isPostponable).leftMap(_.toNonEmptyList)

          val inference: Ior[
            NonEmptyList[PackageError],
            (
                TypeEnv[Kind.Arg],
                Program[
                  TypeEnv[Kind.Arg],
                  TypedExpr[Declaration],
                  List[Statement]
                ]
            )
          ] = Infer
            // Blocked lets (with direct/transitive name errors) are excluded,
            // so missing-name diagnostics come from NameCheck exactly once.
            .typeCheckLets(p, nameCheckResult.typecheckLets, theseExternals)
            .runFully(
              withFQN,
              Referant.typeConstructors(imps) ++ typeEnv.typeConstructors,
              (fullTypeEnv ++ dependencyTypeEnv).toKindMap
            )
            .leftMap { tpeErr =>
              NonEmptyList.one[PackageError](
                PackageError.TypeErrorIn(
                  tpeErr,
                  p,
                  letsForChecks,
                  theseExternals,
                  letNameRegions,
                  localTypeNames
                )
              )
            }
            .toIor
            .flatMap { typedLets =>
              val topLevelDefs = TypedExprRecursionCheck.topLevelDefArgs(stmts)

              val recursionIssues: Chain[PackageError] =
                toErrsChain(
                  TypedExprRecursionCheck
                    .checkLets(p, fullTypeEnv, typedLets, topLevelDefs)
                    .leftMap(
                      _.map {
                        case err: RecursionCheck.Error =>
                          PackageError.RecursionError(p, err): PackageError
                        case lint: RecursionCheck.Lint =>
                          PackageError.RecursionLint(p, lint): PackageError
                      }
                    )
                )

              val shadowedBindingErrors: Chain[PackageError] =
                toErrsChain(
                  ShadowedBindingTypeCheck
                    .checkLets(p, typedLets)
                    .leftMap(
                      _.map(err =>
                        PackageError.ShadowedBindingTypeError(
                          p,
                          err,
                          localTypeNames
                        ): PackageError
                      )
                    )
                )

              val totalityErrors: List[PackageError] =
                toErrs(
                  typedLets
                    .traverse_ { case (_, _, expr) =>
                      TotalityCheck(fullTypeEnv).checkExpr(expr)
                    }
                    .leftMap(
                      _.map(err => PackageError.TotalityCheckError(p, err): PackageError)
                    )
                )

              // Preserve lint diagnostics alongside hard failures, but only
              // keep the typed program on the success path when every
              // diagnostic is postponable.
              diagnosticsToIor(
                (recursionIssues ++ shadowedBindingErrors).toList ::: totalityErrors,
                (
                  fullTypeEnv,
                  Program(typeEnv, typedLets, extDefs, stmts)
                )
              )
            }

          val checkUnusedLets: Ior[NonEmptyList[PackageError], Unit] =
            toIorPostponable(
              letsForChecks
                .traverse_ { case (_, _, expr) =>
                  UnusedLetCheck.check(expr)
                }
                .leftMap(errs =>
                  NonEmptyList.one[PackageError](
                    PackageError.UnusedLetError(p, errs.toNonEmptyList)
                  )
                )
            )(PackageError.isPostponable)

          // Name errors force a Left result, but we still collect any
          // independent inference and import/export diagnostics in the same run.
          (
            exprDagBindableChecks,
            nameCheckErrors,
            checkUnusedLets,
            inference
          ).parMapN { (_, _, _, res) => res }
        }
    }
  }

  private val todoName = Identifier.Name("todo")
  private val todoArgName = Identifier.Name("ignore")

  private val todoStatement: Statement.ExternalDef =
    Statement.ExternalDef(
      name = todoName,
      typeArgs = None,
      params =
        (todoArgName, TypeRef.TypeVar("x")) :: Nil,
      result = TypeRef.TypeForAll(
        NonEmptyList.one((TypeRef.TypeVar("a"), None)),
        TypeRef.TypeVar("a")
      )
    )(Region(0, 1))

  private val todoExport: ExportedName.Binding[Unit] =
    ExportedName.Binding(todoName, ())

  private lazy val predefEmitPackage: Package.Parsed =
    parser.parse(Predef.predefString) match {
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

  private lazy val predefTypeCheckPackage: Package.Parsed =
    predefEmitPackage.copy(
      // `todo` is type-check only: it has no runtime implementation.
      exports = todoExport :: predefEmitPackage.exports,
      program = todoStatement :: predefEmitPackage.program
    )

  def predefPackageForMode(mode: CompileOptions.Mode): Package.Parsed =
    mode match {
      case CompileOptions.Mode.Emit          => predefEmitPackage
      case CompileOptions.Mode.TypeCheckOnly => predefTypeCheckPackage
    }

  /** The parsed representation of the runtime predef.
    */
  lazy val predefPackage: Package.Parsed =
    predefPackageForMode(CompileOptions.Mode.Emit)

  implicit val documentPackage: Document[Package.Typed[Any]] =
    new Document[Package.Typed[Any]] {
      def document(pack: Typed[Any]): Doc =
        Doc.text("package: ") + Doc.text(pack.name.asString) + {
          val lines = Doc.hardLine
          val imps = Doc.text("imports: ") + Doc
            .intercalate(
              Doc.line,
              pack.imports.sortBy(_.pack.name).map { imp =>
                val shownItems = imp.items.toList
                  .map(_.originalName.sourceCodeRepr)
                  .distinct
                  .sorted
                Doc.text(imp.pack.name.asString) + Doc.space + (Doc.char(
                  '['
                ) + Doc.line +
                  Doc.intercalate(
                    Doc.comma + Doc.line,
                    shownItems.map(Doc.text(_))
                  ) + Doc.line + Doc.char(']')).grouped
              }
            )
            .nested(4)

          def exportSection(label: String, names: List[String]): Doc =
            Doc.text(label) + Doc
              .intercalate(Doc.line, names.map(Doc.text))
              .grouped
              .nested(4)

          val exportedTypes = pack.exports.collect {
            case ExportedName.TypeName(name, _) => name.sourceCodeRepr
          }.distinct
          val exportedValues = pack.exports.collect {
            case ExportedName.Binding(name, _)     => name.sourceCodeRepr
            case ExportedName.Constructor(name, _) => name.sourceCodeRepr
          }.distinct

          val exportedTypesDoc =
            exportSection("exported_types: ", exportedTypes)
          val exportedValuesDoc =
            exportSection("exported_values: ", exportedValues)

          val tpes = Doc.text("types: ") + pack.types
            .reprDocForPackage(pack.name)
            .nested(4)

          val eqDoc = Doc.text(" = ")
          val exprs = Doc.intercalate(
            Doc.hardLine + Doc.hardLine,
            pack.lets.map { case (n, _, te) =>
              Doc.text(n.sourceCodeRepr) + eqDoc + te.repr
            }
          )

          val all =
            lines :: imps :: exportedTypesDoc :: exportedValuesDoc :: tpes :: exprs :: Nil

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

    def void: Typed[Unit] = {
      val prog0 = pack.program._1
      val lets1 = prog0.lets.map { case (b, r, t) => (b, r, t.void) }
      val program1 = (prog0.copy(lets = lets1), pack.program._2)
      pack.copy(program = program1)
    }

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
    ): Either[PackageError, ImportedName[NonEmptyList[Referant[Kind.Arg]]]] =
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

    def toIface: Interface = interfaceOf(pack)

    def allImportPacks: List[PackageName] =
      pack.imports.map(_.pack.name).distinct
  }

  implicit class IfaceMethods(private val iface: Interface) extends AnyVal {
    def getImportIface[A](
        inside: PackageName,
        i: ImportedName[A]
    ): Either[PackageError, ImportedName[NonEmptyList[Referant[Kind.Arg]]]] =
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

  implicit class ResolvedMethods(private val resolved: Resolved)
      extends AnyVal {
    def name: PackageName =
      FixType.unfix(resolved) match {
        case Left(iface) => iface.name
        case Right(pack) => pack.name
      }

    def importName[F[_], A](
        fromPackage: PackageName,
        item: ImportedName[Unit]
    )(recurse: ResolvedPackage => F[Typed[A]])(implicit
        F: Applicative[F]
    ): F[Either[
      PackageError,
      (Package.Interface, ImportedName[NonEmptyList[Referant[Kind.Arg]]])
    ]] =
      Package.unfix(resolved) match {
        case Right(p) =>
          /*
           * Here we have a source we need to fully resolve
           */
          recurse(p)
            .map { packF =>
              val packInterface = Package.interfaceOf(packF)
              packF
                .getImport(fromPackage, item)
                .map((packInterface, _))
            }
        case Left(iface) =>
          /*
           * this import is already an interface, we can stop here
           */
          // this is very fast and does not need to be done in a thread
          F.pure(
            iface
              .getImportIface(fromPackage, item)
              .map((iface, _))
          )
      }
  }

  def orderByName[A, B, C, D]: Order[Package[A, B, C, D]] =
    Order.by[Package[A, B, C, D], PackageName](_.name)
}
