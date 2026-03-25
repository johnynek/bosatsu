package dev.bosatsu

import cats.data.{
  Chain,
  Ior,
  NonEmptyList,
  NonEmptySet,
  NonEmptyChain,
  State,
  Validated,
  ValidatedNec
}
import dev.bosatsu.rankn.{DefinedType, Type, TypeAlias, TypeEnv}
import scala.collection.immutable.SortedSet

import cats.syntax.all._
import dev.bosatsu.Referant.Constructor
import dev.bosatsu.Referant.DefinedT
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.graph.Dag

/** This checks the imports and exports of compiled packages and makes sure they
  * are valid
  */
object PackageCustoms {
  private val todoGlobalName: Identifier = Identifier.Name("todo")

  def apply[A: HasRegion](
      pack: Package.Typed[A]
  ): ValidatedNec[PackageError, Package.Typed[A]] =
    checkExportsHaveNoPrivateTypes(pack.name, pack.exports) *>
      noUselessBinds(pack, None) *>
      allImportsAreUsed(pack)

  private def errorsOf[A](validated: ValidatedNec[PackageError, A]): List[PackageError] =
    validated match {
      case Validated.Valid(_)     => Nil
      case Validated.Invalid(errs) =>
        errs.toChain.toList
    }

  // Replay only the postponable customs on successful packages so cache hits
  // surface the same lint diagnostics as cache misses. Use the source-level
  // lets/import graph here so optimized cached packages do not change the
  // lint set by dropping unused private top levels.
  def lintDiagnosticsFromSource(
      packName: PackageName,
      roots: List[Bindable],
      sourceImports: List[
        Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
      ],
      sourceExports: List[ExportedName[Unit]],
      sourceProgram: Program[?, Expr[Declaration], List[Statement]]
  ): List[PackageError] =
    errorsOf(
      checkExprDagBindables(
        packName,
        sourceImports,
        sourceExports,
        sourceProgram.lets,
        sourceProgram.externalDefs
      )
    ) :::
      errorsOf(
        noUselessBindsFromLets(
          packName,
          sourceExports,
          sourceProgram.lets,
          sourceProgram.from,
          roots,
          expr =>
            usedGlobalsExpr(expr).collect {
              case (pn, i: Bindable) if pn == packName => i
            }
        )
      )

  private[bosatsu] def todoUsageLintFromSource(
      packName: PackageName,
      sourceProgram: Program[?, Expr[Declaration], List[Statement]]
  ): List[PackageError] =
    NonEmptyList
      .fromList(
        sourceProgram.lets.iterator
          .flatMap { case (_, _, expr) =>
            expr.globals.iterator.collect {
              case Expr.Global(PackageName.PredefName, `todoGlobalName`, tag) =>
                HasRegion.region(tag)
            }
          }
          .toSet
          .toList
          .sorted
      )
      .map(PackageError.TodoUsage(packName, _): PackageError)
      .toList

  /** Build the exports and check the customs, and then return the Typed package
    */
  def assemble(
      nm: PackageName,
      ilist: List[Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]],
      imap: ImportMap[Package.Interface, NonEmptyList[Referant[Kind.Arg]]],
      exports: List[ExportedName[Unit]],
      program: Program[TypeEnv[Kind.Arg], TypedExpr[Declaration], List[
        Statement
      ]]
  ): Ior[NonEmptyList[PackageError], Package.Typed[Declaration]] = {

    val Program(types, lets, _, _) = program
    val letRegions = letBindableRegions(lets)
    ExportedName
      .buildExports(nm, exports, types, lets) match {
      case Validated.Valid(exports) =>
        // We have a result, which we can continue to check
        val pack = Package(nm, ilist, exports, (program, imap))
        // We have to check the "customs" before any normalization
        // or optimization
        PackageCustoms(pack) match {
          case Validated.Valid(p1)     => Ior.right(p1)
          case Validated.Invalid(errs) =>
            Ior.both(errs.toNonEmptyList, pack)
        }
      case Validated.Invalid(unknowns) =>
        Ior.left(unknowns.map { n =>
          PackageError.UnknownExport(n, nm, letRegions): PackageError
        })
    }
  }

  private def letBindableRegions[A: HasRegion](
      lets: List[(Bindable, RecursionKind, A)]
  ): List[(Bindable, Region)] =
    lets.map { case (name, _, expr) =>
      (name, HasRegion.region(expr))
    }

  private def usedGlobalsExpr[A](
      expr: Expr[A]
  ): Set[(PackageName, Identifier)] = {
    val bldr = Set.newBuilder[(PackageName, Identifier)]

    def addPatternConstructors(
        pat: Pattern[(PackageName, Identifier.Constructor), Type]
    ): Unit =
      pat.traverseStruct[cats.Id, (PackageName, Identifier.Constructor)] {
        (name, parts) =>
          bldr += ((name._1, name._2))
          Pattern.PositionalStruct(name, parts)
      }: Unit

    def loop(e: Expr[A]): Unit =
      e match {
        case Expr.Annotation(inner, _, _) =>
          loop(inner)
        case Expr.Local(_, _)             =>
          ()
        case Expr.Generic(_, inner)       =>
          loop(inner)
        case Expr.Global(pack, name, _)   =>
          bldr += ((pack, name))
        case Expr.App(fn, args, _)        =>
          loop(fn)
          args.toList.foreach(loop)
        case Expr.Lambda(_, inner, _)     =>
          loop(inner)
        case Expr.Let(_, bound, in, _, _) =>
          loop(bound)
          loop(in)
        case Expr.Literal(_, _)           =>
          ()
        case Expr.Match(arg, branches, _) =>
          loop(arg)
          branches.toList.foreach { branch =>
            addPatternConstructors(branch.pattern)
            branch.guard.foreach(loop)
            loop(branch.expr)
          }
      }

    loop(expr)
    bldr.result()
  }

  def checkExprDagBindables[A: HasRegion](
      nm: PackageName,
      imports: List[
        Import[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
      ],
      exports: List[ExportedName[Unit]],
      lets: List[(Bindable, RecursionKind, Expr[A])],
      externalDefs: List[Bindable]
  ): ValidatedNec[PackageError, Unit] = {
    val knownBindings: Set[Bindable] =
      lets.iterator.map(_._1).toSet ++ externalDefs

    val letRegions = letBindableRegions(lets)
    val unknownExports: List[PackageError] =
      exports.collect {
        case ex @ ExportedName.Binding(name, _) if !knownBindings(name) =>
          PackageError.UnknownExport(ex, nm, letRegions): PackageError
      }

    val usedGlobals: Set[(PackageName, Identifier)] =
      lets.iterator
        .flatMap { case (_, _, expr) =>
          usedGlobalsExpr(expr).iterator
        }
        .toSet

    val badImports: List[Import[PackageName, Unit]] =
      imports.iterator
        .flatMap { imp =>
          val fromPack = imp.pack.name
          imp.items.iterator.collect {
            case item
                if item.originalName.toBindable.isDefined &&
                  item.tag.exists {
                    case Referant.Value(_) => true
                    case _                 => false
                  } &&
                  !usedGlobals((fromPack, item.originalName)) =>
              (fromPack, item.withTag(()))
          }
        }
        .toList
        .groupBy(_._1)
        .iterator
        .collect {
          case (pack, items)
              if (pack != PackageName.PredefName) && items.nonEmpty =>
            val deduped = ListUtil
              .distinctByHashSet(
                NonEmptyList.fromListUnsafe(items.iterator.map(_._2).toList)
              )
              .toList
              .sortBy(_.localName)
            Import(pack, NonEmptyList.fromListUnsafe(deduped))
        }
        .toList
        .sortBy(_.pack)

    val allErrors: List[PackageError] =
      NonEmptyList
        .fromList(badImports)
        .map(ni => PackageError.UnusedImport(nm, ni): PackageError)
        .toList ::: unknownExports

    NonEmptyChain.fromChain(Chain.fromSeq(allErrors)) match {
      case Some(errs) => Validated.invalid(errs)
      case None       => Validated.valid(())
    }
  }

  private def removeUnused[A](
      vals: Option[NonEmptySet[(PackageName, Identifier)]],
      types: Option[NonEmptySet[(PackageName, Type.Const)]],
      pack: Package.Typed[A]
  ): Package.Typed[A] =
    (vals, types) match {
      case (None, None) => pack
      case (ov, ot)     =>
        val unV = ov match {
          case Some(v) => v.toSortedSet
          case None    => SortedSet.empty[(PackageName, Identifier)]
        }
        val unT = ot match {
          case Some(v) => v.toSortedSet
          case None    => SortedSet.empty[(PackageName, Type.Const)]
        }
        val i = pack.imports.flatMap { imp =>
          imp.mapFilter { (pack, item) =>
            val t1 = item.tag.toList.filter {
              case Constructor(_, _) | Referant.Value(_) =>
                !unV((pack.name, item.originalName))
              case DefinedT(dt) =>
                !unT((pack.name, dt.toTypeConst))
              case Referant.TypeAliasT(ta) =>
                !unT((pack.name, ta.toTypeConst))
            }
            NonEmptyList.fromList(t1).map(item.withTag(_))
          }
        }

        val cleanImap = ImportMap.fromImportsUnsafe(i)
        pack.copy(imports = i, program = (pack.program._1, cleanImap))
    }

  private type VSet = Set[(PackageName, Identifier)]
  private type VState[X] = State[VSet, X]

  private def usedGlobals[A](
      pack: Package.Typed[A]
  ): Set[(PackageName, Identifier)] = {
    val usedValuesSt: VState[Unit] =
      pack.lets.traverse_ { case (_, _, te) =>
        TypedExpr.usedGlobals(te)
      }

    usedValuesSt.runS(Set.empty).value
  }

  private def usedTypeConsts[A](
      pack: Package.Typed[A]
  ): Set[Type.Const] = {
    val letTypes =
      pack.program._1.lets.iterator
        .flatMap(_._3.allTypes.flatMap(Type.constantsOf(_)))

    val externalTypes =
      pack.program._1.externalDefs.iterator.flatMap { nm =>
        pack.program._1.types
          .getExternalValue(pack.name, nm)
          .iterator
          .flatMap(Type.constantsOf(_))
      }

    val localDefinedTypes =
      for {
        dt <- pack.program._1.types.allDefinedTypes.iterator
        if dt.packageName == pack.name
        constructor <- dt.constructors.iterator
        param <- constructor.args.iterator
        tconst <- Type.constantsOf(param.tpe).iterator
      } yield tconst

    val localAliases =
      for {
        ta <- pack.program._1.types.allTypeAliases.iterator
        if ta.packageName == pack.name
        tconst <- Type.constantsOf(ta.rhs).iterator
      } yield tconst

    (letTypes ++ externalTypes ++ localDefinedTypes ++ localAliases).toSet
  }

  private def allImportsAreUsed[A](
      pack: Package.Typed[A]
  ): ValidatedNec[PackageError, Package.Typed[A]] = {
    // Note, we can't import just a name or just a type when we have Structs,
    // we get both. So, we will count a Constructor used if the Identifier
    // OR the type is used
    val impValues: SortedSet[(PackageName, Identifier)] =
      (for {
        imp <- pack.imports.iterator
        impPack = imp.pack.name
        item <- imp.items.iterator
        ref <- item.tag.iterator
        name <- ref match {
          case Constructor(_, _) | Referant.Value(_) =>
            item.originalName :: Nil
          case DefinedT(_) | Referant.TypeAliasT(_) =>
            Nil
        }
      } yield (impPack, name)).to(SortedSet)

    val impTypes: SortedSet[(PackageName, Type.Const)] =
      (for {
        imp <- pack.imports.iterator
        impPack = imp.pack.name
        item <- imp.items.iterator
        ref <- item.tag.iterator
        tpe <- ref match {
          case Constructor(_, _) | Referant.Value(_) =>
            Nil
          case DefinedT(dt) =>
            dt.toTypeConst :: Nil
          case Referant.TypeAliasT(ta) =>
            ta.toTypeConst :: Nil
        }
      } yield (impPack, tpe)).to(SortedSet)

    if (impValues.isEmpty && impTypes.isEmpty) Validated.valid(pack)
    else {
      val usedValues = usedGlobals(pack)
      val usedTypes = usedTypeConsts(pack)

      val unusedValues = impValues.filterNot { tup =>
        tup match {
          case (pn, cons @ Identifier.Constructor(_)) =>
            // deal with the ambiguity of capital names
            usedValues(tup) || usedTypes(Type.Const.Defined(pn, TypeName(cons)))
          case _ =>
            // no ambiguity for bindable
            usedValues(tup)
        }
      }
      val unusedTypes = impTypes.filterNot { case (pn, t) =>
        // deal with the ambiguity of capital names
        usedTypes(t) || usedValues((pn, t.toDefined.name.ident))
      }

      val unusedValMap = unusedValues.groupByNes(_._1)
      val unusedTypeMap = unusedTypes.groupByNes(_._1)
      val unusedPacks =
        (unusedValMap.keySet | unusedTypeMap.keySet) - PackageName.PredefName

      if (unusedPacks.isEmpty) {
        // remove unused
        Validated.valid(
          removeUnused(
            unusedValMap.get(PackageName.PredefName),
            unusedTypeMap.get(PackageName.PredefName),
            pack
          )
        )
      } else {
        val badImports =
          NonEmptyList.fromListUnsafe(unusedPacks.iterator.map { ipack =>
            val thisVals = unusedValMap.get(ipack) match {
              case Some(nes) =>
                nes.toSortedSet.toList.map { case (_, i) =>
                  ImportedName.OriginalName(i, ())
                }
              case None => Nil
            }

            val thisTpes = unusedTypeMap.get(ipack) match {
              case Some(nes) =>
                nes.toSortedSet.toList.map { case (_, t) =>
                  ImportedName.OriginalName(t.toDefined.name.ident, ())
                }
              case None => Nil
            }
            // one or the other or both of these is non-empty
            Import(
              ipack,
              NonEmptyList.fromListUnsafe((thisVals ::: thisTpes).distinct)
            )
          }.toList)

        Validated.invalidNec(PackageError.UnusedImport(pack.name, badImports))
      }
    }
  }

  private def checkExportsHaveNoPrivateTypes[V](
      pn: PackageName,
      exports: List[ExportedName[Referant[V]]]
  ): ValidatedNec[PackageError, Unit] = {
    val exportedTypes: List[DefinedType[V]] = exports
      .flatMap(_.tag.definedType)
      .distinct
    val exportedAliases: List[TypeAlias[V]] = exports
      .flatMap(_.tag.typeAlias)
      .distinct

    val exportedTE = TypeEnv.fromDefinitionsAndAliases(exportedTypes, exportedAliases)

    type Exp = ExportedName[Referant[V]]
    val exposedTypes: Iterator[(Exp, Type)] = exports.iterator.flatMap {
      case n @ ExportedName.Binding(_, Referant.Value(t)) =>
        Iterator.single((n, t))
      case n @ ExportedName.TypeName(_, Referant.TypeAliasT(ta)) =>
        Iterator.single((n, ta.rhs))
      case _ =>
        Iterator.empty
    }

    val usedTypes: Iterator[(Type.Const, Exp, Type)] = exposedTypes
      .flatMap { n =>
        Type.constantsOf(n._2).iterator.map((_, n._1, n._2))
      }
      .filter { case (Type.Const.Defined(p, _), _, _) => p == pn }

    def errorFor(t: (Type.Const, Exp, Type)): List[PackageError] =
      if (exportedTE.hasTypeConst(t._1)) Nil
      else PackageError.PrivateTypeEscape(t._2, t._3, pn, t._1) :: Nil

    NonEmptyChain.fromChain(
      Chain.fromIterableOnce(usedTypes.flatMap(errorFor).toList.distinct)
    ) match {
      case None      => Validated.valid(())
      case Some(nel) => Validated.invalid(nel)
    }
  }

  private def noUselessBindsFromLets[A: HasRegion](
      packName: PackageName,
      exports: List[ExportedName[?]],
      lets: List[(Bindable, RecursionKind, A)],
      sourceStmts: List[Statement],
      roots: List[Bindable],
      internalDeps: A => Set[Bindable]
  ): ValidatedNec[PackageError, Unit] = {
    val exportedBindings =
      exports.collect { case ExportedName.Binding(name, _) => name }
    val nonBindingUses: List[Node] =
      sourceStmts.iterator
        .collect {
          case Statement.Bind(BindingStatement(bound, decl, _))
              if bound.names.isEmpty =>
            decl.freeVars.iterator
        }
        .flatten
        .toList

    val allRoots: List[Bindable] =
      (exportedBindings :::
        roots :::
        nonBindingUses).distinct

    val bindMap: Map[Bindable, A] =
      lets.iterator.map { case (b, _, te) => (b, te) }.toMap

    val canReach: Set[Bindable] =
      Dag.transitiveSet(allRoots)(value =>
        bindMap.get(value).iterator.flatMap(internalDeps).toList
      )

    val unused = lets.filter { case (bn, _, _) =>
      !Identifier.isSynthetic(bn) && !canReach(bn)
    }

    val statementRegions: Map[Bindable, Region] =
      sourceStmts.iterator
        .collect { case vs: Statement.ValueStatement =>
          vs.names.iterator.map(_ -> vs.region)
        }
        .flatten
        .toMap

    NonEmptyList.fromList(unused) match {
      case None        => Validated.unit
      case Some(value) =>
        Validated.invalidNec(
          PackageError.UnusedLets(
            packName,
            value.map { case (b, r, te) =>
              val region = statementRegions.getOrElse(b, HasRegion.region(te))
              (b, r, region)
            }
          )
        )
    }
  }

  private type Node = Bindable

  private def noUselessBinds[A: HasRegion](
      pack: Package.Typed[A],
      sourceStatements: Option[List[Statement]]
  ): ValidatedNec[PackageError, Unit] =
    sourceStatements.orElse(maybeStatements(pack)) match {
      case Some(sourceStmts) =>
        noUselessBindsFromLets(
          pack.name,
          pack.exports,
          pack.lets,
          sourceStmts,
          Package.testRootBindables(pack).toList ::: Package
            .mainValue(pack)
            .map(_._1)
            .toList,
          te =>
            TypedExpr.usedGlobals(te).runS(Set.empty).value.collect {
              case (pn, i: Identifier.Bindable) if pn == pack.name => i
            }
        )
      case None              =>
        // Compiled/cache-only packages erase Program.from, so this lint can
        // only run when the caller supplied source statements explicitly.
        Validated.unit
    }

  private def maybeStatements[A](
      pack: Package.Typed[A]
  ): Option[List[Statement]] =
    pack.program._1.from match {
      case stmts: List[?] =>
        stmts.traverse {
          case stmt: Statement => Some(stmt)
          case _               => None
        }
      case _              =>
        None
    }
}
