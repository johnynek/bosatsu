package dev.bosatsu

import cats.{Eval, StackSafeMonad}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.implicits._
import dev.bosatsu.pattern.NameMap
import dev.bosatsu.rankn.{ConstructorFn, DefinedType, Type, TypeEnv}
import dev.bosatsu.scalawasiz3.{Z3Platform, Z3Result}
import dev.bosatsu.smt.{SExpr, SmtCommand, SmtExpr, SmtLibRender, SmtScript, SmtScriptScope, SmtSort, Z3Api}

import Identifier.Bindable

object TypedExprRecursionCheck {

  type Res[+A] = ValidatedNec[RecursionCheck.Error, A]

  private def normalizedDefArgs(
      groups: NonEmptyList[List[Pattern.Parsed]]
  ): NonEmptyList[NonEmptyList[Pattern.Parsed]] =
    groups.map {
      case h :: tail => NonEmptyList(h, tail)
      case Nil       => NonEmptyList.one(Pattern.tuple(Nil))
    }

  def topLevelDefArgs(
      stmts: List[Statement]
  ): Map[Bindable, NonEmptyList[NonEmptyList[Pattern.Parsed]]] =
    stmts.collect { case Statement.Def(defstmt) =>
      defstmt.name -> normalizedDefArgs(defstmt.args)
    }.toMap

  def checkLets(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])]
  ): ValidatedNec[RecursionCheck.Error, Unit] =
    checkLets(pack, fullTypeEnv, lets, Map.empty)

  def checkLets(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
      topLevelDefs: Map[Bindable, NonEmptyList[NonEmptyList[Pattern.Parsed]]]
  ): ValidatedNec[RecursionCheck.Error, Unit] = {
    // This checker enforces structural recursion plus type-directed Int
    // decrease obligations in typed IR.
    val totalityCheck = TotalityCheck(fullTypeEnv)
    val topLevelLowerableAliases = Impl.topLevelLowerableAliases(pack, lets)
    val topLevelPredefAliases = Impl.topLevelPredefAliases(pack, lets)
    lets.traverse_ { case (name, rec, expr) =>
      Impl.checkTopLevelLet(
        pack,
        name,
        rec,
        expr,
        topLevelDefs.get(name),
        topLevelLowerableAliases,
        topLevelPredefAliases,
        totalityCheck
      )
    }
  }

  private object Impl {
    import RecursionCheck.ArgLexOrder
    import RecursionCheck.ArgLexOrder.*
    import TypedExpr._

    val unitValid: Res[Unit] = Validated.valid(())

    case class RecurTargetItem(group: Int, index: Int, paramName: Bindable)
        derives CanEqual
    type RecurTarget = NonEmptyList[RecurTargetItem]
    type TypedPattern = Pattern[(PackageName, Identifier.Constructor), Type]

    case class SingletonCtor(pack: PackageName, cons: Identifier.Constructor)
        derives CanEqual
    case class KnownCtor(pack: PackageName, cons: Identifier.Constructor)
        derives CanEqual
    case class ConstructorApp(
        ctor: KnownCtor,
        args: List[TypedExpr[Declaration]]
    ) derives CanEqual

    type TopLevelLowerableAliases = Map[(PackageName, Bindable), TopLevelAlias]
    type TopLevelPredefAliases = Map[(PackageName, Bindable), Identifier.Name]

    case class TopLevelAlias(
        params: NonEmptyList[Bindable],
        body: TypedExpr[Declaration]
    )

    private val comparisonType: Type =
      Type.TyConst(Type.Const.predef("Comparison"))
    private val z3Solver = Z3Platform.create()
    private val z3Runner: Z3Api.RunSmt2 = { smt2 =>
      z3Solver.runSmt2(smt2) match {
        case Z3Result.Success(stdout, stderr, _) =>
          Right(Z3Api.SolverOutput(stdout, stderr))
        case Z3Result.Failure(msg, _, stdout, stderr, _) =>
          Left(Z3Api.RunError.ExecutionFailure(msg, stdout, stderr))
      }
    }
    private val emptyListSingletonCtor =
      SingletonCtor(PackageName.PredefName, Identifier.Constructor("EmptyList"))
    private val emptyListKnownCtor =
      KnownCtor(
        emptyListSingletonCtor.pack,
        emptyListSingletonCtor.cons
      )
    private val nonEmptyListKnownCtor =
      KnownCtor(PackageName.PredefName, Identifier.Constructor("NonEmptyList"))
    private val unitCtorName = Identifier.Constructor("Unit")
    private val lazyPackageName = PackageName.parts("Bosatsu", "Lazy")
    private val getLazyName = Identifier.Name("get_Lazy")
    private val lazyTypeConst =
      Type.Const.Defined(lazyPackageName, TypeName("Lazy"))

    case class SmtBranchState(
        intBindings: Map[Bindable, SmtExpr.IntExpr],
        boolBindings: Map[Bindable, SmtExpr.BoolExpr],
        comparisonBindings: Map[Bindable, SmtExpr.IntExpr],
        declarations: Map[String, SmtSort],
        pathFacts: Vector[SmtExpr.BoolExpr],
        freshId: Int,
        topLevelLowerableAliases: TopLevelLowerableAliases,
        topLevelPredefAliases: TopLevelPredefAliases,
        totalityCheck: TotalityCheck
    ) {
      def withIntBinding(name: Bindable, expr: SmtExpr.IntExpr): SmtBranchState =
        copy(intBindings = intBindings.updated(name, expr))

      def withBoolBinding(
          name: Bindable,
          expr: SmtExpr.BoolExpr
      ): SmtBranchState =
        copy(boolBindings = boolBindings.updated(name, expr))

      def withComparisonBinding(
          name: Bindable,
          expr: SmtExpr.IntExpr
      ): SmtBranchState =
        copy(comparisonBindings = comparisonBindings.updated(name, expr))

      def addPathFact(expr: SmtExpr.BoolExpr): SmtBranchState =
        copy(pathFacts = pathFacts :+ expr)

      def removeBindings(names: Iterable[Bindable]): SmtBranchState = {
        val removeSet = names.toSet
        if (removeSet.isEmpty) this
        else
          copy(
            intBindings = intBindings -- removeSet,
            boolBindings = boolBindings -- removeSet,
            comparisonBindings = comparisonBindings -- removeSet
          )
      }
    }
    object SmtBranchState {
      def empty(totalityCheck: TotalityCheck): SmtBranchState =
        SmtBranchState(
          Map.empty,
          Map.empty,
          Map.empty,
          Map.empty,
          Vector.empty,
          0,
          Map.empty,
          Map.empty,
          totalityCheck
        )
    }

    // We keep wrapper scope tracking explicit so each recursive call-site is
    // checked with the in-scope Generic/Annotation context.
    case class WrapperScope(
        universal: Set[Type.Var.Bound],
        existential: Set[Type.Var.Bound],
        annotationDepth: Int
    ) {
      def pushQuant(q: Quantification): WrapperScope =
        copy(
          universal = universal ++ q.forallList.iterator.map(_._1),
          existential = existential ++ q.existList.iterator.map(_._1)
        )

      def pushAnnotation: WrapperScope =
        copy(annotationDepth = annotationDepth + 1)
    }
    object WrapperScope {
      val Empty: WrapperScope = WrapperScope(Set.empty, Set.empty, 0)
    }

    /*
     * While checking a def we have three states we can be in:
     * 1. we are in a normal def, but have 0 or more outer recursive defs to avoid
     * 2. we are in a recursive def, but have not yet found the recur match.
     * 3. we are checking the branches of the recur match
     */
    sealed abstract class State derives CanEqual {
      final def outerDefNames: Set[Bindable] =
        this match {
          case TopLevel(_)     => Set.empty
          case ids: InDefState =>
            val InDef(outer, n, _, _, _, _, _, _, _) = ids.inDef
            outer.outerDefNames + n
        }

      @annotation.tailrec
      final def defNamesContain(n: Bindable): Boolean =
        this match {
          case TopLevel(_)     => false
          case ids: InDefState =>
            val InDef(outer, dn, _, _, _, _, _, _, _) = ids.inDef
            (dn == n) || outer.defNamesContain(n)
        }

      def inDef(
          fnname: Bindable,
          typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]],
          sourceArgs: NonEmptyList[NonEmptyList[Pattern.Parsed]],
          topLevelLowerableAliases: TopLevelLowerableAliases,
          fnType: Type,
          topLevelPredefAliases: TopLevelPredefAliases,
          totalityCheck: TotalityCheck
      ): InDef =
        InDef(
          this,
          fnname,
          typedArgs,
          sourceArgs,
          topLevelLowerableAliases,
          fnType,
          topLevelPredefAliases,
          totalityCheck,
          Set.empty
        )
    }

    sealed abstract class InDefState extends State {
      final def inDef: InDef =
        this match {
          case id @ InDef(_, _, _, _, _, _, _, _, _)    => id
          case InDefRecurred(ir, _, _, _, _)            => ir.inDef
          case InRecurBranch(
                InDefRecurred(ir, _, _, _, _),
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            ir.inDef
        }

      final def defname: Bindable = inDef.fnname
    }

    case class TopLevel(totalityCheck: TotalityCheck) extends State

    case class InDef(
        outer: State,
        fnname: Bindable,
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]],
        sourceArgs: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        topLevelLowerableAliases: TopLevelLowerableAliases,
        fnType: Type,
        topLevelPredefAliases: TopLevelPredefAliases,
        totalityCheck: TotalityCheck,
        localScope: Set[Bindable]
    ) extends InDefState {
      def addLocal(b: Bindable): InDef =
        InDef(
          outer,
          fnname,
          typedArgs,
          sourceArgs,
          topLevelLowerableAliases,
          fnType,
          topLevelPredefAliases,
          totalityCheck,
          localScope + b
        )

      def setRecur(target: RecurTarget, m: Declaration.Match): InDefRecurred =
        InDefRecurred(this, target, m, 0, Map.empty)

      // This is eta-expansion of the function name as a lambda so we can check
      // using the lambda rule.
      def asLambda(region: Region): TypedExpr.AnnotatedLambda[Declaration] = {
        val allNames = Iterator
          .iterate(0)(_ + 1)
          .map(idx => Identifier.Name(s"a$idx"))
          .filterNot(n => (n: Bindable) == fnname)

        val syntheticTag = Declaration.Var(fnname)(using region)
        val freshGroups: NonEmptyList[NonEmptyList[(Bindable, Type)]] =
          typedArgs.map(_.map { case (_, tpe) => (allNames.next(), tpe) })

        def appResultType(curr: Type): Type =
          curr match {
            case Type.Fun(_, res) => res
            case _                => curr
          }

        val body = freshGroups.toList.foldLeft(
          TypedExpr.Local(fnname, fnType, syntheticTag): TypedExpr[Declaration]
        ) { (called, group) =>
          val argExprs = group.map { case (nm, tpe) =>
            TypedExpr.Local(nm, tpe, Declaration.Var(nm)(using region))
          }
          TypedExpr.App(called, argExprs, appResultType(called.getType), syntheticTag)
        }

        def lambdify(
            args: NonEmptyList[NonEmptyList[(Bindable, Type)]],
            body0: TypedExpr[Declaration]
        ): TypedExpr.AnnotatedLambda[Declaration] = {
          val body1 = args.tail match {
            case Nil       => body0
            case h :: tail => lambdify(NonEmptyList(h, tail), body0)
          }
          TypedExpr.AnnotatedLambda(args.head, body1, syntheticTag)
        }

        lambdify(freshGroups, body)
      }
    }

    case class InDefRecurred(
        inRec: InDef,
        target: RecurTarget,
        recur: Declaration.Match,
        recCount: Int,
        calledNames: Map[Bindable, Int]
    ) extends InDefState {
      def incRecCount: InDefRecurred = copy(recCount = recCount + 1)
      def noteCalledName(nm: Bindable): InDefRecurred =
        copy(calledNames = calledNames.updatedWith(nm) {
          case None        => Some(1)
          case Some(count) => Some(count + 1)
        })
    }

    case class InRecurBranch(
        inRec: InDefRecurred,
        branch: Pattern[(PackageName, Identifier.Constructor), Type],
        allowedPerTarget: NonEmptyList[Set[Bindable]],
        equalAliasesPerTarget: NonEmptyList[Set[Bindable]],
        singletonCtorPerTarget: NonEmptyList[Option[SingletonCtor]],
        currentCtorPerTarget: NonEmptyList[Option[KnownCtor]],
        reachableNames: Set[Bindable],
        smtState: SmtBranchState
    ) extends InDefState {
      def incRecCount: InRecurBranch = copy(inRec = inRec.incRecCount)
      def noteCalledName(nm: Bindable): InRecurBranch =
        copy(inRec = inRec.noteCalledName(nm))
    }

    private def likelyRenameCall(
        fnname: Bindable,
        calledNames: Map[Bindable, Int]
    ): Option[(Bindable, Int)] =
      NameSuggestion
        .best(
          fnname,
          calledNames.iterator.map { case (name, count) =>
            NameSuggestion.Candidate(
              name,
              (name, count),
              NameSuggestion.ScopePriority.Local
            )
          }.toList
        )
        .map(_.value)

    private def defaultSourceArgs(
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]]
    ): NonEmptyList[NonEmptyList[Pattern.Parsed]] =
      typedArgs.map(_.map { case (n, _) =>
        Pattern.Var(n): Pattern.Parsed
      })

    private def shapeMatches[A, B](
        left: NonEmptyList[NonEmptyList[A]],
        right: NonEmptyList[NonEmptyList[B]]
    ): Boolean =
      (left.length == right.length) && left.iterator
        .zip(right.iterator)
        .forall { case (l, r) => l.length == r.length }

    private def sourceArgsForDef(
        fromSource: Option[NonEmptyList[NonEmptyList[Pattern.Parsed]]],
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]]
    ): NonEmptyList[NonEmptyList[Pattern.Parsed]] =
      fromSource.filter(shapeMatches(_, typedArgs)).getOrElse(defaultSourceArgs(typedArgs))

    private def argsRepr(
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]]
    ): String =
      RecursionCheck.renderArgs(args)(Pattern.document[TypeRef].document)

    private def getRecurTargetItemsByName(
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]]
    ): Map[Bindable, RecurTargetItem] =
      args.iterator
        .zipWithIndex
        .flatMap { case (group, gidx) =>
          group.iterator.zipWithIndex.flatMap { case (item, idx) =>
            val maybeTypedName =
              typedArgs
                .get(gidx.toLong)
                .flatMap(_.get(idx.toLong))
                .map(_._1)
            item.topNames.iterator.map { topName =>
              val bindName = maybeTypedName.getOrElse(topName)
              topName -> RecurTargetItem(gidx, idx, bindName)
            }
          }
        }
        .toMap

    private def resolveRecurTargetItem(
        fnname: Bindable,
        recur: Declaration.Match,
        locals: Set[Bindable],
        targetItemsByName: Map[Bindable, RecurTargetItem],
        d: Declaration,
        argsMessage: String
    ): Res[RecurTargetItem] =
      d match {
        case Declaration.Var(b: Bindable) if locals(b) =>
          Validated.invalidNec(
            RecursionCheck.RecurNotOnArg(recur.region, fnname, argsMessage)
          )
        case Declaration.Var(b: Bindable) =>
          targetItemsByName
            .get(b)
            .toValidNec(
              RecursionCheck.RecurNotOnArg(recur.region, fnname, argsMessage)
            )
        case Declaration.Var(_) =>
          Validated.invalidNec(
            RecursionCheck.RecurNotOnArg(recur.region, fnname, argsMessage)
          )
        case _ =>
          Validated.invalidNec(
            RecursionCheck.RecurTargetInvalid(fnname, d.region)
          )
      }

    /*
     * What are the indices into the list of def arguments where we are doing recursion.
     */
    private def getRecurTarget(
        fnname: Bindable,
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]],
        m: Declaration.Match,
        locals: Set[Bindable]
    ): Res[RecurTarget] = {
      import Declaration._
      val targetItemsByName = getRecurTargetItemsByName(args, typedArgs)
      val argsMessage = argsRepr(args)

      def checkDuplicates(
          target: RecurTarget,
          sourceItems: NonEmptyList[Declaration]
      ): Res[RecurTarget] = {
        val (_, errorsRev) =
          target.iterator
            .zip(sourceItems.iterator)
            .foldLeft((Set.empty[Bindable], List.empty[RecursionCheck.Error])) {
              case ((seen, errs), (item, sourceDecl)) =>
                if (seen(item.paramName))
                  (
                    seen,
                    RecursionCheck.RecurTargetDuplicate(
                      fnname,
                      item.paramName,
                      sourceDecl.region
                    ) :: errs
                  )
                else (seen + item.paramName, errs)
            }

        NonEmptyList.fromList(errorsRev.reverse) match {
          case Some(errs) =>
            Validated.invalid(NonEmptyChain.fromNonEmptyList(errs))
          case None       => Validated.valid(target)
        }
      }

      m.arg match {
        case v @ Var(_) =>
          resolveRecurTargetItem(
            fnname,
            m,
            locals,
            targetItemsByName,
            v,
            argsMessage
          ).map(NonEmptyList.one)
        case TupleCons(Nil) =>
          Validated.invalidNec(
            RecursionCheck.RecurTargetInvalid(fnname, m.arg.region)
          )
        case TupleCons(h :: tail) =>
          val sourceItems = NonEmptyList(h, tail)
          sourceItems
            .traverse(
              resolveRecurTargetItem(
                fnname,
                m,
                locals,
                targetItemsByName,
                _,
                argsMessage
              )
            )
            .andThen(checkDuplicates(_, sourceItems))
        case _ =>
          Validated.invalidNec(
            RecursionCheck.RecurTargetInvalid(fnname, m.arg.region)
          )
      }
    }

    @annotation.tailrec
    private def unwrapNamedAnnotation[N, T](
        pat: Pattern[N, T]
    ): Pattern[N, T] =
      pat match {
        case Pattern.Annotation(inner, _) => unwrapNamedAnnotation(inner)
        case Pattern.Named(_, inner)      => unwrapNamedAnnotation(inner)
        case p                            => p
      }

    private def allowedByTargetFromParsed(
        target: RecurTarget,
        branchPat: Pattern.Parsed
    ): NonEmptyList[Set[Bindable]] =
      target.tail match {
        case Nil =>
          NonEmptyList.one(branchPat.substructures.toSet)
        case _ =>
          unwrapNamedAnnotation(branchPat) match {
            case Pattern.PositionalStruct(kind, parts)
                if parts.length == target.length &&
                  ((kind == Pattern.StructKind.Tuple) ||
                    kind.namedStyle.contains(Pattern.StructKind.Style.TupleLike)) =>
              NonEmptyList.fromListUnsafe(parts.map(_.substructures.toSet))
            case _ =>
              target.map(_ => Set.empty[Bindable])
          }
      }

    private def targetPatternPartsFromParsed(
        targetLength: Int,
        branchPat: Pattern.Parsed
    ): Option[List[Pattern.Parsed]] =
      if (targetLength == 1) Some(branchPat :: Nil)
      else
        unwrapNamedAnnotation(branchPat) match {
          case Pattern.PositionalStruct(kind, parts)
              if (parts.length == targetLength) &&
                ((kind == Pattern.StructKind.Tuple) ||
                  kind.namedStyle.contains(Pattern.StructKind.Style.TupleLike)) =>
            Some(parts)
          case _ =>
            None
        }

    @annotation.tailrec
    private def wholeValueAliases[N, T](
        pat: Pattern[N, T],
        seen: Set[Bindable] = Set.empty,
        accRev: List[Bindable] = Nil
    ): List[Bindable] =
      pat match {
        case Pattern.Annotation(inner, _) =>
          wholeValueAliases(inner, seen, accRev)
        case Pattern.Named(name, inner)  =>
          if (seen(name))
            wholeValueAliases(inner, seen, accRev)
          else
            wholeValueAliases(inner, seen + name, name :: accRev)
        case Pattern.Var(name)           =>
          if (seen(name)) accRev.reverse else (name :: accRev).reverse
        case _                           =>
          accRev.reverse
      }

    private def equalAliasesByTargetFromParsed(
        target: RecurTarget,
        branchPat: Pattern.Parsed
    ): NonEmptyList[Set[Bindable]] =
      targetPatternPartsFromParsed(target.length, branchPat) match {
        case Some(parts) =>
          NonEmptyList.fromListUnsafe(parts.map(part => wholeValueAliases(part).toSet))
        case None        =>
          target.map(_ => Set.empty[Bindable])
      }

    private def isTupleConstructor(
        pack: PackageName,
        cons: Identifier.Constructor,
        arity: Int
    ): Boolean =
      (pack == PackageName.PredefName) &&
        (cons.asString == s"Tuple$arity")

    private def targetPatternPartsFromTyped(
        targetLength: Int,
        branchPat: TypedPattern
    ): Option[List[TypedPattern]] =
      if (targetLength == 1) Some(branchPat :: Nil)
      else
        unwrapNamedAnnotation(branchPat) match {
          case Pattern.PositionalStruct((pack, cons), parts)
              if (parts.length == targetLength) &&
                isTupleConstructor(pack, cons, targetLength) =>
            Some(parts)
          case _ =>
            None
        }

    private def singletonCtorFromTypedPart(
        part: TypedPattern
    ): Option[SingletonCtor] =
      unwrapNamedAnnotation(part) match {
        case Pattern.ListPat(Nil) =>
          Some(emptyListSingletonCtor)
        case Pattern.PositionalStruct((pack, cons), Nil) =>
          Some(SingletonCtor(pack, cons))
        case _ =>
          None
      }

    private def singletonCtorByTargetFromTyped(
        target: RecurTarget,
        branchPat: TypedPattern
    ): NonEmptyList[Option[SingletonCtor]] =
      targetPatternPartsFromTyped(target.length, branchPat) match {
        case Some(parts) =>
          NonEmptyList.fromListUnsafe(parts.map(singletonCtorFromTypedPart))
        case None        =>
          target.map(_ => None)
      }

    private def knownCtorFromTypedPart(
        part: TypedPattern
    ): Option[KnownCtor] =
      unwrapNamedAnnotation(part) match {
        case lp @ Pattern.ListPat(_) =>
          // Normalize list syntax so constructor evidence works uniformly.
          Pattern.ListPat
            .toPositionalStruct(
              lp,
              (emptyListKnownCtor.pack, emptyListKnownCtor.cons),
              (nonEmptyListKnownCtor.pack, nonEmptyListKnownCtor.cons)
            )
            .toOption
            .flatMap {
              case Pattern.PositionalStruct((pack, cons), _) =>
                Some(KnownCtor(pack, cons))
              case _ =>
                None
            }
        case Pattern.PositionalStruct((pack, cons), _) =>
          Some(KnownCtor(pack, cons))
        case _ =>
          None
      }

    private def knownCtorByTargetFromTyped(
        target: RecurTarget,
        branchPat: TypedPattern
    ): NonEmptyList[Option[KnownCtor]] =
      targetPatternPartsFromTyped(target.length, branchPat) match {
        case Some(parts) =>
          NonEmptyList.fromListUnsafe(parts.map(knownCtorFromTypedPart))
        case None        =>
          target.map(_ => None)
      }

    private def bindAliasToTarget(
        alias: Bindable,
        inrec: InDefRecurred,
        targetItem: RecurTargetItem,
        state: SmtBranchState
    ): SmtBranchState =
      if (alias == targetItem.paramName) state
      else {
        val tpe = targetItemType(inrec, targetItem)
        if (isIntType(tpe)) {
          val (expr, st1) = ensureIntLocal(targetItem.paramName, state)
          st1.withIntBinding(alias, expr)
        } else if (isBoolType(tpe)) {
          val (expr, st1) = ensureBoolLocal(targetItem.paramName, state)
          st1.withBoolBinding(alias, expr)
        } else if (isComparisonType(tpe)) {
          val (expr, st1) = ensureComparisonLocal(targetItem.paramName, state)
          st1.withComparisonBinding(alias, expr)
        } else state
      }

    private def bindSourcePatternAliases(
        inrec: InDefRecurred,
        sourcePat: Pattern.Parsed,
        state: SmtBranchState
    ): SmtBranchState =
      // Source patterns let us recover whole-value aliases per recur-target
      // component (for example `(Succ(prev), i1)` binds `i1` to the same value
      // as the second recur target).
      targetPatternPartsFromParsed(inrec.target.length, sourcePat) match {
        case Some(parts) =>
          inrec.target.toList
            .zip(parts)
            .foldLeft(state) { case (st, (targetItem, part)) =>
              wholeValueAliases(part).foldLeft(st) { (st1, alias) =>
                bindAliasToTarget(alias, inrec, targetItem, st1)
              }
            }
        case None        =>
          state
      }

    private def isDefLike(rec: RecursionKind, tag: Declaration): Boolean =
      rec.isRecursive || tag.isInstanceOf[Declaration.DefFn]

    private def recurTargetNames(
        target: Declaration
    ): Option[NonEmptyList[Bindable]] =
      target match {
        case Declaration.Var(b: Bindable) =>
          Some(NonEmptyList.one(b))
        case Declaration.TupleCons(Nil) =>
          None
        case Declaration.TupleCons(h :: tail) =>
          NonEmptyList(h, tail).traverse {
            case Declaration.Var(b: Bindable) => Some(b)
            case _                            => None
          }
        case _ =>
          None
      }

    @annotation.tailrec
    private def simpleMatchArgNames(
        currentPackage: PackageName,
        arg: TypedExpr[Declaration]
    ): Option[NonEmptyList[Bindable]] =
      arg match {
        case TypedExpr.Local(nm, _, _) => Some(NonEmptyList.one(nm))
        case TypedExpr.Global(pack, nm: Bindable, _, _)
            if pack == currentPackage =>
          Some(NonEmptyList.one(nm))
        case TypedExpr.Generic(_, in)  => simpleMatchArgNames(currentPackage, in)
        case TypedExpr.Annotation(in, _, _) =>
          simpleMatchArgNames(currentPackage, in)
        case _                         => None
      }

    private def recurTag(
        currentPackage: PackageName,
        matchArg: TypedExpr[Declaration],
        tag: Declaration
    ): Option[Declaration.Match] =
      tag match {
        case m @ Declaration.Match(kind, target, _) if kind.isRecursive =>
          (recurTargetNames(target), simpleMatchArgNames(currentPackage, matchArg)) match {
            case (Some(expected), Some(actual)) =>
              val sameNames =
                (expected.length == actual.length) && expected.iterator
                  .zip(actual.iterator)
                  .forall { case (l, r) => l == r }
              if (sameNames) Some(m) else None
            case _ =>
              Some(m)
          }
        case _ =>
          None
      }

    private def checkLoopTailRecursion(
        fnname: Bindable,
        body: TypedExpr[Declaration],
        recur: Declaration.Match
    ): St[Unit] =
      recur.kind match {
        case Declaration.MatchKind.Loop =>
          SelfCallKind(fnname, body) match {
            case SelfCallKind.NonTailCall =>
              failSt(RecursionCheck.LoopRequiresTailRecursion(fnname, recur.region))
            case _ =>
              unitSt
          }
        case _ =>
          unitSt
      }

    @annotation.tailrec
    private def localNameOf(expr: TypedExpr[Declaration]): Option[Bindable] =
      expr match {
        case TypedExpr.Local(nm, _, _) => Some(nm)
        case TypedExpr.Generic(_, in)  => localNameOf(in)
        case TypedExpr.Annotation(in, _, _) => localNameOf(in)
        case _                         => None
      }

    @annotation.tailrec
    private def localOrLocalPackageGlobalNameOf(
        currentPackage: PackageName,
        expr: TypedExpr[Declaration]
    ): Option[Bindable] =
      expr match {
        case TypedExpr.Local(nm, _, _) => Some(nm)
        case TypedExpr.Global(pack, nm: Bindable, _, _)
            if pack == currentPackage =>
          Some(nm)
        case TypedExpr.Generic(_, in)  =>
          localOrLocalPackageGlobalNameOf(currentPackage, in)
        case TypedExpr.Annotation(in, _, _) =>
          localOrLocalPackageGlobalNameOf(currentPackage, in)
        case _                         => None
      }

    @annotation.tailrec
    private def asAnnotatedLambda(
        expr: TypedExpr[Declaration]
    ): Option[TypedExpr.AnnotatedLambda[Declaration]] =
      expr match {
        case lam @ TypedExpr.AnnotatedLambda(_, _, _) => Some(lam)
        case TypedExpr.Generic(_, in)                 => asAnnotatedLambda(in)
        case TypedExpr.Annotation(in, _, _)           => asAnnotatedLambda(in)
        case _                                        => None
      }

    private case class LexStep(
        order: ArgLexOrder,
        immediateError: Option[RecursionCheck.Error],
        noDecreaseError: Option[RecursionCheck.Error]
    )

    private sealed trait ProofOutcome derives CanEqual
    private object ProofOutcome {
      case object Proved extends ProofOutcome
      case class Failed(
          model: Option[String],
          detail: Option[String]
      ) extends ProofOutcome
    }

    private def isIntType(tpe: Type): Boolean =
      tpe.sameAs(Type.IntType)

    private def isBoolType(tpe: Type): Boolean =
      tpe.sameAs(Type.BoolType)

    private def isComparisonType(tpe: Type): Boolean =
      tpe.sameAs(comparisonType)

    private def isLowerableMatchScrutinee(tpe: Type): Boolean =
      isBoolType(tpe) || isComparisonType(tpe) || isIntType(tpe)

    @annotation.tailrec
    private def stripExprWrappers(
        expr: TypedExpr[Declaration]
    ): TypedExpr[Declaration] =
      expr match {
        case TypedExpr.Generic(_, in)       => stripExprWrappers(in)
        case TypedExpr.Annotation(in, _, _) => stripExprWrappers(in)
        case other                          => other
      }

    private def mkAnd(args: Vector[SmtExpr.BoolExpr]): SmtExpr.BoolExpr =
      args.size match {
        case 0 => SmtExpr.BoolConst.True
        case 1 => args.head
        case _ => SmtExpr.And(args)
      }

    private def mkOr(args: Vector[SmtExpr.BoolExpr]): SmtExpr.BoolExpr =
      args.size match {
        case 0 => SmtExpr.BoolConst.False
        case 1 => args.head
        case _ => SmtExpr.Or(args)
      }

    private def simplifyBoolExpr(expr: SmtExpr.BoolExpr): SmtExpr.BoolExpr =
      expr match {
        case SmtExpr.BoolConst(_) | SmtExpr.EqInt(_, _) | SmtExpr.Lt(_, _) |
            SmtExpr.Lte(_, _) | SmtExpr.Gt(_, _) | SmtExpr.Gte(_, _) =>
          expr
        case SmtExpr.Not(in) =>
          simplifyBoolExpr(in) match {
            case SmtExpr.BoolConst(value) => SmtExpr.BoolConst(!value)
            case SmtExpr.Not(in1)         => in1
            case other                    => SmtExpr.Not(other)
          }
        case SmtExpr.And(args) =>
          val simp = args.map(simplifyBoolExpr)
          if (simp.contains(SmtExpr.BoolConst.False)) SmtExpr.BoolConst.False
          else mkAnd(simp.filterNot(_ == SmtExpr.BoolConst.True))
        case SmtExpr.Or(args)  =>
          val simp = args.map(simplifyBoolExpr)
          if (simp.contains(SmtExpr.BoolConst.True)) SmtExpr.BoolConst.True
          else mkOr(simp.filterNot(_ == SmtExpr.BoolConst.False))
        case SmtExpr.Ite(cond, ifTrue, ifFalse) =>
          val c = simplifyBoolExpr(cond)
          val t = simplifyBoolExpr(ifTrue)
          val f = simplifyBoolExpr(ifFalse)
          (c, t, f) match {
            case (_, SmtExpr.BoolConst.True, SmtExpr.BoolConst.False) =>
              c
            case (_, SmtExpr.BoolConst.False, SmtExpr.BoolConst.True) =>
              simplifyBoolExpr(SmtExpr.Not(c))
            case (SmtExpr.BoolConst.True, _, _) =>
              t
            case (SmtExpr.BoolConst.False, _, _) =>
              f
            case _ if t == f =>
              t
            case _ =>
              SmtExpr.Ite(c, t, f)
          }
        case _ =>
          expr
      }

    private def renderBoolForSolver(expr: SmtExpr.BoolExpr): String =
      SmtLibRender
        .renderExpr(SmtExpr.normalizeBoolForSolver(expr))
        .render(120)

    private def comparisonInDomain(
        term: SmtExpr.IntExpr
    ): SmtExpr.BoolExpr =
      mkOr(
        Vector(
          SmtExpr.EqInt(term, SmtExpr.IntConst(BigInt(-1))),
          SmtExpr.EqInt(term, SmtExpr.IntConst(BigInt(0))),
          SmtExpr.EqInt(term, SmtExpr.IntConst(BigInt(1)))
        )
      )

    private def sanitizeSymbolPart(part: String): String = {
      val mapped = part.iterator
        .map {
          case ch if ch.isLetterOrDigit || ch == '_' => ch
          case _                                      => '_'
        }
        .mkString
      val base = if (mapped.isEmpty) "v" else mapped
      if (base.headOption.exists(_.isDigit)) s"v_$base" else base
    }

    private def freshSymbol(
        state: SmtBranchState,
        base: String,
        sort: SmtSort
    ): (String, SmtBranchState) = {
      val sym = s"${sanitizeSymbolPart(base)}_${state.freshId}"
      (
        sym,
        state.copy(
          declarations = state.declarations.updated(sym, sort),
          freshId = state.freshId + 1
        )
      )
    }

    private def ensureIntLocal(
        name: Bindable,
        state: SmtBranchState
    ): (SmtExpr.IntExpr, SmtBranchState) =
      state.intBindings.get(name) match {
        case Some(expr) => (expr, state)
        case None       =>
          val (sym, state1) = freshSymbol(state, name.sourceCodeRepr, SmtSort.IntS)
          val expr = SmtExpr.Var[SmtSort.IntSort](sym)
          (expr, state1.withIntBinding(name, expr))
      }

    private def ensureBoolLocal(
        name: Bindable,
        state: SmtBranchState
    ): (SmtExpr.BoolExpr, SmtBranchState) =
      state.boolBindings.get(name) match {
        case Some(expr) => (expr, state)
        case None       =>
          val (sym, state1) = freshSymbol(state, name.sourceCodeRepr, SmtSort.BoolS)
          val expr = SmtExpr.Var[SmtSort.BoolSort](sym)
          (expr, state1.withBoolBinding(name, expr))
      }

    private def ensureComparisonLocal(
        name: Bindable,
        state: SmtBranchState
    ): (SmtExpr.IntExpr, SmtBranchState) =
      state.comparisonBindings.get(name) match {
        case Some(expr) => (expr, state)
        case None       =>
          val (sym, state1) = freshSymbol(state, name.sourceCodeRepr, SmtSort.IntS)
          val expr = SmtExpr.Var[SmtSort.IntSort](sym)
          (
            expr,
            state1
              .withComparisonBinding(name, expr)
              .addPathFact(comparisonInDomain(expr))
          )
      }

    private def predefFnName(
        fn: TypedExpr[Declaration]
    ): Option[Identifier] =
      stripExprWrappers(fn) match {
        case TypedExpr.Global(PackageName.PredefName, name, _, _) =>
          Some(name)
        case _ =>
          None
      }

    private val lowerablePredefFnNames: Set[Identifier.Name] = Set(
      Identifier.Name("add"),
      Identifier.Name("sub"),
      Identifier.Name("mul"),
      Identifier.Name("div"),
      Identifier.Name("mod_Int"),
      Identifier.Name("cmp_Int"),
      Identifier.Name("eq_Int")
    )

    private def resolvedPredefFnName(
        fn: TypedExpr[Declaration],
        state: SmtBranchState
    ): Option[Identifier.Name] =
      predefFnName(fn)
        .collect { case n: Identifier.Name => n }
        .filter(lowerablePredefFnNames)
        .orElse {
          stripExprWrappers(fn) match {
            case TypedExpr.Global(pack, nm: Bindable, _, _) =>
              state.topLevelPredefAliases.get((pack, nm))
            case _ =>
              None
          }
        }

    private def inlinedTopLevelAliasApp(
        fn: TypedExpr[Declaration],
        args: NonEmptyList[TypedExpr[Declaration]],
        state: SmtBranchState
    ): Option[TypedExpr[Declaration]] =
      stripExprWrappers(fn) match {
        case TypedExpr.Global(pack, nm: Bindable, _, _) =>
          state.topLevelLowerableAliases.get((pack, nm)).flatMap { alias =>
            if (alias.params.length == args.length) {
              val substitutions = alias.params.iterator.zip(args.iterator).map {
                case (param, argExpr) =>
                  param -> ((_: TypedExpr.Local[Declaration]) => argExpr)
              }.toMap
              TypedExpr.substituteAll(substitutions, alias.body, enterLambda = true)
            } else None
          }
        case _ =>
          None
      }

    private def boolLiteralValue(
        expr: TypedExpr[Declaration]
    ): Option[Boolean] =
      stripExprWrappers(expr) match {
        case TypedExpr.Global(
              PackageName.PredefName,
              cons: Identifier.Constructor,
              _,
              _
            ) =>
          cons.asString match {
            case "True"  => Some(true)
            case "False" => Some(false)
            case _       => None
          }
        case _ =>
          None
      }

    private def comparisonLiteralValue(
        expr: TypedExpr[Declaration]
    ): Option[BigInt] =
      stripExprWrappers(expr) match {
        case TypedExpr.Global(
              PackageName.PredefName,
              cons: Identifier.Constructor,
              _,
              _
            ) =>
          cons.asString match {
            case "LT" => Some(BigInt(-1))
            case "EQ" => Some(BigInt(0))
            case "GT" => Some(BigInt(1))
            case _    => None
          }
        case _ =>
          None
      }

    private def lowerBinaryInt[A](
        args: NonEmptyList[TypedExpr[Declaration]],
        state: SmtBranchState
    )(
        fn: (SmtExpr.IntExpr, SmtExpr.IntExpr) => A
    ): (Option[A], SmtBranchState) =
      args.toList match {
        case left :: right :: Nil =>
          val (leftExpr, state1) = lowerIntExpr(left, state)
          val (rightExpr, state2) = lowerIntExpr(right, state1)
          ((leftExpr, rightExpr).mapN(fn), state2)
        case _ =>
          (None, state)
      }

    private def lowerWithLetBinding[A](
        name: Bindable,
        valueExpr: TypedExpr[Declaration],
        inExpr: TypedExpr[Declaration],
        rec: RecursionKind,
        state: SmtBranchState
    )(
        lower: (
            TypedExpr[Declaration],
            SmtBranchState
        ) => (Option[A], SmtBranchState)
    ): (Option[A], SmtBranchState) = {
      val state0 = state.removeBindings(name :: Nil)
      val state1 =
        if (rec.isRecursive) bindSymbolForType(name, valueExpr.getType, state0)
        else state0
      lower(inExpr, bindLetName(name, valueExpr, state1))
    }

    private def lowerCmpIntArgs(
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): (Option[(SmtExpr.IntExpr, SmtExpr.IntExpr)], SmtBranchState) =
      stripExprWrappers(expr) match {
        case TypedExpr.App(fn, args, _, _) =>
          resolvedPredefFnName(fn, state) match {
            case Some(Identifier.Name("cmp_Int")) =>
              lowerBinaryInt(args, state)((_, _))
            case _ =>
              inlinedTopLevelAliasApp(fn, args, state) match {
                case Some(inlined) => lowerCmpIntArgs(inlined, state)
                case None          => (None, state)
              }
          }
        case TypedExpr.Let(name, valueExpr, inExpr, rec, _) =>
          lowerWithLetBinding(name, valueExpr, inExpr, rec, state)(lowerCmpIntArgs)
        case _ =>
          (None, state)
      }

    private def lowerComparisonExpr(
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): (Option[SmtExpr.IntExpr], SmtBranchState) =
      stripExprWrappers(expr) match {
        case TypedExpr.Local(name, tpe, _) if isComparisonType(tpe) =>
          val (cmpExpr, state1) = ensureComparisonLocal(name, state)
          (Some(cmpExpr), state1)
        case TypedExpr.App(fn, args, _, _) =>
          resolvedPredefFnName(fn, state) match {
            case Some(Identifier.Name("cmp_Int")) =>
              val (pair, state1) = lowerBinaryInt(args, state)((_, _))
              val compared = pair.map { case (left, right) =>
                SmtExpr.Ite(
                  SmtExpr.Lt(left, right),
                  SmtExpr.IntConst(BigInt(-1)),
                  SmtExpr.Ite(
                    SmtExpr.EqInt(left, right),
                    SmtExpr.IntConst(BigInt(0)),
                    SmtExpr.IntConst(BigInt(1))
                  )
                )
              }
              (compared, state1)
            case _ =>
              inlinedTopLevelAliasApp(fn, args, state) match {
                case Some(inlined) => lowerComparisonExpr(inlined, state)
                case None          => (None, state)
              }
          }
        case TypedExpr.Let(name, valueExpr, inExpr, rec, _) =>
          lowerWithLetBinding(name, valueExpr, inExpr, rec, state)(lowerComparisonExpr)
        case TypedExpr.Match(arg, branches, _) if isLowerableMatchScrutinee(arg.getType) =>
          lowerComparisonFromMatches(arg, branches, state)
        case other =>
          val lit = comparisonLiteralValue(other).map(SmtExpr.IntConst(_))
          (lit, state)
      }

    private def lowerMatchBranchCondition(
        argExpr: TypedExpr[Declaration],
        branch: TypedExpr.Branch[Declaration],
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) = {
      val (patOpt, state1) = lowerPatternCondition(argExpr, branch.pattern, state)
      val (guardOpt, state2) =
        branch.guard match {
          case Some(guard) => lowerBoolExpr(guard, state1)
          case None        => (Some(SmtExpr.BoolConst.True), state1)
        }
      (
        (patOpt, guardOpt).mapN((pat, guard) =>
          simplifyBoolExpr(mkAnd(Vector(pat, guard)))
        ),
        state2
      )
    }

    private def lowerIntIfExpr(
        arg: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        state: SmtBranchState
    ): (Option[SmtExpr.IntExpr], SmtBranchState) =
      branches.toList match {
        case trueBranch :: falseBranch :: Nil
            if trueBranch.guard.isEmpty &&
              falseBranch.guard.isEmpty &&
              (trueBranch.pattern == Pattern.PositionalStruct(
                (PackageName.PredefName, Identifier.Constructor("True")),
                Nil
              )) &&
              (falseBranch.pattern == Pattern.PositionalStruct(
                (PackageName.PredefName, Identifier.Constructor("False")),
                Nil
              )) =>
          val (condOpt, state1) = lowerBoolExpr(arg, state)
          val (thenOpt, state2) = lowerIntExpr(trueBranch.expr, state1)
          val (elseOpt, state3) = lowerIntExpr(falseBranch.expr, state2)
          ((condOpt, thenOpt, elseOpt).mapN(SmtExpr.Ite(_, _, _)), state3)
        case _ =>
          (None, state)
      }

    private def lowerIntFromMatches(
        argExpr: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        state: SmtBranchState
    ): (Option[SmtExpr.IntExpr], SmtBranchState) =
      branches.reverse.toList match {
        case last :: revInit =>
          val (lastCondOpt, state1) = lowerMatchBranchCondition(argExpr, last, state)
          val stateForLastExpr = bindPatternNames(argExpr, last.pattern, state1)
          val (lastExprOpt, state2) = lowerIntExpr(last.expr, stateForLastExpr)
          (lastCondOpt, lastExprOpt) match {
            // Totality checking guarantees match exhaustiveness, so once every
            // branch condition/result lowers we can use the final branch as the
            // default arm in the ite chain.
            // We lower branch expressions with bindPatternNames so aliases such
            // as `case 12 as twelve:` are available while building SMT terms.
            case (Some(_), Some(lastExpr)) =>
              val initState = (
                lastExpr,
                SmtExpr.BoolConst.True: SmtExpr.BoolExpr,
                state2,
                true
              )
              val (lowered, _, stateN, ok) =
                revInit.foldLeft(initState) {
                  case ((elseExpr, priorMiss, st, allOk), branch) =>
                    val (condOpt, st1) = lowerMatchBranchCondition(argExpr, branch, st)
                    val stForExpr = bindPatternNames(argExpr, branch.pattern, st1)
                    val (exprOpt, st2) = lowerIntExpr(branch.expr, stForExpr)
                    (condOpt, exprOpt) match {
                      case (Some(cond), Some(value)) =>
                        val hit = mkAnd(Vector(priorMiss, cond))
                        val nextElse = SmtExpr.Ite(hit, value, elseExpr)
                        val nextMiss = mkAnd(Vector(priorMiss, SmtExpr.Not(cond)))
                        (nextElse, nextMiss, st2, allOk)
                      case _ =>
                        (elseExpr, priorMiss, st2, false)
                    }
                }
              if (ok) (Some(lowered), stateN) else (None, stateN)
            case _ =>
              (None, state2)
          }
        case _ =>
          (None, state)
      }

    private def lowerIntExpr(
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): (Option[SmtExpr.IntExpr], SmtBranchState) =
      stripExprWrappers(expr) match {
        case TypedExpr.Literal(Lit.Integer(value), _, _) =>
          (Some(SmtExpr.IntConst(value)), state)
        case TypedExpr.Local(name, tpe, _) if isIntType(tpe) =>
          val (intExpr, state1) = ensureIntLocal(name, state)
          (Some(intExpr), state1)
        case TypedExpr.App(fn, args, _, _) =>
          resolvedPredefFnName(fn, state) match {
            case Some(Identifier.Name("add")) =>
              lowerBinaryInt(args, state)((left, right) =>
                SmtExpr.Add(Vector(left, right))
              )
            case Some(Identifier.Name("sub")) =>
              lowerBinaryInt(args, state)((left, right) =>
                SmtExpr.Sub(Vector(left, right))
              )
            case Some(Identifier.Name("mul")) =>
              lowerBinaryInt(args, state)((left, right) =>
                SmtExpr.Mul(Vector(left, right))
              )
            case Some(Identifier.Name("div")) =>
              lowerBinaryInt(args, state)((left, right) =>
                // Bosatsu defines `x / 0 == 0`; Z3 div on zero does not.
                SmtExpr.Ite(
                  SmtExpr.EqInt(right, SmtExpr.IntConst(BigInt(0))),
                  SmtExpr.IntConst(BigInt(0)),
                  SmtExpr.Div(left, right)
                )
              )
            case Some(Identifier.Name("mod_Int")) =>
              lowerBinaryInt(args, state)((left, right) =>
                // Bosatsu defines `x % 0 == x`; model that instead of SMT's mod-by-zero behavior.
                SmtExpr.Ite(
                  SmtExpr.EqInt(right, SmtExpr.IntConst(BigInt(0))),
                  left,
                  SmtExpr.Mod(left, right)
                )
              )
            case _ =>
              inlinedTopLevelAliasApp(fn, args, state) match {
                case Some(inlined) => lowerIntExpr(inlined, state)
                case None          => (None, state)
              }
          }
        case TypedExpr.Match(arg, branches, _) if isBoolType(arg.getType) =>
          val loweredIf @ (ifOpt, state1) = lowerIntIfExpr(arg, branches, state)
          if (ifOpt.nonEmpty) loweredIf
          else lowerIntFromMatches(arg, branches, state1)
        case TypedExpr.Match(arg, branches, _) =>
          lowerIntFromMatches(arg, branches, state)
        case TypedExpr.Let(name, valueExpr, inExpr, rec, _) =>
          lowerWithLetBinding(name, valueExpr, inExpr, rec, state)(lowerIntExpr)
        case _ =>
          (None, state)
      }

    private def lowerComparisonFromMatches(
        argExpr: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        state: SmtBranchState
    ): (Option[SmtExpr.IntExpr], SmtBranchState) =
      branches.reverse.toList match {
        case last :: revInit =>
          val (lastCondOpt, state1) = lowerMatchBranchCondition(argExpr, last, state)
          val stateForLastExpr = bindPatternNames(argExpr, last.pattern, state1)
          val (lastExprOpt, state2) = lowerComparisonExpr(last.expr, stateForLastExpr)
          (lastCondOpt, lastExprOpt) match {
            case (Some(_), Some(lastExpr)) =>
              val initState = (
                lastExpr,
                SmtExpr.BoolConst.True: SmtExpr.BoolExpr,
                state2,
                true
              )
              val (lowered, _, stateN, ok) =
                revInit.foldLeft(initState) {
                  case ((elseExpr, priorMiss, st, allOk), branch) =>
                    val (condOpt, st1) = lowerMatchBranchCondition(argExpr, branch, st)
                    val stForExpr = bindPatternNames(argExpr, branch.pattern, st1)
                    val (exprOpt, st2) = lowerComparisonExpr(branch.expr, stForExpr)
                    (condOpt, exprOpt) match {
                      case (Some(cond), Some(value)) =>
                        val hit = mkAnd(Vector(priorMiss, cond))
                        val nextElse = SmtExpr.Ite(hit, value, elseExpr)
                        val nextMiss = mkAnd(Vector(priorMiss, SmtExpr.Not(cond)))
                        (nextElse, nextMiss, st2, allOk)
                      case _ =>
                        (elseExpr, priorMiss, st2, false)
                    }
                }
              if (ok) (Some(lowered), stateN) else (None, stateN)
            case _ =>
              (None, state2)
          }
        case _ =>
          (None, state)
      }

    private def comparisonGoalFromPattern(
        argExpr: TypedExpr[Declaration],
        cons: Identifier.Constructor,
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) = {
      val (cmpPair, state1) = lowerCmpIntArgs(argExpr, state)
      val direct =
        cmpPair.map { case (left, right) =>
            cons.asString match {
              case "LT" => SmtExpr.Lt(left, right)
              case "EQ" => SmtExpr.EqInt(left, right)
              case "GT" => SmtExpr.Gt(left, right)
              case _    => SmtExpr.BoolConst.False
            }
        }

      direct match {
        case Some(expr) => (Some(expr), state1)
        case None       =>
          val (cmpExpr, state2) = lowerComparisonExpr(argExpr, state)
          val asEq = cmpExpr.flatMap { term =>
            cons.asString match {
              case "LT" => Some(SmtExpr.EqInt(term, SmtExpr.IntConst(BigInt(-1))))
              case "EQ" => Some(SmtExpr.EqInt(term, SmtExpr.IntConst(BigInt(0))))
              case "GT" => Some(SmtExpr.EqInt(term, SmtExpr.IntConst(BigInt(1))))
              case _    => None
            }
          }
          (asEq, state2)
      }
    }

    private def patternIsIrrefutable(
        pattern: Pattern[(PackageName, Identifier.Constructor), Type],
        state: SmtBranchState
    ): Boolean =
      state.totalityCheck.missingBranches(pattern :: Nil).isEmpty

    private def lowerPatternCondition(
        argExpr: TypedExpr[Declaration],
        pattern: Pattern[(PackageName, Identifier.Constructor), Type],
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) =
      // Lower a branch pattern into a boolean condition over the scrutinee.
      // The resulting expression is used as a path fact when proving recursion
      // obligations inside that branch.
      pattern match {
        case Pattern.WildCard =>
          (Some(SmtExpr.BoolConst.True), state)
        case Pattern.Var(_) =>
          // Pattern variable bindings are handled in bindPatternNames.
          (Some(SmtExpr.BoolConst.True), state)
        case Pattern.Named(_, inner) =>
          lowerPatternCondition(argExpr, inner, state)
        case Pattern.Annotation(inner, _) =>
          lowerPatternCondition(argExpr, inner, state)
        case Pattern.Union(head, rest) =>
          val (bools, state1, ok) = (head :: rest.toList).foldLeft(
            (List.empty[SmtExpr.BoolExpr], state, true)
          ) { case ((acc, st, ok), pat) =>
            val (condOpt, st1) = lowerPatternCondition(argExpr, pat, st)
            condOpt match {
              case Some(cond) => (cond :: acc, st1, ok)
              case None       => (acc, st1, false)
            }
          }
          val bools1 =
            if (ok) Some(mkOr(bools.reverse.toVector))
            else None
          (bools1, state1)
        case Pattern.Literal(Lit.Integer(value)) =>
          val (argInt, state1) = lowerIntExpr(argExpr, state)
          (
            argInt.map(SmtExpr.EqInt(_, SmtExpr.IntConst(value))),
            state1
          )
        case Pattern.Literal(_) =>
          (None, state)
        case Pattern.PositionalStruct((pack, cons), params)
            if (pack == PackageName.PredefName) && params.isEmpty =>
          cons.asString match {
            case "True" | "False" =>
              val (argBool, state1) = lowerBoolExpr(argExpr, state)
              val cond = argBool.map { b =>
                if (cons.asString == "True") b else SmtExpr.Not(b)
              }
              (cond, state1)
            case "LT" | "EQ" | "GT" =>
              comparisonGoalFromPattern(argExpr, cons, state)
            case _ =>
              (None, state)
          }
        case ps @ Pattern.PositionalStruct(_, _)
            if patternIsIrrefutable(ps, state) =>
          (Some(SmtExpr.BoolConst.True), state)
        case Pattern.PositionalStruct(_, _) =>
          (None, state)
        case Pattern.ListPat(_) | Pattern.StrPat(_) =>
          (None, state)
      }

    private def lowerBoolFromMatches(
        argExpr: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) =
      branches.reverse.toList match {
        case last :: revInit =>
          val (lastCondOpt, state1) = lowerMatchBranchCondition(argExpr, last, state)
          val stateForLastExpr = bindPatternNames(argExpr, last.pattern, state1)
          val (lastExprOpt, state2) = lowerBoolExpr(last.expr, stateForLastExpr)
          (lastCondOpt, lastExprOpt) match {
            case (Some(_), Some(lastExpr)) =>
              val initState = (
                lastExpr,
                SmtExpr.BoolConst.True: SmtExpr.BoolExpr,
                state2,
                true
              )
              val (lowered, _, stateN, ok) =
                revInit.foldLeft(initState) {
                  case ((elseExpr, priorMiss, st, allOk), branch) =>
                    val (condOpt, st1) = lowerMatchBranchCondition(argExpr, branch, st)
                    val stForExpr = bindPatternNames(argExpr, branch.pattern, st1)
                    val (exprOpt, st2) = lowerBoolExpr(branch.expr, stForExpr)
                    (condOpt, exprOpt) match {
                      case (Some(cond), Some(value)) =>
                        val hit = mkAnd(Vector(priorMiss, cond))
                        val nextElse = SmtExpr.Ite(hit, value, elseExpr)
                        val nextMiss = mkAnd(Vector(priorMiss, SmtExpr.Not(cond)))
                        (nextElse, nextMiss, st2, allOk)
                      case _ =>
                        (elseExpr, priorMiss, st2, false)
                    }
                }
              if (ok) (Some(lowered), stateN) else (None, stateN)
            case _ =>
              (None, state2)
          }
        case _ =>
          (None, state)
      }

    private def lowerBoolExpr(
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) =
      stripExprWrappers(expr) match {
        case TypedExpr.Local(name, tpe, _) if isBoolType(tpe) =>
          val (boolExpr, state1) = ensureBoolLocal(name, state)
          (Some(boolExpr), state1)
        case TypedExpr.App(fn, args, _, _) =>
          resolvedPredefFnName(fn, state) match {
            case Some(Identifier.Name("eq_Int")) =>
              lowerBinaryInt(args, state)(SmtExpr.EqInt(_, _))
            case _ =>
              inlinedTopLevelAliasApp(fn, args, state) match {
                case Some(inlined) => lowerBoolExpr(inlined, state)
                case None          => (None, state)
              }
          }
        case TypedExpr.Match(arg, branches, _) =>
          lowerBoolFromMatches(arg, branches, state)
        case TypedExpr.Let(name, valueExpr, inExpr, rec, _) =>
          lowerWithLetBinding(name, valueExpr, inExpr, rec, state)(lowerBoolExpr)
        case other =>
          (boolLiteralValue(other).map(SmtExpr.BoolConst(_)), state)
      }

    private def bindPatternNames(
        argExpr: TypedExpr[Declaration],
        pattern: Pattern[(PackageName, Identifier.Constructor), Type],
        state: SmtBranchState
    ): SmtBranchState = {
      def bindOne(
          name: Bindable,
          tpe: Type,
          st: SmtBranchState
      ): SmtBranchState =
        if (isIntType(tpe)) {
          val (intExprOpt, st1) = lowerIntExpr(argExpr, st)
          intExprOpt match {
            case Some(intExpr) => st1.withIntBinding(name, intExpr)
            case None          =>
              val (fresh, st2) = ensureIntLocal(name, st1)
              st2.withIntBinding(name, fresh)
          }
        } else if (isBoolType(tpe)) {
          val (boolExprOpt, st1) = lowerBoolExpr(argExpr, st)
          boolExprOpt match {
            case Some(boolExpr) => st1.withBoolBinding(name, boolExpr)
            case None           =>
              val (fresh, st2) = ensureBoolLocal(name, st1)
              st2.withBoolBinding(name, fresh)
          }
        } else if (isComparisonType(tpe)) {
          val (cmpExprOpt, st1) = lowerComparisonExpr(argExpr, st)
          cmpExprOpt match {
            case Some(cmpExpr) => st1.withComparisonBinding(name, cmpExpr)
            case None          =>
              val (fresh, st2) = ensureComparisonLocal(name, st1)
              st2.withComparisonBinding(name, fresh)
          }
        } else st

      pattern match {
        case Pattern.Var(name) =>
          bindOne(name, argExpr.getType, state)
        case Pattern.Named(name, inner) =>
          val bound = bindOne(name, argExpr.getType, state)
          bindPatternNames(argExpr, inner, bound)
        case Pattern.Annotation(inner, _) =>
          bindPatternNames(argExpr, inner, state)
        case _ =>
          state
      }
    }

    private def addPatternFactsAndBindings(
        argExpr: TypedExpr[Declaration],
        pattern: Pattern[(PackageName, Identifier.Constructor), Type],
        state: SmtBranchState
    ): SmtBranchState = {
      val state0 = state.removeBindings(pattern.names)
      val (patCondOpt, state1) = lowerPatternCondition(argExpr, pattern, state0)
      val state2 = patCondOpt match {
        case Some(SmtExpr.BoolConst.True) => state1
        case Some(cond)                    => state1.addPathFact(simplifyBoolExpr(cond))
        case None                          => state1
      }
      bindPatternNames(argExpr, pattern, state2)
    }

    private def addGuardPathFact(
        guard: TypedExpr[Declaration],
        state: SmtBranchState
    ): SmtBranchState = {
      val (guardOpt, state1) = lowerBoolExpr(guard, state)
      guardOpt match {
        case Some(SmtExpr.BoolConst.True) => state1
        case Some(cond)                    => state1.addPathFact(simplifyBoolExpr(cond))
        case None                          => state1
      }
    }

    private def addPathFactIfNonTrivial(
        fact: SmtExpr.BoolExpr,
        state: SmtBranchState
    ): SmtBranchState =
      simplifyBoolExpr(fact) match {
        case SmtExpr.BoolConst.True => state
        case other                  => state.addPathFact(other)
      }

    private def lowerBranchHitCondition(
        argExpr: TypedExpr[Declaration],
        branch: TypedExpr.Branch[Declaration],
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) = {
      val state0 = state.removeBindings(branch.pattern.names)
      val state1 = bindPatternNames(argExpr, branch.pattern, state0)
      lowerMatchBranchCondition(argExpr, branch, state1)
    }

    private case class FallthroughFacts(
        facts: NonEmptyList[(TypedExpr.Branch[Declaration], SmtExpr.BoolExpr)],
        symbolState: SmtBranchState
    )

    private def mergeSymbolState(
        symbols: SmtBranchState,
        state: SmtBranchState
    ): SmtBranchState =
      state.copy(
        declarations = state.declarations ++ symbols.declarations,
        freshId = state.freshId.max(symbols.freshId)
      )

    private def matchFallthroughFacts(
        argExpr: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        initialState: SmtBranchState
    ): FallthroughFacts = {
      val baseIntBindings = initialState.intBindings
      val baseBoolBindings = initialState.boolBindings
      val baseComparisonBindings = initialState.comparisonBindings
      val basePathFacts = initialState.pathFacts

      def resetBranchBindings(state: SmtBranchState): SmtBranchState =
        state.copy(
          intBindings = baseIntBindings,
          boolBindings = baseBoolBindings,
          comparisonBindings = baseComparisonBindings,
          pathFacts = basePathFacts
        )

      // This returns one fallthrough fact per branch, in the same order
      // and size as `branches`.
      val init: (
          List[(TypedExpr.Branch[Declaration], SmtExpr.BoolExpr)],
          SmtExpr.BoolExpr,
          SmtBranchState
      ) =
        (Nil, SmtExpr.BoolConst.True, initialState)

      val (revFacts, _, stateN) =
        branches.toList.foldLeft(init) {
          case ((acc, priorMiss, state), branch) =>
            val state0 = resetBranchBindings(state)
            val (hitOpt, state1) = lowerBranchHitCondition(argExpr, branch, state0)
            val nextMiss = hitOpt match {
              case Some(hit) =>
                simplifyBoolExpr(mkAnd(Vector(priorMiss, SmtExpr.Not(hit))))
              case None      =>
                priorMiss
            }
            ((branch, priorMiss) :: acc, nextMiss, state1)
        }

      FallthroughFacts(
        NonEmptyList.fromListUnsafe(revFacts.reverse),
        // Keep only declaration/fresh-symbol effects from lowering branch-hit
        // conditions; branch-local bindings/path facts should not leak across
        // branches.
        resetBranchBindings(stateN)
      )
    }

    private def patternSubsumes(
        superPattern: TypedPattern,
        subPattern: TypedPattern,
        state: SmtBranchState
    ): Boolean =
      state.totalityCheck.difference(subPattern, superPattern).isEmpty

    private def alignSubsumedGuardExprAlternatives(
        guardExpr: TypedExpr[Declaration],
        superPattern: TypedPattern,
        subPattern: TypedPattern
    ): List[TypedExpr[Declaration]] = {
      val guardFree = guardExpr.freeVarsDup.toSet
      val superBoundNames = superPattern.names.toSet
      val guardBoundByPattern = guardFree.intersect(superBoundNames)
      NameMap
        .alignSubsumedPatternNames(superPattern, subPattern)
        .toList
        .flatMap(_.substitutionAlternatives(guardBoundByPattern))
        .flatMap { renames =>
          val substitutions: Map[
            Bindable,
            TypedExpr.Local[Declaration] => TypedExpr[Declaration]
          ] =
            renames.iterator.collect {
              case (from, to) if to != from =>
                from -> { (local: TypedExpr.Local[Declaration]) =>
                  TypedExpr.Local(to, local.tpe, local.tag): TypedExpr[
                    Declaration
                  ]
                }
            }.toMap

          if (substitutions.isEmpty) List(guardExpr)
          else
            TypedExpr
              .substituteAll(substitutions, guardExpr, enterLambda = true)
              .toList
        }
        .distinct
    }

    private def availableBranchNames(
        currentPattern: TypedPattern,
        state: SmtBranchState
    ): Set[Bindable] =
      state.intBindings.keySet ++
        state.boolBindings.keySet ++
        state.comparisonBindings.keySet ++
        currentPattern.names

    private def addSubsumedGuardFallthroughFacts(
        priorBranches: List[TypedExpr.Branch[Declaration]],
        currentPattern: TypedPattern,
        state: SmtBranchState
    ): SmtBranchState =
      // If a previous branch pattern fully covers the current pattern, falling
      // through implies that prior guard was false.
      priorBranches.foldLeft(state) { (st, priorBranch) =>
        priorBranch.guard match {
          case Some(guardExpr)
              if patternSubsumes(priorBranch.pattern, currentPattern, st) =>
            val alignedGuards =
              alignSubsumedGuardExprAlternatives(
                guardExpr,
                priorBranch.pattern,
                currentPattern
              )

            val (condsRev, st1) =
              alignedGuards.foldLeft((List.empty[SmtExpr.BoolExpr], st)) {
                case ((conds, st0), alignedGuardExpr) =>
                  val guardFree = alignedGuardExpr.freeVarsDup.toSet
                  if (guardFree.subsetOf(availableBranchNames(currentPattern, st0))) {
                    val (guardOpt, stNext) = lowerBoolExpr(alignedGuardExpr, st0)
                    guardOpt match {
                      case Some(guardCond) =>
                        (simplifyBoolExpr(guardCond) :: conds, stNext)
                      case None =>
                        (conds, stNext)
                    }
                  } else (conds, st0)
              }

            addPathFactIfNonTrivial(
              SmtExpr.Not(mkOr(condsRev.reverse.distinct.toVector)),
              st1
            )
          case _ =>
            st
        }
      }

    private def buildPathCondition(state: SmtBranchState): SmtExpr.BoolExpr =
      mkAnd(state.pathFacts)

    private def renderPathCondition(state: SmtBranchState): String =
      renderBoolForSolver(buildPathCondition(state))

    private def renderModel(model: Option[Vector[SExpr]]): Option[String] =
      model.map(SExpr.renderAll(_))

    private def prove(
        goal: SmtExpr.BoolExpr,
        state: SmtBranchState
    ): ProofOutcome = {
      if (SmtExpr.pathImplies(goal, state.pathFacts)) {
        ProofOutcome.Proved
      } else {
        val goal1 = SmtExpr.normalizeBoolForSolver(goal)
        val pathCondition1 = SmtExpr.normalizeBoolForSolver(buildPathCondition(state))
        val declarations = state.declarations.toList.sortBy(_._1).map {
          case (name, sort) =>
            SmtCommand.DeclareConst(name, sort)
        }
        val script = SmtScript(
          Vector(SmtCommand.SetLogic.QF_LIA) ++
            declarations ++
            Vector(
              SmtCommand.Assert(pathCondition1),
              SmtCommand.Assert(SmtExpr.Not(goal1)),
              SmtCommand.CheckSat
            )
        )

        val undeclared = SmtScriptScope.undeclaredVars(script)
        if (undeclared.nonEmpty) {
          ProofOutcome.Failed(
            None,
            Some(
              s"internal SMT script uses undeclared variables: ${undeclared.toList.sorted.mkString(", ")}"
            )
          )
        } else
          Z3Api.run(script, parseModel = false, z3Runner) match {
          case Right(res) =>
            res.status match {
              case Z3Api.Status.Unsat =>
                ProofOutcome.Proved
              case Z3Api.Status.Sat   =>
                val withModel = SmtScript(script.commands :+ SmtCommand.GetModel)
                Z3Api.run(withModel, z3Runner) match {
                  case Right(modelRes) =>
                    ProofOutcome.Failed(renderModel(modelRes.model), None)
                  case Left(err)       =>
                    ProofOutcome.Failed(None, Some(err.message))
                }
              case Z3Api.Status.Unknown =>
                ProofOutcome.Failed(None, Some("solver returned unknown"))
            }
          case Left(err) =>
            ProofOutcome.Failed(
              None,
              Some(err.message)
            )
        }
      }
    }

    private def targetItemType(
        inrec: InDefRecurred,
        targetItem: RecurTargetItem
    ): Type =
      inrec.inRec.typedArgs
        .get(targetItem.group.toLong)
        .flatMap(_.get(targetItem.index.toLong))
        .map(_._2)
        .getOrElse {
          // $COVERAGE-OFF$ this should be unreachable when target indices are valid
          sys.error(s"invalid recur target index: $targetItem")
          // $COVERAGE-ON$
        }

    private def bindSymbolForType(
        name: Bindable,
        tpe: Type,
        state: SmtBranchState
    ): SmtBranchState =
      if (isIntType(tpe)) {
        val (_, state1) = ensureIntLocal(name, state)
        state1
      } else if (isBoolType(tpe)) {
        val (_, state1) = ensureBoolLocal(name, state)
        state1
      } else if (isComparisonType(tpe)) {
        val (_, state1) = ensureComparisonLocal(name, state)
        state1
      } else state

    private def initBranchSmtState(inrec: InDefRecurred): SmtBranchState =
      inrec.inRec.typedArgs.iterator.flatMap(_.iterator).foldLeft(
        SmtBranchState.empty(inrec.inRec.totalityCheck).copy(
          topLevelLowerableAliases = inrec.inRec.topLevelLowerableAliases,
          topLevelPredefAliases = inrec.inRec.topLevelPredefAliases
        )
      ) { case (state, (name, tpe)) =>
        bindSymbolForType(name, tpe, state)
      }

    private def bindLetName(
        name: Bindable,
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): SmtBranchState = {
      val tpe = expr.getType
      if (isIntType(tpe)) {
        val (intExprOpt, state1) = lowerIntExpr(expr, state)
        intExprOpt match {
          case Some(intExpr) => state1.withIntBinding(name, intExpr)
          case None          =>
            val (fresh, state2) = ensureIntLocal(name, state1)
            state2.withIntBinding(name, fresh)
        }
      } else if (isBoolType(tpe)) {
        val (boolExprOpt, state1) = lowerBoolExpr(expr, state)
        boolExprOpt match {
          case Some(boolExpr) => state1.withBoolBinding(name, boolExpr)
          case None           =>
            val (fresh, state2) = ensureBoolLocal(name, state1)
            state2.withBoolBinding(name, fresh)
        }
      } else if (isComparisonType(tpe)) {
        val (cmpExprOpt, state1) = lowerComparisonExpr(expr, state)
        cmpExprOpt match {
          case Some(cmpExpr) => state1.withComparisonBinding(name, cmpExpr)
          case None          =>
            val (fresh, state2) = ensureComparisonLocal(name, state1)
            state2.withComparisonBinding(name, fresh)
        }
      } else state
    }

    @annotation.tailrec
    private def unwrapDeclExpr(
        decl: Declaration
    ): Declaration =
      decl match {
        case Declaration.Annotation(inner, _) => unwrapDeclExpr(inner)
        case Declaration.Parens(inner)        => unwrapDeclExpr(inner)
        case other                            => other
      }

    private def singletonCtorFromArgExpr(
        arg: TypedExpr[Declaration]
    ): Option[SingletonCtor] = {
      unwrapDeclExpr(arg.tag) match {
        case Declaration.ListDecl(ListLang.Cons(Nil)) =>
          Some(emptyListSingletonCtor)
        case _                                         =>
          stripExprWrappers(arg) match {
            case TypedExpr.Global(
                  pack,
                  cons: Identifier.Constructor,
                  _,
                  _
                ) =>
              Some(SingletonCtor(pack, cons))
            case _ =>
              None
          }
      }
    }

    @annotation.tailrec
    private def constructorAppFromExpr(
        expr: TypedExpr[Declaration],
        acc: List[TypedExpr[Declaration]]
    ): Option[ConstructorApp] =
      stripExprWrappers(expr) match {
        case TypedExpr.App(fn, args, _, _) =>
          constructorAppFromExpr(fn, args.toList ::: acc)
        case TypedExpr.Global(
              pack,
              cons: Identifier.Constructor,
              _,
              _
            ) =>
          Some(ConstructorApp(KnownCtor(pack, cons), acc))
        case _ =>
          None
      }

    private def constructorAppFromArgExpr(
        arg: TypedExpr[Declaration]
    ): Option[ConstructorApp] =
      unwrapDeclExpr(arg.tag) match {
        case Declaration.ListDecl(ListLang.Cons(Nil)) =>
          Some(ConstructorApp(emptyListKnownCtor, Nil))
        case _                                         =>
          constructorAppFromExpr(arg, Nil)
      }

    private case class ConstructorMeta(
        owner: DefinedType[Kind.Arg],
        ctor: ConstructorFn[Kind.Arg],
        index: Int
    )

    private def constructorMeta(
        typeEnv: TypeEnv[Kind.Arg],
        ctor: KnownCtor
    ): Option[ConstructorMeta] =
      typeEnv.getConstructor(ctor.pack, ctor.cons).flatMap { case (owner, cfn) =>
        owner.constructors.zipWithIndex.find(_._1.name == cfn.name).map {
          case (_, idx) =>
            ConstructorMeta(owner, cfn, idx)
        }
      }

    private def fieldPayloadAllowed(
        fieldType: Type,
        fieldArgExpr: TypedExpr[Declaration],
        allowedSmallerNames: Set[Bindable],
        targetType: Type
    ): Boolean =
      // v1 safety rule: exact recursive payloads must use known-smaller names,
      // and wrapped recursive payloads are rejected conservatively.
      if (!Type.containsType(fieldType, targetType)) true
      else if (fieldType.sameAs(targetType))
        localNameOf(fieldArgExpr).exists(allowedSmallerNames)
      else false

    private def classifyConstructorRankArg(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        currentCtor: KnownCtor,
        allowedSmallerNames: Set[Bindable],
        arg: TypedExpr[Declaration]
    ): ArgLexOrder = {
      val typeEnv = inrec.inRec.totalityCheck.inEnv
      val targetType = targetItemType(inrec, target)
      val maybeSmaller =
        for {
          argCtorApp <- constructorAppFromArgExpr(arg)
          currentMeta <- constructorMeta(typeEnv, currentCtor)
          nextMeta <- constructorMeta(typeEnv, argCtorApp.ctor)
          if currentMeta.owner.toTypeConst == nextMeta.owner.toTypeConst
          targetArgs <- currentMeta.owner.extractTypeArgs(targetType)
          if nextMeta.index < currentMeta.index
          fieldTypes <- nextMeta.owner.instantiateConstructorFieldTypes(
            nextMeta.ctor,
            targetArgs
          )
          if fieldTypes.lengthCompare(argCtorApp.args.length) == 0
          if fieldTypes.iterator
            .zip(argCtorApp.args.iterator)
            .forall { case (fieldType, fieldArg) =>
              fieldPayloadAllowed(
                fieldType,
                fieldArg,
                allowedSmallerNames,
                targetType
              )
            }
        } yield Smaller

      maybeSmaller.getOrElse(Other)
    }

    private def isCanonicalUnitLiteral(
        expr: TypedExpr[Declaration]
    ): Boolean =
      unwrapDeclExpr(expr.tag) match {
        case Declaration.TupleCons(Nil) => true
        case _                          =>
          stripExprWrappers(expr) match {
            case TypedExpr.Global(
                  PackageName.PredefName,
                  cons: Identifier.Constructor,
                  _,
                  _
                ) =>
              cons == unitCtorName
            case _ =>
              false
          }
      }

    private def oneArgApp(
        expr: TypedExpr[Declaration]
    ): Option[(TypedExpr[Declaration], TypedExpr[Declaration])] =
      stripExprWrappers(expr) match {
        case TypedExpr.App(fn, args, _, _) if args.tail.isEmpty =>
          Some((fn, args.head))
        case _ =>
          None
      }

    private def hasUnitToTargetType(
        fnExpr: TypedExpr[Declaration],
        targetType: Type
    ): Boolean =
      // Keep this force recognizer tied to the current recur-target component
      // type; we intentionally do not treat force as a generic wrapper-unroll.
      fnExpr.getType match {
        case Type.Fun(args, resultType) =>
          args.tail.isEmpty &&
          args.head.sameAs(Type.UnitType) &&
          resultType.sameAs(targetType)
        case _ =>
          false
      }

    private def hasLazyTargetType(
        expr: TypedExpr[Declaration],
        targetType: Type
    ): Boolean =
      // Same boundary as thunk forcing: only Lazy[targetType] qualifies.
      expr.getType match {
        case Type.TyApply(Type.TyConst(const), itemType) =>
          (const == lazyTypeConst) && itemType.sameAs(targetType)
        case _ =>
          false
      }

    private def isTrustedGlobalFn(
        expr: TypedExpr[Declaration],
        pack: PackageName,
        name: Identifier.Name
    ): Boolean =
      stripExprWrappers(expr) match {
        case TypedExpr.Global(p0, n0: Identifier.Name, _, _) =>
          (p0 == pack) && (n0 == name)
        case _ =>
          false
      }

    private def classifyThunkForceArg(
        targetType: Type,
        allowed: Set[Bindable],
        arg: TypedExpr[Declaration]
    ): Option[ArgLexOrder] =
      // Narrow exception: forcing an already-proven smaller local thunk.
      oneArgApp(arg).collect {
        case (fnExpr, unitArg)
            if isCanonicalUnitLiteral(unitArg) &&
              localNameOf(fnExpr).exists(allowed) &&
              hasUnitToTargetType(fnExpr, targetType) =>
          Smaller
      }

    private def classifyLazyForceArg(
        targetType: Type,
        allowed: Set[Bindable],
        arg: TypedExpr[Declaration]
    ): Option[ArgLexOrder] =
      // Trust boundary: only the canonical Bosatsu/Lazy.get_Lazy force form.
      oneArgApp(arg).collect {
        case (fnExpr, lazyArg)
            if isTrustedGlobalFn(fnExpr, lazyPackageName, getLazyName) &&
              localNameOf(lazyArg).exists(allowed) &&
              hasLazyTargetType(lazyArg, targetType) =>
          Smaller
      }

    private def classifyStructuralArg(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        allowed: Set[Bindable],
        equalAliases: Set[Bindable],
        singletonCtor: Option[SingletonCtor],
        currentCtor: Option[KnownCtor],
        arg: TypedExpr[Declaration]
    ): LexStep = {
      val targetType = targetItemType(inrec, target)
      val order =
        localNameOf(arg) match {
          case Some(nm) if allowed(nm)            => Smaller
          case Some(nm) if (nm == target.paramName) || equalAliases(nm) =>
            Equal
          case _                                  =>
            singletonCtor match {
              case Some(s0) if singletonCtorFromArgExpr(arg).contains(s0) =>
                Equal
              case _                                                      =>
                classifyThunkForceArg(targetType, allowed, arg)
                  .orElse(classifyLazyForceArg(targetType, allowed, arg))
                  .orElse(
                    currentCtor.map(
                      classifyConstructorRankArg(
                        inrec,
                        target,
                        _,
                        allowed,
                        arg
                      )
                    )
                  )
                  .getOrElse(Other)
            }
        }
      LexStep(order, None, None)
    }

    private def intObligationError(
        fnname: Bindable,
        target: RecurTargetItem,
        obligation: SmtExpr.BoolExpr,
        state: SmtBranchState,
        proof: ProofOutcome,
        region: Region
    ): RecursionCheck.Error = {
      val (model, detail) =
        proof match {
          case ProofOutcome.Proved => (None, None)
          case ProofOutcome.Failed(m, d) => (m, d)
        }
      RecursionCheck.IntRecursionObligationFailed(
        fnname,
        target.paramName,
        renderBoolForSolver(obligation),
        renderPathCondition(state),
        model,
        detail,
        region
      )
    }

    private def lowerAsIntOrFail(
        fnname: Bindable,
        target: RecurTargetItem,
        arg: TypedExpr[Declaration],
        state: SmtBranchState,
        region: Region
    ): (Option[SmtExpr.IntExpr], SmtBranchState, Option[RecursionCheck.Error]) = {
      val (argOpt, state1) = lowerIntExpr(arg, state)
      argOpt match {
        case Some(intExpr) =>
          (Some(intExpr), state1, None)
        case None          =>
          val targetRepr = target.paramName.sourceCodeRepr
          val hint =
            s"hint: rewrite recursive argument using canonical Int operations, e.g. $targetRepr.sub(1) or $targetRepr.add(-1)"
          val detail =
            Some(
              s"unable to lower recursive argument ${arg.reprString} to SMT Int expression; $hint"
            )
          (
            None,
            state1,
            Some(
              RecursionCheck.IntRecursionObligationFailed(
                fnname,
                target.paramName,
                "lower recursive Int argument",
                renderPathCondition(state1),
                None,
                detail,
                region
              )
            )
          )
      }
    }

    private def classifyIntArg(
        fnname: Bindable,
        target: RecurTargetItem,
        arg: TypedExpr[Declaration],
        state: SmtBranchState,
        region: Region
    ): (LexStep, SmtBranchState) = {
      val (argOpt, state1, lowerErr) =
        lowerAsIntOrFail(fnname, target, arg, state, region)
      lowerErr match {
        case Some(err) =>
          (LexStep(Other, Some(err), None), state1)
        case None      =>
          val argInt = argOpt.get
          val (currInt, state2) = ensureIntLocal(target.paramName, state1)
          val nonNegGoal = SmtExpr.Gte(argInt, SmtExpr.IntConst(BigInt(0)))
          val lessGoal = SmtExpr.Lt(argInt, currInt)
          val nonNegProof = prove(nonNegGoal, state2)
          nonNegProof match {
            case ProofOutcome.Proved =>
              val lessProof = prove(lessGoal, state2)
              lessProof match {
                case ProofOutcome.Proved =>
                  (LexStep(Smaller, None, None), state2)
                case failedLess: ProofOutcome.Failed =>
                  val sameName = localNameOf(arg).contains(target.paramName)
                  if (sameName) {
                    val deferredErr =
                      intObligationError(
                        fnname,
                        target,
                        lessGoal,
                        state2,
                        failedLess,
                        region
                      )
                    (LexStep(Equal, None, Some(deferredErr)), state2)
                  } else {
                    val equalGoal = SmtExpr.EqInt(argInt, currInt)
                    prove(equalGoal, state2) match {
                      case ProofOutcome.Proved =>
                        val deferredErr =
                          intObligationError(
                            fnname,
                            target,
                            lessGoal,
                            state2,
                            failedLess,
                            region
                          )
                        (LexStep(Equal, None, Some(deferredErr)), state2)
                      case _ =>
                        val immediateErr =
                          intObligationError(
                            fnname,
                            target,
                            lessGoal,
                            state2,
                            failedLess,
                            region
                          )
                        (LexStep(Other, Some(immediateErr), None), state2)
                    }
                  }
              }
            case failedNonNeg: ProofOutcome.Failed =>
              val err =
                intObligationError(
                  fnname,
                  target,
                  nonNegGoal,
                  state2,
                  failedNonNeg,
                  region
                )
              (LexStep(Other, Some(err), None), state2)
          }
      }
    }

    private def recurAllowedByLexOrder(
        fnname: Bindable,
        inrec: InDefRecurred,
        target: RecurTarget,
        allowedPerTarget: NonEmptyList[Set[Bindable]],
        callArgsByTarget: NonEmptyList[TypedExpr[Declaration]],
        region: Region
    ): St[Unit] =
      getSt.flatMap {
        case InRecurBranch(
              _,
              branch,
              allowedNow,
              equalAliasesNow,
              singletonNow,
              currentCtorNow,
              namesNow,
              smtState0
            ) =>
          val defaultLexErr: RecursionCheck.Error = {
            val targetParams = target.map(_.paramName)
            RecursionCheck.RecursionNotLexicographic(fnname, targetParams, region)
          }
          val equalAliasesByTarget = equalAliasesNow.toList.toVector
          val singletonByTarget = singletonNow.toList.toVector
          val currentCtorByTarget = currentCtorNow.toList.toVector
          val targetItems = target.toList.toVector
          val allowedItems = allowedPerTarget.toList.toVector
          val callItems = callArgsByTarget.toList.toVector
          var smtState = smtState0
          var deferredError: Option[RecursionCheck.Error] = None
          var immediateError: Option[RecursionCheck.Error] = None
          var accepted = false
          var idx = 0

          while (
            (idx < targetItems.length) &&
            immediateError.isEmpty &&
            !accepted
          ) {
            val targetItem = targetItems(idx)
            val allowed = allowedItems(idx)
            val equalAliases = equalAliasesByTarget(idx)
            val singletonCtor = singletonByTarget(idx)
            val currentCtor = currentCtorByTarget(idx)
            val argExpr = callItems(idx)
            val step =
              if (isIntType(targetItemType(inrec, targetItem))) {
                val (intStep, smt1) =
                  classifyIntArg(fnname, targetItem, argExpr, smtState, region)
                smtState = smt1
                intStep
              } else
                classifyStructuralArg(
                  inrec,
                  targetItem,
                  allowed,
                  equalAliases,
                  singletonCtor,
                  currentCtor,
                  argExpr
                )

            step.order match {
              case Smaller =>
                accepted = true
              case Equal   =>
                if (deferredError.isEmpty) deferredError = step.noDecreaseError
              case Other   =>
                immediateError = Some(step.immediateError.getOrElse(defaultLexErr))
            }
            idx += 1
          }

          val finalError = immediateError.orElse(deferredError).getOrElse(defaultLexErr)

          setSt(
            InRecurBranch(
              inrec,
              branch,
              allowedNow,
              equalAliasesNow,
              singletonNow,
              currentCtorNow,
              namesNow,
              smtState
            )
          ) *> (if (accepted) unitSt else failSt(finalError))
        case notRecur =>
          // $COVERAGE-OFF$ this should be unreachable
          sys.error(s"recurAllowedByLexOrder called outside recur branch: $notRecur")
          // $COVERAGE-ON$
      }

    /*
     * We disallow shadowing recursive defs. This code checks that we have not done so as we
     * introduce new bindings.
     *
     * We disallow such shadowing currently only in this code. We do it to make it easier
     * for the algorithm here, but also for human readers to see that recursion is total.
     */
    private def checkForIllegalBinds[A](
        state: State,
        bs: Iterable[Bindable],
        region: Region
    )(
        next: Res[A]
    ): Res[A] = {
      val outerSet = state.outerDefNames
      if (outerSet.isEmpty) next
      else {
        NonEmptyList.fromList(
          bs.iterator.filter(outerSet).toList.sorted
        ) match {
          case Some(nel) =>
            Validated.invalid(
              NonEmptyChain.fromNonEmptyList(
                nel.map(RecursionCheck.IllegalShadow(_, region))
              )
            )
          case None =>
            next
        }
      }
    }

    /*
     * Unfortunately we lose the Applicative structure inside Declaration checking.
     * This is because the state changes are not nested: if we see a recur on one
     * variable, we cannot later recur on a different one. During checkDecl, we switch
     * to a sequential (Monadic) State tracking, and can only accumulate errors
     * until we hit the first one.
     */
    type ErrorOr[+A] = Either[NonEmptyChain[RecursionCheck.Error], A]

    sealed trait St[+A] { self =>
      def run(state: State): Eval[ErrorOr[(State, A)]]

      final def map[B](fn: A => B): St[B] =
        new St[B] {
          def run(state: State): Eval[ErrorOr[(State, B)]] =
            self.run(state).map {
              case Right((st1, a)) => Right((st1, fn(a)))
              case Left(errs)      => Left(errs)
            }
        }

      final def flatMap[B](fn: A => St[B]): St[B] =
        new St[B] {
          def run(state: State): Eval[ErrorOr[(State, B)]] =
            self.run(state).flatMap {
              case Right((st1, a)) =>
                Eval.defer(fn(a).run(st1))
              case Left(errs)      =>
                Eval.now(Left(errs))
            }
        }

      final def runA(state: State): ErrorOr[A] =
        run(state).value.map(_._2)
    }

    object St {
      def pure[A](a: A): St[A] =
        new St[A] {
          def run(state: State): Eval[ErrorOr[(State, A)]] =
            Eval.now(Right((state, a)))
        }

      def liftEither[A](res: ErrorOr[A]): St[A] =
        new St[A] {
          def run(state: State): Eval[ErrorOr[(State, A)]] =
            Eval.now(res.map((state, _)))
        }

      val get: St[State] =
        new St[State] {
          def run(state: State): Eval[ErrorOr[(State, State)]] =
            Eval.now(Right((state, state)))
        }

      def set(state: State): St[Unit] =
        new St[Unit] {
          def run(state0: State): Eval[ErrorOr[(State, Unit)]] = {
            val _ = state0
            Eval.now(Right((state, ())))
          }
        }

      def defer[A](st: => St[A]): St[A] =
        new St[A] {
          def run(state: State): Eval[ErrorOr[(State, A)]] =
            Eval.defer(st.run(state))
        }

      implicit val monadForSt: StackSafeMonad[St] =
        new StackSafeMonad[St] {
          def pure[A](a: A): St[A] = St.pure(a)

          override def map[A, B](fa: St[A])(fn: A => B): St[B] =
            fa.map(fn)

          def flatMap[A, B](fa: St[A])(fn: A => St[B]): St[B] =
            fa.flatMap(fn)

          override def tailRecM[A, B](init: A)(fn: A => St[Either[A, B]]): St[B] =
            new St[B] {
              def run(state: State): Eval[ErrorOr[(State, B)]] = {
                def loop(st: State, a: A): Eval[ErrorOr[(State, B)]] =
                  fn(a).run(st).flatMap {
                    case Left(errs)                 =>
                      Eval.now(Left(errs))
                    case Right((st1, Left(nextA)))  =>
                      Eval.defer(loop(st1, nextA))
                    case Right((st1, Right(doneB))) =>
                      Eval.now(Right((st1, doneB)))
                  }

                Eval.defer(loop(state, init))
              }
            }
        }
    }

    implicit val parallelSt: cats.Parallel[St] = {
      val m = cats.Monad[St]

      new ParallelViaProduct[St] {
        def monad = m
        def parallelProduct[A, B](fa: St[A], fb: St[B]) = {
          new St[(A, B)] {
            def run(state: State): Eval[ErrorOr[(State, (A, B))]] =
              fa.run(state).flatMap {
                case Right((s2, a)) =>
                  fb.run(s2).map {
                    case Right((st, b)) => Right((st, (a, b)))
                    case Left(errs)     => Left(errs)
                  }
                case Left(errs1) =>
                  // skip state changes in fb and merge errors if both fail
                  fb.run(state).map {
                    case Right(_)      => Left(errs1)
                    case Left(errs2)   => Left(errs1 ++ errs2)
                  }
              }
          }
        }
      }
    }

    // Scala has trouble inferring types like St, so we make these typed
    // helper functions to use below.
    private def failSt[A](err: RecursionCheck.Error): St[A] =
      St.liftEither(Left(NonEmptyChain.one(err)))
    private val getSt: St[State] = St.get
    private def setSt(s: State): St[Unit] = St.set(s)
    private def toSt[A](v: Res[A]): St[A] =
      St.liftEither(v.toEither)
    private def pureSt[A](a: A): St[A] = St.pure(a)
    private def deferSt[A](st: => St[A]): St[A] = St.defer(st)
    private val unitSt: St[Unit] = pureSt(())

    private def checkForIllegalBindsSt(
        bs: Iterable[Bindable],
        region: Region
    ): St[Unit] =
      for {
        state <- getSt
        _ <- toSt(checkForIllegalBinds(state, bs, region)(unitValid))
        _ <- (state match {
          case id @ InDef(_, _, _, _, _, _, _, _, _) =>
            setSt(bs.foldLeft(id)(_.addLocal(_)))
          case _                                =>
            unitSt
        })
      } yield ()

    private def argsOnDefName(
        currentPackage: PackageName,
        fn: TypedExpr[Declaration],
        groups: NonEmptyList[NonEmptyList[TypedExpr[Declaration]]]
    ): Option[(Bindable, NonEmptyList[NonEmptyList[TypedExpr[Declaration]]])] =
      fn match {
        case TypedExpr.Local(nm, _, _)    => Some((nm, groups))
        case TypedExpr.Global(pack, nm: Bindable, _, _)
            if pack == currentPackage =>
          Some((nm, groups))
        case TypedExpr.Generic(_, in)     => argsOnDefName(currentPackage, in, groups)
        case TypedExpr.Annotation(in, _, _) =>
          argsOnDefName(currentPackage, in, groups)
        case TypedExpr.App(fn1, args, _, _) =>
          argsOnDefName(currentPackage, fn1, args :: groups)
        case _ => None
      }

    private def withTemporaryRecurBranchNames[A](
        in: St[A],
        onMissing: State => St[A]
    )(
        update: (NonEmptyList[Set[Bindable]], Set[Bindable]) => (
            NonEmptyList[Set[Bindable]],
            Set[Bindable]
      )
    ): St[A] =
      getSt.flatMap {
        case start @ InRecurBranch(
              inrec,
              branch,
              allowed,
              equalAliases,
              singletonCtor,
              currentCtor,
              names,
              smtState
            ) =>
          val (allowed1, names1) = update(allowed, names)
          (
            setSt(
              InRecurBranch(
                inrec,
                branch,
                allowed1,
                equalAliases,
                singletonCtor,
                currentCtor,
                names1,
                smtState
              )
            ) *> in,
            getSt
          )
            .flatMapN {
              case (a, InRecurBranch(ir1, b1, _, eq1, sc1, cc1, _, smt1)) =>
                setSt(
                  InRecurBranch(ir1, b1, allowed, eq1, sc1, cc1, names, smt1)
                ).as(a)
              // $COVERAGE-OFF$ this should be unreachable
              case (_, unexpected) =>
                sys.error(
                  s"invariant violation expected InRecurBranch: start = $start, end = $unexpected"
                )
              // $COVERAGE-ON$
            }
        case notRecur =>
          onMissing(notRecur)
      }

    private def withTemporaryRecurBranchSmtState[A](
        in: St[A],
        onMissing: State => St[A]
    )(
        update: SmtBranchState => SmtBranchState
    ): St[A] =
      getSt.flatMap {
        case start @ InRecurBranch(
              inrec,
              branch,
              allowed,
              equalAliases,
              singletonCtor,
              currentCtor,
              names,
              smtState
            ) =>
          val smtState1 = update(smtState)
          (
            setSt(
              InRecurBranch(
                inrec,
                branch,
                allowed,
                equalAliases,
                singletonCtor,
                currentCtor,
                names,
                smtState1
              )
            ) *> in,
            getSt
          ).flatMapN {
            case (a, InRecurBranch(ir1, b1, allowed1, eq1, sc1, cc1, names1, _)) =>
              setSt(
                InRecurBranch(ir1, b1, allowed1, eq1, sc1, cc1, names1, smtState)
              ).as(a)
            // $COVERAGE-OFF$ this should be unreachable
            case (_, unexpected) =>
              sys.error(
                s"invariant violation expected InRecurBranch: start = $start, end = $unexpected"
              )
            // $COVERAGE-ON$
          }
        case notRecur =>
          onMissing(notRecur)
      }

    private def unionNames[A](newNames: Iterable[Bindable])(in: St[A]): St[A] =
      withTemporaryRecurBranchNames(
        in,
        notRecur => sys.error(s"called setNames on $notRecur with names: $newNames")
      ) { (allowed, names) =>
        // Single-target recursion keeps the old behavior where lambda args
        // from reachable substructures are also considered recursive args.
        val allowed1 = allowed.tail match {
          case Nil    => NonEmptyList.one(allowed.head ++ newNames)
          case _ :: _ => allowed
        }
        (allowed1, names ++ newNames)
      }

    private def filterNames[A](newNames: Iterable[Bindable])(in: St[A]): St[A] =
      withTemporaryRecurBranchNames(in, _ => in) { (allowed, names) =>
        (allowed.map(_ -- newNames), names -- newNames)
      }

    private def checkReachableLambdaBody(
        currentPackage: PackageName,
        body: TypedExpr[Declaration],
        wrappers: WrapperScope,
        lambdaTag: Declaration
    ): St[Unit] =
      deferSt {
        body match {
        case TypedExpr.Match(arg, branches, tag)
            if (tag == lambdaTag) &&
              (branches.length == 1) &&
              branches.head.guard.isEmpty =>
          val branch = branches.head
          val newBinds = branch.pattern.names.toList
          checkExpr(currentPackage, arg, wrappers) *>
            checkForIllegalBindsSt(newBinds, tag.region) *>
            unionNames(newBinds)(
              checkReachableLambdaBody(
                currentPackage,
                branch.expr,
                wrappers,
                lambdaTag
              )
            )
        case _ =>
          checkExpr(currentPackage, body, wrappers)
      }
      }

    private def checkApply(
        currentPackage: PackageName,
        fn: TypedExpr[Declaration],
        args: NonEmptyList[TypedExpr[Declaration]],
        region: Region,
        wrappers: WrapperScope
    ): St[Unit] =
      deferSt {
        getSt.flatMap {
        case TopLevel(_) =>
          // without any recursion, normal typechecking will detect bad states:
          checkExpr(currentPackage, fn, wrappers) *> args.parTraverse_(
            checkExpr(currentPackage, _, wrappers)
          )
        case irb @ InRecurBranch(inrec, _, allowedPerTarget, _, _, _, names, _) =>
          argsOnDefName(currentPackage, fn, NonEmptyList.one(args)) match {
            case Some((nm, groups)) =>
              if (nm == irb.defname) {
                val targetArgsV: Res[NonEmptyList[TypedExpr[Declaration]]] =
                  inrec.target.traverse { targetItem =>
                    groups
                      .get(targetItem.group.toLong)
                      .flatMap(_.get(targetItem.index.toLong))
                      .toValidNec(RecursionCheck.NotEnoughRecurArgs(nm, region))
                  }

                val allArgs = groups.iterator.flatMap(_.iterator).toList
                toSt(targetArgsV).flatMap { targetArgs =>
                  recurAllowedByLexOrder(
                    irb.defname,
                    inrec,
                    inrec.target,
                    allowedPerTarget,
                    targetArgs,
                    region
                  )
                } *>
                  getSt.flatMap {
                    case irbNow: InRecurBranch => setSt(irbNow.incRecCount)
                    case _                     => setSt(irb.incRecCount)
                  } *> allArgs.parTraverse_(
                    checkExpr(currentPackage, _, wrappers)
                  )
              } else if (irb.defNamesContain(nm)) {
                failSt(RecursionCheck.InvalidRecursion(nm, region))
              } else if (names.contains(nm)) {
                // we are calling a reachable function. Any lambda args are new names:
                args.parTraverse_[St, Unit] {
                  case argExpr =>
                    asAnnotatedLambda(argExpr) match {
                      case Some(TypedExpr.AnnotatedLambda(lambdaArgs, body, lambdaTag)) =>
                        val names1 = lambdaArgs.toList.map(_._1)
                        unionNames(names1)(
                          checkReachableLambdaBody(
                            currentPackage,
                            body,
                            wrappers,
                            lambdaTag
                          )
                        )
                      case None =>
                        localOrLocalPackageGlobalNameOf(
                          currentPackage,
                          argExpr
                        ) match {
                          case Some(fnname) if irb.defname == fnname =>
                            val asLambda = irb.inDef.asLambda(argExpr.tag.region)
                            val names1 = asLambda.args.toList.map(_._1)
                            unionNames(names1)(
                              checkExpr(currentPackage, asLambda.expr, wrappers)
                            )
                          case _ =>
                            checkExpr(currentPackage, argExpr, wrappers)
                        }
                    }
                }
              } else {
                // not a recursive call
                setSt(irb.noteCalledName(nm)) *> args.parTraverse_(
                  checkExpr(currentPackage, _, wrappers)
                )
              }
            case None =>
              // this isn't a recursive call
              checkExpr(currentPackage, fn, wrappers) *> args.parTraverse_(
                checkExpr(currentPackage, _, wrappers)
              )
          }
        case ir: InDefState =>
          // we have either not yet, or already done the recursion
          argsOnDefName(currentPackage, fn, NonEmptyList.one(args)) match {
            case Some((nm, _)) if ir.defNamesContain(nm) =>
              failSt(RecursionCheck.InvalidRecursion(nm, region))
            case _ =>
              checkExpr(currentPackage, fn, wrappers) *> args.parTraverse_(
                checkExpr(currentPackage, _, wrappers)
              )
          }
      }
      }

    private def checkExpr(
        currentPackage: PackageName,
        expr: TypedExpr[Declaration],
        wrappers: WrapperScope
    ): St[Unit] =
      deferSt {
        expr match {
        case TypedExpr.Generic(q, in) =>
          checkExpr(currentPackage, in, wrappers.pushQuant(q))
        case TypedExpr.Annotation(term, _, _) =>
          checkExpr(currentPackage, term, wrappers.pushAnnotation)
        case TypedExpr.AnnotatedLambda(args, body, _) =>
          val newBinds = args.toList.map(_._1)
          val bodyCheck = filterNames(newBinds)(checkExpr(currentPackage, body, wrappers))
          checkForIllegalBindsSt(newBinds, expr.tag.region) *>
            withTemporaryRecurBranchSmtState(
              bodyCheck,
              _ => bodyCheck
            ) { smtState =>
              args.foldLeft(smtState.removeBindings(newBinds)) {
                case (st, (name, tpe)) =>
                  bindSymbolForType(name, tpe, st)
              }
            }
        case TypedExpr.Local(v, _, _) =>
          getSt.flatMap {
            case TopLevel(_) =>
              // without any recursion, normal typechecking will detect bad states:
              unitSt
            case ir: InDefState =>
              // if this were an apply, it would have been handled by App(Local(...
              if (ir.defNamesContain(v))
                failSt(RecursionCheck.InvalidRecursion(v, expr.tag.region))
              else unitSt
          }
        case TypedExpr.Global(_, _, _, _) =>
          unitSt
        case TypedExpr.App(fn, args, _, _) =>
          checkApply(currentPackage, fn, args, expr.tag.region, wrappers)
        case TypedExpr.Let(arg, ex, in, rec, tag) =>
          if (isDefLike(rec, tag)) {
            val fromSource =
              tag match {
                case Declaration.DefFn(defstmt) =>
                  Some(normalizedDefArgs(defstmt.args))
                case _                          => None
              }
            getSt.flatMap { state =>
              val defn = toSt(checkDef(currentPackage, state, arg, ex, fromSource))
              val nextRes = checkExpr(currentPackage, in, wrappers)
              defn *> nextRes
            }
          } else {
            val inCheck = filterNames(arg :: Nil)(checkExpr(currentPackage, in, wrappers))
            checkForIllegalBindsSt(arg :: Nil, tag.region) *>
              checkExpr(currentPackage, ex, wrappers) *>
              withTemporaryRecurBranchSmtState(
                inCheck,
                _ => inCheck
              ) { smtState =>
                bindLetName(arg, ex, smtState)
              }
          }
        case TypedExpr.Loop(loopArgs, body, _) =>
          val newBinds = loopArgs.toList.map(_._1)
          val checkArgs = loopArgs.parTraverse_ { case (_, argExpr) =>
            checkExpr(currentPackage, argExpr, wrappers)
          }
          val bodyCheck = filterNames(newBinds)(checkExpr(currentPackage, body, wrappers))
          checkForIllegalBindsSt(newBinds, expr.tag.region) *>
            checkArgs *>
            withTemporaryRecurBranchSmtState(
              bodyCheck,
              _ => bodyCheck
            ) { smtState =>
              loopArgs.toList.foldLeft(smtState.removeBindings(newBinds)) {
                case (st, (name, valueExpr)) =>
                  bindLetName(name, valueExpr, st)
              }
            }
        case TypedExpr.Recur(args, _, _) =>
          args.parTraverse_(checkExpr(currentPackage, _, wrappers))
        case TypedExpr.Literal(_, _, _) =>
          unitSt
        case TypedExpr.Match(arg, branches, tag) =>
          recurTag(currentPackage, arg, tag) match {
            case None =>
              // the arg can't use state, but cases introduce new bindings:
              val argRes = checkExpr(currentPackage, arg, wrappers)
              val optRes = getSt.flatMap { state =>
                val (fallthroughFacts, fallthroughSymbols) =
                  state match {
                    case InRecurBranch(_, _, _, _, _, _, _, smtState) =>
                      val analyzed =
                        matchFallthroughFacts(arg, branches, smtState)
                      (analyzed.facts, Some(analyzed.symbolState))
                    case _                                  =>
                      (
                        branches.map(_ -> (SmtExpr.BoolConst.True: SmtExpr.BoolExpr)),
                        None
                      )
                  }

                val branchFacts = fallthroughFacts.toList
                branchFacts.zipWithIndex.parTraverse_ {
                  case ((branch, fallthroughFact), idx) =>
                    val priorBranches = branchFacts.take(idx).map(_._1)
                    val branchExprCheck =
                      branch.guard match {
                        case Some(guardExpr) =>
                          checkExpr(currentPackage, guardExpr, wrappers) *>
                            {
                              val bodyCheck =
                                checkExpr(currentPackage, branch.expr, wrappers)
                              withTemporaryRecurBranchSmtState(
                                bodyCheck,
                                _ => bodyCheck
                              ) { smtState =>
                                addGuardPathFact(guardExpr, smtState)
                              }
                            }
                        case None            =>
                          checkExpr(currentPackage, branch.expr, wrappers)
                      }
                    val withFallthroughContext =
                      withTemporaryRecurBranchSmtState(
                        branchExprCheck,
                        _ => branchExprCheck
                      ) { smtState =>
                        addPathFactIfNonTrivial(fallthroughFact, smtState)
                      }
                    val withSubsumedGuardContext =
                      withTemporaryRecurBranchSmtState(
                        withFallthroughContext,
                        _ => withFallthroughContext
                      ) { smtState =>
                        addSubsumedGuardFallthroughFacts(
                          priorBranches,
                          branch.pattern,
                          smtState
                        )
                      }
                    val withPatternContext =
                      withTemporaryRecurBranchSmtState(
                        withSubsumedGuardContext,
                        _ => withSubsumedGuardContext
                      ) { smtState =>
                        val smtState0 =
                          fallthroughSymbols.fold(smtState)(mergeSymbolState(_, smtState))
                        addPatternFactsAndBindings(arg, branch.pattern, smtState0)
                      }
                    checkForIllegalBindsSt(branch.pattern.names, tag.region) *>
                      filterNames(branch.pattern.names)(withPatternContext)
                }
              }
              argRes *> optRes
            case Some(recur) =>
              // this is a state change
              getSt.flatMap {
                case TopLevel(_) | InRecurBranch(_, _, _, _, _, _, _, _) |
                    InDefRecurred(_, _, _, _, _) =>
                  failSt(RecursionCheck.UnexpectedRecur(recur.region))
                case ir @ InDef(_, defname, typedArgs, sourceArgs, _, _, _, _, locals) =>
                  toSt(getRecurTarget(defname, sourceArgs, typedArgs, recur, locals)).flatMap {
                    target =>
                      val sourcePatterns = recur.cases.get.map(_.pattern)
                      val inrec = ir.setRecur(target, recur)
                      // Same order and length as `branches`.
                      val fallthroughAnalyzed =
                        matchFallthroughFacts(arg, branches, initBranchSmtState(inrec))
                      val fallthroughFacts = fallthroughAnalyzed.facts
                      val fallthroughSymbols = fallthroughAnalyzed.symbolState

                      // on all these branches, use the same parent state
                      def beginBranch(
                          matchArg: TypedExpr[Declaration],
                          sourcePat: Pattern.Parsed,
                          compiledPat: Pattern[(PackageName, Identifier.Constructor), Type],
                          fallthroughFact: SmtExpr.BoolExpr,
                          priorBranches: List[TypedExpr.Branch[Declaration]]
                      ): St[Unit] =
                        getSt.flatMap {
                          case ir @ InDef(_, _, _, _, _, _, _, _, _) =>
                            val rec = ir.setRecur(target, recur)
                            setSt(rec) *> beginBranch(
                              matchArg,
                              sourcePat,
                              compiledPat,
                              fallthroughFact,
                              priorBranches
                            )
                          case irr @ InDefRecurred(_, _, _, _, _) =>
                            val allowed =
                              allowedByTargetFromParsed(irr.target, sourcePat)
                            val equalAliases =
                              equalAliasesByTargetFromParsed(irr.target, sourcePat)
                            val singletonCtor =
                              singletonCtorByTargetFromTyped(irr.target, compiledPat)
                            val currentCtor =
                              knownCtorByTargetFromTyped(irr.target, compiledPat)
                            val reachable = allowed.iterator.flatMap(_.iterator).toSet
                            val smtState0 =
                              addPatternFactsAndBindings(
                                matchArg,
                                compiledPat,
                                mergeSymbolState(
                                  fallthroughSymbols,
                                  initBranchSmtState(irr)
                                )
                              )
                            val smtState1 =
                              addSubsumedGuardFallthroughFacts(
                                priorBranches,
                                compiledPat,
                                smtState0
                              )
                            val smtState2 =
                              addPathFactIfNonTrivial(fallthroughFact, smtState1)
                            val smtState =
                              bindSourcePatternAliases(irr, sourcePat, smtState2)
                            setSt(
                              InRecurBranch(
                                irr,
                                compiledPat,
                                allowed,
                                equalAliases,
                                singletonCtor,
                                currentCtor,
                                reachable,
                                smtState
                              )
                            )
                          case illegal =>
                            // $COVERAGE-OFF$ this should be unreachable
                            sys.error(s"unreachable: $compiledPat -> $illegal")
                          // $COVERAGE-ON$
                        }

                      val endBranch: St[Unit] =
                        getSt.flatMap {
                          case InRecurBranch(irr, _, _, _, _, _, _, _) =>
                            setSt(irr)
                          case illegal                  =>
                            // $COVERAGE-OFF$ this should be unreachable
                            sys.error(s"unreachable end state: $illegal")
                          // $COVERAGE-ON$
                        }

                      val recurBranchData = fallthroughFacts.zip(sourcePatterns).toList
                      recurBranchData.zipWithIndex.parTraverse_ {
                        case (((branch, fallthroughFact), sourcePat), idx) =>
                          val priorBranches =
                            recurBranchData.take(idx).map(_._1._1)
                          for {
                            _ <- checkForIllegalBindsSt(branch.pattern.names, tag.region)
                            _ <- beginBranch(
                              arg,
                              sourcePat,
                              branch.pattern,
                              fallthroughFact,
                              priorBranches
                            )
                            _ <- branch.guard.parTraverse_(
                              checkExpr(currentPackage, _, wrappers)
                            )
                            _ <- (branch.guard match {
                              case Some(guardExpr) =>
                                getSt.flatMap {
                                  case InRecurBranch(
                                        inrec,
                                        bpat,
                                        allowed,
                                        equalAliases,
                                        singletonCtor,
                                        currentCtor,
                                        reachable,
                                        smtState
                                      ) =>
                                    val smtState1 =
                                      addGuardPathFact(guardExpr, smtState)
                                    setSt(
                                      InRecurBranch(
                                        inrec,
                                        bpat,
                                        allowed,
                                        equalAliases,
                                        singletonCtor,
                                        currentCtor,
                                        reachable,
                                        smtState1
                                      )
                                    ) *> checkExpr(currentPackage, branch.expr, wrappers)
                                  case _ =>
                                    checkExpr(currentPackage, branch.expr, wrappers)
                                }
                              case None =>
                                checkExpr(currentPackage, branch.expr, wrappers)
                            })
                            _ <- endBranch
                          } yield ()
                      }
                  }
              }
          }
        }
      }

    private def checkExprV(
        currentPackage: PackageName,
        state: State,
        expr: TypedExpr[Declaration]
    ): Res[Unit] =
      // Expression traversal uses StateT[Either, ...] because recur-state updates
      // are sequential; convert back to ValidatedNec at this API boundary.
      Validated.fromEither(
        checkExpr(currentPackage, expr, WrapperScope.Empty).as(()).runA(state)
      )

    private def collectArgGroupsAndBody(
        expr: TypedExpr[Declaration]
    ): Option[(NonEmptyList[NonEmptyList[(Bindable, Type)]], TypedExpr[
      Declaration
    ])] = {
      @annotation.tailrec
      def loop(
          curr: TypedExpr[Declaration],
          groupsRev: List[NonEmptyList[(Bindable, Type)]]
      ): Option[(NonEmptyList[NonEmptyList[(Bindable, Type)]], TypedExpr[
        Declaration
      ])] =
        curr match {
          case TypedExpr.Generic(_, in)     => loop(in, groupsRev)
          case TypedExpr.Annotation(in, _, _) => loop(in, groupsRev)
          case TypedExpr.AnnotatedLambda(args, body, _) =>
            loop(body, args :: groupsRev)
          case body =>
            NonEmptyList
              .fromList(groupsRev.reverse)
              .map((_, body))
        }

      loop(expr, Nil)
    }

    private def directPredefAliasTarget(
        expr: TypedExpr[Declaration]
    ): Option[Identifier.Name] =
      predefFnName(expr)
        .collect { case n: Identifier.Name => n }
        .filter(lowerablePredefFnNames)
        .orElse {
          collectArgGroupsAndBody(expr).flatMap { case (argGroups, body) =>
            val argNames = argGroups.iterator.flatMap(_.iterator.map(_._1)).toList
            stripExprWrappers(body) match {
              case TypedExpr.App(fn, args, _, _) if args.length == argNames.length =>
                val forwardsArgs =
                  argNames.iterator.zip(args.iterator).forall {
                    case (argName, argExpr) =>
                      localNameOf(argExpr).contains(argName)
                  }
                if (forwardsArgs)
                  predefFnName(fn)
                    .collect { case n: Identifier.Name => n }
                    .filter(lowerablePredefFnNames)
                else None
              case _ =>
                None
            }
          }
        }

    private def directLowerableAlias(
        expr: TypedExpr[Declaration]
    ): Option[TopLevelAlias] =
      collectArgGroupsAndBody(expr).flatMap { case (argGroups, body) =>
        argGroups.toList match {
          case params :: Nil =>
            Some(TopLevelAlias(params.map(_._1), body))
          case _             =>
            None
        }
      }

    def topLevelLowerableAliases(
        currentPackage: PackageName,
        lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])]
    ): TopLevelLowerableAliases =
      lets.iterator.flatMap { case (name, rec, expr) =>
        if (rec.isRecursive) None
        else
          directLowerableAlias(expr).map { alias =>
            ((currentPackage, name), alias)
          }
      }.toMap

    def topLevelPredefAliases(
        currentPackage: PackageName,
        lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])]
    ): TopLevelPredefAliases =
      lets.iterator.flatMap { case (name, _, expr) =>
        directPredefAliasTarget(expr).map { predefName =>
          ((currentPackage, name), predefName)
        }
      }.toMap

    /*
     * Binds are not allowed to be recursive, only defs, so here we just make sure
     * none of the free variables of the pattern are used in expr.
     */
    private def checkDef(
        currentPackage: PackageName,
        state: State,
        fnname: Bindable,
        expr: TypedExpr[Declaration],
        sourceArgPatterns: Option[NonEmptyList[NonEmptyList[Pattern.Parsed]]],
        topLevelLowerableAliases: TopLevelLowerableAliases = Map.empty,
        topLevelPredefAliases: TopLevelPredefAliases = Map.empty
    ): Res[Unit] =
      collectArgGroupsAndBody(expr) match {
        case None =>
          checkExprV(currentPackage, state, expr)
        case Some((typedArgs, body)) =>
          val inheritedLowerableAliases =
            state match {
              case inDefState: InDefState =>
                inDefState.inDef.topLevelLowerableAliases
              case TopLevel(_)           =>
                topLevelLowerableAliases
            }
          val inheritedPredefAliases =
            state match {
              case inDefState: InDefState =>
                inDefState.inDef.topLevelPredefAliases
              case TopLevel(_)           =>
                topLevelPredefAliases
            }
          val inheritedTotalityCheck =
            state match {
              case inDefState: InDefState =>
                inDefState.inDef.totalityCheck
              case top: TopLevel         =>
                top.totalityCheck
            }
          val sourceArgs = sourceArgsForDef(sourceArgPatterns, typedArgs)
          val nameArgs = sourceArgs.toList.flatMap(_.patternNames)
          val state1 = state.inDef(
            fnname,
            typedArgs,
            sourceArgs,
            inheritedLowerableAliases,
            expr.getType,
            inheritedPredefAliases,
            inheritedTotalityCheck
          )
          checkForIllegalBinds(state, fnname :: nameArgs, body.tag.region) {
            val st = setSt(state1) *> checkExpr(
              currentPackage,
              body,
              WrapperScope.Empty
            ) *> (
              getSt.flatMap {
                case InDef(_, _, _, _, _, _, _, _, _) =>
                  // we never hit a recur
                  unitSt
                case InDefRecurred(_, _, recur, cnt, _) if cnt > 0 =>
                  // we did hit a recur
                  checkLoopTailRecursion(fnname, body, recur)
                case InDefRecurred(_, _, recur, 0, calledNames) =>
                  // we hit a recur, but we didn't recurse
                  failSt[Unit](
                    RecursionCheck.RecursiveDefNoRecur(
                      fnname,
                      recur.region,
                      recur.kind,
                      likelyRenameCall(fnname, calledNames)
                    )
                  )
                case unreachable =>
                  // $COVERAGE-OFF$ this should be unreachable
                  sys.error(
                    s"we would like to prove in the types we can't get here: $unreachable, $fnname"
                  ): St[Unit]
                // $COVERAGE-ON$
              }
            )
            // Note a def can't change the state:
            // we either have a valid nested def, or we don't,
            // but that can't change the state of the outer def
            // that is calling this. We use Either in St for sequential state,
            // then convert to ValidatedNec at this boundary.
            Validated.fromEither(st.runA(state))
          }
      }

    def checkTopLevelLet(
        currentPackage: PackageName,
        name: Bindable,
        rec: RecursionKind,
        expr: TypedExpr[Declaration],
        sourceArgs: Option[NonEmptyList[NonEmptyList[Pattern.Parsed]]],
        topLevelLowerableAliases: TopLevelLowerableAliases,
        topLevelPredefAliases: TopLevelPredefAliases,
        totalityCheck: TotalityCheck
    ): Res[Unit] = {
      val shouldCheckAsDef = sourceArgs.nonEmpty || rec.isRecursive
      if (shouldCheckAsDef)
        checkDef(
          currentPackage,
          TopLevel(totalityCheck),
          name,
          expr,
          sourceArgs,
          topLevelLowerableAliases,
          topLevelPredefAliases
        )
      else checkExprV(currentPackage, TopLevel(totalityCheck), expr)
    }
  }
}
