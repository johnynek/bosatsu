package dev.bosatsu

import cats.{Eval, StackSafeMonad}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.implicits._
import dev.bosatsu.pattern.NameMap
import dev.bosatsu.rankn.{ConstructorFn, DefinedType, Type, TypeEnv}
import dev.bosatsu.scalawasiz3.{Z3Platform, Z3Result}
import dev.bosatsu.smt.{SExpr, SmtCommand, SmtExpr, SmtLibRender, SmtScript, SmtScriptScope, SmtSort, Z3Api}
import scala.collection.mutable

import Identifier.Bindable

object TypedExprRecursionCheck {

  type Res[+A] = ValidatedNec[RecursionCheck.Issue, A]

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
  ): Res[Unit] =
    checkLets(pack, fullTypeEnv, lets, Map.empty)

  def checkLets(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
      topLevelDefs: Map[Bindable, NonEmptyList[NonEmptyList[Pattern.Parsed]]]
  ): Res[Unit] = {
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

  def replayLints(
      pack: PackageName,
      lets: List[(Bindable, RecursionKind, TypedExpr[Region])]
  ): List[RecursionCheck.Lint] =
    Impl.replayLints(pack, lets)

  /** The SMT term for an Int, Bool or Comparison expression. A Comparison is
    * an Int: -1 for LT, 0 for EQ and 1 for GT.
    */
  private[bosatsu] enum LoweredTerm derives CanEqual {
    case IntTerm(expr: SmtExpr.IntExpr)
    case BoolTerm(expr: SmtExpr.BoolExpr)
    case ComparisonTerm(expr: SmtExpr.IntExpr)
  }

  /** A def body lowered to SMT. The arguments are bound to the SMT terms in
    * `intArgs`, `boolArgs` and `comparisonArgs`. `pathFacts` are the facts
    * about the symbols: the -1, 0, 1 domain of each Comparison symbol,
    * including the arguments'.
    */
  private[bosatsu] final case class LoweredDefBody(
      intArgs: Map[Bindable, SmtExpr.IntExpr],
      boolArgs: Map[Bindable, SmtExpr.BoolExpr],
      comparisonArgs: Map[Bindable, SmtExpr.IntExpr],
      term: Option[LoweredTerm],
      pathFacts: Vector[SmtExpr.BoolExpr]
  )

  /** Test hook: lower the body of the top level def `name` in `lets` the way
    * the recursion check lowers an expression in a recur branch of that def,
    * starting from one SMT symbol per argument. `term` is None when the body
    * is not an Int, Bool or Comparison or does not lower.
    */
  private[bosatsu] def lowerDefBodyForTest(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
      name: Bindable
  ): Option[LoweredDefBody] =
    Impl.lowerDefBodyForTest(pack, fullTypeEnv, lets, name)

  /** The path facts at each Int literal leaf of a def body, keyed by the
    * literal. The arguments are bound to the SMT terms in `intArgs`,
    * `boolArgs` and `comparisonArgs`.
    */
  private[bosatsu] final case class LeafPathFacts(
      intArgs: Map[Bindable, SmtExpr.IntExpr],
      boolArgs: Map[Bindable, SmtExpr.BoolExpr],
      comparisonArgs: Map[Bindable, SmtExpr.IntExpr],
      leaves: Map[BigInt, Vector[SmtExpr.BoolExpr]]
  )

  /** Test hook: the path facts the recursion check assumes at each Int
    * literal leaf of the body of the top level def `name` in `lets`, as if
    * the body were in a recur branch of that def. The walk goes through lets
    * and match branches, using the check's own functions for a let's binding,
    * a branch's entry state and its guard facts. A leaf's facts must hold
    * whenever Bosatsu's evaluator returns that leaf.
    */
  private[bosatsu] def leafPathFactsForTest(
      pack: PackageName,
      fullTypeEnv: TypeEnv[Kind.Arg],
      lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
      name: Bindable
  ): Option[LeafPathFacts] =
    Impl.leafPathFactsForTest(pack, fullTypeEnv, lets, name)

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
    enum ProvenRel derives CanEqual {
      case Equal
      case Smaller
    }

    // What is known about the names in scope relative to one recur target.
    // The target's own parameter name starts in equalNames, and leaves it
    // like any other name when a let, pattern or lambda rebinds it.
    case class TargetProof(
        smallerNames: Set[Bindable],
        equalNames: Set[Bindable],
        singletonCtorIfEqual: Option[SingletonCtor],
        currentCtorIfEqual: Option[KnownCtor]
    ) derives CanEqual {
      def relationOf(name: Bindable): Option[ProvenRel] =
        if (smallerNames(name)) Some(ProvenRel.Smaller)
        else if (equalNames(name)) Some(ProvenRel.Equal)
        else None

      def withFact(name: Bindable, rel: ProvenRel): TargetProof =
        rel match {
          case ProvenRel.Smaller =>
            copy(
              smallerNames = smallerNames + name,
              equalNames = equalNames - name
            )
          case ProvenRel.Equal =>
            if (smallerNames(name)) this
            else copy(equalNames = equalNames + name)
        }

      def withFacts(
          names: Iterable[Bindable],
          rel: ProvenRel
      ): TargetProof =
        names.foldLeft(this)(_.withFact(_, rel))

      def without(names: Iterable[Bindable]): TargetProof =
        copy(
          smallerNames = smallerNames -- names,
          equalNames = equalNames -- names
        )
    }
    object TargetProof {
      val empty: TargetProof = TargetProof(Set.empty, Set.empty, None, None)
    }

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
    private val evalPackageName = PackageName.parts("Bosatsu", "Eval")
    private val evalName = Identifier.Name("eval")
    private val flatMapName = Identifier.Name("flat_map")
    private val evalTypeConst =
      Type.Const.Defined(evalPackageName, TypeName("Eval"))

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

      // `names` go out of scope: they get back the bindings they have in
      // `outer`. Symbols declared and facts added since stay. That is sound
      // because the only facts lowering adds are the domains of fresh
      // Comparison symbols, which hold in any scope.
      def restoreBindings(
          names: Iterable[Bindable],
          outer: SmtBranchState
      ): SmtBranchState = {
        def restore[V](
            inner: Map[Bindable, V],
            before: Map[Bindable, V]
        ): Map[Bindable, V] =
          names.foldLeft(inner) { (acc, name) =>
            before.get(name) match {
              case Some(v) => acc.updated(name, v)
              case None    => acc - name
            }
          }
        copy(
          intBindings = restore(intBindings, outer.intBindings),
          boolBindings = restore(boolBindings, outer.boolBindings),
          comparisonBindings = restore(comparisonBindings, outer.comparisonBindings)
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
      // The SMT state every recur branch starts from: one symbol for each
      // Int, Bool or Comparison argument. A target's value is its symbol
      // here, whatever a let or pattern later rebinds its name to.
      lazy val entrySmtState: SmtBranchState = initBranchSmtState(this)

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
        proofsPerTarget: NonEmptyList[TargetProof],
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

    private def proofsByTargetFromTyped(
        target: RecurTarget,
        branchPat: TypedPattern
    ): NonEmptyList[TargetProof] = {
      // the parameter name is the target, unless the branch pattern rebinds it
      val atEntry = target.map { item =>
        TargetProof.empty
          .withFact(item.paramName, ProvenRel.Equal)
          .without(branchPat.names)
      }
      targetPatternPartsFromTyped(target.length, branchPat) match {
        case Some(parts) =>
          atEntry.zipWith(NonEmptyList.fromListUnsafe(parts)) { (proof, part) =>
            proof
              .withFacts(wholeValueAliases(part), ProvenRel.Equal)
              .withFacts(part.substructures, ProvenRel.Smaller)
              .copy(
                singletonCtorIfEqual = singletonCtorFromTypedPart(part),
                currentCtorIfEqual = knownCtorFromTypedPart(part)
              )
          }
        case None        =>
          atEntry
      }
    }

    private def smallerNamesFromProofs(
        proofs: NonEmptyList[TargetProof]
    ): Set[Bindable] =
      proofs.iterator.flatMap(_.smallerNames.iterator).toSet

    // Bind `alias` to the target's value at recur entry. Reading the target
    // by name would see an earlier alias that rebinds the name, as in
    // `recur (i, j): case (j, i):`.
    private def bindAliasToTarget(
        alias: Bindable,
        inrec: InDefRecurred,
        targetItem: RecurTargetItem,
        state: SmtBranchState
    ): SmtBranchState = {
      val entry = inrec.entrySmtState
      val name = targetItem.paramName
      entry.intBindings
        .get(name)
        .map(state.withIntBinding(alias, _))
        .orElse(entry.boolBindings.get(name).map(state.withBoolBinding(alias, _)))
        .orElse(
          entry.comparisonBindings
            .get(name)
            .map(state.withComparisonBinding(alias, _))
        )
        .getOrElse(state)
    }

    // The SMT symbol for an Int recur target's value at recur entry.
    private def targetIntSymbol(
        inrec: InDefRecurred,
        targetItem: RecurTargetItem
    ): SmtExpr.IntExpr =
      inrec.entrySmtState.intBindings.getOrElse(
        targetItem.paramName,
        // $COVERAGE-OFF$ every Int argument has a symbol at entry
        sys.error(s"no SMT symbol for Int recur target: $targetItem")
        // $COVERAGE-ON$
      )

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

    private type FinalDefState = InDef | InDefRecurred

    private def finalDefState(state: State): FinalDefState =
      state match {
        case in: InDef          => in
        case in: InDefRecurred  => in
        case unreachable        =>
          // $COVERAGE-OFF$ this should be unreachable
          sys.error(
            s"we would like to prove in the types we can't get here: $unreachable"
          )
        // $COVERAGE-ON$
      }

    private def successfulDefResult(
        currentPackage: PackageName,
        fnname: Bindable,
        body: TypedExpr[Declaration],
        finalState: FinalDefState
    ): Res[Unit] =
      finalState match {
        case InDef(_, _, _, _, _, _, _, _, _) =>
          unitValid
        case InDefRecurred(_, _, recur, _, _) =>
          val selfCallKind = SelfCallKind(fnname, body)
          recur.kind match {
            case Declaration.MatchKind.Loop
                if selfCallKind == SelfCallKind.NonTailCall =>
              Validated.invalidNec(
                RecursionCheck.LoopRequiresTailRecursion(
                  fnname,
                  recur.region
                )
              )
            case Declaration.MatchKind.Recur
                if selfCallKind == SelfCallKind.TailCall =>
              Validated.invalidNec(
                RecursionCheck.TailRecursiveRecur(fnname, recur.region)
              )
            case _ =>
              NonEmptyChain
                .fromSeq(bindingOwnLints(SourceLintSyntax, currentPackage, fnname, body))
                .fold(unitValid)(Validated.invalid)
          }
      }

    private case class LintHeader[A](
        kind: Declaration.MatchKind,
        region: Region,
        branches: NonEmptyList[TypedExpr.Branch[A]]
    )

    private trait LintSyntax[A] {
      def isDefLike(rec: RecursionKind, expr: TypedExpr[A]): Boolean
      def selfCallKind(fnname: Bindable, expr: TypedExpr[A]): SelfCallKind
      def recursiveHeader(
          currentPackage: PackageName,
          arg: TypedExpr[A],
          m: TypedExpr.MatchExpr[A]
      ): Option[LintHeader[A]]
    }

    private object SourceLintSyntax extends LintSyntax[Declaration] {
      def isDefLike(
          rec: RecursionKind,
          expr: TypedExpr[Declaration]
      ): Boolean =
        Impl.isDefLike(rec, expr.tag)

      def selfCallKind(
          fnname: Bindable,
          expr: TypedExpr[Declaration]
      ): SelfCallKind =
        SelfCallKind(fnname, expr)

      def recursiveHeader(
          currentPackage: PackageName,
          arg: TypedExpr[Declaration],
          m: TypedExpr.MatchExpr[Declaration]
      ): Option[LintHeader[Declaration]] =
        recurTag(currentPackage, arg, m.tag).map { recur =>
          LintHeader(recur.kind, recur.region, m.branches)
        }
    }

    private object ReplayLintSyntax extends LintSyntax[Region] {
      def isDefLike(
          rec: RecursionKind,
          expr: TypedExpr[Region]
      ): Boolean =
        rec.isRecursive || asAnnotatedLambda(expr).nonEmpty

      def selfCallKind(
          fnname: Bindable,
          expr: TypedExpr[Region]
      ): SelfCallKind =
        SelfCallKind.afterLoopRecurLowering(fnname, expr)

      def recursiveHeader(
          currentPackage: PackageName,
          arg: TypedExpr[Region],
          m: TypedExpr.MatchExpr[Region]
      ): Option[LintHeader[Region]] =
        if (m.matchKind.isRecursive)
          Some(LintHeader(m.matchKind, m.tag, m.branches))
        else None
    }

    def replayLints(
        currentPackage: PackageName,
        lets: List[(Bindable, RecursionKind, TypedExpr[Region])]
    ): List[RecursionCheck.Lint] =
      lets.flatMap { case (name, rec, expr) =>
        if (ReplayLintSyntax.isDefLike(rec, expr))
          bindingAndNestedLints(ReplayLintSyntax, currentPackage, name, expr)
        else nestedBindingLints(ReplayLintSyntax, currentPackage, expr)
      }

    private def bindingOwnLints[A](
        syntax: LintSyntax[A],
        currentPackage: PackageName,
        fnname: Bindable,
        expr: TypedExpr[A]
    ): List[RecursionCheck.Lint] =
      findRecursiveHeader(syntax, currentPackage, expr).toList.flatMap { header =>
        syntax.selfCallKind(fnname, expr) match {
          case SelfCallKind.NoCall =>
            RecursionCheck
              .NoRecursiveCall(
                fnname,
                header.kind,
                header.region,
                likelyRenameCall(
                  fnname,
                  calledNamesInBranches(currentPackage, header.branches)
                )
              ) :: Nil
          case SelfCallKind.TailCall
              if header.kind == Declaration.MatchKind.Recur =>
            RecursionCheck.TailRecursiveRecur(fnname, header.region) :: Nil
          case _ =>
            Nil
        }
      }

    private def bindingAndNestedLints[A](
        syntax: LintSyntax[A],
        currentPackage: PackageName,
        fnname: Bindable,
        expr: TypedExpr[A]
    ): List[RecursionCheck.Lint] =
      bindingOwnLints(syntax, currentPackage, fnname, expr) :::
        nestedBindingLints(syntax, currentPackage, expr)

    private def nestedBindingLints[A](
        syntax: LintSyntax[A],
        currentPackage: PackageName,
        expr: TypedExpr[A]
    ): List[RecursionCheck.Lint] =
      expr match {
        case TypedExpr.Generic(_, in)       =>
          nestedBindingLints(syntax, currentPackage, in)
        case TypedExpr.Annotation(in, _, _) =>
          nestedBindingLints(syntax, currentPackage, in)
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          nestedBindingLints(syntax, currentPackage, body)
        case TypedExpr.App(fn, args, _, _)  =>
          nestedBindingLints(syntax, currentPackage, fn) ::: args.toList.flatMap(
            nestedBindingLints(syntax, currentPackage, _)
          )
        case TypedExpr.Let(arg, rhs, in, rec, _) =>
          val nestedDefLints =
            if (syntax.isDefLike(rec, rhs))
              bindingAndNestedLints(syntax, currentPackage, arg, rhs)
            else nestedBindingLints(syntax, currentPackage, rhs)

          nestedDefLints ::: nestedBindingLints(syntax, currentPackage, in)
        case TypedExpr.Loop(args, body, _) =>
          args.toList.flatMap { case (_, initExpr) =>
            nestedBindingLints(syntax, currentPackage, initExpr)
          } ::: nestedBindingLints(syntax, currentPackage, body)
        case TypedExpr.Recur(args, _, _)   =>
          args.toList.flatMap(nestedBindingLints(syntax, currentPackage, _))
        case TypedExpr.Match(arg, branches, _) =>
          nestedBindingLints(syntax, currentPackage, arg) ::: branches.toList.flatMap {
            branch =>
              branch.guardExprIterator.toList.flatMap(
                nestedBindingLints(syntax, currentPackage, _)
              ) ::: nestedBindingLints(syntax, currentPackage, branch.expr)
          }
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          Nil
      }

    private def findRecursiveHeader[A](
        syntax: LintSyntax[A],
        currentPackage: PackageName,
        expr: TypedExpr[A]
    ): Option[LintHeader[A]] =
      expr match {
        case TypedExpr.Generic(_, in)       =>
          findRecursiveHeader(syntax, currentPackage, in)
        case TypedExpr.Annotation(in, _, _) =>
          findRecursiveHeader(syntax, currentPackage, in)
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          findRecursiveHeader(syntax, currentPackage, body)
        case TypedExpr.App(fn, args, _, _)  =>
          findRecursiveHeader(syntax, currentPackage, fn).orElse(
            args.toList.collectFirstSome(findRecursiveHeader(syntax, currentPackage, _))
          )
        case TypedExpr.Let(_, rhs, in, rec, _) =>
          val rhsHeader =
            if (syntax.isDefLike(rec, rhs)) None
            else findRecursiveHeader(syntax, currentPackage, rhs)
          rhsHeader.orElse(findRecursiveHeader(syntax, currentPackage, in))
        case TypedExpr.Loop(args, body, _) =>
          args.toList.collectFirstSome { case (_, initExpr) =>
            findRecursiveHeader(syntax, currentPackage, initExpr)
          }.orElse(findRecursiveHeader(syntax, currentPackage, body))
        case TypedExpr.Recur(args, _, _)   =>
          args.toList.collectFirstSome(findRecursiveHeader(syntax, currentPackage, _))
        case m @ TypedExpr.Match(arg, branches, _) =>
          syntax
            .recursiveHeader(currentPackage, arg, m)
            .orElse(
              findRecursiveHeader(syntax, currentPackage, arg)
            )
            .orElse(
              branches.toList.collectFirstSome { branch =>
                branch.foldGuardExpr(None: Option[LintHeader[A]]) {
                  (acc, guardExpr) =>
                    acc.orElse(findRecursiveHeader(syntax, currentPackage, guardExpr))
                }.orElse(findRecursiveHeader(syntax, currentPackage, branch.expr))
              }
            )
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          None
      }

    private def calledNamesInBranches[A](
        currentPackage: PackageName,
        branches: NonEmptyList[TypedExpr.Branch[A]]
    ): Map[Bindable, Int] =
      branches.foldLeft(Map.empty[Bindable, Int]) { (acc, branch) =>
        val outerBlocked = branch.pattern.names.toSet
        val innerBlocked = outerBlocked ++ branch.guardBindings
        val guardCalls =
          branch.guardNode match {
            case Some(TypedExpr.BoolGuard(guardExpr)) =>
              calledNamesInExpr(currentPackage, guardExpr, outerBlocked)
            case Some(TypedExpr.MatchGuard(argExpr, _, guardOpt)) =>
              calledNamesInExpr(currentPackage, argExpr, outerBlocked) |+|
                guardOpt.fold(Map.empty[Bindable, Int])(
                  calledNamesInExpr(currentPackage, _, innerBlocked)
                )
            case None                                 =>
              Map.empty
          }
        acc |+| guardCalls |+| calledNamesInExpr(
          currentPackage,
          branch.expr,
          innerBlocked
        )
      }

    private def calledNamesInExpr[A](
        currentPackage: PackageName,
        expr: TypedExpr[A],
        blocked: Set[Bindable]
    ): Map[Bindable, Int] =
      expr match {
        case TypedExpr.Generic(_, in)       =>
          calledNamesInExpr(currentPackage, in, blocked)
        case TypedExpr.Annotation(in, _, _) =>
          calledNamesInExpr(currentPackage, in, blocked)
        case TypedExpr.AnnotatedLambda(args, body, _) =>
          calledNamesInExpr(
            currentPackage,
            body,
            blocked ++ args.iterator.map(_._1)
          )
        case TypedExpr.App(fn, args, _, _)  =>
          val fnCalls = calledNamesInExpr(currentPackage, fn, blocked)
          val argCalls = args.foldLeft(Map.empty[Bindable, Int]) { (acc, arg) =>
            acc |+| calledNamesInExpr(currentPackage, arg, blocked)
          }
          val headCall =
            localOrLocalPackageGlobalNameOf(currentPackage, fn)
              .filterNot(blocked)
              .fold(Map.empty[Bindable, Int])(nm => Map(nm -> 1))
          fnCalls |+| argCalls |+| headCall
        case TypedExpr.Let(arg, rhs, in, rec, _) =>
          val rhsBlocked =
            if (rec.isRecursive) blocked + arg
            else blocked
          calledNamesInExpr(currentPackage, rhs, rhsBlocked) |+|
            calledNamesInExpr(currentPackage, in, blocked + arg)
        case TypedExpr.Loop(args, body, _) =>
          val blocked1 = blocked ++ args.iterator.map(_._1)
          val argCalls = args.foldLeft(Map.empty[Bindable, Int]) {
            case (acc, (_, initExpr)) =>
              acc |+| calledNamesInExpr(currentPackage, initExpr, blocked1)
          }
          argCalls |+| calledNamesInExpr(currentPackage, body, blocked1)
        case TypedExpr.Recur(args, _, _)   =>
          args.foldLeft(Map.empty[Bindable, Int]) { (acc, arg) =>
            acc |+| calledNamesInExpr(currentPackage, arg, blocked)
          }
        case TypedExpr.Match(arg, branches, _) =>
          val argCalls = calledNamesInExpr(currentPackage, arg, blocked)
          val branchCalls = branches.foldLeft(Map.empty[Bindable, Int]) {
            case (acc, branch) =>
              val outerBlocked = blocked ++ branch.pattern.names
              val innerBlocked = outerBlocked ++ branch.guardBindings
              val guardCalls =
                branch.guardNode match {
                  case Some(TypedExpr.BoolGuard(guardExpr)) =>
                    calledNamesInExpr(currentPackage, guardExpr, outerBlocked)
                  case Some(TypedExpr.MatchGuard(argExpr, _, guardOpt)) =>
                    calledNamesInExpr(currentPackage, argExpr, outerBlocked) |+|
                      guardOpt.fold(Map.empty[Bindable, Int])(
                        calledNamesInExpr(currentPackage, _, innerBlocked)
                      )
                  case None                                 =>
                    Map.empty
                }
              acc |+| guardCalls |+| calledNamesInExpr(
                currentPackage,
                branch.expr,
                innerBlocked
              )
          }
          argCalls |+| branchCalls
        case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
            TypedExpr.Literal(_, _, _) =>
          Map.empty
      }

    @annotation.tailrec
    private def localNameOf[A](expr: TypedExpr[A]): Option[Bindable] =
      expr match {
        case TypedExpr.Local(nm, _, _) => Some(nm)
        case TypedExpr.Generic(_, in)  => localNameOf(in)
        case TypedExpr.Annotation(in, _, _) => localNameOf(in)
        case _                         => None
      }

    @annotation.tailrec
    private def localOrLocalPackageGlobalNameOf[A](
        currentPackage: PackageName,
        expr: TypedExpr[A]
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
    private def asAnnotatedLambda[A](
        expr: TypedExpr[A]
    ): Option[TypedExpr.AnnotatedLambda[A]] =
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
    private def stripExprWrappers[A](
        expr: TypedExpr[A]
    ): TypedExpr[A] =
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
      Identifier.Name("eq_Int"),
      Identifier.Name("and"),
      Identifier.Name("or"),
      Identifier.Name("not"),
      Identifier.Name("xor"),
      Identifier.Name("implies")
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
      args match {
        case NonEmptyList(left, right :: Nil) =>
          val (leftExpr, state1) = lowerIntExpr(left, state)
          val (rightExpr, state2) = lowerIntExpr(right, state1)
          ((leftExpr, rightExpr).mapN(fn), state2)
        case _ =>
          (None, state)
      }

    private def lowerUnaryBool[A](
        args: NonEmptyList[TypedExpr[Declaration]],
        state: SmtBranchState
    )(
        fn: SmtExpr.BoolExpr => A
    ): (Option[A], SmtBranchState) =
      args match {
        case NonEmptyList(arg, Nil) =>
          val (argExpr, state1) = lowerBoolExpr(arg, state)
          (argExpr.map(fn), state1)
        case _ =>
          (None, state)
      }

    private def lowerBinaryBool[A](
        args: NonEmptyList[TypedExpr[Declaration]],
        state: SmtBranchState
    )(
        fn: (SmtExpr.BoolExpr, SmtExpr.BoolExpr) => A
    ): (Option[A], SmtBranchState) =
      args match {
        case NonEmptyList(left, right :: Nil) =>
          val (leftExpr, state1) = lowerBoolExpr(left, state)
          val (rightExpr, state2) = lowerBoolExpr(right, state1)
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
      val state1 =
        if (rec.isRecursive) {
          // the value is in the scope of name
          val state0 = state.removeBindings(name :: Nil)
          bindLetName(
            name,
            valueExpr,
            bindSymbolForType(name, valueExpr.getType, state0)
          )
        } else {
          // the value is outside the scope of name
          bindLetName(name, valueExpr, state)
        }
      val (res, state2) = lower(inExpr, state1)
      // the rest of the enclosing expression is outside the scope of name
      (res, state2.restoreBindings(name :: Nil, state))
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
          lowerFromMatches(arg, branches, state)(lowerComparisonExpr)
        case other =>
          val lit = comparisonLiteralValue(other).map(SmtExpr.IntConst(_))
          (lit, state)
      }

    private def canUseFinalBranchAsFallback(
        branch: TypedExpr.Branch[Declaration],
        condOpt: Option[SmtExpr.BoolExpr]
    ): Boolean =
      // Totality makes the final branch the default fallback arm. If we
      // cannot lower its condition, only use that fallback when the pattern
      // binds no names. This is only conservative: names a pattern binds that
      // do not lower get fresh symbols.
      condOpt.nonEmpty || branch.allBindings.isEmpty

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

    // Lower a match to an ite chain over the branch hit conditions. Nested ites
    // already give first-match order, so a branch's condition is its own hit
    // condition. Totality makes the final branch the default arm.
    private def lowerFromMatches[S <: SmtSort](
        argExpr: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        state: SmtBranchState
    )(
        lowerBody: (
            TypedExpr[Declaration],
            SmtBranchState
        ) => (Option[SmtExpr[S]], SmtBranchState)
    ): (Option[SmtExpr[S]], SmtBranchState) = {
      // Wrap the ite chain built so far in the remaining branches, from the
      // last one back, stopping at the first branch that does not lower.
      @annotation.tailrec
      def loop(
          rest: List[TypedExpr.Branch[Declaration]],
          elseExpr: SmtExpr[S],
          st: SmtBranchState
      ): (Option[SmtExpr[S]], SmtBranchState) =
        rest match {
          case Nil            => (Some(elseExpr), st)
          case branch :: tail =>
            val (condOpt, valueOpt, st1) =
              lowerBranch(argExpr, branch, st)(lowerBody)
            (condOpt, valueOpt) match {
              case (Some(cond), Some(value)) =>
                loop(tail, SmtExpr.Ite(cond, value, elseExpr), st1)
              case _ =>
                (None, st1)
            }
        }

      val reversed = branches.reverse
      val last = reversed.head
      val (lastCondOpt, lastOpt, state1) =
        lowerBranch(argExpr, last, state)(lowerBody)
      lastOpt match {
        case Some(lastExpr) if canUseFinalBranchAsFallback(last, lastCondOpt) =>
          loop(reversed.tail, lastExpr, state1)
        case _ =>
          (None, state1)
      }
    }

    // The hit condition and the lowered body of one branch.
    private def lowerBranch[A](
        argExpr: TypedExpr[Declaration],
        branch: TypedExpr.Branch[Declaration],
        state: SmtBranchState
    )(
        lowerBody: (
            TypedExpr[Declaration],
            SmtBranchState
        ) => (Option[A], SmtBranchState)
    ): (Option[SmtExpr.BoolExpr], Option[A], SmtBranchState) = {
      val (condOpt, state1) = lowerBranchHitCondition(argExpr, branch, state)
      val (bodyOpt, state2) = lowerBody(branch.expr, state1)
      // the other branches are outside the scope of this branch's names
      (condOpt, bodyOpt, state2.restoreBindings(branch.allBindings, state))
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
              // Bosatsu rounds toward negative infinity and defines
              // `x / 0 == 0`; SMT-LIB `div` differs on both
              lowerBinaryInt(args, state)(SmtExpr.bosatsuDiv)
            case Some(Identifier.Name("mod_Int")) =>
              // Bosatsu's `mod_Int` has the sign of the divisor and defines
              // `x % 0 == x`; SMT-LIB `mod` differs on both
              lowerBinaryInt(args, state)(SmtExpr.bosatsuMod)
            case _ =>
              inlinedTopLevelAliasApp(fn, args, state) match {
                case Some(inlined) => lowerIntExpr(inlined, state)
                case None          => (None, state)
              }
          }
        case TypedExpr.Match(arg, branches, _) if isBoolType(arg.getType) =>
          val loweredIf @ (ifOpt, state1) = lowerIntIfExpr(arg, branches, state)
          if (ifOpt.nonEmpty) loweredIf
          else lowerFromMatches(arg, branches, state1)(lowerIntExpr)
        case TypedExpr.Match(arg, branches, _) =>
          lowerFromMatches(arg, branches, state)(lowerIntExpr)
        case TypedExpr.Let(name, valueExpr, inExpr, rec, _) =>
          lowerWithLetBinding(name, valueExpr, inExpr, rec, state)(lowerIntExpr)
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
            case Some(Identifier.Name("and")) =>
              lowerBinaryBool(args, state)((left, right) =>
                simplifyBoolExpr(mkAnd(Vector(left, right)))
              )
            case Some(Identifier.Name("or")) =>
              lowerBinaryBool(args, state)((left, right) =>
                simplifyBoolExpr(mkOr(Vector(left, right)))
              )
            case Some(Identifier.Name("not")) =>
              lowerUnaryBool(args, state)(arg =>
                simplifyBoolExpr(SmtExpr.Not(arg))
              )
            case Some(Identifier.Name("xor")) =>
              lowerBinaryBool(args, state)((left, right) =>
                simplifyBoolExpr(
                  mkOr(
                    Vector(
                      mkAnd(Vector(left, SmtExpr.Not(right))),
                      mkAnd(Vector(SmtExpr.Not(left), right))
                    )
                  )
                )
              )
            case Some(Identifier.Name("implies")) =>
              lowerBinaryBool(args, state)((left, right) =>
                simplifyBoolExpr(mkOr(Vector(SmtExpr.Not(left), right)))
              )
            case _ =>
              inlinedTopLevelAliasApp(fn, args, state) match {
                case Some(inlined) => lowerBoolExpr(inlined, state)
                case None          => (None, state)
              }
          }
        case TypedExpr.Match(arg, branches, _) =>
          lowerFromMatches(arg, branches, state)(lowerBoolExpr)
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
      // Var and Named bind the whole scrutinee. Names inside a struct or a
      // union get no binding, so a use of one gets a fresh symbol, not the
      // outer binding it shadows.
      @annotation.tailrec
      def wholeNames(
          pat: Pattern[(PackageName, Identifier.Constructor), Type],
          acc: List[Bindable]
      ): List[Bindable] =
        pat match {
          case Pattern.Var(name)            => name :: acc
          case Pattern.Named(name, inner)   => wholeNames(inner, name :: acc)
          case Pattern.Annotation(inner, _) => wholeNames(inner, acc)
          case _                            => acc
        }

      bindNamesTo(wholeNames(pattern, Nil), pattern.names, argExpr, state)
    }

    private def addPatternFactsAndBindings(
        argExpr: TypedExpr[Declaration],
        pattern: Pattern[(PackageName, Identifier.Constructor), Type],
        state: SmtBranchState
    ): SmtBranchState = {
      // the scrutinee is outside the scope of the names the pattern binds
      val (patCondOpt, state1) = lowerPatternCondition(argExpr, pattern, state)
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

    private def lowerBranchGuardCondition(
        guardNode: Option[TypedExpr.BranchGuard[Declaration]],
        state: SmtBranchState
    ): (Option[SmtExpr.BoolExpr], SmtBranchState) =
      guardNode match {
        case Some(TypedExpr.BoolGuard(guardExpr)) =>
          lowerBoolExpr(guardExpr, state)
        case Some(TypedExpr.MatchGuard(argExpr, pattern, guardOpt)) =>
          // as for a branch, the pattern matches argExpr outside the scope of
          // the names it binds
          val (patCondOpt, state1) = lowerPatternCondition(argExpr, pattern, state)
          val state2 = bindPatternNames(argExpr, pattern, state1)
          val (innerGuardOpt, state3) =
            guardOpt match {
              case Some(innerGuard) =>
                lowerBoolExpr(innerGuard, state2)
              case None             =>
                (Some(SmtExpr.BoolConst.True), state2)
            }

          (
            (patCondOpt, innerGuardOpt).mapN((pat, guard) =>
              simplifyBoolExpr(mkAnd(Vector(pat, guard)))
            ),
            state3
          )
        case None =>
          (Some(SmtExpr.BoolConst.True), state)
      }

    private def addBranchGuardFacts(
        guardNode: Option[TypedExpr.BranchGuard[Declaration]],
        state: SmtBranchState
    ): SmtBranchState =
      guardNode match {
        case Some(TypedExpr.BoolGuard(guardExpr)) =>
          addGuardPathFact(guardExpr, state)
        case Some(TypedExpr.MatchGuard(argExpr, pattern, guardOpt)) =>
          val state1 = addPatternFactsAndBindings(argExpr, pattern, state)
          guardOpt.fold(state1)(addGuardPathFact(_, state1))
        case None =>
          state
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
      // The pattern matches the scrutinee in the scope outside the branch,
      // and the guard sees the names the pattern binds.
      val (patOpt, state1) = lowerPatternCondition(argExpr, branch.pattern, state)
      val state2 = bindPatternNames(argExpr, branch.pattern, state1)
      val (guardOpt, state3) = lowerBranchGuardCondition(branch.guardNode, state2)
      (
        (patOpt, guardOpt).mapN((pat, guard) =>
          simplifyBoolExpr(mkAnd(Vector(pat, guard)))
        ),
        state3
      )
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

    // An arm count past which subsumed-guard facts are not worth the
    // cartesian product of nested unions.
    private val maxUnionArms = 16

    // The union-free patterns `pattern` is a union of, in the order Bosatsu
    // tries them. A union binds names by its first arm that matches, and so
    // does a union nested in a struct or list item, so the arms of a product
    // come in order with the first part varying slowest. None when there
    // would be more than maxUnionArms, or when a list search (two or more
    // globs) would try positions before arms, which no such order describes.
    private def unionArms(pattern: TypedPattern): Option[List[TypedPattern]] = {
      def capped[A](arms: List[A]): Option[List[A]] =
        if (arms.lengthCompare(maxUnionArms) <= 0) Some(arms) else None

      // every choice of one arm per part, the first part varying slowest
      def product[A](parts: List[List[A]]): Option[List[List[A]]] =
        parts.foldRight(Option(List(List.empty[A]))) { (part, accOpt) =>
          accOpt.flatMap(acc => capped(part.flatMap(a => acc.map(a :: _))))
        }

      pattern match {
        case Pattern.Union(head, rest) =>
          (head :: rest.toList).traverse(unionArms).flatMap(arms => capped(arms.flatten))
        case Pattern.Named(name, inner) =>
          unionArms(inner).map(_.map(Pattern.Named(name, _)))
        case Pattern.Annotation(inner, tpe) =>
          unionArms(inner).map(_.map(Pattern.Annotation(_, tpe)))
        case Pattern.PositionalStruct(cons, params) =>
          params
            .traverse(unionArms)
            .flatMap(product)
            .map(_.map(Pattern.PositionalStruct(cons, _)))
        case Pattern.ListPat(parts) =>
          val globs = parts.count {
            case _: Pattern.ListPart.Glob => true
            case Pattern.ListPart.Item(_) => false
          }
          parts
            .traverse {
              case Pattern.ListPart.Item(item) =>
                unionArms(item).map(_.map(Pattern.ListPart.Item(_)))
              case glob: Pattern.ListPart.Glob =>
                Some(List(glob))
            }
            .flatMap { partArms =>
              if ((globs > 1) && partArms.exists(_.lengthCompare(1) > 0)) None
              else product(partArms).map(_.map(Pattern.ListPat(_)))
            }
        case Pattern.WildCard | Pattern.Literal(_) | Pattern.Var(_) |
            Pattern.StrPat(_) =>
          Some(pattern :: Nil)
      }
    }

    // The arms of `priorPattern` that can bind a value `currentPattern`
    // matches: each arm that overlaps it, up to the first arm that covers
    // it, since a value that arm matches never reaches a later one. Leaving
    // an arm out is sound only because the pattern algebra is exact here:
    // intersection is empty only for disjoint patterns, and difference only
    // when the arm covers the pattern, as patternSubsumes already assumes.
    private def candidateArms(
        priorPattern: TypedPattern,
        currentPattern: TypedPattern,
        state: SmtBranchState
    ): Option[NonEmptyList[TypedPattern]] =
      unionArms(priorPattern).flatMap { arms =>
        val (beforeCover, fromCover) =
          arms.span(arm => !patternSubsumes(arm, currentPattern, state))
        NonEmptyList.fromList(
          (beforeCover ::: fromCover.take(1)).filter(arm =>
            state.totalityCheck.intersection(arm, currentPattern).nonEmpty
          )
        )
      }

    // `name` has the same SMT binding in both states.
    private def sameBinding(
        name: Bindable,
        a: SmtBranchState,
        b: SmtBranchState
    ): Boolean =
      a.intBindings.get(name).exists(e => b.intBindings.get(name).contains(e)) ||
        a.boolBindings.get(name).exists(e => b.boolBindings.get(name).contains(e)) ||
        a.comparisonBindings
          .get(name)
          .exists(e => b.comparisonBindings.get(name).contains(e))

    // That the guard held, if `arm` bound its pattern's names: the guard with
    // those names renamed to the current pattern's names that bind the same
    // values, lowered in `state`. Whole-value aliases give several renames
    // that are all true at once, so any that lowers will do. None when none
    // lowers.
    private def subsumedArmGuardHeld(
        guardExpr: TypedExpr[Declaration],
        arm: TypedPattern,
        currentPattern: TypedPattern,
        state: SmtBranchState
    ): Option[(SmtExpr.BoolExpr, SmtBranchState)] = {
      // a name bound by a list search may be a different element in each
      // pattern, so it does not rename
      val searchBound = currentPattern.ambiguousBindings
      val (heldRev, state1) =
        alignSubsumedGuardExprAlternatives(guardExpr, arm, currentPattern)
          .foldLeft((List.empty[SmtExpr.BoolExpr], state)) {
            case ((held, st0), alignedGuardExpr) =>
              val guardFree = alignedGuardExpr.freeVarsDup.toSet
              if (
                guardFree.subsetOf(availableBranchNames(currentPattern, st0)) &&
                !guardFree.exists(searchBound)
              ) {
                val (guardOpt, stNext) = lowerBoolExpr(alignedGuardExpr, st0)
                (guardOpt.fold(held)(simplifyBoolExpr(_) :: held), stNext)
              } else (held, st0)
          }
      NonEmptyList
        .fromList(heldRev.reverse.distinct)
        .map(held => (mkOr(held.toList.toVector), state1))
    }

    // The fact that an earlier branch's guard was false, when that branch's
    // pattern covers the current pattern. `outer` is the state before the
    // current pattern bound its names, where the earlier guard's outer names
    // have the values the guard read.
    // - A union binds names by the arm that matched first, so the fact is
    //   that the guard was false for one of the arms that can match here. If
    //   one of them gives no fact, there is none.
    // - A name the earlier arm binds by a list search does not rename.
    // - An outer name the guard reads must have its outer value here: the
    //   current pattern may rebind it only to the same value.
    private def subsumedGuardFact(
        guardExpr: TypedExpr[Declaration],
        priorPattern: TypedPattern,
        currentPattern: TypedPattern,
        outer: SmtBranchState,
        state: SmtBranchState
    ): Option[(SmtExpr.BoolExpr, SmtBranchState)] = {
      val guardNames = guardExpr.freeVarsDup.toSet
      val outerNames = guardNames -- priorPattern.names
      val outerKept =
        currentPattern.names.forall(n => !outerNames(n) || sameBinding(n, outer, state))
      val readsSearchBound = guardNames.exists(priorPattern.ambiguousBindings)
      if (!outerKept || readsSearchBound) None
      else
        candidateArms(priorPattern, currentPattern, state).flatMap { arms =>
          arms.toList
            .foldLeft(Option((List.empty[SmtExpr.BoolExpr], state))) {
              case (acc, arm) =>
                acc.flatMap { case (heldRev, st) =>
                  subsumedArmGuardHeld(guardExpr, arm, currentPattern, st).map {
                    case (held, st1) => (held :: heldRev, st1)
                  }
                }
            }
            .map { case (heldRev, st1) =>
              (SmtExpr.Not(mkAnd(heldRev.reverse.toVector)), st1)
            }
        }
    }

    private def addSubsumedGuardFallthroughFacts(
        priorBranches: List[TypedExpr.Branch[Declaration]],
        currentPattern: TypedPattern,
        outer: SmtBranchState,
        state: SmtBranchState
    ): SmtBranchState =
      // If a previous branch pattern fully covers the current pattern, falling
      // through implies that prior guard was false.
      priorBranches.foldLeft(state) { (st, priorBranch) =>
        priorBranch.guard match {
          case Some(guardExpr)
              if patternSubsumes(priorBranch.pattern, currentPattern, st) =>
            subsumedGuardFact(
              guardExpr,
              priorBranch.pattern,
              currentPattern,
              outer,
              st
            ).fold(st) { case (fact, st1) => addPathFactIfNonTrivial(fact, st1) }
          case _ =>
            st
        }
      }

    // The SMT state on entry to each branch of `match argExpr` from `state`,
    // before the branch's guard: the pattern's facts and bindings, the names
    // `bindAliases` binds for the branch at that index, then that the earlier
    // branches missed. Same order and length as `branches`.
    private def matchBranchEntryStates(
        argExpr: TypedExpr[Declaration],
        branches: NonEmptyList[TypedExpr.Branch[Declaration]],
        state: SmtBranchState
    )(
        bindAliases: (Int, SmtBranchState) => SmtBranchState
    ): NonEmptyList[SmtBranchState] = {
      val analyzed = matchFallthroughFacts(argExpr, branches, state)
      val start = mergeSymbolState(analyzed.symbolState, state)
      val priorBranches = analyzed.facts.toList.map(_._1)
      analyzed.facts.zipWithIndex.map {
        case ((branch, fallthroughFact), idx) =>
          val withPattern =
            bindAliases(
              idx,
              addPatternFactsAndBindings(argExpr, branch.pattern, start)
            )
          val withSubsumed =
            addSubsumedGuardFallthroughFacts(
              priorBranches.take(idx),
              branch.pattern,
              start,
              withPattern
            )
          addPathFactIfNonTrivial(fallthroughFact, withSubsumed)
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
      initSmtState(
        inrec.inRec.typedArgs,
        inrec.inRec.totalityCheck,
        inrec.inRec.topLevelLowerableAliases,
        inrec.inRec.topLevelPredefAliases
      )

    // One SMT symbol for each Int, Bool or Comparison argument of a def.
    private def initSmtState(
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]],
        totalityCheck: TotalityCheck,
        topLevelLowerableAliases: TopLevelLowerableAliases,
        topLevelPredefAliases: TopLevelPredefAliases
    ): SmtBranchState =
      typedArgs.iterator.flatMap(_.iterator).foldLeft(
        SmtBranchState.empty(totalityCheck).copy(
          topLevelLowerableAliases = topLevelLowerableAliases,
          topLevelPredefAliases = topLevelPredefAliases
        )
      ) { case (state, (name, tpe)) =>
        bindSymbolForType(name, tpe, state)
      }

    def lowerDefBodyForTest(
        currentPackage: PackageName,
        fullTypeEnv: TypeEnv[Kind.Arg],
        lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
        name: Bindable
    ): Option[LoweredDefBody] =
      lets.reverseIterator
        .collectFirst { case (n, _, expr) if n == name => expr }
        .flatMap(collectArgGroupsAndBody)
        .map { case (typedArgs, body) =>
          val state0 = initSmtState(
            typedArgs,
            TotalityCheck(fullTypeEnv),
            topLevelLowerableAliases(currentPackage, lets),
            topLevelPredefAliases(currentPackage, lets)
          )
          val tpe = body.getType
          val (term, state1) =
            if (isIntType(tpe)) {
              val (t, s) = lowerIntExpr(body, state0)
              (t.map(LoweredTerm.IntTerm(_)), s)
            } else if (isBoolType(tpe)) {
              val (t, s) = lowerBoolExpr(body, state0)
              (t.map(LoweredTerm.BoolTerm(_)), s)
            } else if (isComparisonType(tpe)) {
              val (t, s) = lowerComparisonExpr(body, state0)
              (t.map(LoweredTerm.ComparisonTerm(_)), s)
            } else (None, state0)

          LoweredDefBody(
            state0.intBindings,
            state0.boolBindings,
            state0.comparisonBindings,
            term,
            state1.pathFacts
          )
        }

    def leafPathFactsForTest(
        currentPackage: PackageName,
        fullTypeEnv: TypeEnv[Kind.Arg],
        lets: List[(Bindable, RecursionKind, TypedExpr[Declaration])],
        name: Bindable
    ): Option[LeafPathFacts] =
      lets.reverseIterator
        .collectFirst { case (n, _, expr) if n == name => expr }
        .flatMap(collectArgGroupsAndBody)
        .map { case (typedArgs, body) =>
          val state0 = initSmtState(
            typedArgs,
            TotalityCheck(fullTypeEnv),
            topLevelLowerableAliases(currentPackage, lets),
            topLevelPredefAliases(currentPackage, lets)
          )

          // Mirrors how checkExpr threads the SMT state into a let's body and
          // a match branch's body. A work list keeps deep bodies off the stack.
          @annotation.tailrec
          def walk(
              todo: List[(TypedExpr[Declaration], SmtBranchState)],
              leaves: Map[BigInt, Vector[SmtExpr.BoolExpr]]
          ): Map[BigInt, Vector[SmtExpr.BoolExpr]] =
            todo match {
              case Nil                   => leaves
              case (expr, state) :: rest =>
                stripExprWrappers(expr) match {
                  case TypedExpr.Literal(Lit.Integer(leaf), _, _) =>
                    walk(rest, leaves.updated(BigInt(leaf), state.pathFacts))
                  case TypedExpr.Let(n, value, in, rec, _) if !rec.isRecursive =>
                    walk((in, bindLetName(n, value, state)) :: rest, leaves)
                  case TypedExpr.Match(arg, branches, _) =>
                    val entryStates =
                      matchBranchEntryStates(arg, branches, state)((_, st) => st)
                    val bodies =
                      branches.toList.zip(entryStates.toList).map {
                        case (branch, entry) =>
                          (branch.expr, addBranchGuardFacts(branch.guardNode, entry))
                      }
                    walk(bodies ::: rest, leaves)
                  case _ =>
                    walk(rest, leaves)
                }
            }

          LeafPathFacts(
            state0.intBindings,
            state0.boolBindings,
            state0.comparisonBindings,
            walk((body, state0) :: Nil, Map.empty)
          )
        }

    private def bindLetName(
        name: Bindable,
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): SmtBranchState =
      bindNamesTo(name :: Nil, name :: Nil, expr, state)

    // Bind `names` to the value of `expr`, which is lowered in `state`, so
    // outside the scope of `names` and `shadowed`. Those names lose any outer
    // binding first, so if `expr` does not lower, `names` get one fresh
    // symbol rather than an outer binding they shadow.
    private def bindNamesTo(
        names: List[Bindable],
        shadowed: List[Bindable],
        expr: TypedExpr[Declaration],
        state: SmtBranchState
    ): SmtBranchState = {
      val tpe = expr.getType
      val removed = names ::: shadowed
      names match {
        case Nil => state.removeBindings(removed)
        case first :: _ =>
          if (isIntType(tpe)) {
            val (intExprOpt, state1) = lowerIntExpr(expr, state)
            val state2 = state1.removeBindings(removed)
            val (value, state3) = intExprOpt match {
              case Some(intExpr) => (intExpr, state2)
              case None          => ensureIntLocal(first, state2)
            }
            names.foldLeft(state3)(_.withIntBinding(_, value))
          } else if (isBoolType(tpe)) {
            val (boolExprOpt, state1) = lowerBoolExpr(expr, state)
            val state2 = state1.removeBindings(removed)
            val (value, state3) = boolExprOpt match {
              case Some(boolExpr) => (boolExpr, state2)
              case None           => ensureBoolLocal(first, state2)
            }
            names.foldLeft(state3)(_.withBoolBinding(_, value))
          } else if (isComparisonType(tpe)) {
            val (cmpExprOpt, state1) = lowerComparisonExpr(expr, state)
            val state2 = state1.removeBindings(removed)
            val (value, state3) = cmpExprOpt match {
              case Some(cmpExpr) => (cmpExpr, state2)
              case None          => ensureComparisonLocal(first, state2)
            }
            names.foldLeft(state3)(_.withComparisonBinding(_, value))
          } else state.removeBindings(removed)
      }
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
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        fieldType: Type,
        fieldArgExpr: TypedExpr[Declaration],
        targetType: Type
    ): Boolean =
      // v1 safety rule: exact recursive payloads must use known-smaller names,
      // and wrapped recursive payloads are rejected conservatively.
      if (!Type.containsType(fieldType, targetType)) true
      else if (fieldType.sameAs(targetType))
        classifyExpr(inrec, target, proof, fieldArgExpr) == Smaller
      else false

    private def classifyConstructorRankArg(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        currentCtor: KnownCtor,
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
                inrec,
                target,
                proof,
                fieldType,
                fieldArg,
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

    private def hasUnitArgFunction(
        fnExpr: TypedExpr[Declaration]
    ): Boolean =
      fnExpr.getType match {
        case Type.Fun(args, _) =>
          args.tail.isEmpty &&
          args.head.sameAs(Type.UnitType)
        case _ =>
          false
      }

    private def hasLazyType(
        expr: TypedExpr[Declaration]
    ): Boolean =
      expr.getType match {
        case Type.TyApply(Type.TyConst(const), _) =>
          const == lazyTypeConst
        case _ =>
          false
      }

    private def hasEvalType(
        expr: TypedExpr[Declaration]
    ): Boolean =
      expr.getType match {
        case Type.TyApply(Type.TyConst(const), _) =>
          const == evalTypeConst
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
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        arg: TypedExpr[Declaration]
    ): Option[ArgLexOrder] =
      // Trusted destructor: forcing a thunk yields a strict child of the thunk.
      oneArgApp(arg).collect {
        case (fnExpr, unitArg)
            if isCanonicalUnitLiteral(unitArg) &&
              (classifyExpr(inrec, target, proof, fnExpr) != Other) &&
              hasUnitArgFunction(fnExpr) =>
          Smaller
      }

    private def classifyLazyForceArg(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        arg: TypedExpr[Declaration]
    ): Option[ArgLexOrder] =
      // Trust boundary: only the canonical Bosatsu/Lazy.get_Lazy force form.
      oneArgApp(arg).collect {
        case (fnExpr, lazyArg)
            if isTrustedGlobalFn(fnExpr, lazyPackageName, getLazyName) &&
              (classifyExpr(inrec, target, proof, lazyArg) != Other) &&
              hasLazyType(lazyArg) =>
          Smaller
      }

    private def classifyEvalForceArg(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        arg: TypedExpr[Declaration]
    ): Option[ArgLexOrder] =
      // Trust boundary: only the canonical Bosatsu/Eval.eval force form.
      oneArgApp(arg).collect {
        case (fnExpr, evalArg)
            if isTrustedGlobalFn(fnExpr, evalPackageName, evalName) &&
              (classifyExpr(inrec, target, proof, evalArg) != Other) &&
              hasEvalType(evalArg) =>
          Smaller
      }

    private def classifyExpr(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        arg: TypedExpr[Declaration]
    ): ArgLexOrder = {
      localNameOf(arg)
        .flatMap(proof.relationOf(_))
        .map {
          case ProvenRel.Equal   => Equal
          case ProvenRel.Smaller => Smaller
        }
        .orElse {
          proof.singletonCtorIfEqual.collect {
            case s0 if singletonCtorFromArgExpr(arg).contains(s0) => Equal
          }
        }
        .orElse(classifyThunkForceArg(inrec, target, proof, arg))
        .orElse(classifyLazyForceArg(inrec, target, proof, arg))
        .orElse(classifyEvalForceArg(inrec, target, proof, arg))
        .orElse(
          proof.currentCtorIfEqual.map(
            classifyConstructorRankArg(inrec, target, proof, _, arg)
          )
        )
        .getOrElse(Other)
    }

    // `name = expr`: expr is classified in the outer scope, then `name` loses
    // what was known about the name it shadows.
    private def extendProofWithExpr(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        name: Bindable,
        expr: TypedExpr[Declaration]
    ): TargetProof = {
      val inner = proof.without(name :: Nil)
      classifyExpr(inrec, target, proof, expr) match {
        case Equal   => inner.withFact(name, ProvenRel.Equal)
        case Smaller => inner.withFact(name, ProvenRel.Smaller)
        case Other   => inner
      }
    }

    private def extendProofWithStrictChildExpr(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        name: Bindable,
        expr: TypedExpr[Declaration]
    ): TargetProof =
      classifyExpr(inrec, target, proof, expr) match {
        case Equal | Smaller =>
          proof.withFact(name, ProvenRel.Smaller)
        case Other =>
          proof
      }

    // `match scrutinee: case pattern`: the scrutinee is classified in the
    // outer scope, then the pattern's names lose what was known about the
    // names they shadow.
    private def extendProofWithPattern(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        scrutinee: TypedExpr[Declaration],
        pattern: TypedPattern
    ): TargetProof = {
      val inner = proof.without(pattern.names)
      classifyExpr(inrec, target, proof, scrutinee) match {
        case Equal =>
          val knownCtor = knownCtorFromTypedPart(pattern)
          inner
            .withFacts(wholeValueAliases(pattern), ProvenRel.Equal)
            .withFacts(pattern.substructures, ProvenRel.Smaller)
            .copy(
              singletonCtorIfEqual = knownCtor.fold(proof.singletonCtorIfEqual)(_ =>
                singletonCtorFromTypedPart(pattern)
              ),
              currentCtorIfEqual = knownCtor.orElse(proof.currentCtorIfEqual)
            )
        case Smaller =>
          inner
            .withFacts(wholeValueAliases(pattern), ProvenRel.Smaller)
            .withFacts(pattern.substructures, ProvenRel.Smaller)
        case Other   =>
          inner
      }
    }

    private def classifyStructuralArg(
        inrec: InDefRecurred,
        target: RecurTargetItem,
        proof: TargetProof,
        arg: TypedExpr[Declaration]
    ): LexStep = {
      LexStep(classifyExpr(inrec, target, proof, arg), None, None)
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

    // currInt is the target's value at recur entry, not whatever its name is
    // bound to at the call, which a let or pattern may have rebound.
    private def classifyIntArg(
        fnname: Bindable,
        target: RecurTargetItem,
        currInt: SmtExpr.IntExpr,
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
          val nonNegGoal = SmtExpr.Gte(argInt, SmtExpr.IntConst(BigInt(0)))
          val lessGoal = SmtExpr.Lt(argInt, currInt)
          val nonNegProof = prove(nonNegGoal, state1)
          nonNegProof match {
            case ProofOutcome.Proved =>
              val lessProof = prove(lessGoal, state1)
              lessProof match {
                case ProofOutcome.Proved =>
                  (LexStep(Smaller, None, None), state1)
                case failedLess: ProofOutcome.Failed =>
                  // the argument is the target's own value, not just a
                  // name that may have been rebound
                  if (argInt == currInt) {
                    val deferredErr =
                      intObligationError(
                        fnname,
                        target,
                        lessGoal,
                        state1,
                        failedLess,
                        region
                      )
                    (LexStep(Equal, None, Some(deferredErr)), state1)
                  } else {
                    val equalGoal = SmtExpr.EqInt(argInt, currInt)
                    prove(equalGoal, state1) match {
                      case ProofOutcome.Proved =>
                        val deferredErr =
                          intObligationError(
                            fnname,
                            target,
                            lessGoal,
                            state1,
                            failedLess,
                            region
                          )
                        (LexStep(Equal, None, Some(deferredErr)), state1)
                      case _ =>
                        val immediateErr =
                          intObligationError(
                            fnname,
                            target,
                            lessGoal,
                            state1,
                            failedLess,
                            region
                          )
                        (LexStep(Other, Some(immediateErr), None), state1)
                    }
                  }
              }
            case failedNonNeg: ProofOutcome.Failed =>
              val err =
                intObligationError(
                  fnname,
                  target,
                  nonNegGoal,
                  state1,
                  failedNonNeg,
                  region
                )
              (LexStep(Other, Some(err), None), state1)
          }
      }
    }

    private def recurAllowedByLexOrder(
        fnname: Bindable,
        inrec: InDefRecurred,
        target: RecurTarget,
        callArgsByTarget: NonEmptyList[TypedExpr[Declaration]],
        region: Region
    ): St[Unit] =
      getSt.flatMap {
        case InRecurBranch(
              _,
              branch,
              proofsNow,
              smtState0
            ) =>
          val defaultLexErr: RecursionCheck.Error = {
            val targetParams = target.map(_.paramName)
            RecursionCheck.RecursionNotLexicographic(fnname, targetParams, region)
          }
          val targetItems = target.toList.toVector
          val proofItems = proofsNow.toList.toVector
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
            val proof = proofItems(idx)
            val argExpr = callItems(idx)
            val step =
              if (isIntType(targetItemType(inrec, targetItem))) {
                val (intStep, smt1) =
                  classifyIntArg(
                    fnname,
                    targetItem,
                    targetIntSymbol(inrec, targetItem),
                    argExpr,
                    smtState,
                    region
                  )
                smtState = smt1
                intStep
              } else
                classifyStructuralArg(
                  inrec,
                  targetItem,
                  proof,
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
              proofsNow,
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
    type ErrorOr[+A] = Either[NonEmptyChain[RecursionCheck.Issue], A]

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
    private def failSt[A](err: RecursionCheck.Issue): St[A] =
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

    private def withTemporaryRecurBranchProofs[A](
        in: St[A],
        onMissing: State => St[A]
    )(
        update: NonEmptyList[TargetProof] => NonEmptyList[TargetProof]
    ): St[A] =
      getSt.flatMap {
        case start @ InRecurBranch(
              inrec,
              branch,
              proofs,
              smtState
            ) =>
          val proofs1 = update(proofs)
          (
            setSt(
              InRecurBranch(
                inrec,
                branch,
                proofs1,
                smtState
              )
            ) *> in,
            getSt
          )
            .flatMapN {
              case (a, InRecurBranch(ir1, b1, _, smt1)) =>
                setSt(
                  InRecurBranch(ir1, b1, proofs, smt1)
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
              proofs,
              smtState
            ) =>
          val smtState1 = update(smtState)
          (
            setSt(
              InRecurBranch(
                inrec,
                branch,
                proofs,
                smtState1
              )
            ) *> in,
            getSt
          ).flatMapN {
            case (a, InRecurBranch(ir1, b1, proofs1, _)) =>
              setSt(
                InRecurBranch(ir1, b1, proofs1, smtState)
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

    // Single-target recursion keeps the old behavior where lambda args
    // from reachable substructures are also considered recursive args.
    // With several targets we do not know which target the reachable
    // function came from, so the new binders get no facts at all.
    private def smallerIfSingleTarget(
        proofs: NonEmptyList[TargetProof],
        newNames: Iterable[Bindable]
    ): NonEmptyList[TargetProof] =
      proofs.tail match {
        case Nil    =>
          NonEmptyList.one(proofs.head.withFacts(newNames, ProvenRel.Smaller))
        case _ :: _ =>
          proofs
      }

    // The new names are fresh binders: any facts about shadowed names with the
    // same spelling (structural or SMT) must be dropped before adding new ones.
    private def unionNames[A](newNames: Iterable[Bindable])(in: St[A]): St[A] = {
      val withProofs =
        withTemporaryRecurBranchProofs(
          in,
          notRecur => sys.error(s"called setNames on $notRecur with names: $newNames")
        ) { proofs =>
          smallerIfSingleTarget(proofs.map(_.without(newNames)), newNames)
        }

      withTemporaryRecurBranchSmtState(withProofs, _ => withProofs)(
        _.removeBindings(newNames)
      )
    }

    private def filterNames[A](newNames: Iterable[Bindable])(in: St[A]): St[A] =
      withTemporaryRecurBranchProofs(in, _ => in) { proofs =>
        proofs.map(_.without(newNames))
      }

    // Check `in` with `newNames` bound to values nothing is known about: the
    // proofs and the SMT state drop what they knew about the outer names
    // with the same spelling.
    private def withShadowingNames[A](newNames: List[Bindable])(in: St[A]): St[A] = {
      val withProofs = filterNames(newNames)(in)
      withTemporaryRecurBranchSmtState(withProofs, _ => withProofs)(
        _.removeBindings(newNames)
      )
    }

    private def withTemporaryExprBindingProofs[A](
        name: Bindable,
        expr: TypedExpr[Declaration]
    )(in: St[A]): St[A] =
      getSt.flatMap {
        case InRecurBranch(inrec, _, _, _) =>
          withTemporaryRecurBranchProofs(in, _ => in) { proofs0 =>
            inrec.target
              .zip(proofs0)
              .map { case (targetItem, proof) =>
                extendProofWithExpr(inrec, targetItem, proof, name, expr)
              }
          }
        case _ =>
          in
      }

    private def withTemporaryPatternProofs[A](
        scrutinee: TypedExpr[Declaration],
        pattern: TypedPattern
    )(in: St[A]): St[A] =
      getSt.flatMap {
        case InRecurBranch(inrec, _, _, _) =>
          withTemporaryRecurBranchProofs(in, _ => in) { proofs0 =>
            inrec.target
              .zip(proofs0)
              .map { case (targetItem, proof) =>
                extendProofWithPattern(
                  inrec,
                  targetItem,
                  proof,
                  scrutinee,
                  pattern
                )
              }
          }
        case _ =>
          in
      }

    private def checkAnnotatedLambdaWithProofs(
        currentPackage: PackageName,
        args: NonEmptyList[(Bindable, Type)],
        body: TypedExpr[Declaration],
        wrappers: WrapperScope,
        region: Region
    )(
        updateProofs: NonEmptyList[TargetProof] => NonEmptyList[TargetProof]
    ): St[Unit] =
      checkForIllegalBindsSt(args.toList.map(_._1), region) *>
        withLambdaBinders(args, checkExpr(currentPackage, body, wrappers))(
          updateProofs
        )

    // Check a lambda body with its binders in scope: facts about any shadowed
    // names are dropped from the proofs and the SMT state, then updateProofs
    // may add facts about the new binders.
    private def withLambdaBinders(
        args: NonEmptyList[(Bindable, Type)],
        bareBodyCheck: St[Unit]
    )(
        updateProofs: NonEmptyList[TargetProof] => NonEmptyList[TargetProof]
    ): St[Unit] = {
      val newBinds = args.toList.map(_._1)
      val bodyCheck =
        withTemporaryRecurBranchProofs(
          bareBodyCheck,
          _ => bareBodyCheck
        ) { proofs =>
          updateProofs(proofs.map(_.without(newBinds)))
        }

      withTemporaryRecurBranchSmtState(
        bodyCheck,
        _ => bodyCheck
      ) { smtState =>
        args.foldLeft(smtState.removeBindings(newBinds)) {
          case (st, (name, tpe)) =>
            bindSymbolForType(name, tpe, st)
        }
      }
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

    private def trustedEvalFlatMapCall(
        fn: TypedExpr[Declaration],
        args: NonEmptyList[TypedExpr[Declaration]]
    ): Option[(TypedExpr[Declaration], TypedExpr.AnnotatedLambda[Declaration])] =
      args.toList match {
        case evalArg :: lambdaArg :: Nil
            if isTrustedGlobalFn(fn, evalPackageName, flatMapName) &&
              hasEvalType(evalArg) =>
          asAnnotatedLambda(lambdaArg).collect {
            case lam @ TypedExpr.AnnotatedLambda(lambdaArgs, _, _)
                if lambdaArgs.tail.isEmpty =>
              (evalArg, lam)
          }
        case _ =>
          None
      }

    /*
     * The callback rule treats the binders of lambdas passed to a reachable
     * function f as smaller than the recur target. That is sound only when
     * every value f can hand to those binders was built by f itself (from its
     * closure, which predates the target). Recursive types are covariant, so a
     * field type never takes values of the target type as input; but once f
     * is used at a type with a quantifier, or at an instantiation (possibly
     * laundered through a let annotation) that accepts values which may
     * contain target values, the caller can pass the target itself in and f
     * can pass it straight back, e.g. f: forall a. (a, a -> Int) -> Int.
     *
     * Everything the caller can pass into f is one of the call's arguments
     * (in any group of a curried call), including whatever the callbacks
     * return. So we look at the types of the arguments at this call, which
     * already reflect how the caller instantiated f's quantifiers, and
     * require that no type which may contain values of a target type is
     * something the caller hands to f (an input position of the argument
     * types). For example `forall r. (T -> r) -> r` is accepted at r := Int.
     * At r := (T, Int) it is rejected: a callback could return the target to
     * f, and the argument types alone don't show that f is parametric in r.
     *
     * Quantifiers left in the argument types are looked through. A forall in
     * an input position is instantiated by f (as in the existential encoding
     * `forall res. (forall b. (Box[b], b -> a) -> res) -> res`), and one in
     * an output position is instantiated by a callback, at a later call that
     * gets its own check. Values whose type is a type variable can never be
     * used where a target type is required, so variables are harmless.
     */
    private def reachableCallbackRuleApplies(
        inrec: InDefRecurred,
        argTypes: List[Type]
    ): Boolean = {
      val typeEnv = inrec.inRec.totalityCheck.inEnv

      // Typed expressions keep type aliases as written (`type CbA = Cb`), so
      // every comparison of type constants below must see through them.
      // Returns a type whose head is not an alias, or None if the alias can't
      // be expanded (under-applied, or a cycle, which source conversion
      // already rejects); callers treat None as unsafe.
      @annotation.tailrec
      def expandAliasHead(t: Type, expanding: Set[Type.Const]): Option[Type] =
        Type.unapplyAll(t) match {
          case (Type.TyConst(c), args) =>
            typeEnv.getTypeAlias(Type.TyConst(c)) match {
              case Some(alias) =>
                if (expanding(c)) None
                else
                  alias.expandWith(args) match {
                    case Some(t1) => expandAliasHead(t1, expanding + c)
                    case None     => None
                  }
              case None => Some(t)
            }
          case _ => Some(t)
        }

      val targetConsts: List[Type.Const] =
        inrec.target.toList
          .traverse { targetItem =>
            expandAliasHead(targetItemType(inrec, targetItem), Set.empty)
              .map(Type.unapplyAll(_)._1)
              .collect { case Type.TyConst(c) => c }
          }
          .getOrElse(Nil)

      // Can values of type constant `c` contain values of a target type?
      // True if `c` is a target, or its definition (or alias right-hand side)
      // transitively mentions one. Constants we can't resolve are assumed to.
      // Function type constants carry no values of their own: their
      // arguments are walked separately by `unsafe`. Phantom parameters,
      // including those declared on external types, are trusted to hold no
      // values.
      val mayContainCache = mutable.Map.empty[Type.Const, Boolean]
      def mayContainTarget(c: Type.Const): Boolean = {
        @annotation.tailrec
        def loop(
            todo: List[Type.Const],
            seen: Set[Type.Const]
        ): Boolean =
          todo match {
            case Nil => false
            case h :: tail =>
              if (targetConsts.contains(h)) true
              else if (seen(h)) loop(tail, seen)
              else {
                val tc = Type.TyConst(h)
                val deps: Option[List[Type.TyConst]] =
                  tc match {
                    case Type.FnType(_) => Some(Nil)
                    case _ =>
                      typeEnv
                        .getType(tc)
                        .map(_.dependsOn)
                        .orElse(typeEnv.getTypeAlias(tc).map(_.dependsOn))
                  }
                deps match {
                  case Some(ds) => loop(ds.map(_.tpe) ::: tail, seen + h)
                  case None     => true
                }
              }
          }

        mayContainCache.getOrElseUpdate(c, loop(c :: Nil, Set.empty))
      }

      def isInput(v: Variance): Boolean =
        (v == Variance.Contravariant) || (v == Variance.Invariant)

      def paramVariance(c: Type.Const, idx: Int): Variance =
        typeEnv
          .getType(Type.TyConst(c))
          .flatMap(_.annotatedTypeParams.lift(idx))
          .fold(Variance.in)(_._2.variance)

      // true if the type (at polarity v) is unsafe for the callback rule
      def unsafe(t: Type, v: Variance): Boolean =
        if (v == Variance.Phantom) false
        else
          expandAliasHead(t, Set.empty) match {
            case None => true
            case Some(Type.ForAll(_, in)) => unsafe(in, v)
            case Some(Type.Exists(_, in)) => unsafe(in, v)
            case Some(Type.Fun(args, res)) =>
              args.exists(unsafe(_, -v)) || unsafe(res, v)
            case Some(t1) =>
              Type.unapplyAll(t1) match {
                case (Type.TyConst(c), args) =>
                  (isInput(v) && mayContainTarget(c)) ||
                  args.iterator.zipWithIndex.exists { case (arg, idx) =>
                    unsafe(arg, v * paramVariance(c, idx))
                  }
                case (Type.TyVar(_), args) =>
                  args.exists(unsafe(_, Variance.in))
                // $COVERAGE-OFF$ typechecked expressions have no metas left
                case _ =>
                  // be conservative about anything unexpected
                  true
                // $COVERAGE-ON$
              }
          }

      targetConsts.nonEmpty &&
      !argTypes.exists(unsafe(_, Variance.contra))
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
        case irb @ InRecurBranch(inrec, _, proofsPerTarget, _) =>
          trustedEvalFlatMapCall(fn, args) match {
            case Some((evalArg, TypedExpr.AnnotatedLambda(lambdaArgs, body, lambdaTag))) =>
              val binderName = lambdaArgs.head._1
              checkExpr(currentPackage, fn, wrappers) *>
                checkExpr(currentPackage, evalArg, wrappers) *>
                checkAnnotatedLambdaWithProofs(
                  currentPackage,
                  lambdaArgs,
                  body,
                  wrappers,
                  lambdaTag.region
                ) { proofs =>
                  val updated =
                    inrec.target.toList
                      .zip(proofs.toList)
                      .map { case (targetItem, proof) =>
                        extendProofWithStrictChildExpr(
                          inrec,
                          targetItem,
                          proof,
                          binderName,
                          evalArg
                        )
                      }
                  NonEmptyList.fromListUnsafe(updated)
                }
            case None =>
              argsOnDefName(currentPackage, fn, NonEmptyList.one(args)) match {
                case Some((nm, groups)) =>
                  // every argument group of a curried call
                  val allArgs = groups.iterator.flatMap(_.iterator).toList
                  if (nm == irb.defname) {
                    val targetArgsV: Res[NonEmptyList[TypedExpr[Declaration]]] =
                      inrec.target.traverse { targetItem =>
                        groups
                          .get(targetItem.group.toLong)
                          .flatMap(_.get(targetItem.index.toLong))
                          .toValidNec(RecursionCheck.NotEnoughRecurArgs(nm, region))
                      }

                    toSt(targetArgsV).flatMap { targetArgs =>
                      recurAllowedByLexOrder(
                        irb.defname,
                        inrec,
                        inrec.target,
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
                  } else if (
                    smallerNamesFromProofs(proofsPerTarget).contains(nm) &&
                    reachableCallbackRuleApplies(
                      inrec,
                      allArgs.map(_.getType)
                    )
                  ) {
                    // we are calling a reachable function. Any lambda args are new names
                    // that may only see values the reachable function builds itself
                    // (see reachableCallbackRuleApplies), so they are smaller.
                    // Every argument group of a curried call is checked.
                    allArgs.parTraverse_[St, Unit] {
                      case argExpr =>
                        asAnnotatedLambda(argExpr) match {
                          case Some(TypedExpr.AnnotatedLambda(lambdaArgs, body, lambdaTag)) =>
                            val names1 = lambdaArgs.toList.map(_._1)
                            checkForIllegalBindsSt(names1, lambdaTag.region) *>
                              withLambdaBinders(
                                lambdaArgs,
                                checkReachableLambdaBody(
                                  currentPackage,
                                  body,
                                  wrappers,
                                  lambdaTag
                                )
                              )(smallerIfSingleTarget(_, names1))
                          case None =>
                            localOrLocalPackageGlobalNameOf(
                              currentPackage,
                              argExpr
                            ) match {
                              case Some(fnname) if irb.defname == fnname =>
                                val asLambda = irb.inDef.asLambda(argExpr.tag.region)
                                val names1 = asLambda.args.toList.map(_._1)
                                withLambdaBinders(
                                  asLambda.args,
                                  checkExpr(currentPackage, asLambda.expr, wrappers)
                                )(smallerIfSingleTarget(_, names1))
                              case _ =>
                                checkExpr(currentPackage, argExpr, wrappers)
                            }
                        }
                    }
                  } else {
                    // not a recursive call, or a call to a reachable function
                    // whose type could hand its callbacks values chosen by the caller.
                    // Every argument group of a curried call is checked.
                    setSt(irb.noteCalledName(nm)) *> allArgs.parTraverse_(
                      checkExpr(currentPackage, _, wrappers)
                    )
                  }
                case None =>
                  // this isn't a recursive call
                  checkExpr(currentPackage, fn, wrappers) *> args.parTraverse_(
                    checkExpr(currentPackage, _, wrappers)
                  )
              }
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
          checkAnnotatedLambdaWithProofs(
            currentPackage,
            args,
            body,
            wrappers,
            expr.tag.region
          )(identity)
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
            // in `in`, arg is the new def, so drop facts about the name
            // it shadows
            val inCheck = withShadowingNames(arg :: Nil)(
              checkExpr(currentPackage, in, wrappers)
            )
            getSt.flatMap { state =>
              val checkedDef =
                checkDef(currentPackage, state, arg, ex, fromSource)
              val checkedIn =
                Validated.fromEither(inCheck.run(state).value).map(_._1)

              (checkedDef *> checkedIn) match {
                case Validated.Valid(nextState) =>
                  setSt(nextState)
                case Validated.Invalid(errs)    =>
                  St.liftEither(Left(errs))
              }
            }
          } else {
            val inCheck =
              withTemporaryExprBindingProofs(arg, ex)(
                checkExpr(currentPackage, in, wrappers)
              )
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
          def checkBranchGuardAndExpr(
              branch: TypedExpr.Branch[Declaration]
          ): St[Unit] = {
            val bodyCheck =
              checkExpr(currentPackage, branch.expr, wrappers)

            def bodyWithGuardFacts: St[Unit] =
              withTemporaryRecurBranchSmtState(
                bodyCheck,
                _ => bodyCheck
              ) { smtState =>
                addBranchGuardFacts(branch.guardNode, smtState)
              }

            branch.guardNode match {
              case Some(TypedExpr.BoolGuard(guardExpr)) =>
                checkExpr(currentPackage, guardExpr, wrappers) *>
                  bodyWithGuardFacts
              case Some(guard @ TypedExpr.MatchGuard(argExpr, pattern, guardOpt)) =>
                val innerGuardCheck =
                  guardOpt match {
                    case Some(innerGuard) =>
                      val checkInnerGuard =
                        checkExpr(currentPackage, innerGuard, wrappers)
                      withTemporaryRecurBranchSmtState(
                        checkInnerGuard,
                        _ => checkInnerGuard
                      ) { smtState =>
                        addPatternFactsAndBindings(argExpr, pattern, smtState)
                      }
                    case None             =>
                      unitSt
                  }

                // the guard's pattern binds names in the inner guard and the
                // body, as a branch pattern does
                checkExpr(currentPackage, argExpr, wrappers) *>
                  checkForIllegalBindsSt(pattern.names, guard.patternRegion) *>
                  withTemporaryPatternProofs(argExpr, pattern)(
                    innerGuardCheck *> bodyWithGuardFacts
                  )
              case None                                      =>
                bodyCheck
            }
          }

          recurTag(currentPackage, arg, tag) match {
            case None =>
              // the arg can't use state, but cases introduce new bindings:
              val argRes = checkExpr(currentPackage, arg, wrappers)
              val optRes = getSt.flatMap { state =>
                // each branch starts from the SMT state before the match
                val entryStates: List[Option[SmtBranchState]] =
                  state match {
                    case InRecurBranch(_, _, _, smtState) =>
                      matchBranchEntryStates(arg, branches, smtState)((_, st) => st)
                        .toList
                        .map(Some(_))
                    case _                                  =>
                      branches.toList.map(_ => None)
                  }

                branches.toList.zip(entryStates).parTraverse_ {
                  case (branch, entryState) =>
                    val branchExprCheck = checkBranchGuardAndExpr(branch)
                    val withEntryState =
                      entryState.fold(branchExprCheck) { entry =>
                        withTemporaryRecurBranchSmtState(
                          branchExprCheck,
                          _ => branchExprCheck
                        )(_ => entry)
                      }
                    checkForIllegalBindsSt(branch.pattern.names, tag.region) *>
                      withTemporaryPatternProofs(
                        arg,
                        branch.pattern
                      )(withEntryState)
                }
              }
              argRes *> optRes
            case Some(recur) =>
              // this is a state change
              getSt.flatMap {
                case TopLevel(_) | InRecurBranch(_, _, _, _) |
                    InDefRecurred(_, _, _, _, _) =>
                  failSt(RecursionCheck.UnexpectedRecur(recur.region))
                case ir @ InDef(_, defname, typedArgs, sourceArgs, _, _, _, _, locals) =>
                  toSt(getRecurTarget(defname, sourceArgs, typedArgs, recur, locals)).flatMap {
                    target =>
                      val sourcePatterns = recur.cases.get.map(_.pattern).toList.toVector
                      val inrec = ir.setRecur(target, recur)
                      // Same order and length as `branches`. The source
                      // patterns' whole-value aliases of the targets are bound
                      // before the earlier branches' guards are read.
                      val entryStates =
                        matchBranchEntryStates(arg, branches, inrec.entrySmtState) {
                          (idx, st) =>
                            sourcePatterns
                              .lift(idx)
                              .fold(st)(bindSourcePatternAliases(inrec, _, st))
                        }

                      // on all these branches, use the same parent state
                      def beginBranch(
                          compiledPat: Pattern[(PackageName, Identifier.Constructor), Type],
                          entryState: SmtBranchState
                      ): St[Unit] =
                        getSt.flatMap {
                          case ir @ InDef(_, _, _, _, _, _, _, _, _) =>
                            val rec = ir.setRecur(target, recur)
                            setSt(rec) *> beginBranch(compiledPat, entryState)
                          case irr @ InDefRecurred(_, _, _, _, _) =>
                            setSt(
                              InRecurBranch(
                                irr,
                                compiledPat,
                                proofsByTargetFromTyped(irr.target, compiledPat),
                                entryState
                              )
                            )
                          case illegal =>
                            // $COVERAGE-OFF$ this should be unreachable
                            sys.error(s"unreachable: $compiledPat -> $illegal")
                          // $COVERAGE-ON$
                        }

                      val endBranch: St[Unit] =
                        getSt.flatMap {
                          case InRecurBranch(irr, _, _, _) =>
                            setSt(irr)
                          case illegal                  =>
                            // $COVERAGE-OFF$ this should be unreachable
                            sys.error(s"unreachable end state: $illegal")
                          // $COVERAGE-ON$
                        }

                      branches.toList.zip(entryStates.toList).parTraverse_ {
                        case (branch, entryState) =>
                          for {
                            _ <- checkForIllegalBindsSt(branch.pattern.names, tag.region)
                            _ <- beginBranch(branch.pattern, entryState)
                            _ <- checkBranchGuardAndExpr(branch)
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
            // We start the body checker in `InDef`, and a successful run must
            // return to either `InDef` or `InDefRecurred` after any recur branch.
            Validated
              .fromEither(
                checkExpr(
                  currentPackage,
                  body,
                  WrapperScope.Empty
                ).run(state1).value
              )
              .andThen { case (state2, _) =>
                successfulDefResult(
                  currentPackage,
                  fnname,
                  body,
                  finalDefState(state2)
                )
              }
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
