package dev.bosatsu

import cats.data.{NonEmptyChain, NonEmptyList, StateT, Validated, ValidatedNec}
import cats.implicits._
import dev.bosatsu.rankn.{Type, TypeEnv}

import Identifier.Bindable

object TypedExprRecursionCheck {

  type Res[+A] = ValidatedNec[RecursionCheck.Error, A]

  def topLevelDefArgs(
      stmts: List[Statement]
  ): Map[Bindable, NonEmptyList[NonEmptyList[Pattern.Parsed]]] =
    stmts.collect { case Statement.Def(defstmt) =>
      defstmt.name -> defstmt.args
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
    // This checker currently keeps structural parity with DefRecursionCheck.
    // `fullTypeEnv` is passed to keep this API ready for typed decrease
    // strategies (for example Int decrease) in follow-up work.
    val _ = fullTypeEnv
    lets.traverse_ { case (name, rec, expr) =>
      Impl.checkTopLevelLet(pack, name, rec, expr, topLevelDefs.get(name))
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
          case TopLevel        => Set.empty
          case ids: InDefState =>
            val InDef(outer, n, _, _, _, _) = ids.inDef
            outer.outerDefNames + n
        }

      @annotation.tailrec
      final def defNamesContain(n: Bindable): Boolean =
        this match {
          case TopLevel        => false
          case ids: InDefState =>
            val InDef(outer, dn, _, _, _, _) = ids.inDef
            (dn == n) || outer.defNamesContain(n)
        }

      def inDef(
          fnname: Bindable,
          typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]],
          sourceArgs: NonEmptyList[NonEmptyList[Pattern.Parsed]],
          fnType: Type
      ): InDef =
        InDef(this, fnname, typedArgs, sourceArgs, fnType, Set.empty)
    }

    sealed abstract class InDefState extends State {
      final def inDef: InDef =
        this match {
          case id @ InDef(_, _, _, _, _, _)             => id
          case InDefRecurred(ir, _, _, _, _)            => ir.inDef
          case InRecurBranch(InDefRecurred(ir, _, _, _, _), _, _, _) =>
            ir.inDef
        }

      final def defname: Bindable = inDef.fnname
    }

    case object TopLevel extends State

    case class InDef(
        outer: State,
        fnname: Bindable,
        typedArgs: NonEmptyList[NonEmptyList[(Bindable, Type)]],
        sourceArgs: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        fnType: Type,
        localScope: Set[Bindable]
    ) extends InDefState {
      def addLocal(b: Bindable): InDef =
        InDef(outer, fnname, typedArgs, sourceArgs, fnType, localScope + b)

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
        reachableNames: Set[Bindable]
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
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]]
    ): Map[Bindable, RecurTargetItem] =
      args.iterator
        .zipWithIndex
        .flatMap { case (group, gidx) =>
          group.iterator.zipWithIndex.flatMap { case (item, idx) =>
            item.topNames.iterator.map { topName =>
              topName -> RecurTargetItem(gidx, idx, topName)
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
        m: Declaration.Match,
        locals: Set[Bindable]
    ): Res[RecurTarget] = {
      import Declaration._
      val targetItemsByName = getRecurTargetItemsByName(args)
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
    private def unwrapNamedAnnotation(
        pat: Pattern.Parsed
    ): Pattern.Parsed =
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

    private def allowedByTargetFromCompiled(
        target: RecurTarget,
        branchPat: Pattern[(PackageName, Identifier.Constructor), Type]
    ): NonEmptyList[Set[Bindable]] =
      target.tail match {
        case Nil =>
          NonEmptyList.one(branchPat.substructures.toSet)
        case _ =>
          branchPat match {
            case Pattern.PositionalStruct(_, parts)
                if parts.length == target.length =>
              NonEmptyList.fromListUnsafe(parts.map(_.substructures.toSet))
            case _ =>
              target.map(_ => Set.empty[Bindable])
          }
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

    private def classifyArg(
        target: RecurTargetItem,
        allowed: Set[Bindable],
        arg: TypedExpr[Declaration]
    ): ArgLexOrder =
      localNameOf(arg) match {
        case Some(nm) if allowed(nm)            => Smaller
        case Some(nm) if nm == target.paramName => Equal
        case _                                  => Other
      }

    private def recurAllowedByLexOrder(
        fnname: Bindable,
        target: RecurTarget,
        allowedPerTarget: NonEmptyList[Set[Bindable]],
        callArgsByTarget: NonEmptyList[TypedExpr[Declaration]],
        region: Region
    ): Res[Unit] =
      if (
        RecursionCheck.isLexicographicallySmaller(
          target,
          allowedPerTarget,
          callArgsByTarget
        )(classifyArg)
      )
        unitValid
      else {
        val targetParams = target.map(_.paramName)
        Validated.invalidNec(
          RecursionCheck.RecursionNotLexicographic(fnname, targetParams, region)
        )
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
    type ErrorOr[A] = Either[NonEmptyChain[RecursionCheck.Error], A]
    type St[A] = StateT[ErrorOr, State, A]

    implicit val parallelSt: cats.Parallel[St] = {
      val m = cats.Monad[St]

      new ParallelViaProduct[St] {
        def monad = m
        def parallelProduct[A, B](fa: St[A], fb: St[B]) = {
          type E[+X] = ErrorOr[X]
          val fna: E[State => E[(State, A)]] = fa.runF
          val fnb: E[State => E[(State, B)]] = fb.runF

          new cats.data.IndexedStateT((fna, fnb).parMapN {
            (fn1, fn2) => (state: State) =>
              fn1(state) match {
                case Right((s2, a)) =>
                  fn2(s2).map { case (st, b) => (st, (a, b)) }
                case Left(nel1) =>
                  // just skip and merge
                  fn2(state) match {
                    case Right(_)   => Left(nel1)
                    case Left(nel2) => Left(nel1 ++ nel2)
                  }
              }
          })
        }
      }
    }

    // Scala has trouble inferring types like St, so we make these typed
    // helper functions to use below.
    private def failSt[A](err: RecursionCheck.Error): St[A] =
      StateT.liftF(Left(NonEmptyChain.one(err)))
    private val getSt: St[State] = StateT.get
    private def setSt(s: State): St[Unit] = StateT.set(s)
    private def toSt[A](v: Res[A]): St[A] =
      StateT.liftF(v.toEither)
    private def pureSt[A](a: A): St[A] = StateT.pure(a)
    private val unitSt: St[Unit] = pureSt(())

    private def checkForIllegalBindsSt(
        bs: Iterable[Bindable],
        region: Region
    ): St[Unit] =
      for {
        state <- getSt
        _ <- toSt(checkForIllegalBinds(state, bs, region)(unitValid))
        _ <- (state match {
          case id @ InDef(_, _, _, _, _, _) => setSt(bs.foldLeft(id)(_.addLocal(_)))
          case _                             => unitSt
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
        case start @ InRecurBranch(inrec, branch, allowed, names) =>
          val (allowed1, names1) = update(allowed, names)
          (
            setSt(InRecurBranch(inrec, branch, allowed1, names1)) *> in,
            getSt
          )
            .flatMapN {
              case (a, InRecurBranch(ir1, b1, _, _)) =>
                setSt(InRecurBranch(ir1, b1, allowed, names)).as(a)
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

    private def checkApply(
        currentPackage: PackageName,
        fn: TypedExpr[Declaration],
        args: NonEmptyList[TypedExpr[Declaration]],
        region: Region,
        wrappers: WrapperScope
    ): St[Unit] =
      getSt.flatMap {
        case TopLevel =>
          // without any recursion, normal typechecking will detect bad states:
          checkExpr(currentPackage, fn, wrappers) *> args.parTraverse_(
            checkExpr(currentPackage, _, wrappers)
          )
        case irb @ InRecurBranch(inrec, _, allowedPerTarget, names) =>
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
                toSt(
                  targetArgsV.andThen(
                    recurAllowedByLexOrder(
                      irb.defname,
                      inrec.target,
                      allowedPerTarget,
                      _,
                      region
                    )
                  )
                ) *>
                  setSt(irb.incRecCount) *> allArgs.parTraverse_(
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

    private def checkExpr(
        currentPackage: PackageName,
        expr: TypedExpr[Declaration],
        wrappers: WrapperScope
    ): St[Unit] =
      expr match {
        case TypedExpr.Generic(q, in) =>
          checkExpr(currentPackage, in, wrappers.pushQuant(q))
        case TypedExpr.Annotation(term, _, _) =>
          checkExpr(currentPackage, term, wrappers.pushAnnotation)
        case TypedExpr.AnnotatedLambda(args, body, _) =>
          val newBinds = args.toList.map(_._1)
          checkForIllegalBindsSt(newBinds, expr.tag.region) *>
            filterNames(newBinds)(checkExpr(currentPackage, body, wrappers))
        case TypedExpr.Local(v, _, _) =>
          getSt.flatMap {
            case TopLevel =>
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
                case Declaration.DefFn(defstmt) => Some(defstmt.args)
                case _                          => None
              }
            getSt.flatMap { state =>
              val defn = toSt(checkDef(currentPackage, state, arg, ex, fromSource))
              val nextRes = checkExpr(currentPackage, in, wrappers)
              defn *> nextRes
            }
          } else {
            checkForIllegalBindsSt(arg :: Nil, tag.region) *>
              checkExpr(currentPackage, ex, wrappers) *>
              filterNames(arg :: Nil)(checkExpr(currentPackage, in, wrappers))
          }
        case TypedExpr.Loop(loopArgs, body, _) =>
          val newBinds = loopArgs.toList.map(_._1)
          val checkArgs = loopArgs.parTraverse_ { case (_, argExpr) =>
            checkExpr(currentPackage, argExpr, wrappers)
          }
          checkForIllegalBindsSt(newBinds, expr.tag.region) *>
            checkArgs *>
            filterNames(newBinds)(checkExpr(currentPackage, body, wrappers))
        case TypedExpr.Recur(args, _, _) =>
          args.parTraverse_(checkExpr(currentPackage, _, wrappers))
        case TypedExpr.Literal(_, _, _) =>
          unitSt
        case TypedExpr.Match(arg, branches, tag) =>
          recurTag(currentPackage, arg, tag) match {
            case None =>
              // the arg can't use state, but cases introduce new bindings:
              val argRes = checkExpr(currentPackage, arg, wrappers)
              val optRes = branches.parTraverse_ { branch =>
                checkForIllegalBindsSt(branch.pattern.names, tag.region) *>
                  filterNames(branch.pattern.names) {
                    branch.guard.parTraverse_(checkExpr(currentPackage, _, wrappers)) *>
                      checkExpr(currentPackage, branch.expr, wrappers)
                  }
              }
              argRes *> optRes
            case Some(recur) =>
              // this is a state change
              getSt.flatMap {
                case TopLevel | InRecurBranch(_, _, _, _) |
                    InDefRecurred(_, _, _, _, _) =>
                  failSt(RecursionCheck.UnexpectedRecur(recur.region))
                case InDef(_, defname, _, sourceArgs, _, locals) =>
                  toSt(getRecurTarget(defname, sourceArgs, recur, locals)).flatMap {
                    target =>
                      val sourcePatterns = recur.cases.get.toList.map(_.pattern)

                      // on all these branches, use the same parent state
                      def beginBranch(
                          sourcePat: Option[Pattern.Parsed],
                          compiledPat: Pattern[(PackageName, Identifier.Constructor), Type]
                      ): St[Unit] =
                        getSt.flatMap {
                          case ir @ InDef(_, _, _, _, _, _) =>
                            val rec = ir.setRecur(target, recur)
                            setSt(rec) *> beginBranch(sourcePat, compiledPat)
                          case irr @ InDefRecurred(_, _, _, _, _) =>
                            val allowed =
                              sourcePat match {
                                case Some(sp) =>
                                  allowedByTargetFromParsed(irr.target, sp)
                                case None     =>
                                  allowedByTargetFromCompiled(irr.target, compiledPat)
                              }
                            val reachable = allowed.iterator.flatMap(_.iterator).toSet
                            setSt(
                              InRecurBranch(irr, compiledPat, allowed, reachable)
                            )
                          case illegal =>
                            // $COVERAGE-OFF$ this should be unreachable
                            sys.error(s"unreachable: $compiledPat -> $illegal")
                          // $COVERAGE-ON$
                        }

                      val endBranch: St[Unit] =
                        getSt.flatMap {
                          case InRecurBranch(irr, _, _, _) => setSt(irr)
                          case illegal                  =>
                            // $COVERAGE-OFF$ this should be unreachable
                            sys.error(s"unreachable end state: $illegal")
                          // $COVERAGE-ON$
                        }

                      branches.toList.zipWithIndex.parTraverse_ {
                        case (branch, idx) =>
                          for {
                            _ <- checkForIllegalBindsSt(branch.pattern.names, tag.region)
                            _ <- beginBranch(sourcePatterns.lift(idx), branch.pattern)
                            _ <- branch.guard.parTraverse_(
                              checkExpr(currentPackage, _, wrappers)
                            )
                            _ <- checkExpr(currentPackage, branch.expr, wrappers)
                            _ <- endBranch
                          } yield ()
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

    /*
     * Binds are not allowed to be recursive, only defs, so here we just make sure
     * none of the free variables of the pattern are used in expr.
     */
    private def checkDef(
        currentPackage: PackageName,
        state: State,
        fnname: Bindable,
        expr: TypedExpr[Declaration],
        sourceArgPatterns: Option[NonEmptyList[NonEmptyList[Pattern.Parsed]]]
    ): Res[Unit] =
      collectArgGroupsAndBody(expr) match {
        case None =>
          checkExprV(currentPackage, state, expr)
        case Some((typedArgs, body)) =>
          val sourceArgs = sourceArgsForDef(sourceArgPatterns, typedArgs)
          val nameArgs = sourceArgs.toList.flatMap(_.patternNames)
          val state1 = state.inDef(fnname, typedArgs, sourceArgs, expr.getType)
          checkForIllegalBinds(state, fnname :: nameArgs, body.tag.region) {
            val st = setSt(state1) *> checkExpr(
              currentPackage,
              body,
              WrapperScope.Empty
            ) *> (
              getSt.flatMap {
                case InDef(_, _, _, _, _, _) =>
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
        sourceArgs: Option[NonEmptyList[NonEmptyList[Pattern.Parsed]]]
    ): Res[Unit] = {
      val shouldCheckAsDef = sourceArgs.nonEmpty || rec.isRecursive
      if (shouldCheckAsDef)
        checkDef(currentPackage, TopLevel, name, expr, sourceArgs)
      else checkExprV(currentPackage, TopLevel, expr)
    }
  }
}
