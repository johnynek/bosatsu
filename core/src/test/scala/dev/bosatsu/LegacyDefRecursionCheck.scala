package dev.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel, StateT}
import org.typelevel.paiges.Doc

import cats.implicits._

import Identifier.Bindable

/** Recursion in bosatsu is only allowed on a substructural match of one of the
  * parameters to the def. This strict rule, along with strictly finite data,
  * ensures that all recursion terminates
  *
  * The rules are as follows: 0. defs may not be shadowed. This makes checking
  * for legal recursion easier
  *   1. until we reach a recur match, we cannot access an outer def name. We
  *      want to avoid aliasing 2. a recur match must occur on one of the
  *      literal parameters to the def, and there can be only one recur match 3.
  *      inside each branch of the recur match, we may only recur on
  *      substructures in the match position. 4. if there is a recur match,
  *      there must be at least one real recursion
  */
object LegacyDefRecursionCheck {

  type Res = ValidatedNel[RecursionError, Unit]

  sealed abstract class RecursionError {
    def region: Region
    def message: String
  }
  case class InvalidRecursion(name: Bindable, illegalPosition: Region)
      extends RecursionError {
    def region = illegalPosition
    def message =
      s"invalid recursion on ${name.sourceCodeRepr}. Consider replacing `match` with `recur`."
  }

  case class NotEnoughRecurArgs(name: Bindable, illegalPosition: Region)
      extends RecursionError {
    def region = illegalPosition
    def message =
      s"not enough args to ${name.sourceCodeRepr} to check recursion safety."
  }
  case class IllegalShadow(fnname: Bindable, decl: Declaration)
      extends RecursionError {
    def region = decl.region
    def message =
      s"illegal shadowing on: ${fnname.sourceCodeRepr}. Recursive shadowing of def names disallowed"
  }
  case class UnexpectedRecur(decl: Declaration.Match) extends RecursionError {
    def region = decl.region
    def message = "unexpected recur: may only appear unnested inside a def"
  }
  case class RecurNotOnArg(
      decl: Declaration.Match,
      fnname: Bindable,
      args: NonEmptyList[NonEmptyList[Pattern.Parsed]]
  ) extends RecursionError {

    def region = decl.region
    def message = {
      val argsDoc =
        Doc.intercalate(
          Doc.empty,
          args.toList.map { group =>
            (Doc.char('(') +
              Doc.intercalate(
                Doc.comma + Doc.line,
                group.toList.map { pat =>
                  Pattern.document[TypeRef].document(pat)
                }
              ) +
              Doc.char(')')).grouped
          }
        )
      val argStr = argsDoc.render(80)
      s"recur not on an argument to the def of ${fnname.sourceCodeRepr}, args: $argStr"
    }
  }
  case class RecurTargetInvalid(fnname: Bindable, invalidArg: Declaration)
      extends RecursionError {
    def region = invalidArg.region
    def message =
      s"recur target for ${fnname.sourceCodeRepr} must be a name or tuple of names bound to def args"
  }
  case class RecurTargetDuplicate(
      fnname: Bindable,
      duplicated: Bindable,
      invalidArg: Declaration
  ) extends RecursionError {
    def region = invalidArg.region
    def message =
      s"recur target for ${fnname.sourceCodeRepr} contains duplicate parameter ${duplicated.sourceCodeRepr}"
  }
  case class RecursionNotLexicographic(
      fnname: Bindable,
      target: NonEmptyList[Bindable],
      illegalPosition: Region
  ) extends RecursionError {
    def region = illegalPosition
    def message = {
      val targetStr = target.toList.map(_.sourceCodeRepr).mkString(", ")
      s"recursive call to ${fnname.sourceCodeRepr} is not lexicographically smaller on recur target ($targetStr)."
    }
  }
  case class RecursiveDefNoRecur(
      defstmt: DefStatement[Pattern.Parsed, Declaration],
      recur: Declaration.Match,
      likelyRenamedCall: Option[(Bindable, Int)]
  ) extends RecursionError {
    def region = recur.region
    def message =
      likelyRenamedCall match {
        case Some((calledName, count)) =>
          s"Function name looks renamed: declared `${defstmt.name.sourceCodeRepr}`, but recursive calls use `${calledName.sourceCodeRepr}`.\nDid you mean `${defstmt.name.sourceCodeRepr}` in recursive calls? ($count occurrences)"
        case None =>
          s"recur but no recursive call to ${defstmt.name.sourceCodeRepr}"
      }
  }

  /** Check a statement that all inner declarations contain legal recursion, or
    * none at all. Note, we don't check for cases that will be caught by
    * typechecking: namely, when we have nonrecursive defs, their names are not
    * in scope during typechecking, so illegal recursion there simply won't
    * typecheck.
    */
  def checkStatement(s: Statement): Res = {
    import Statement._
    import Impl._
    s match {
      case vs: ValueStatement =>
        vs match {
          case Bind(BindingStatement(_, decl, _)) =>
            checkDeclV(TopLevel, decl)
          case Def(defn) =>
            // make this the same shape as a in declaration
            checkDef(TopLevel, defn.copy(result = (defn.result, ())))
          case ExternalDef(_, _, _, _) =>
            unitValid
        }
      case _ => unitValid
    }
  }

  private object Impl {
    val unitValid: Res = Validated.valid(())
    case class RecurTargetItem(group: Int, index: Int, paramName: Bindable)
        derives CanEqual
    type RecurTarget = NonEmptyList[RecurTargetItem]

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
            val InDef(outer, n, _, _) = ids.inDef
            outer.outerDefNames + n
        }

      @annotation.tailrec
      final def defNamesContain(n: Bindable): Boolean =
        this match {
          case TopLevel        => false
          case ids: InDefState =>
            val InDef(outer, dn, _, _) = ids.inDef
            (dn == n) || outer.defNamesContain(n)
        }

      def inDef(
          fnname: Bindable,
          args: NonEmptyList[NonEmptyList[Pattern.Parsed]]
      ): InDef =
        InDef(this, fnname, args, Set.empty)
    }
    sealed abstract class InDefState extends State {
      final def inDef: InDef =
        this match {
          case id @ InDef(_, _, _, _)                    => id
          case InDefRecurred(ir, _, _, _, _)             => ir.inDef
          case InRecurBranch(InDefRecurred(ir, _, _, _, _), _, _, _) =>
            ir.inDef
        }

      final def defname: Bindable = inDef.fnname
    }

    case object TopLevel extends State
    case class InDef(
        outer: State,
        fnname: Bindable,
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        localScope: Set[Bindable]
    ) extends InDefState {

      def addLocal(b: Bindable): InDef =
        InDef(outer, fnname, args, localScope + b)

      def setRecur(target: RecurTarget, m: Declaration.Match): InDefRecurred =
        InDefRecurred(this, target, m, 0, Map.empty)

      // This is eta-expansion of the function name as a lambda so we can check using the lambda rule
      def asLambda(region: Region): Declaration.Lambda = {
        val allNames = Iterator
          .iterate(0)(_ + 1)
          .map(idx => Identifier.Name(s"a$idx"))
          .filterNot(n => (n: Bindable) == fnname)

        val func = cats.Functor[NonEmptyList].compose[NonEmptyList]
        // we allocate the names first. There is only one name inside: fnname
        val argsB = func.map(args)(_ => allNames.next())

        val argsV: NonEmptyList[NonEmptyList[Declaration.NonBinding]] =
          func.map(argsB)(n => Declaration.Var(n)(using region))

        val argsP: NonEmptyList[NonEmptyList[Pattern.Parsed]] =
          func.map(argsB)(n => Pattern.Var(n))

        // fn == (x, y) -> z -> f(x, y)(z)
        val body = argsV.toList.foldLeft(
          Declaration.Var(fnname)(using region): Declaration.NonBinding
        ) { (called, group) =>
          Declaration.Apply(called, group, Declaration.ApplyKind.Parens)(using
            region
          )
        }

        def lambdify(
            args: NonEmptyList[NonEmptyList[Pattern.Parsed]],
            body: Declaration
        ): Declaration.Lambda = {
          val body1 = args.tail match {
            case Nil       => body
            case h :: tail => lambdify(NonEmptyList(h, tail), body)
          }
          Declaration.Lambda(args.head, body1)(using region)
        }

        lambdify(argsP, body)
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
        branch: Pattern.Parsed,
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
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        recur: Declaration.Match,
        locals: Set[Bindable],
        targetItemsByName: Map[Bindable, RecurTargetItem],
        d: Declaration
    ): ValidatedNel[RecursionError, RecurTargetItem] =
      d match {
        case Declaration.Var(b: Bindable) if locals(b) =>
          Validated.invalidNel(RecurNotOnArg(recur, fnname, args))
        case Declaration.Var(b: Bindable) =>
          targetItemsByName
            .get(b)
            .toValidNel(RecurNotOnArg(recur, fnname, args))
        case Declaration.Var(_) =>
          Validated.invalidNel(RecurNotOnArg(recur, fnname, args))
        case _ =>
          Validated.invalidNel(RecurTargetInvalid(fnname, d))
      }

    /*
     * What are the indices into the list of def arguments where we are doing recursion.
     */
    def getRecurTarget(
        fnname: Bindable,
        args: NonEmptyList[NonEmptyList[Pattern.Parsed]],
        m: Declaration.Match,
        locals: Set[Bindable]
    ): ValidatedNel[RecursionError, RecurTarget] = {
      import Declaration._
      val targetItemsByName = getRecurTargetItemsByName(args)

      def checkDuplicates(
          target: RecurTarget,
          sourceItems: NonEmptyList[Declaration]
      ): ValidatedNel[RecursionError, RecurTarget] = {
        val (_, errorsRev) =
          target.iterator
            .zip(sourceItems.iterator)
            .foldLeft((Set.empty[Bindable], List.empty[RecursionError])) {
              case ((seen, errs), (item, sourceDecl)) =>
                if (seen(item.paramName))
                  (
                    seen,
                    RecurTargetDuplicate(fnname, item.paramName, sourceDecl) :: errs
                  )
                else (seen + item.paramName, errs)
            }

        NonEmptyList.fromList(errorsRev.reverse) match {
          case Some(errs) => Validated.invalid(errs)
          case None       => Validated.valid(target)
        }
      }

      m.arg match {
        case v @ Var(_) =>
          resolveRecurTargetItem(
            fnname,
            args,
            m,
            locals,
            targetItemsByName,
            v
          ).map(NonEmptyList.one)
        case TupleCons(Nil) =>
          Validated.invalidNel(RecurTargetInvalid(fnname, m.arg))
        case TupleCons(h :: tail) =>
          val sourceItems = NonEmptyList(h, tail)
          sourceItems
            .traverse(
              resolveRecurTargetItem(
                fnname,
                args,
                m,
                locals,
                targetItemsByName,
                _
              )
            )
            .andThen(checkDuplicates(_, sourceItems))
        case _ =>
          Validated.invalidNel(RecurTargetInvalid(fnname, m.arg))
      }
    }

    private enum ArgLexOrder derives CanEqual {
      case Equal
      case Smaller
      case Other
    }
    import ArgLexOrder.*

    private def classifyArg(
        target: RecurTargetItem,
        allowed: Set[Bindable],
        arg: Declaration
    ): ArgLexOrder =
      arg match {
        case Declaration.Var(nm: Bindable) if allowed(nm)            => Smaller
        case Declaration.Var(nm: Bindable) if nm == target.paramName => Equal
        case _                                                       => Other
      }

    private def isLexicographicallySmaller(
        target: RecurTarget,
        allowedPerTarget: NonEmptyList[Set[Bindable]],
        callArgsByTarget: NonEmptyList[Declaration]
    ): Boolean = {
      val stepIter = target.iterator
        .zip(allowedPerTarget.iterator)
        .zip(callArgsByTarget.iterator)

      while (stepIter.hasNext) {
        val ((targetItem, allowed), arg) = stepIter.next()
        classifyArg(targetItem, allowed, arg) match {
          case Smaller => return true
          case Equal   => ()
          case Other   => return false
        }
      }

      false
    }

    private def recurAllowedByLexOrder(
        fnname: Bindable,
        target: RecurTarget,
        allowedPerTarget: NonEmptyList[Set[Bindable]],
        callArgsByTarget: NonEmptyList[Declaration],
        region: Region
    ): Res =
      if (isLexicographicallySmaller(target, allowedPerTarget, callArgsByTarget))
        unitValid
      else {
        val targetParams = target.map(_.paramName)
        Validated.invalidNel(RecursionNotLexicographic(fnname, targetParams, region))
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

    private def allowedByTarget(
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

    /*
     * We disallow shadowing recursive defs. This code checks that we have not done so as we
     * introduce new bindings.
     *
     * We disallow such shadowing currently only in this code. We do it to make it easier
     * for the algorithm here, but also for human readers to see that recursion is total
     */
    def checkForIllegalBinds[A](
        state: State,
        bs: Iterable[Bindable],
        decl: Declaration
    )(
        next: ValidatedNel[RecursionError, A]
    ): ValidatedNel[RecursionError, A] = {
      val outerSet = state.outerDefNames
      if (outerSet.isEmpty) next
      else {
        NonEmptyList.fromList(
          bs.iterator.filter(outerSet).toList.sorted
        ) match {
          case Some(nel) =>
            Validated.invalid(nel.map(IllegalShadow(_, decl)))
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
     * until we hit the first one
     */
    type ErrorOr[A] = Either[NonEmptyList[RecursionError], A]
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
                    case Left(nel2) => Left(nel1 ::: nel2)
                  }
              }
          })
        }
      }
    }

    // Scala has trouble infering types like St, we we make these typed
    // helper functions to use below
    def failSt[A](err: RecursionError): St[A] =
      StateT.liftF(Left(NonEmptyList.of(err)))
    val getSt: St[State] = StateT.get
    def setSt(s: State): St[Unit] = StateT.set(s)
    def toSt[A](v: ValidatedNel[RecursionError, A]): St[A] =
      StateT.liftF(v.toEither)
    def pureSt[A](a: A): St[A] = StateT.pure(a)
    val unitSt: St[Unit] = pureSt(())

    def checkForIllegalBindsSt[A](
        bs: Iterable[Bindable],
        decl: Declaration
    ): St[Unit] =
      for {
        state <- getSt
        _ <- toSt(checkForIllegalBinds(state, bs, decl)(unitValid))
        _ <- (state match {
          case id @ InDef(_, _, _, _) => setSt(bs.foldLeft(id)(_.addLocal(_)))
          case _                      => unitSt
        })
      } yield ()

    private def argsOnDefName(
        fn: Declaration,
        groups: NonEmptyList[NonEmptyList[Declaration]]
    ): Option[(Bindable, NonEmptyList[NonEmptyList[Declaration]])] =
      fn match {
        case Declaration.Var(nm: Bindable)   => Some((nm, groups))
        case Declaration.Apply(fn1, args, _) =>
          argsOnDefName(fn1, args :: groups)
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

    def checkApply(
        fn: Declaration,
        args: NonEmptyList[Declaration],
        region: Region
    ): St[Unit] =
      getSt.flatMap {
        case TopLevel =>
          // without any recursion, normal typechecking will detect bad states:
          checkDecl(fn) *> args.parTraverse_(checkDecl)
        case irb @ InRecurBranch(inrec, _, allowedPerTarget, names) =>
          argsOnDefName(fn, NonEmptyList.one(args)) match {
            case Some((nm, groups)) =>
              if (nm == irb.defname) {
                val targetArgsV: ValidatedNel[RecursionError, NonEmptyList[
                  Declaration
                ]] =
                  inrec.target.traverse { targetItem =>
                    groups
                      .get(targetItem.group.toLong)
                      .flatMap(_.get(targetItem.index.toLong))
                      .toValidNel(NotEnoughRecurArgs(nm, region))
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
                  setSt(irb.incRecCount) *> allArgs.parTraverse_(checkDecl)
              } else if (irb.defNamesContain(nm)) {
                failSt(InvalidRecursion(nm, region))
              } else if (names.contains(nm)) {
                // we are calling a reachable function. Any lambda args are new names:
                args.parTraverse_[St, Unit] {
                  case Declaration.Lambda(args, body) =>
                    val names1 = args.toList.flatMap(_.names)
                    unionNames(names1)(checkDecl(body))
                  case v @ Declaration.Var(fn: Bindable) if irb.defname == fn =>
                    val Declaration.Lambda(args, body) =
                      irb.inDef.asLambda(v.region)
                    val names1 = args.toList.flatMap(_.names)
                    unionNames(names1)(checkDecl(body))
                  case notLambda => checkDecl(notLambda)
                }
              } else {
                // traverse converting Var(name) to the lambda version to use the above check
                // not a recursive call
                setSt(irb.noteCalledName(nm)) *> args.parTraverse_(checkDecl)
              }
            case None =>
              // this isn't a recursive call
              checkDecl(fn) *> args.parTraverse_(checkDecl)
          }
        case ir: InDefState =>
          // we have either not yet, or already done the recursion
          argsOnDefName(fn, NonEmptyList.one(args)) match {
            case Some((nm, _)) if ir.defNamesContain(nm) =>
              failSt(InvalidRecursion(nm, region))
            case _ =>
              checkDecl(fn) *> args.parTraverse_(checkDecl)
          }
      }
    /*
     * With the given state, check the given Declaration to see if
     * we have valid recursion
     */
    def checkDecl(decl: Declaration): St[Unit] = {
      import Declaration._
      decl match {
        case Annotation(t, _)   => checkDecl(t)
        case Apply(fn, args, _) =>
          checkApply(fn, args, decl.region)
        case ApplyOp(left, op, right) =>
          checkApply(
            Var(op)(using decl.region),
            NonEmptyList(left, right :: Nil),
            decl.region
          )
        case Binding(BindingStatement(pat, thisDecl, next)) =>
          checkForIllegalBindsSt(pat.names, decl) *>
            checkDecl(thisDecl) *>
            filterNames(pat.names)(checkDecl(next.padded))
        case Comment(cs) =>
          checkDecl(cs.on.padded)
        case CommentNB(cs) =>
          checkDecl(cs.on.padded)
        case DefFn(defstmt) =>
          // we can use the name of the def after we have defined it, which is the next part
          getSt.flatMap { state =>
            val defn = toSt(checkDef(state, defstmt))
            val nextRes = checkDecl(defstmt.result._2.padded)
            defn *> nextRes
          }
        case IfElse(ifCases, elseCase) =>
          val ifs = ifCases.parTraverse_ { case (d, od) =>
            checkDecl(d) *> checkDecl(od.get)
          }
          val e = checkDecl(elseCase.get)
          ifs *> e
        case la @ LeftApply(_, _, _, _) =>
          checkDecl(la.rewrite)
        case Ternary(t, c, f) =>
          checkDecl(t) *> checkDecl(c) *> checkDecl(f)
        case Lambda(args, body) =>
          // these args create new bindings:
          val newBinds = args.patternNames
          checkForIllegalBindsSt(newBinds, decl) *>
            filterNames(newBinds)(checkDecl(body))
        case Literal(_) =>
          unitSt
        case Match(RecursionKind.NonRecursive, arg, cases) =>
          // the arg can't use state, but cases introduce new bindings:
          val argRes = checkDecl(arg)
          val optRes = cases.get.parTraverse_ {
            case MatchBranch(pat, guard, next) =>
              checkForIllegalBindsSt(pat.names, decl) *>
                filterNames(pat.names) {
                  guard.parTraverse_(checkDecl) *> checkDecl(next.get)
                }
          }
          argRes *> optRes
        case recur @ Match(RecursionKind.Recursive, _, cases) =>
          // this is a state change
          getSt.flatMap {
            case TopLevel | InRecurBranch(_, _, _, _) |
                InDefRecurred(_, _, _, _, _) =>
              failSt(UnexpectedRecur(recur))
            case InDef(_, defname, args, locals) =>
              toSt(getRecurTarget(defname, args, recur, locals)).flatMap {
                target =>
                // on all these branchs, use the the same
                // parent state
                def beginBranch(pat: Pattern.Parsed): St[Unit] =
                  getSt.flatMap {
                    case ir @ InDef(_, _, _, _) =>
                      val rec = ir.setRecur(target, recur)
                      setSt(rec) *> beginBranch(pat)
                    case irr @ InDefRecurred(_, _, _, _, _) =>
                      val allowed = allowedByTarget(irr.target, pat)
                      val reachable = allowed.iterator.flatMap(_.iterator).toSet
                      setSt(
                        InRecurBranch(irr, pat, allowed, reachable)
                      )
                    case illegal =>
                      // $COVERAGE-OFF$ this should be unreachable
                      sys.error(s"unreachable: $pat -> $illegal")
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

                cases.get.parTraverse_ { case MatchBranch(pat, guard, next) =>
                  for {
                    _ <- checkForIllegalBindsSt(pat.names, decl)
                    _ <- beginBranch(pat)
                    _ <- guard.parTraverse_(checkDecl)
                    _ <- checkDecl(next.get)
                    _ <- endBranch
                  } yield ()
                }
              }
          }
        case Matches(a, _) =>
          // patterns don't use values
          checkDecl(a)
        case Parens(p) =>
          checkDecl(p)
        case TupleCons(tups) =>
          tups.parTraverse_(checkDecl)
        case Var(Identifier.Constructor(_)) =>
          unitSt
        case Var(v: Bindable) =>
          getSt.flatMap {
            case TopLevel =>
              // without any recursion, normal typechecking will detect bad states:
              unitSt
            case ir: InDefState =>
              // if this were an apply, it would have been handled by Apply(Var(...
              if (ir.defNamesContain(v))
                failSt(InvalidRecursion(v, decl.region))
              else unitSt
          }
        case StringDecl(parts) =>
          parts.parTraverse_ {
            case StringDecl.CharExpr(nb)  => checkDecl(nb)
            case StringDecl.StrExpr(nb)   => checkDecl(nb)
            case StringDecl.Literal(_, _) => unitSt
          }
        case ListDecl(ll) =>
          ll match {
            case ListLang.Cons(items) =>
              items.parTraverse_(s => checkDecl(s.value))
            case ListLang.Comprehension(e, _, i, f) =>
              checkDecl(e.value) *>
                checkDecl(i) *>
                (f.parTraverse_(checkDecl))
          }
        case DictDecl(ll) =>
          ll match {
            case ListLang.Cons(items) =>
              items.parTraverse_(s => checkDecl(s.key) *> checkDecl(s.value))
            case ListLang.Comprehension(e, _, i, f) =>
              checkDecl(e.key) *>
                checkDecl(e.value) *>
                checkDecl(i) *>
                (f.parTraverse_(checkDecl))
          }

        case RecordConstructor(_, args, updateFrom) =>
          def checkArg(arg: RecordArg): St[Unit] =
            arg match {
              case RecordArg.Simple(b) =>
                checkDecl(Var(b)(using decl.region))
              case RecordArg.Pair(_, v) =>
                checkDecl(v)
            }
          args.parTraverse_(checkArg) *> updateFrom.parTraverse_(checkDecl)
      }
    }

    def checkDeclV(state: State, decl: Declaration): Res =
      Validated.fromEither(checkDecl(decl).as(()).runA(state))

    /*
     * Binds are not allowed to be recursive, only defs, so here we just make sure
     * none of the free variables of the pattern are used in decl
     */
    def checkDef[A](
        state: State,
        defstmt: DefStatement[Pattern.Parsed, (OptIndent[Declaration], A)]
    ): Res = {
      val body = defstmt.result._1.get
      val nameArgs = defstmt.args.toList.flatMap(_.patternNames)
      val state1 = state.inDef(defstmt.name, defstmt.args)
      checkForIllegalBinds(state, defstmt.name :: nameArgs, body) {
        val st = setSt(state1) *> checkDecl(body) *> (getSt.flatMap {
          case InDef(_, _, _, _) =>
            // we never hit a recur
            unitSt
          case InDefRecurred(_, _, _, cnt, _) if cnt > 0 =>
            // we did hit a recur
            unitSt
          case InDefRecurred(_, _, recur, 0, calledNames) =>
            // we hit a recur, but we didn't recurse
            failSt[Unit](
              RecursiveDefNoRecur(
                defstmt.copy(result = defstmt.result._1.get),
                recur,
                likelyRenameCall(defstmt.name, calledNames)
              )
            )
          case unreachable =>
            // $COVERAGE-OFF$ this should be unreachable
            sys.error(
              s"we would like to prove in the types we can't get here: $unreachable, $defstmt"
            ): St[Unit]
          // $COVERAGE-ON$
        })
        // Note a def can't change the state
        // we either have a valid nested def, or we don't
        // but that can't change the state of the outer
        // def that is calling this. So, we don't
        // return the final state in this method
        Validated.fromEither(st.runA(state))
      }
    }
  }
}
