package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel, StateT}

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
object DefRecursionCheck {

  type Res = ValidatedNel[RecursionError, Unit]

  sealed abstract class RecursionError {
    def region: Region
    def message: String
  }
  case class InvalidRecursion(name: Bindable, illegalPosition: Region)
      extends RecursionError {
    def region = illegalPosition
    def message = s"invalid recursion on ${name.sourceCodeRepr}"
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
      args: NonEmptyList[Pattern.Parsed]
  ) extends RecursionError {
    def region = decl.region
    def message = {
      val argStr = args.iterator
        .map { pat => Pattern.document[TypeRef].document(pat).render(80) }
        .mkString(", ")
      s"recur not on an argument to the def of ${fnname.sourceCodeRepr}, args: $argStr"
    }
  }
  case class RecursionArgNotVar(fnname: Bindable, invalidArg: Declaration)
      extends RecursionError {
    def region = invalidArg.region
    def message =
      s"recursion in ${fnname.sourceCodeRepr} is not on a name (expect a name which is exactly a arg to the def)"
  }
  case class RecursionNotSubstructural(
      fnname: Bindable,
      recurPat: Pattern.Parsed,
      arg: Declaration.Var
  ) extends RecursionError {
    def region = arg.region
    def message = s"recursion is ${fnname.sourceCodeRepr} not substructual"
  }
  case class RecursiveDefNoRecur(
      defstmt: DefStatement[Pattern.Parsed, Declaration],
      recur: Declaration.Match
  ) extends RecursionError {
    def region = recur.region
    def message =
      s"recur but no recursive call to ${defstmt.name.sourceCodeRepr}"
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
          case ExternalDef(_, _, _) =>
            unitValid
        }
      case _ => unitValid
    }
  }

  private object Impl {
    val unitValid: Res = Validated.valid(())

    /*
     * While checking a def we have three states we can be in:
     * 1. we are in a normal def, but have 0 or more outer recursive defs to avoid
     * 2. we are in a recursive def, but have not yet found the recur match.
     * 3. we are checking the branches of the recur match
     */
    sealed abstract class State {
      final def outerDefNames: List[Bindable] =
        this match {
          case TopLevel                   => Nil
          case InDef(outer, n, _, _)      => n :: outer.outerDefNames
          case InDefRecurred(id, _, _, _) => id.outerDefNames
          case InRecurBranch(ir, _)       => ir.outerDefNames
        }

      final def defNamesContain(n: Bindable): Boolean =
        this match {
          case TopLevel               => false
          case InDef(outer, dn, _, _) => (dn == n) || outer.defNamesContain(n)
          case InDefRecurred(id, _, _, _) => id.defNamesContain(n)
          case InRecurBranch(ir, _)       => ir.defNamesContain(n)
        }

      def inDef(fnname: Bindable, args: NonEmptyList[Pattern.Parsed]): InDef =
        InDef(this, fnname, args, Set.empty)
    }
    sealed abstract class InDefState extends State {
      final def defname: Bindable =
        this match {
          case InDef(_, defname, _, _)                      => defname
          case InDefRecurred(ir, _, _, _)                   => ir.defname
          case InRecurBranch(InDefRecurred(ir, _, _, _), _) => ir.defname
        }
    }
    case object TopLevel extends State
    case class InDef(
        outer: State,
        fnname: Bindable,
        args: NonEmptyList[Pattern.Parsed],
        localScope: Set[Bindable]
    ) extends InDefState {

      def addLocal(b: Bindable): InDef =
        InDef(outer, fnname, args, localScope + b)

      def setRecur(index: Int, m: Declaration.Match): InDefRecurred =
        InDefRecurred(this, index, m, 0)
    }
    case class InDefRecurred(
        inRec: InDef,
        index: Int,
        recur: Declaration.Match,
        recCount: Int
    ) extends InDefState {
      def incRecCount: InDefRecurred = copy(recCount = recCount + 1)
    }
    case class InRecurBranch(inRec: InDefRecurred, branch: Pattern.Parsed)
        extends InDefState {
      def incRecCount: InRecurBranch = copy(inRec = inRec.incRecCount)
    }

    /*
     * What is the index into the list of def arguments where we are doing our recursion
     */
    def getRecurIndex(
        fnname: Bindable,
        args: NonEmptyList[Pattern.Parsed],
        m: Declaration.Match,
        locals: Set[Bindable]
    ): ValidatedNel[RecursionError, Int] = {
      import Declaration._
      m.arg match {
        case Var(v) =>
          v match {
            case b: Bindable if locals(b) =>
              Validated.invalidNel(RecurNotOnArg(m, fnname, args))
            case _ =>
              val idx = args.toList.indexWhere { p => p.topNames.contains(v) }
              if (idx < 0) Validated.invalidNel(RecurNotOnArg(m, fnname, args))
              else Validated.valid(idx)
          }
        case _ =>
          Validated.invalidNel(RecurNotOnArg(m, fnname, args))
      }
    }

    /*
     * Check that decl is a strict substructure of pat. We do this by making sure decl is a Var
     * and that var is one of the strict substrutures of the pattern.
     */
    def strictSubstructure(
        fnname: Bindable,
        pat: Pattern.Parsed,
        decl: Declaration
    ): Res =
      decl match {
        case v @ Declaration.Var(nm) =>
          if (pat.substructures.contains(nm)) unitValid
          else Validated.invalidNel(RecursionNotSubstructural(fnname, pat, v))
        case _ =>
          // we can only recur with vars
          Validated.invalidNel(RecursionArgNotVar(fnname, decl))
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
    )(next: ValidatedNel[RecursionError, A]): ValidatedNel[RecursionError, A] =
      state.outerDefNames match {
        case Nil => next
        case nonEmpty =>
          NonEmptyList.fromList(bs.filter(nonEmpty.toSet).toList.sorted) match {
            case Some(nel) =>
              Validated.invalid(nel.map(IllegalShadow(_, decl)))
            case None =>
              next
          }
      }

    /*
     * Unfortunately we lose the Applicative structure inside Declaration checking.
     * This is because the state changes are not nested: if we see a recur on one
     * variable, we cannot later recur on a different one. During checkDecl, we switch
     * to a sequential (Monadic) State tracking, and can only accumulate errors
     * until we hit the first one
     */
    type St[A] = StateT[Either[NonEmptyList[RecursionError], *], State, A]

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

    def checkApply(
        nm: Bindable,
        args: NonEmptyList[Declaration],
        region: Region
    ): St[Unit] =
      getSt.flatMap {
        case TopLevel =>
          // without any recursion, normal typechecking will detect bad states:
          args.traverse_(checkDecl)
        case irb @ InRecurBranch(inrec, branch) =>
          val idx = inrec.index
          // here we are calling our recursive function
          // make sure we do so on a substructural match
          if (nm == irb.defname) {
            args.get(idx.toLong) match {
              case None =>
                // not enough args to check recursion
                failSt(InvalidRecursion(nm, region))
              case Some(arg) =>
                toSt(strictSubstructure(irb.defname, branch, arg)) *>
                  setSt(irb.incRecCount) // we have recurred again
            }
          } else if (irb.defNamesContain(nm)) {
            failSt(InvalidRecursion(nm, region))
          } else {
            // not a recursive call
            args.traverse_(checkDecl)
          }
        case ir: InDefState =>
          // we have either not yet, or already done the recursion
          if (ir.defNamesContain(nm)) failSt(InvalidRecursion(nm, region))
          else args.traverse_(checkDecl)
      }
    /*
     * With the given state, check the given Declaration to see if
     * we have valid recursion
     */
    def checkDecl(decl: Declaration): St[Unit] = {
      import Declaration._
      decl match {
        case Annotation(t, _) => checkDecl(t)
        case Apply(Var(nm: Bindable), args, _) =>
          checkApply(nm, args, decl.region)
        case Apply(fn, args, _) =>
          checkDecl(fn) *> args.traverse_(checkDecl)
        case ApplyOp(left, op, right) =>
          checkApply(op, NonEmptyList(left, right :: Nil), decl.region)
        case Binding(BindingStatement(pat, thisDecl, next)) =>
          checkForIllegalBindsSt(pat.names, decl) *>
            checkDecl(thisDecl) *>
            checkDecl(next.padded)
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
          val ifs = ifCases.traverse_ { case (d, od) =>
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
          checkForIllegalBindsSt(args.patternNames, decl) *> checkDecl(body)
        case Literal(_) =>
          unitSt
        case Match(RecursionKind.NonRecursive, arg, cases) =>
          // the arg can't use state, but cases introduce new bindings:
          val argRes = checkDecl(arg)
          val optRes = cases.get.traverse_ { case (pat, next) =>
            checkForIllegalBindsSt(pat.names, decl) *>
              checkDecl(next.get)
          }
          argRes *> optRes
        case recur @ Match(RecursionKind.Recursive, _, cases) =>
          // this is a state change
          getSt.flatMap {
            case TopLevel | InRecurBranch(_, _) | InDefRecurred(_, _, _, _) =>
              failSt(UnexpectedRecur(recur))
            case InDef(_, defname, args, locals) =>
              toSt(getRecurIndex(defname, args, recur, locals)).flatMap { idx =>
                // on all these branchs, use the the same
                // parent state
                def beginBranch(pat: Pattern.Parsed): St[Unit] =
                  getSt.flatMap {
                    case ir @ InDef(_, _, _, _) =>
                      val rec = ir.setRecur(idx, recur)
                      setSt(rec) *> beginBranch(pat)
                    case irr @ InDefRecurred(_, _, _, _) =>
                      setSt(InRecurBranch(irr, pat))
                    case illegal =>
                      // $COVERAGE-OFF$ this should be unreachable
                      sys.error(s"unreachable: $pat -> $illegal")
                    // $COVERAGE-ON$
                  }

                val endBranch: St[Unit] =
                  getSt.flatMap {
                    case InRecurBranch(irr, _) => setSt(irr)
                    case illegal               =>
                      // $COVERAGE-OFF$ this should be unreachable
                      sys.error(s"unreachable end state: $illegal")
                    // $COVERAGE-ON$
                  }

                cases.get.traverse_ { case (pat, next) =>
                  for {
                    _ <- checkForIllegalBindsSt(pat.names, decl)
                    _ <- beginBranch(pat)
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
          tups.traverse_(checkDecl)
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
          parts.traverse_ {
            case Left(nb) => checkDecl(nb)
            case Right(_) => unitSt
          }
        case ListDecl(ll) =>
          ll match {
            case ListLang.Cons(items) =>
              items.traverse_ { s => checkDecl(s.value) }
            case ListLang.Comprehension(e, _, i, f) =>
              checkDecl(e.value) *>
                checkDecl(i) *>
                (f.traverse_(checkDecl))
          }
        case DictDecl(ll) =>
          ll match {
            case ListLang.Cons(items) =>
              items.traverse_ { s => checkDecl(s.key) *> checkDecl(s.value) }
            case ListLang.Comprehension(e, _, i, f) =>
              checkDecl(e.key) *>
                checkDecl(e.value) *>
                checkDecl(i) *>
                (f.traverse_(checkDecl))
          }

        case RecordConstructor(_, args) =>
          def checkArg(arg: RecordArg): St[Unit] =
            arg match {
              case RecordArg.Simple(b) =>
                checkDecl(Var(b)(decl.region))
              case RecordArg.Pair(_, v) =>
                checkDecl(v)
            }
          args.traverse_(checkArg)
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
      val nameArgs = defstmt.args.patternNames
      val state1 = state.inDef(defstmt.name, defstmt.args)
      checkForIllegalBinds(state, defstmt.name :: nameArgs, body) {
        val st = setSt(state1) *> checkDecl(body) *> (getSt.flatMap {
          case InDef(_, _, _, _) =>
            // we never hit a recur
            unitSt
          case InDefRecurred(_, _, _, cnt) if cnt > 0 =>
            // we did hit a recur
            unitSt
          case InDefRecurred(_, _, recur, 0) =>
            // we hit a recur, but we didn't recurse
            failSt[Unit](
              RecursiveDefNoRecur(
                defstmt.copy(result = defstmt.result._1.get),
                recur
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
