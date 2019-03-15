package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel, StateT}

import cats.implicits._

/**
 * Recursion in bosatsu is only allowed on a substructural match
 * of one of the parameters to the def. This strict rule, along
 * with strictly finite data, ensures that all recursion terminates
 *
 * The rules are as follows:
 * 0. recursive defs may not be shadowed. This makes checking for legal recursion easier
 * 1. until we reach a recur match, we cannot access a recursive name. We want to avoid aliasing
 * 2. a recur match must occur on one of the literal parameters to the recursive def, and there can
 *    be only one recur match
 * 3. inside each branch of the recur match, we may only recur on substructures in the match
 *    position.
 * 4. recursive defs may not be nested: you cannot define a new recursive def inside a recursive def
 */
object DefRecursionCheck {

  type Res = ValidatedNel[RecursionError, Unit]

  sealed abstract class RecursionError
  case class InvalidRecusion(name: String, illegalPosition: Region) extends RecursionError
  case class IllegalNesting(insideDefName: String, nextDef: String, region: Region) extends RecursionError
  case class IllegalShadow(fnname: String, decl: Declaration) extends RecursionError
  case class UnexpectedRecur(decl: Declaration.Match) extends RecursionError
  case class RecurNotOnArg(decl: Declaration.Match, fnname: String, args: List[String]) extends RecursionError
  case class RecursionArgNotVar(fnname: String, invalidArg: Declaration) extends RecursionError
  case class RecursionNotSubstructural(fnname: String, recurPat: Pattern[Option[String], TypeRef], arg: Declaration.Var) extends RecursionError
  case class RecursiveDefNoRecur(defstmt: DefStatement[Declaration]) extends RecursionError

  /**
   * Check a statement that all inner statements and declarations contain legal
   * recursion, or none at all. Note, we don't check for cases that will be caught
   * by typechecking: namely, when we have nonrecursive defs, their names are not
   * in scope during typechecking, so illegal recursion there simply won't typecheck.
   */
  def checkStatement(s: Statement): Res = {
    import Statement._
    import Impl._
    s match {
      case Bind(BindingStatement(pat, decl, rest)) =>
        val state = Default(None)
        checkDeclV(state, decl) *> checkStatement(rest.padded)
      case Comment(cs) =>
        checkStatement(cs.on.padded)
      case Def(defn) =>
        val state = Default(None)
        checkDef(state, defn) *> checkStatement(defn.result._2.padded)
      case Struct(_, _, rest) =>
        checkStatement(rest.padded)
      case ExternalDef(_, _, _, rest) =>
        checkStatement(rest.padded)
      case ExternalStruct(_, _, rest) =>
        checkStatement(rest.padded)
      case Enum(_, _, rest) =>
        checkStatement(rest.padded)
      case EndOfFile =>
        unitValid
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
      final def outerRecName: Option[String] =
        this match {
          case Default(None) => None
          case Default(Some(ir)) => Some(ir.defname)
          case s: RecState => Some(s.defname)
        }

      def normalDef: State =
        this match {
          case d@Default(_) => d
          case s: RecState => Default(Some(s))
        }
    }
    sealed abstract class RecState extends State {
      final def defname: String =
        this match {
          case InRecursive(defname, _) => defname
          case InRecursiveRecurred(ir, _, _, _) => ir.defname
          case InRecurBranch(InRecursiveRecurred(ir, _, _, _), _) => ir.defname
        }
    }
    case class Default(outerRec: Option[RecState]) extends State
    case class InRecursive(fnname: String, args: List[String]) extends RecState {

      def setRecur(index: Int, m: Declaration.Match): InRecursiveRecurred =
        InRecursiveRecurred(this, index, m, 0)
    }
    case class InRecursiveRecurred(inRec: InRecursive, index: Int, recur: Declaration.Match, recCount: Int) extends RecState {
      def incRecCount: InRecursiveRecurred = copy(recCount = recCount + 1)
    }
    case class InRecurBranch(inRec: InRecursiveRecurred, branch: Pattern[Option[String], TypeRef]) extends RecState {
      def incRecCount: InRecurBranch = copy(inRec = inRec.incRecCount)
    }

    /*
     * What is the index into the list of def arguments where we are doing our recursion
     */
    def getRecurIndex(
      fnname: String,
      args: List[String],
      m: Declaration.Match): ValidatedNel[RecursionError, Int] = {
      import Declaration._
      m.arg match {
        case Var(v) =>
          val idx = args.indexOf(v)
          if (idx < 0) Validated.invalidNel(RecurNotOnArg(m, fnname, args))
          else Validated.valid(idx)
        case notVar =>
          Validated.invalidNel(RecursionArgNotVar(fnname, notVar))
      }
    }

    /*
     * Check that decl is a strict substructure of pat. We do this by making sure decl is a Var
     * and that var is one of the strict substrutures of the pattern.
     */
    def strictSubstructure(fnname: String, pat: Pattern[Option[String], TypeRef], decl: Declaration): Res =
      decl match {
        case v@Declaration.Var(nm) =>
          if (pat.substructures.contains(nm)) unitValid
          else Validated.invalidNel(RecursionNotSubstructural(fnname, pat, v))
        case notVar =>
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
      bs: Iterable[String],
      decl: Declaration)(next: ValidatedNel[RecursionError, A]): ValidatedNel[RecursionError, A] =
      state.outerRecName match {
        case None => next
        case Some(n) =>
          NonEmptyList.fromList(bs.filter(Set(n)).toList.sorted) match {
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
    type St[A] = StateT[Either[NonEmptyList[RecursionError], ?], State, A]

    // Scala has trouble infering types like St, we we make these typed
    // helper functions to use below
    def failSt[A](err: RecursionError): St[A] =
      StateT.liftF(Left(NonEmptyList.of(err)))
    val getSt: St[State] = StateT.get
    def setSt(s: State): St[Unit] = StateT.set(s)
    def toSt[A](v: ValidatedNel[RecursionError, A]): St[A] =
      StateT.liftF(v.toEither)
    val unitSt: St[Unit] = StateT.pure(())

    def checkForIllegalBindsSt[A](
      bs: Iterable[String],
      decl: Declaration): St[Unit] =
        getSt.flatMap { state =>
          toSt(checkForIllegalBinds(state, bs, decl)(unitValid))
        }
    /*
     * With the given state, check the given Declaration to see if
     * we have valid recursion
     */
    def checkDecl(decl: Declaration): St[Unit] = {
      import Declaration._
      decl match {
        case Apply(Var(nm), args, _) =>
          getSt.flatMap {
            case Default(_) =>
              // without any recursion, normal typechecking will detect bad states:
              args.traverse_(checkDecl)
            case irb@InRecurBranch(inrec, branch) =>
              val idx = inrec.index
              // here we are calling our recursive function
              // make sure we do so on a substructural match
              val defname = inrec.defname
              if (nm == defname) {
                args.get(idx.toLong) match {
                  case None =>
                    // not enough args to check recursion
                    failSt(InvalidRecusion(nm, decl.region))
                  case Some(arg) =>
                    toSt(strictSubstructure(defname, branch, arg)) *>
                      setSt(irb.incRecCount) // we have recurred again
                }
              }
              else {
                // not a recursive call
                args.traverse_(checkDecl)
              }
            case ir: RecState =>
              // we have either not yet, or already done the recursion
              if (nm == ir.defname) failSt(InvalidRecusion(nm, decl.region))
              else args.traverse_(checkDecl)
            }
        case Apply(fn, args, _) =>
          checkDecl(fn) *> args.traverse_(checkDecl)
        case Binding(BindingStatement(pat, thisDecl, next)) =>
          checkForIllegalBindsSt(pat.names, decl) *>
              checkDecl(thisDecl) *>
              checkDecl(next.padded)
        case Comment(cs) =>
          checkDecl(cs.on.padded)
        case Constructor(_) =>
          // constructors can't be bindings:
          unitSt
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
        case Lambda(args, body) =>
          // these args create new bindings:
          checkForIllegalBindsSt(args.toList, decl) *> checkDecl(body)
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
        case recur@Match(RecursionKind.Recursive, _, cases) =>
          // this is a state change
          getSt.flatMap {
            case Default(_) | InRecurBranch(_, _) | InRecursiveRecurred(_, _, _, _) =>
              failSt(UnexpectedRecur(recur))
            case ir@InRecursive(defname, args) =>
              toSt(getRecurIndex(defname, args, recur)).flatMap { idx =>
                // on all these branchs, use the the same
                // parent state
                def beginBranch(pat: Pattern[Option[String], TypeRef]): St[Unit] =
                  getSt.flatMap {
                    case ir@InRecursive(_, _) =>
                      val rec = ir.setRecur(idx, recur)
                      setSt(rec) *> beginBranch(pat)
                    case irr@InRecursiveRecurred(_, _, _, _) =>
                      setSt(InRecurBranch(irr, pat))
                    case illegal =>
                      // $COVERAGE-OFF$ this should be unreachable
                      sys.error(s"unreachable: $ir -> $pat -> $illegal")
                      // $COVERAGE-ON$
                    }

                val endBranch: St[Unit] =
                  getSt.flatMap {
                    case InRecurBranch(irr, _) => setSt(irr)
                    case illegal =>
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
        case Parens(p) =>
          checkDecl(p)
        case TupleCons(tups) =>
          tups.traverse_(checkDecl)
        case Var(v) =>
          getSt.flatMap {
            case Default(_) =>
              // without any recursion, normal typechecking will detect bad states:
              unitSt
            case ir: RecState =>
              // if this were an apply, it would have been handled by Apply(Var(...
              if (v == ir.defname) failSt(InvalidRecusion(v, decl.region))
              else unitSt
          }
        case ListDecl(ll) =>
          ll match {
            case ListLang.Cons(items) =>
              items.traverse_ { s => checkDecl(s.value) }
            case ListLang.Comprehension(e, b, i, f) =>
              checkDecl(e.value) *>
                checkDecl(b) *>
                checkDecl(i) *>
                (f.traverse_(checkDecl))
          }
      }
    }

    def checkDeclV(state: State, decl: Declaration): Res =
      Validated.fromEither(checkDecl(decl).as(()).runA(state))

    /*
     * Binds are not allowed to be recursive, only defs, so here we just make sure
     * none of the free variables of the pattern are used in decl
     */
    def checkDef[A](state: State, defstmt: DefStatement[(OptIndent[Declaration], Padding[A])]): Res = {
      val body = defstmt.result._1.get
      val args = defstmt.args.map(_._1)
      checkForIllegalBinds(state, defstmt.name :: args, body) {
        defstmt.kind match {
          case RecursionKind.NonRecursive =>
            checkDeclV(state.normalDef, body)
          case RecursionKind.Recursive =>
            state.outerRecName match {
              case None =>
                // we change state
                val newState = InRecursive(defstmt.name, args)
                val st = setSt(newState) *> checkDecl(body) *> (getSt.flatMap {
                  case InRecursiveRecurred(_, _, _, cnt) if cnt > 0 =>
                    unitSt
                  case InRecursiveRecurred(_, _, _, 0) | InRecursive(_, _) =>
                    // we never recursed
                    failSt[Unit](RecursiveDefNoRecur(defstmt.map(_._1.get)))
                  case unreachable =>
                    // $COVERAGE-OFF$ this should be unreachable
                    sys.error(s"we would like to prove in the types we can't get here: $unreachable, $defstmt"): St[Unit]
                    // $COVERAGE-ON$
                })
                // Note a def can't change the state
                // we either have a valid nested def, or we don't
                // but that can't change the state of the outer
                // def that is calling this. So, we don't
                // return the final state in this method
                Validated.fromEither(st.runA(state))
              case Some(defname) =>
                Validated.invalidNel(IllegalNesting(defname, defstmt.name, body.region))
            }
        }
      }
    }
  }
}
