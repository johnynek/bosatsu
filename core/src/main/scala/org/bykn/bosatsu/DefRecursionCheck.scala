package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel}

import cats.implicits._

/**
 * Recursion in bosatsu is only allowed on a substructural match
 * of one of the parameters to the def. This strict rule, along
 * with strictly finite data, ensures that all recursion terminates
 *
 * The rules are as follows:
 * 0. recursive defs may not be shadowed. This makes checking for legal recursion easier
 * 1. until we reach a recur match, we cannot call a recursive def
 * 2. a recur match must occur on one of the literal parameters to the recursive def
 * 3. inside each branch of the recur match, we may only recur on on substructures in the match
 *    position
 */
object DefRecursionCheck {

  type Res = ValidatedNel[RecursionError, Unit]
  sealed abstract class RecursionError
  case class InvalidRecusion(name: String, illegalPosition: Region) extends RecursionError
  case class ZeroArgRecursiveDef(name: String) extends RecursionError
  case class ExpectedRecur(name: String, found: Declaration) extends RecursionError
  case class IllegalNesting(scope: Set[String], insideDefName: String, nextDef: String, region: Region) extends RecursionError
  case class IllegalShadow(fnname: String, decl: Declaration) extends RecursionError
  case class UnexpectedRecur(decl: Declaration.Match) extends RecursionError
  case class RecurNotOnArg(decl: Declaration.Match, fnname: String, args: List[String]) extends RecursionError
  case class RecurNotOnVar(decl: Declaration.Match, fnname: String) extends RecursionError
  case class RecursionArgNotVar(fnname: String, invalidArg: Declaration) extends RecursionError
  case class RecursionNotSubstructural(fnname: String, recurPat: Pattern[Option[String], TypeRef], arg: Declaration.Var) extends RecursionError

  def checkStatement(s: Statement): Res = {
    import Statement._
    import Impl._
    s match {
      case Bind(BindingStatement(pat, decl, rest)) =>
        val state = Default(Set.empty)
        checkDecl(state, decl) *> checkStatement(rest.padded)
      case Comment(cs) =>
        checkStatement(cs.on.padded)
      case Def(defn) =>
        val state = Default(Set.empty)
        checkDef(state, defn)(checkStatement(_))
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

    sealed abstract class State {
      def outerRecs: Set[String]
    }
    case class Default(outerRecs: Set[String]) extends State
    case class InRecursive(outerRecs: Set[String], defname: String, args: List[String]) extends State {
      def branchState(index: Int, branchPat: Pattern[Option[String], TypeRef]): State =
        InRecurBranch(outerRecs, branchPat, defname, index)
    }
    case class InRecurBranch(outerRecs: Set[String], branch: Pattern[Option[String], TypeRef], defname: String, index: Int) extends State


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
        case _ =>
          Validated.invalidNel(RecurNotOnVar(m, fnname))
      }
    }

    def strictSubstructure(fnname: String, pat: Pattern[Option[String], TypeRef], decl: Declaration): Res =
      decl match {
        case v@Declaration.Var(nm) =>
          if (pat.substructures.contains(nm)) unitValid
          else Validated.invalidNel(RecursionNotSubstructural(fnname, pat, v))
        case notVar =>
          // we can only recur with vars
          Validated.invalidNel(RecursionArgNotVar(fnname, decl))
      }

    def checkForIllegalBinds[A](
      state: State,
      bs: Iterable[String],
      decl: Declaration)(next: ValidatedNel[RecursionError, A]): ValidatedNel[RecursionError, A] =
      NonEmptyList.fromList(bs.filter(state.outerRecs).toList.sorted) match {
        case Some(nel) =>
          Validated.invalid(nel.map(IllegalShadow(_, decl)))
        case None =>
          next
      }

    def checkDecl(state: State, decl: Declaration): Res = {
      import Declaration._
      decl match {
        case Apply(Var(nm), args, _) =>
          state match {
            case Default(_) =>
              // without any recursion, normal typechecking will detect bad states:
              args.traverse_(checkDecl(state, _))
            case ir@InRecursive(_, defname, _) =>
              if (nm == defname) Validated.invalidNel(InvalidRecusion(nm, decl.region))
              else args.traverse_(checkDecl(state, _))
            case InRecurBranch(_, branch, defname, idx) =>
              if (nm == defname) {
                args.get(idx.toLong) match {
                  case None =>
                    // not enough args to check recursion
                    Validated.invalidNel(InvalidRecusion(nm, decl.region))
                  case Some(arg) =>
                    strictSubstructure(defname, branch, arg)
                }
              }
              else {
                // not a recursive call
                args.traverse_(checkDecl(state, _))
              }
            }
        case Apply(fn, args, _) =>
          checkDecl(state, fn) *> args.traverse_(checkDecl(state, _))
        case Binding(BindingStatement(pat, thisDecl, next)) =>
            checkForIllegalBinds(state, pat.names, decl) {
              checkDecl(state, thisDecl) *> checkDecl(state, next.padded)
            }
        case Comment(cs) =>
          checkDecl(state, cs.on.padded)
        case Constructor(_) =>
          // constructors can't be bindings:
          unitValid
        case DefFn(defstmt) =>
          // we can use the name of the def after we have defined it, which is the next part
          checkDef(state, defstmt) { decl =>
            checkDecl(state, decl)
          }
        case IfElse(ifCases, elseCase) =>
          val ifs = ifCases.traverse_ { case (d, od) =>
            checkDecl(state, d) *> checkDecl(state, od.get)
          }
          val e = checkDecl(state, elseCase.get)
          ifs *> e
        case Lambda(args, body) =>
          // these args create new bindings:
          checkForIllegalBinds(state, args.toList, decl) {
            checkDecl(state, body)
          }
        case Literal(_) =>
          unitValid
        case Match(RecursionKind.NonRecursive, arg, cases) =>
          // the arg can't use state, but cases introduce new bindings:
          val argRes = checkDecl(state, arg)
          val optRes = cases.get.traverse_ { case (pat, next) =>
            checkForIllegalBinds(state, pat.names, decl) {
              checkDecl(state, next.get)
            }
          }
          argRes *> optRes
        case recur@Match(RecursionKind.Recursive, _, cases) =>
          // this is a state change
          state match {
            case Default(_) | InRecurBranch(_, _, _, _) =>
              Validated.invalidNel(UnexpectedRecur(recur))
            case ir@InRecursive(_, defname, args) =>
              getRecurIndex(defname, args, recur).andThen { idx =>
                cases.get.traverse_ { case (pat, next) =>
                  val bstate = ir.branchState(idx, pat)
                  checkDecl(bstate, next.get)
                }
              }
            }
        case Parens(p) =>
          checkDecl(state, p)
        case TupleCons(tups) =>
          tups.traverse_(checkDecl(state, _))
        case Var(v) =>
          state match {
            case Default(_) =>
              // without any recursion, normal typechecking will detect bad states:
              unitValid
            case ir@InRecursive(_, defname, _) =>
              if (v == defname) Validated.invalidNel(InvalidRecusion(v, decl.region))
              else unitValid
            case InRecurBranch(_, _, defname, _) =>
              if (v == defname) Validated.invalidNel(InvalidRecusion(v, decl.region))
              else unitValid
            }
        case ListDecl(ll) =>
          ll match {
            case ListLang.Cons(items) =>
              items.traverse_ { s => checkDecl(state, s.value) }
            case ListLang.Comprehension(e, b, i, f) =>
              checkDecl(state, e.value) *>
                checkDecl(state, b) *>
                checkDecl(state, i) *>
                (f.traverse_(checkDecl(state, _)))
          }
      }
    }

    /*
     * Binds are not allowed to be recursive, only defs, so here we just make sure
     * none of the free variables of the pattern are used in decl
     */
    // def checkBind(pat: Pattern[Option[String], TypeRef], decl: Declaration): Res =
    //   ensureUnused(pat.names, decl)

    def checkDef[A](state: State, defstmt: DefStatement[(OptIndent[Declaration], Padding[A])])(next: A => Res): Res = {
      val body = defstmt.result._1.get
      val args = defstmt.args.map(_._1)
      val thisItem =
        checkForIllegalBinds(state, defstmt.name :: args, body) {
          defstmt.kind match {
            case RecursionKind.NonRecursive =>
              // this is a non-recursive binding, so the name is not
              // in scope, however, of course, the arguments are
              checkDecl(Default(state.outerRecs), body)
            case RecursionKind.Recursive =>
              state match {
                case default@Default(_) =>
                  // we change state
                  val newState = InRecursive(default.outerRecs + defstmt.name, defstmt.name, args)
                  checkDecl(newState, body)
                case InRecursive(scope, defname, _) =>
                  // illegal nested recursion
                  Validated.invalidNel(IllegalNesting(scope, defname, defstmt.name, body.region))
                case InRecurBranch(scope, _, defname, _) =>
                  // illegal nested recursion
                  Validated.invalidNel(IllegalNesting(scope, defname, defstmt.name, body.region))
                }
          }
        }
      val nextRes = next(defstmt.result._2.padded)
      thisItem *> nextRes
    }
  }
}
