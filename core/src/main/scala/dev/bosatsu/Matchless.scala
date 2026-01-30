package dev.bosatsu

import cats.{Monad, Monoid}
import cats.data.{Chain, NonEmptyList, WriterT}
import dev.bosatsu.pattern.StrPart
import dev.bosatsu.rankn.{DataRepr, Type, RefSpace}

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  sealed abstract class Expr[+A] derives CanEqual
  // these hold bindings either in the code, or temporary
  // local ones, note CheapExpr never trigger a side effect
  sealed trait CheapExpr[+A] extends Expr[A]

  // name is set for recursive (but not tail recursive) methods
  case class Lambda[+A](
      captures: List[Expr[A]],
      recursiveName: Option[Bindable],
      args: NonEmptyList[Bindable],
      body: Expr[A]
  ) extends Expr[A] {
    def recursionKind: RecursionKind =
      RecursionKind.recursive(recursiveName.isDefined)

    def arity: Int = args.length
  }

  // This is a while loop, the result of which is result and the body is evaluated
  // while cond is true
  case class WhileExpr[A](
      cond: BoolExpr[A],
      effectExpr: Expr[A],
      result: LocalAnonMut
  ) extends Expr[A]

  case class Global[A](from: A, pack: PackageName, name: Bindable)
      extends CheapExpr[A]

  // these are immutable (but can be shadowed)
  case class Local(arg: Bindable) extends CheapExpr[Nothing]
  case class ClosureSlot(idx: Int) extends CheapExpr[Nothing]
  // these are is a separate namespace from Expr
  case class LocalAnon(ident: Long) extends CheapExpr[Nothing]
  // these are mutable variables that can be updated while evaluating an BoolExpr
  case class LocalAnonMut(ident: Long) extends CheapExpr[Nothing]

  // we aggregate all the applications to potentially make dispatch more efficient
  // note fn is never an App
  case class App[A](fn: Expr[A], arg: NonEmptyList[Expr[A]]) extends Expr[A]
  case class Let[A](
      arg: Either[LocalAnon, Bindable],
      expr: Expr[A],
      in: Expr[A]
  ) extends Expr[A]

  object Let {
    def apply[A](arg: Bindable, expr: Expr[A], in: Expr[A]): Expr[A] =
      // don't create let x = y in x, just return y
      in match {
        case Local(a) if a == arg => expr
        case _                    => Let(Right(arg), expr, in)
      }

    def apply[A](arg: LocalAnon, expr: Expr[A], in: Expr[A]): Expr[A] =
      // don't create let x = y in x, just return y
      in match {
        case LocalAnon(id) if id == arg.ident => expr
        case _                                => Let(Left(arg), expr, in)
      }
  }

  case class LetMut[A](name: LocalAnonMut, span: Expr[A]) extends Expr[A] {
    // often we have several LetMut at once, return all them
    def flatten: (NonEmptyList[LocalAnonMut], Expr[A]) =
      span match {
        case next @ LetMut(_, _) =>
          val (anons, expr) = next.flatten
          (name :: anons, expr)
        case notLetMut =>
          (NonEmptyList.one(name), notLetMut)
      }
  }
  case class Literal(lit: Lit) extends CheapExpr[Nothing]

  // these result in Int values which are also used as booleans
  // evaluating these CAN have side effects of mutating LocalAnon
  // variables.
  sealed abstract class BoolExpr[+A] derives CanEqual {
    final def &&[A1 >: A](that: BoolExpr[A1]): BoolExpr[A1] =
      (this, that) match {
        case (TrueConst, r) => r
        case (l, TrueConst) => l
        case _              => And(this, that)
      }
  }
  // returns 1 if it does, else 0
  case class EqualsLit[A](expr: CheapExpr[A], lit: Lit) extends BoolExpr[A]
  case class EqualsNat[A](expr: CheapExpr[A], nat: DataRepr.Nat)
      extends BoolExpr[A]
  // 1 if both are > 0
  case class And[A](e1: BoolExpr[A], e2: BoolExpr[A]) extends BoolExpr[A]
  // checks if variant matches, and if so, writes to
  // a given mut
  case class CheckVariant[A](
      expr: CheapExpr[A],
      expect: Int,
      size: Int,
      famArities: List[Int]
  ) extends BoolExpr[A]
  // set the mutable variable to the given expr and return true
  // string matching is complex done at a lower level
  case class MatchString[A](
      arg: CheapExpr[A],
      parts: List[StrPart],
      binds: List[LocalAnonMut],
      mustMatch: Boolean
  ) extends BoolExpr[A]
  // set the mutable variable to the given expr and return true
  case class SetMut[A](target: LocalAnonMut, expr: Expr[A]) extends BoolExpr[A]
  case object TrueConst extends BoolExpr[Nothing]
  case class LetBool[A](
      arg: Either[LocalAnon, Bindable],
      expr: Expr[A],
      in: BoolExpr[A]
  ) extends BoolExpr[A]

  case class LetMutBool[A](name: LocalAnonMut, span: BoolExpr[A])
      extends BoolExpr[A]
  object LetMutBool {
    def apply[A](lst: List[LocalAnonMut], span: BoolExpr[A]): BoolExpr[A] =
      lst.foldRight(span)(LetMutBool(_, _))
  }

  def hasSideEffect(bx: BoolExpr[Any]): Boolean =
    bx match {
      case SetMut(_, _) => true
      case TrueConst | CheckVariant(_, _, _, _) | EqualsLit(_, _) |
          EqualsNat(_, _) =>
        false
      case MatchString(_, _, b, _) => b.nonEmpty
      case And(b1, b2)             => hasSideEffect(b1) || hasSideEffect(b2)
      case LetBool(_, x, b)        =>
        hasSideEffect(b) || hasSideEffect(x)
      case LetMutBool(_, b) => hasSideEffect(b)
    }

  def hasSideEffect(bx: Expr[Any]): Boolean =
    bx match {
      case _: CheapExpr[?] => false
      case Always(b, x)    => hasSideEffect(b) || hasSideEffect(x)
      case App(f, as)      =>
        (f :: as).exists(hasSideEffect(_))
      case If(c, t, f) =>
        hasSideEffect(c) || hasSideEffect(t) || hasSideEffect(f)
      case Let(_, x, b) =>
        hasSideEffect(b) || hasSideEffect(x)
      case LetMut(_, in) => hasSideEffect(in)
      case PrevNat(n)    => hasSideEffect(n)
      case MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | ZeroNat |
          Lambda(_, _, _, _) =>
        // making a lambda or const is a pure function
        false
      case WhileExpr(_, _, _) =>
        // not all while loops have side effects technically, but we assume yes
        // for now. We could have a list of all the known control mutables here
        // but it seems hard
        true
    }

  case class If[A](cond: BoolExpr[A], thenExpr: Expr[A], elseExpr: Expr[A])
      extends Expr[A] {
    def flatten: (NonEmptyList[(BoolExpr[A], Expr[A])], Expr[A]) = {
      def combine(expr: Expr[A]): (List[(BoolExpr[A], Expr[A])], Expr[A]) =
        expr match {
          case If(c1, t1, e1) =>
            val (ifs, e2) = combine(e1)
            (((c1, t1)) :: ifs, e2)
          case last => (Nil, last)
        }

      val (rest, last) = combine(elseExpr)
      (NonEmptyList((cond, thenExpr), rest), last)
    }
  }
  case class Always[A](cond: BoolExpr[A], thenExpr: Expr[A]) extends Expr[A]
  object Always {
    object SetChain {
      // a common pattern is Always(SetMut(m, e), r)
      def unapply[A](
          expr: Expr[A]
      ): Option[(NonEmptyList[(LocalAnonMut, Expr[A])], Expr[A])] =
        expr match {
          case Always(SetMut(mut, v), res) =>
            val pair = (mut, v)
            unapply(res) match {
              case None              => Some((NonEmptyList.one(pair), res))
              case Some((muts, res)) => Some((pair :: muts, res))
            }
          case _ => None
        }
    }
  }
  def always[A](cond: BoolExpr[A], thenExpr: Expr[A]): Expr[A] =
    if (hasSideEffect(cond)) Always(cond, thenExpr)
    else thenExpr

  /** These aren't really super cheap, but when we treat them cheap we check
    * that we will only call them one time
    */
  case class GetEnumElement[A](
      arg: CheapExpr[A],
      variant: Int,
      index: Int,
      size: Int
  ) extends CheapExpr[A]
  case class GetStructElement[A](arg: CheapExpr[A], index: Int, size: Int)
      extends CheapExpr[A]

  sealed abstract class ConsExpr extends Expr[Nothing] {
    def arity: Int
  }
  // we need to compile calls to constructors into these
  case class MakeEnum(variant: Int, arity: Int, famArities: List[Int])
      extends ConsExpr

  private val boolFamArities = 0 :: 0 :: Nil
  private val listFamArities = 0 :: 2 :: Nil
  val FalseExpr: Expr[Nothing] = MakeEnum(0, 0, boolFamArities)
  val TrueExpr: Expr[Nothing] = MakeEnum(1, 0, boolFamArities)
  val UnitExpr: Expr[Nothing] = MakeStruct(0)

  def isTrueExpr[A](e: CheapExpr[A]): BoolExpr[A] =
    CheckVariant(e, 1, 0, boolFamArities)

  object ListExpr {
    val Nil: Expr[Nothing] = MakeEnum(0, 0, listFamArities)
    private val consFn = MakeEnum(1, 2, listFamArities)

    def cons[A](h: Expr[A], t: Expr[A]): Expr[A] =
      App(consFn, NonEmptyList(h, t :: List.empty))

    def notNil[A](e: CheapExpr[A]): BoolExpr[A] =
      CheckVariant(e, 1, 2, listFamArities)

    def head[A](arg: CheapExpr[A]): CheapExpr[A] =
      GetEnumElement(arg, 1, 0, 2)

    def tail[A](arg: CheapExpr[A]): CheapExpr[A] =
      GetEnumElement(arg, 1, 1, 2)
  }
  case class MakeStruct(arity: Int) extends ConsExpr
  case object ZeroNat extends ConsExpr {
    def arity = 0
  }
  // this is the function Nat -> Nat
  case object SuccNat extends ConsExpr {
    def arity = 1
  }

  case class PrevNat[A](of: Expr[A]) extends Expr[A]

  private def maybeMemo[F[_]: Monad, A](
      tmp: F[Long]
  )(fn: CheapExpr[A] => F[Expr[A]]): Expr[A] => F[Expr[A]] = { (arg: Expr[A]) =>
    arg match {
      case c: CheapExpr[A] => fn(c)
      case _               =>
        for {
          nm <- tmp
          bound = LocalAnon(nm)
          res <- fn(bound)
        } yield Let(bound, arg, res)
    }
  }

  private val empty = (PackageName.PredefName, Constructor("EmptyList"))
  private val cons = (PackageName.PredefName, Constructor("NonEmptyList"))
  private val revName = Identifier.Name("reverse")
  private def reverseFn[A](from: A) =
    Global[A](from, PackageName.PredefName, revName)

  // drop all items in the tail after the first time fn returns true
  // as a result, we have 0 or 1 items where fn is true in the result
  // and it is always the last if there is 1
  def stopAt[A](nel: NonEmptyList[A])(fn: A => Boolean): NonEmptyList[A] =
    nel match {
      case NonEmptyList(h, _) if fn(h) => NonEmptyList(h, Nil)
      case s @ NonEmptyList(_, Nil)    => s
      case NonEmptyList(h0, h1 :: t)   => h0 :: stopAt(NonEmptyList(h1, t))(fn)
    }

  // same as fromLet below, but uses RefSpace
  def fromLet[A, B](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A]
  )(
      variantOf: (PackageName, Constructor) => Option[DataRepr]
  ): Expr[B] =
    (for {
      c <- RefSpace.allocCounter
      expr <- fromLet(from, name, rec, te, variantOf, c)
    } yield expr).run.value

  // we need a TypeEnv to inline the creation of structs and variants
  def fromLet[F[_]: Monad, A, B](
      from: B,
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      variantOf: (PackageName, Constructor) => Option[DataRepr],
      makeAnon: F[Long]
  ): F[Expr[B]] = {

    type UnionMatch =
      NonEmptyList[(List[LocalAnonMut], BoolExpr[B], List[(Bindable, Expr[B])])]

    val wildMatch: UnionMatch = NonEmptyList((Nil, TrueConst, Nil), Nil)
    def isWildMatch(um: UnionMatch): Boolean =
      um match {
        case NonEmptyList((Nil, TrueConst, Nil), Nil) => true
        case _                                        => false
      }

    val emptyExpr: Expr[B] =
      empty match {
        case (p, c) =>
          variantOf(p, c) match {
            case Some(DataRepr.Enum(v, s, f)) => MakeEnum(v, s, f)
            case other                        =>
              /* We assume the structure of Lists to be standard linked lists
               * Empty cannot be a struct
               */

              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"empty List should be an enum, found: $other"
              )
            // $COVERAGE-ON$
          }
      }

    case class LambdaState(
        name: Option[Bindable],
        slots: Map[Bindable, Expr[B]]
    ) {
      def unname: LambdaState = LambdaState(None, slots)

      def apply(b: Bindable): Expr[B] =
        slots.get(b) match {
          case Some(expr) => expr
          case None       => Local(b)
        }

      def lambdaFrees(frees: List[Bindable]): (LambdaState, List[Expr[B]]) =
        name match {
          case None =>
            val newSlots = frees.iterator.zipWithIndex.map { case (b, idx) =>
              (b, ClosureSlot(idx))
            }.toMap
            val captures = frees.map(apply(_))
            (copy(slots = newSlots), captures)
          case Some(n) =>
            val newSlots = frees.iterator
              .filterNot(_ == n)
              .zipWithIndex
              .map { case (b, idx) => (b, ClosureSlot(idx)) }
              .toMap
            val captures = frees.flatMap { f =>
              if (cats.Eq[Identifier].neqv(f, n)) (apply(f) :: Nil)
              else Nil
            }
            (copy(slots = newSlots), captures)
        }

      def inLet(b: Bindable): LambdaState = copy(name = Some(b))
    }

    def substituteLocalsBool(
        m: Map[Bindable, CheapExpr[B]],
        e: BoolExpr[B]
    ): BoolExpr[B] =
      e match {
        case SetMut(mut, e) => SetMut(mut, substituteLocals(m, e))
        case And(b1, b2)    =>
          And(substituteLocalsBool(m, b1), substituteLocalsBool(m, b2))
        case EqualsLit(x, l) =>
          EqualsLit(substituteLocalsCheap(m, x), l)
        case EqualsNat(x, n) =>
          EqualsNat(substituteLocalsCheap(m, x), n)
        case TrueConst                           => TrueConst
        case CheckVariant(expr, expect, sz, fam) =>
          CheckVariant(substituteLocalsCheap(m, expr), expect, sz, fam)
        case ms: MatchString[b] =>
          ms.copy(arg = substituteLocalsCheap(m, ms.arg))
        case LetBool(b, a, in) =>
          val m1 = b match {
            case Right(b) => m - b
            case _        => m
          }
          LetBool(b, substituteLocals(m, a), substituteLocalsBool(m1, in))
        case LetMutBool(b, in) =>
          LetMutBool(b, substituteLocalsBool(m, in))
      }

    def substituteLocals(m: Map[Bindable, CheapExpr[B]], e: Expr[B]): Expr[B] =
      e match {
        case App(fn, appArgs) =>
          App(substituteLocals(m, fn), appArgs.map(substituteLocals(m, _)))
        case If(c, tcase, fcase) =>
          If(
            substituteLocalsBool(m, c),
            substituteLocals(m, tcase),
            substituteLocals(m, fcase)
          )
        case Always(c, e) =>
          Always(substituteLocalsBool(m, c), substituteLocals(m, e))
        case LetMut(mut, e) =>
          LetMut(mut, substituteLocals(m, e))
        case Let(n, v, in) =>
          val m1 = n match {
            case Right(b) => m - b
            case _        => m
          }
          Let(n, substituteLocals(m, v), substituteLocals(m1, in))
        case Local(n) =>
          m.get(n) match {
            case Some(mut) => mut
            case None      => e
          }
        case PrevNat(n)            => PrevNat(substituteLocals(m, n))
        case ge: GetEnumElement[b] =>
          ge.copy(arg = substituteLocalsCheap(m, ge.arg))
        case gs: GetStructElement[b] =>
          gs.copy(arg = substituteLocalsCheap(m, gs.arg))
        case Lambda(c, r, as, b) =>
          val m1 = m -- as.toList
          val b1 = substituteLocals(m1, b)
          Lambda(c, r, as, b1)
        case WhileExpr(c, ef, r) =>
          WhileExpr(substituteLocalsBool(m, c), substituteLocals(m, ef), r)
        case ClosureSlot(_) | Global(_, _, _) | LocalAnon(_) | LocalAnonMut(_) |
            MakeEnum(_, _, _) | MakeStruct(_) | SuccNat | Literal(_) |
            ZeroNat =>
          e
      }
    def substituteLocalsCheap(
        m: Map[Bindable, CheapExpr[B]],
        e: CheapExpr[B]
    ): CheapExpr[B] =
      substituteLocals(m, e) match {
        case ch: CheapExpr[B] => ch
        case notCheap         =>
          sys.error(
            s"invariant violation: substitution didn't maintain cheap: $e => $notCheap"
          )
      }

    def loopFn(
        captures: List[Expr[B]],
        name: Bindable,
        args: NonEmptyList[Bindable],
        body: Expr[B]
    ): F[Expr[B]] = {

      // assign any results to result and set the condition to false
      // and replace any tail calls to nm(args) with assigning args to those values
      case class ArgRecord(
          name: Bindable,
          tmp: LocalAnon,
          loopVar: LocalAnonMut
      )
      def toWhileBody(
          args: NonEmptyList[ArgRecord],
          cond: LocalAnonMut,
          result: LocalAnonMut
      ): Expr[B] = {

        def returnValue(v: Expr[B]): Expr[B] =
          setAll((cond, FalseExpr) :: (result, v) :: Nil, UnitExpr)

        // return Some(e) if this expression can be rewritten into a tail call to name,
        // in instead of the call, do a bunch of SetMut on the args, and set cond to false
        // else None
        def loop(expr: Expr[B]): Option[Expr[B]] =
          expr match {
            case App(Local(fnName), appArgs) if fnName == name =>
              // this is a tail call
              // we know the length of appArgs must match args or the code wouldn't have compiled
              // we have to first assign to the temp variables, and then assign the temp variables
              // to the results to make sure we don't have any data dependency issues with the values;
              val tmpAssigns = appArgs.iterator
                .zip(args.iterator)
                .flatMap { case (appArg, argRecord) =>
                  val isSelfAssign = appArg match {
                    case LocalAnonMut(id) => id == argRecord.loopVar.ident
                    case _                => false
                  }
                  if (isSelfAssign)
                    Iterator.empty
                  else
                    // don't create self assignments
                    Iterator.single(
                      (
                        (argRecord.tmp, appArg),
                        (argRecord.loopVar, argRecord.tmp)
                      )
                    )
                }
                .toList

              // there must be at least one assignment
              Some(
                letAnons(
                  tmpAssigns.map(_._1),
                  setAll(tmpAssigns.map(_._2), UnitExpr)
                )
              )
            case If(c, tcase, fcase) =>
              // this can possible have tail calls inside the branches
              (loop(tcase), loop(fcase)) match {
                case (Some(t), Some(f)) =>
                  Some(If(c, t, f))
                case (None, Some(f)) =>
                  Some(If(c, returnValue(tcase), f))
                case (Some(t), None) =>
                  Some(If(c, t, returnValue(fcase)))
                case (None, None) => None
              }
            case Always(c, e) =>
              loop(e).map(Always(c, _))
            case LetMut(m, e) =>
              loop(e).map(LetMut(m, _))
            case Let(b, v, in) =>
              // in is in tail position
              loop(in).map(Let(b, v, _))
            // the rest cannot have a call in tail position
            case App(_, _) | ClosureSlot(_) | GetEnumElement(_, _, _, _) |
                GetStructElement(_, _, _) | Global(_, _, _) |
                Lambda(_, _, _, _) | Literal(_) | Local(_) | LocalAnon(_) |
                LocalAnonMut(_) | MakeEnum(_, _, _) | MakeStruct(_) |
                PrevNat(_) | SuccNat | WhileExpr(_, _, _) | ZeroNat =>
              None
          }

        val bodyTrans = substituteLocals(
          args.toList.map(a => (a.name, a.loopVar)).toMap,
          body
        )

        loop(bodyTrans) match {
          case Some(expr) => expr
          case None       =>
            sys.error(
              "invariant violation: could not find tail calls in:" +
                s"toWhileBody(name = $name, body = $body)"
            )
        }
      }
      val mut = makeAnon.map(LocalAnonMut(_))
      val anon = makeAnon.map(LocalAnon(_))
      for {
        cond <- mut
        result <- mut
        args1 <- args.traverse(b => (anon, mut).mapN(ArgRecord(b, _, _)))
        whileLoop = toWhileBody(args1, cond, result)
        allMuts = cond :: result :: args1.toList.map(_.loopVar)
        // we don't need to set the name on the lambda because this is no longer recursive
      } yield Lambda(
        captures,
        None,
        args,
        letMutAll(
          allMuts,
          setAll(
            args1.toList.map(arg => (arg.loopVar, Local(arg.name))),
            Always(
              SetMut(cond, TrueExpr),
              WhileExpr(isTrueExpr(cond), whileLoop, result)
            )
          )
        )
      )
    }

    def loopLetVal(
        name: Bindable,
        e: TypedExpr[A],
        rec: RecursionKind,
        slots: LambdaState
    ): F[Expr[B]] =
      rec match {
        case RecursionKind.Recursive =>
          lazy val e0 = loop(e, slots.inLet(name))
          def letrec(expr: Expr[B]): Expr[B] =
            expr match {
              case fn: Lambda[B] if fn.recursiveName == Some(name) => fn
              case fn: Lambda[?]                                   =>
                // loops always have a function name
                sys.error(
                  s"expected ${fn.recursiveName} == Some($name) in ${e.repr.render(80)} which compiled to $fn"
                )
              case _ =>
                sys.error(
                  s"expected ${e.repr.render(80)} to compile to a function, but got: $expr"
                )
            }

          // this could be tail recursive
          if (SelfCallKind(name, e) == SelfCallKind.TailCall) {
            val arity = Type.Fun.arity(e.getType)
            // we know that arity > 0 because, otherwise we can't have a total
            // self recursive loop, but property checks send in ill-typed
            // e and so we handle that by checking for arity > 0
            TypedExpr.toArgsBody(arity, e) match {
              case Some((params, body)) =>
                // we know params is non-empty because arity > 0
                val args = params.map(_._1)
                val frees = TypedExpr.freeVars(e :: Nil)
                val (slots1, caps) = slots.inLet(name).lambdaFrees(frees)
                loop(body, slots1)
                  .flatMap { v =>
                    loopFn(caps, name, args, v)
                  }
              // $COVERAGE-OFF$
              case _ =>
                // TODO: I don't think this case should ever happen in real code
                // but it definitely does in fuzz tests
                e0.map(letrec)
              // $COVERAGE-ON$
            }
          } else {
            e0.map(letrec)
          }
        case RecursionKind.NonRecursive => loop(e, slots)
      }

    def loop(te: TypedExpr[A], slots: LambdaState): F[Expr[B]] =
      te match {
        case TypedExpr.Generic(_, expr)              => loop(expr, slots)
        case TypedExpr.Annotation(term, _)           => loop(term, slots)
        case TypedExpr.AnnotatedLambda(args, res, _) =>
          val frees = TypedExpr.freeVars(te :: Nil)
          val (slots1, captures) = slots.lambdaFrees(frees)
          loop(res, slots1.unname).map(
            Lambda(captures, slots.name, args.map(_._1), _)
          )
        case TypedExpr.Global(pack, cons @ Constructor(_), _, _) =>
          Monad[F].pure(variantOf(pack, cons) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Enum(v, a, f) => MakeEnum(v, a, f)
                case DataRepr.Struct(a)     => MakeStruct(a)
                case DataRepr.NewType       => MakeStruct(1)
                case DataRepr.ZeroNat       => ZeroNat
                case DataRepr.SuccNat       => SuccNat
              }
            // $COVERAGE-OFF$
            case None =>
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          })
        case TypedExpr.Global(pack, notCons: Bindable, _, _) =>
          Monad[F].pure(Global(from, pack, notCons))
        case TypedExpr.Local(bind, _, _) =>
          Monad[F].pure(slots(bind))
        case TypedExpr.App(fn, as, _, _) =>
          (loop(fn, slots.unname), as.traverse(loop(_, slots.unname)))
            .mapN(App(_, _))
        case TypedExpr.Let(a, e, in, r, _) =>
          (loopLetVal(a, e, r, slots.unname), loop(in, slots))
            .mapN(Let(a, _, _))
        case TypedExpr.Literal(lit, _, _)      => Monad[F].pure(Literal(lit))
        case TypedExpr.Match(arg, branches, _) =>
          (
            loop(arg, slots.unname),
            branches.traverse { case (p, te) =>
              loop(te, slots.unname).map((p, _))
            }
          ).tupled
            .flatMap { case (a, b) => matchExpr(a, makeAnon, b) }
      }

    /*
     * A simple pattern is either:
     * 1. one that has no binding what-so-ever
     * 2. a total binding to a given name
     * 3. or we return None indicating not one of these
     */
    def maybeSimple(
        p: Pattern[(PackageName, Constructor), Type]
    ): Option[Either[Bindable, Unit]] =
      p match {
        case Pattern.WildCard   => Some(Right(()))
        case Pattern.Literal(_) =>
          // Literals are never total
          None
        case Pattern.Var(v)      => Some(Left(v))
        case Pattern.Named(v, p) =>
          maybeSimple(p) match {
            case Some(Right(_)) => Some(Left(v))
            case _              => None
          }
        case Pattern.StrPat(s) =>
          s match {
            case NonEmptyList(Pattern.StrPart.WildStr, Nil) => Some(Right(()))
            case NonEmptyList(Pattern.StrPart.NamedStr(n), Nil) => Some(Left(n))
            case _                                              => None
          }
        case Pattern.ListPat(l) =>
          l match {
            case Pattern.ListPart.WildList :: Nil     => Some(Right(()))
            case Pattern.ListPart.NamedList(n) :: Nil => Some(Left(n))
            case _                                    => None
          }
        case Pattern.Annotation(p, _)                    => maybeSimple(p)
        case Pattern.PositionalStruct((pack, cname), ps) =>
          // Only branch-free structs with no inner names are simple
          variantOf(pack, cname) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Struct(_) | DataRepr.NewType =>
                  ps.traverse(maybeSimple).flatMap { inners =>
                    if (inners.forall(_ == Right(()))) Some(Right(()))
                    else None
                  }
                case _ => None
              }
            // $COVERAGE-OFF$
            case None =>
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Union(h, t) =>
          (h :: t.toList).traverse(maybeSimple).flatMap { inners =>
            if (inners.forall(_ == Right(()))) Some(Right(()))
            else None
          }
      }

    // handle list matching, this is a while loop, that is evaluting
    // lst is initialized to init, leftAcc is initialized to empty
    // tail until it is true while mutating lst => lst.tail
    // this has the side-effect of mutating lst and leftAcc as well as any side effects that check has
    // which could have nested searches of its own
    def searchList(
        lst: LocalAnonMut,
        init: CheapExpr[B],
        check: BoolExpr[B],
        leftAcc: Option[LocalAnonMut]
    ): F[BoolExpr[B]] =
      (
        makeAnon.map(LocalAnonMut(_)),
        makeAnon.map(LocalAnon(_)),
        makeAnon.map(LocalAnonMut(_))
      )
        .mapN { (resMut, letBind, currentList) =>
          val initSets =
            (resMut, FalseExpr) ::
              (currentList, init) ::
              (leftAcc.toList.map { left =>
                (left, ListExpr.Nil)
              })

          val whileCheck = ListExpr.notNil(currentList)
          val effect: Expr[B] =
            setAll(
              (lst, currentList) :: Nil,
              If(
                check,
                setAll(
                  (currentList, ListExpr.Nil) ::
                    (resMut, TrueExpr) ::
                    Nil,
                  UnitExpr
                ),
                setAll(
                  (currentList, ListExpr.tail(currentList)) ::
                    leftAcc.toList.map { left =>
                      (left, ListExpr.cons(ListExpr.head(currentList), left))
                    },
                  UnitExpr
                )
              )
            )
          val searchLoop =
            setAll(initSets, WhileExpr(whileCheck, effect, resMut))

          LetMutBool(
            resMut :: currentList :: Nil,
            LetBool(Left(letBind), searchLoop, isTrueExpr(resMut))
          )
        }
      /*
            Dynamic { (scope: Scope) =>
              var res = false
              var currentList = initF(scope)
              var leftList = VList.VNil
              scope.updateMut(left, leftList)
              while (currentList ne null) {
                currentList match {
                  case nonempty @ VList.Cons(head, tail) =>
                    scope.updateMut(mutV, nonempty)
                    res = checkF(scope)
                    if (res) { currentList = null }
                    else {
                      currentList = tail
                      leftList = VList.Cons(head, leftList)
                      scope.updateMut(left, leftList)
                    }
                  case _ =>
                    currentList = null
                  // we don't match empty lists
                }
              }
              res
            }
       */

    // return the check expression for the check we need to do, and the list of bindings
    // if must match is true, we know that the pattern must match, so we can potentially remove some checks
    def doesMatch(
        arg: CheapExpr[B],
        pat: Pattern[(PackageName, Constructor), Type],
        mustMatch: Boolean
    ): F[UnionMatch] = {
      pat match {
        case Pattern.WildCard =>
          // this is a total pattern
          Monad[F].pure(wildMatch)
        case Pattern.Literal(lit) =>
          Monad[F].pure(NonEmptyList((Nil, EqualsLit(arg, lit), Nil), Nil))
        case Pattern.Var(v) =>
          Monad[F].pure(NonEmptyList((Nil, TrueConst, (v, arg) :: Nil), Nil))
        case Pattern.Named(v, p) =>
          doesMatch(arg, p, mustMatch).map(_.map { case (l0, cond, bs) =>
            (l0, cond, (v, arg) :: bs)
          })
        case strPat @ Pattern.StrPat(items) =>
          strPat.simplify match {
            case Some(simpler) => doesMatch(arg, simpler, mustMatch)
            case None          =>
              val sbinds: List[Bindable] =
                items.toList
                  .collect {
                    // that each name is distinct
                    // should be checked in the SourceConverter/TotalityChecking code
                    case Pattern.StrPart.NamedStr(n)  => n
                    case Pattern.StrPart.NamedChar(n) => n
                  }

              val pat = items.toList.map {
                case Pattern.StrPart.NamedStr(_)  => StrPart.IndexStr
                case Pattern.StrPart.NamedChar(_) => StrPart.IndexChar
                case Pattern.StrPart.WildStr      => StrPart.WildStr
                case Pattern.StrPart.WildChar     => StrPart.WildChar
                case Pattern.StrPart.LitStr(s)    => StrPart.LitStr(s)
              }

              sbinds
                .traverse { b =>
                  makeAnon.map(LocalAnonMut(_)).map((b, _))
                }
                .map { binds =>
                  val ms = binds.map(_._2)

                  NonEmptyList.one(
                    (ms, MatchString(arg, pat, ms, mustMatch), binds)
                  )
                }
          }
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p) => doesMatch(arg, p, mustMatch)
            case Left(
                  (glob, right @ NonEmptyList(Pattern.ListPart.Item(_), _))
                ) =>
              // we have a non-trailing list pattern
              // to match, this becomes a search problem
              // we loop over all the matches of p in the list,
              // then we put the prefix on the glob, and the suffix against
              // the tail.
              //
              // we know all the bindings we will make, allocate
              // anons for them, do the loop, and then return
              // the boolean of did we match
              val leftF: F[Option[(LocalAnonMut, Bindable)]] =
                glob match {
                  case Pattern.ListPart.WildList =>
                    Monad[F].pure(None)
                  case Pattern.ListPart.NamedList(ln) =>
                    makeAnon.map(nm => Some((LocalAnonMut(nm), ln)))
                }

              (leftF, makeAnon).tupled
                .flatMap { case (optAnonLeft, tmpList) =>
                  val anonList = LocalAnonMut(tmpList)

                  doesMatch(anonList, Pattern.ListPat(right.toList), false)
                    .flatMap { cases =>
                      cases.traverse {
                        case (_, TrueConst, _) =>
                          // $COVERAGE-OFF$

                          // this shouldn't be possible, since there are no total list matches with
                          // one item since we recurse on a ListPat with the first item being Right
                          // which as we can see above always returns Some(_)
                          throw new IllegalStateException(
                            s"$right should not be a total match"
                          )
                        // $COVERAGE-ON$
                        case (preLet, expr, binds) =>
                          val letTail = anonList :: preLet

                          val (resLet, leftOpt, resBind) =
                            optAnonLeft match {
                              case Some((anonLeft, ln)) =>
                                val revList =
                                  App(
                                    reverseFn(from),
                                    NonEmptyList.one(anonLeft)
                                  )
                                (
                                  anonLeft :: letTail,
                                  Some(anonLeft),
                                  (ln, revList) :: binds
                                )
                              case None =>
                                (letTail, None, binds)
                            }

                          searchList(anonList, arg, expr, leftOpt)
                            .map(s => (resLet, s, resBind))
                      }
                    }
                }
            case Left(
                  (glob, right @ NonEmptyList(_: Pattern.ListPart.Glob, _))
                ) =>
              // we search on the right side, so the left will match nothing
              // this should be banned by SourceConverter/TotalityChecker because
              // it is confusing, but it can be handled

              // $COVERAGE-OFF$
              glob match {
                case Pattern.ListPart.WildList =>
                  // no binding on the let
                  doesMatch(arg, Pattern.ListPat(right.toList), mustMatch)
                case Pattern.ListPart.NamedList(ln) =>
                  // bind empty to ln
                  doesMatch(arg, Pattern.ListPat(right.toList), mustMatch)
                    .map { nel =>
                      nel.map { case (preLet, expr, binds) =>
                        (preLet, expr, (ln, emptyExpr) :: binds)
                      }
                    }
              }
            // $COVERAGE-ON$
          }

        case Pattern.Annotation(p, _) =>
          // we discard types at this point
          doesMatch(arg, p, mustMatch)
        case Pattern.PositionalStruct((pack, cname), params) =>
          // we assume the patterns have already been optimized
          // so that useless total patterns have been replaced with _
          type Locals = Chain[(LocalAnonMut, Expr[B])]
          def asStruct(
              getter: Int => CheapExpr[B]
          ): WriterT[F, Locals, UnionMatch] = {
            // we have an and of a series of ors:
            // (m1 + m2 + m3) * (m4 + m5 + m6) ... =
            // we need to multiply them all out into a single set of ors
            def operate(
                pat: Pattern[(PackageName, Constructor), Type],
                idx: Int
            ): WriterT[F, Locals, UnionMatch] =
              maybeSimple(pat) match {
                case Some(Right(())) =>
                  // this is a total match
                  WriterT.value(wildMatch)
                case Some(Left(v)) =>
                  // this is just an alias
                  WriterT.value(
                    NonEmptyList((Nil, TrueConst, (v, getter(idx)) :: Nil), Nil)
                  )
                case None =>
                  // we make an anonymous variable and write to that:
                  for {
                    nm <- WriterT.valueT[F, Locals, Long](makeAnon)
                    lam = LocalAnonMut(nm)
                    um <- WriterT.valueT[F, Locals, UnionMatch](
                      doesMatch(lam, pat, mustMatch)
                    )
                    // if this is a total match, we don't need to do the getter at all
                    chain =
                      if (isWildMatch(um)) Chain.empty
                      else Chain.one((lam, getter(idx)))
                    _ <- WriterT.tell[F, Locals](chain)
                  } yield um
              }

            val ands: WriterT[F, Locals, List[UnionMatch]] =
              params.zipWithIndex
                .traverse { case (pati, i) => operate(pati, i) }

            ands.map(NonEmptyList.fromList(_) match {
              case None      => wildMatch
              case Some(nel) =>
                product(nel) { case ((l1, o1, b1), (l2, o2, b2)) =>
                  (l1 ::: l2, o1 && o2, b1 ::: b2)
                }
            })
          }

          def forStruct(size: Int) =
            asStruct(pos => GetStructElement(arg, pos, size)).run
              .map { case (anons, ums) =>
                ums.map { case (pre, cond, bind) =>
                  val pre1 = anons.foldLeft(pre) { case (pre, (a, _)) =>
                    a :: pre
                  }
                  // we have to set these variables before we can evaluate the condition
                  val cond1 = anons.foldLeft(cond) { case (c, (a, e)) =>
                    SetMut(a, e) && c
                  }
                  (pre1, cond1, bind)
                }
              }

          variantOf(pack, cname) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Struct(size)        => forStruct(size)
                case DataRepr.NewType             => forStruct(1)
                case DataRepr.Enum(vidx, size, f) =>
                  // if we match the variant, then treat it as a struct
                  val cv: BoolExpr[B] =
                    if (mustMatch) TrueConst
                    else CheckVariant(arg, vidx, size, f)
                  asStruct(pos => GetEnumElement(arg, vidx, pos, size)).run
                    .map { case (anons, ums) =>
                      if (isWildMatch(ums)) {
                        // we just need to check the variant
                        assert(
                          anons.isEmpty,
                          "anons must by construction always be empty on wildMatch"
                        )
                        NonEmptyList((Nil, cv, Nil), Nil)
                      } else {
                        // now we need to set up the binds if the variant is right
                        val cond1 = anons.foldLeft(cv) {
                          case (c, (mut, expr)) =>
                            c && SetMut(mut, expr)
                        }

                        ums.map { case (pre, cond, b) =>
                          val pre1 = anons.foldLeft(pre) {
                            case (pre, (mut, _)) => mut :: pre
                          }
                          (pre1, cond1 && cond, b)
                        }
                      }
                    }
                case DataRepr.ZeroNat =>
                  val cv: BoolExpr[B] =
                    if (mustMatch) TrueConst
                    else EqualsNat(arg, DataRepr.ZeroNat)
                  Monad[F].pure(NonEmptyList((Nil, cv, Nil), Nil))
                case DataRepr.SuccNat =>
                  params match {
                    case single :: Nil =>
                      // if we match, we recur on the inner pattern and prev of current
                      val check =
                        if (mustMatch) TrueConst
                        else EqualsNat(arg, DataRepr.SuccNat)

                      val ignoreInner = single match {
                        case Pattern.WildCard => true
                        case _                => false
                      }

                      if (!ignoreInner) {
                        for {
                          nm <- makeAnon
                          loc = LocalAnonMut(nm)
                          prev = PrevNat(arg)
                          rest <- doesMatch(loc, single, mustMatch)
                        } yield rest.map { case (preLets, cond, res) =>
                          (
                            loc :: preLets,
                            check && SetMut(loc, prev) && cond,
                            res
                          )
                        }
                      } else {
                        // we don't need to bind the prev
                        Monad[F].pure(wildMatch.map {
                          case (preLets, cond, res) =>
                            (
                              preLets,
                              check && cond,
                              res
                            )
                        })
                      }
                    case other =>
                      // $COVERAGE-OFF$
                      throw new IllegalStateException(
                        s"expected typechecked Nat to only have one param, found: $other in $pat"
                      )
                    // $COVERAGE-ON$
                  }
              }
            case None =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Union(h, ts) =>
          // note this list is exactly as long as h :: ts
          val unionMustMatch = NonEmptyList.fromListUnsafe(
            List.fill(ts.size)(false) ::: mustMatch :: Nil
          )
          ((h :: ts)
            .zip(unionMustMatch))
            .traverse { case (p, mm) => doesMatch(arg, p, mm) }
            .map { nene =>
              val nel = nene.flatten
              // at the first total match, we can stop
              stopAt(nel) {
                case (_, TrueConst, _) => true
                case _                 => false
              }
            }
      }
    }

    def lets(binds: List[(Bindable, Expr[B])], in: Expr[B]): Expr[B] =
      binds.foldRight(in) { case ((b, e), r) =>
        Let(b, e, r)
      }

    def letAnons(binds: List[(LocalAnon, Expr[B])], in: Expr[B]): Expr[B] =
      binds.foldRight(in) { case ((b, e), r) =>
        Let(Left(b), e, r)
      }

    def letMutAll(binds: List[LocalAnonMut], in: Expr[B]): Expr[B] =
      binds.foldRight(in) { case (anon, rest) =>
        // TODO: sometimes we generate code like
        // LetMut(x, Always(SetMut(x, y), f))
        // with no side effects in y or f
        // this would be better written as
        // Let(x, y, f)
        LetMut(anon, rest)
      }

    def setAll(ls: List[(LocalAnonMut, Expr[B])], ret: Expr[B]): Expr[B] =
      ls.foldRight(ret) { case ((l, e), r) =>
        Always(SetMut(l, e), r)
      }

    // A row in the pattern matrix: patterns for each occurrence,
    // a right-hand side, and the bindings accumulated so far.
    case class MatchRow(
        pats: List[Pattern[(PackageName, Constructor), Type]],
        rhs: Expr[B],
        binds: List[(Bindable, Expr[B])]
    ) {
      // Replace exactly one column's pattern with a new list of patterns.
      // We always patch with count == 1 because each column represents a
      // single occurrence; specialization removes or expands that one column.
      def patchPats(
          colIdx: Int,
          pats1: List[Pattern[(PackageName, Constructor), Type]]
      ): MatchRow =
        copy(pats = pats.patch(colIdx, pats1, 1))
    }

    // Head signatures represent refutable "shapes" we can branch on.
    // These correspond to Maranget's constructor/literal head cases.
    // See: https://compiler.club/compiling-pattern-matching/
    sealed trait HeadSig derives CanEqual
    case class EnumSig(
        ctor: (PackageName, Constructor),
        variant: Int,
        arity: Int,
        famArities: List[Int]
    ) extends HeadSig
    case class StructSig(ctor: (PackageName, Constructor), arity: Int)
        extends HeadSig
    case object ZeroSig extends HeadSig
    case object SuccSig extends HeadSig
    case class LitSig(lit: Lit) extends HeadSig

    // Normalize patterns to shrink the grammar into cases the matrix
    // algorithm handles (e.g., collapse simple string/list patterns).
    def normalizePattern(
        p: Pattern[(PackageName, Constructor), Type]
    ): Pattern[(PackageName, Constructor), Type] =
      p match {
        case Pattern.Annotation(p1, _) => normalizePattern(p1)
        case Pattern.Named(v, p1) =>
          Pattern.Named(v, normalizePattern(p1))
        case Pattern.Var(_) | Pattern.WildCard | Pattern.Literal(_) => p
        case sp @ Pattern.StrPat(_) =>
          sp.simplify match {
            case Some(p1) => normalizePattern(p1)
            case None     => sp
          }
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p1) => normalizePattern(p1)
            case Left(_)   =>
              val parts = lp.parts.map {
                case Pattern.ListPart.Item(p1) =>
                  Pattern.ListPart.Item(normalizePattern(p1))
                case g => g
              }
              Pattern.ListPat(parts)
          }
        case Pattern.PositionalStruct(n, ps) =>
          Pattern.PositionalStruct(n, ps.map(normalizePattern))
        case Pattern.Union(h, t) =>
          Pattern.Union(normalizePattern(h), t.map(normalizePattern))
      }

    // Non-orthogonal patterns (string globs, non-trailing list globs) have
    // backtracking/search semantics and are not handled by the matrix compiler.
    def isNonOrthogonal(
        p: Pattern[(PackageName, Constructor), Type]
    ): Boolean =
      p match {
        case Pattern.Annotation(p1, _) => isNonOrthogonal(p1)
        case Pattern.Named(_, p1)      => isNonOrthogonal(p1)
        case Pattern.Var(_) | Pattern.WildCard | Pattern.Literal(_) =>
          false
        case sp @ Pattern.StrPat(_) => sp.simplify.isEmpty
        case lp @ Pattern.ListPat(_) =>
          Pattern.ListPat.toPositionalStruct(lp, empty, cons) match {
            case Right(p1) => isNonOrthogonal(p1)
            case Left(_)   => true
          }
        case Pattern.PositionalStruct(_, ps) =>
          ps.exists(isNonOrthogonal)
        case Pattern.Union(h, t) =>
          isNonOrthogonal(h) || t.exists(isNonOrthogonal)
      }

    // Pull off aliases and bindings that capture the whole occurrence.
    // This keeps the matrix focused on refutable structure.
    // Return patterns are normalized and never Named/Var/Annotation.
    // For matrix compilation, the remaining cases are:
    // WildCard, Literal, PositionalStruct, or Union.
    def peelPattern(
        p: Pattern[(PackageName, Constructor), Type],
        occ: CheapExpr[B]
    ): (List[(Bindable, Expr[B])], Pattern[(PackageName, Constructor), Type]) =
      p match {
        case Pattern.Named(v, inner) =>
          val (bs, core) = peelPattern(inner, occ)
          ((v, occ) :: bs, core)
        case Pattern.Var(v) =>
          ((v, occ) :: Nil, Pattern.WildCard)
        case Pattern.Annotation(inner, _) =>
          peelPattern(inner, occ)
        case Pattern.WildCard | Pattern.Literal(_) |
            Pattern.PositionalStruct(_, _) | Pattern.Union(_, _) =>
          (Nil, p)
        case Pattern.StrPat(_) | Pattern.ListPat(_) =>
          // normalizePattern converts orthogonal list/string patterns into
          // PositionalStruct/Literal/WildCard, and isNonOrthogonal filters out
          // the remaining list/string search patterns before matrix compilation.
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"unexpected non-orthogonal pattern in peelPattern: $p"
          )
        // $COVERAGE-ON$
      }

    // Expand all top-level unions in the row's pattern vector.
    def expandRows(rows: List[MatchRow]): List[MatchRow] = {
      def expandPats(
          pats: List[Pattern[(PackageName, Constructor), Type]]
      ): List[List[Pattern[(PackageName, Constructor), Type]]] =
        pats match {
          case Nil => Nil :: Nil
          case p :: tail =>
            val ps = Pattern.flatten(p).toList
            for {
              p1 <- ps
              rest <- expandPats(tail)
            } yield p1 :: rest
        }

      rows.flatMap { row =>
        expandPats(row.pats).map(ps => row.copy(pats = ps))
      }
    }

    // Normalize a row by collapsing aliases into bindings and simplifying
    // patterns, without changing the left-to-right match semantics.
    // Invariant: row.pats.length == occs.length.
    def normalizeRow(
        row: MatchRow,
        occs: List[CheapExpr[B]]
    ): MatchRow = {
      val (binds, patsRev) =
        row.pats.iterator
          .zip(occs.iterator)
          .foldLeft((row.binds, List.empty[Pattern[(PackageName, Constructor), Type]])) {
            case ((bs, acc), (pat, occ)) =>
              val p1 = normalizePattern(pat)
              val (moreBinds, core) = peelPattern(p1, occ)
              (bs ::: moreBinds, core :: acc)
          }

      row.copy(pats = patsRev.reverse, binds = binds)
    }

    // Drop columns that are all wildcards to keep the matrix small.
    // Invariant: every row has the same arity as occs, and that arity is
    // preserved in the returned rows/occs.
    def dropWildColumns(
        rows: List[MatchRow],
        occs: List[CheapExpr[B]]
    ): (List[MatchRow], List[CheapExpr[B]]) = {
      val keepIdxs = occs.indices.filter { idx =>
        rows.exists(r => r.pats(idx) != Pattern.WildCard)
      }.toList

      if (keepIdxs.length == occs.length) (rows, occs)
      else {
        val newRows = rows.map { r =>
          val ps = keepIdxs.map(r.pats(_))
          r.copy(pats = ps)
        }
        val newOccs = keepIdxs.map(occs(_))
        (newRows, newOccs)
      }
    }

    // Compute the head signature for a refutable pattern in a column.
    def headSig(
        pat: Pattern[(PackageName, Constructor), Type]
    ): Option[HeadSig] =
      pat match {
        case Pattern.WildCard => None
        case Pattern.Literal(lit) =>
          Some(LitSig(lit))
        case Pattern.PositionalStruct((pack, cname), params) =>
          variantOf(pack, cname) match {
            case Some(DataRepr.Enum(v, s, f)) =>
              Some(EnumSig((pack, cname), v, s, f))
            case Some(DataRepr.Struct(s)) =>
              Some(StructSig((pack, cname), s))
            case Some(DataRepr.NewType) =>
              Some(StructSig((pack, cname), 1))
            case Some(DataRepr.ZeroNat) =>
              if (params.isEmpty) Some(ZeroSig)
              else {
                // $COVERAGE-OFF$
                throw new IllegalStateException(
                  s"ZeroNat should have 0 params, found: $params"
                )
                // $COVERAGE-ON$
              }
            case Some(DataRepr.SuccNat) =>
              if (params.length == 1) Some(SuccSig)
              else {
                // $COVERAGE-OFF$
                throw new IllegalStateException(
                  s"SuccNat should have 1 param, found: $params"
                )
                // $COVERAGE-ON$
              }
            case None =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"could not find $cname in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Annotation(_, _) | Pattern.Var(_) | Pattern.Named(_, _) |
            Pattern.StrPat(_) | Pattern.ListPat(_) | Pattern.Union(_, _) =>
          // headSig is only called on rows that have been normalized
          // (normalizeRow + peelPattern), had unions flattened
          // (expandRows), and are known to be orthogonal
          // (isNonOrthogonal == false). These cases should be eliminated
          // before headSig, so reaching them is an invariant violation.
          // $COVERAGE-OFF$
          throw new IllegalStateException(
            s"unexpected pattern in headSig: $pat"
          )
        // $COVERAGE-ON$
      }

    // Stable de-duplication to keep constructor cases in source order.
    def distinctInOrder[A](as: List[A]): List[A] = {
      val seen = scala.collection.mutable.HashSet.empty[A]
      val bldr = scala.collection.mutable.ListBuffer.empty[A]
      as.foreach { a =>
        if (!seen(a)) {
          seen.add(a)
          bldr.append(a)
        }
      }
      bldr.toList
    }

    // Specialize the matrix for a given head signature (constructor/literal),
    // producing the submatrix for that case.
    // Invariant: colIdx is in-bounds and each row has the same arity.
    def specializeRows(
        sig: HeadSig,
        rows: List[MatchRow],
        colIdx: Int,
        arity: Int
    ): List[MatchRow] = {
      val wilds: List[Pattern[(PackageName, Constructor), Type]] =
        List.fill(arity)(Pattern.WildCard)

      rows.flatMap { row =>
        val pat = row.pats(colIdx)

        pat match {
          case Pattern.WildCard =>
            row.patchPats(colIdx, wilds) :: Nil
          case Pattern.Literal(lit) =>
            sig match {
              case LitSig(l) if l === lit =>
                row.patchPats(colIdx, Nil) :: Nil
              case _ => Nil
            }
          case Pattern.PositionalStruct((pack, cname), params) =>
            variantOf(pack, cname) match {
              case Some(DataRepr.Enum(v, s, f)) =>
                sig match {
                  case EnumSig(_, v1, s1, f1)
                      if (v == v1) && (s == s1) && (f == f1) =>
                    row.patchPats(colIdx, params) :: Nil
                  case _ => Nil
                }
              case Some(DataRepr.Struct(s)) =>
                sig match {
                  case StructSig(_, s1) if s == s1 =>
                    row.patchPats(colIdx, params) :: Nil
                  case _ => Nil
                }
              case Some(DataRepr.NewType) =>
                sig match {
                  case StructSig(_, s1) if s1 == 1 =>
                    row.patchPats(colIdx, params) :: Nil
                  case _ => Nil
                }
              case Some(DataRepr.ZeroNat) =>
                sig match {
                  case ZeroSig if params.isEmpty =>
                    row.patchPats(colIdx, Nil) :: Nil
                  case _ => Nil
                }
              case Some(DataRepr.SuccNat) =>
                sig match {
                  case SuccSig if params.length == 1 =>
                    row.patchPats(colIdx, params) :: Nil
                  case _ => Nil
                }
              case None =>
                // $COVERAGE-OFF$
                throw new IllegalStateException(
                  s"could not find $cname in global data types"
                )
              // $COVERAGE-ON$
            }
          case Pattern.Var(_) | Pattern.Named(_, _) | Pattern.Annotation(_, _) |
              Pattern.StrPat(_) | Pattern.ListPat(_) | Pattern.Union(_, _) =>
            // These cases are eliminated before specialization:
            // - normalizeRow peels Var/Named/Annotation into bindings/wildcards.
            // - normalizePattern strips Annotation and converts list/string
            //   patterns where possible.
            // - expandRows flattens Union into separate rows.
            // - isNonOrthogonal excludes list/string search patterns.
            // Hitting this means the matrix invariants were violated.
            // $COVERAGE-OFF$
            throw new IllegalStateException(
              s"unexpected pattern in specializeRows: pat=$pat, sig=$sig, col=$colIdx, arity=$arity"
            )
          // $COVERAGE-ON$
        }
      }
    }

    // Legacy ordered compiler: compile each pattern into a BoolExpr and chain
    // Ifs. This preserves the semantics of non-orthogonal patterns.
    def matchExprOrderedCheap(
        arg: CheapExpr[B],
        branches: NonEmptyList[
          (Pattern[(PackageName, Constructor), Type], Expr[B])
        ]
    ): F[Expr[B]] = {
      def recur(
          arg: CheapExpr[B],
          branches: NonEmptyList[
            (Pattern[(PackageName, Constructor), Type], Expr[B])
          ]
      ): F[Expr[B]] = {
        val (p1Raw, r1) = branches.head
        // Normalize to simplify list/string patterns while preserving
        // ordered semantics for non-orthogonal matches.
        val p1 = normalizePattern(p1Raw)

        def loop(
            cbs: NonEmptyList[
              (List[LocalAnonMut], BoolExpr[B], List[(Bindable, Expr[B])])
            ]
        ): F[Expr[B]] =
          cbs match {
            case NonEmptyList((b0, TrueConst, binds), _) =>
              // this is a total match, no fall through
              val right = lets(binds, r1)
              Monad[F].pure(letMutAll(b0, right))
            case NonEmptyList((b0, cond, binds), others) =>
              val thisBranch = lets(binds, r1)
              val res = others match {
                case oh :: ot =>
                  loop(NonEmptyList(oh, ot)).map { te =>
                    If(cond, thisBranch, te)
                  }
                case Nil =>
                  branches.tail match {
                    case Nil =>
                      // this must be total, but we still need
                      // to evaluate cond since it can have side
                      // effects
                      Monad[F].pure(always(cond, thisBranch))
                    case bh :: bt =>
                      recur(arg, NonEmptyList(bh, bt)).map { te =>
                        If(cond, thisBranch, te)
                      }
                  }
              }

              res.map(letMutAll(b0, _))
          }

        doesMatch(arg, p1, branches.tail.isEmpty).flatMap(loop)
      }

      recur(arg, branches)
    }

    def matchExprOrdered(
        arg: Expr[B],
        tmp: F[Long],
        branches: NonEmptyList[
          (Pattern[(PackageName, Constructor), Type], Expr[B])
        ]
    ): F[Expr[B]] = {
      val argFn = maybeMemo(tmp)(matchExprOrderedCheap(_, branches))
      argFn(arg)
    }

    // Compile matches with the Maranget pattern-matrix algorithm:
    // 1) Represent branches as a matrix of patterns (rows) over occurrences.
    // 2) Pick a column, branch on its head constructors/literals.
    // 3) Specialize the matrix per case, and recurse; default to wildcards.
    // This yields a shared decision tree without adding new Matchless nodes.
    //
    // Efficiency notes: compared to the ordered chain, the matrix compiler
    // shares tests across branches and avoids re-checking the scrutinee, which
    // tends to reduce redundant work for larger matches. This approach is the
    // standard state-of-the-art in ML-family compilers, but it is not
    // globally optimal: finding the minimal decision tree is hard, and the
    // quality depends on the column-selection heuristic (we use leftmost).
    // See: https://compiler.club/compiling-pattern-matching/
    def matchExprMatrixCheap(
        arg: CheapExpr[B],
        branches: NonEmptyList[
          (Pattern[(PackageName, Constructor), Type], Expr[B])
        ]
    ): F[Expr[B]] = {
      val rows0 = branches.toList.map { case (p, e) =>
        MatchRow(p :: Nil, e, Nil)
      }

      // Compile a pattern matrix into Matchless Expr by building a decision tree.
      // rowsIn: matrix rows (each row has a pattern per column + rhs + binds)
      // occsIn: occurrences for each column (the scrutinee/projection expression)
      // Invariants: every row has the same arity, and
      // rowsIn.forall(r => r.pats.length == occsIn.length).
      // Also, rowsIn order matches source branch order (left-to-right).
      def compileRows(
          rowsIn: List[MatchRow],
          occsIn: List[CheapExpr[B]]
      ): F[Expr[B]] = {
        val norm = rowsIn.map(normalizeRow(_, occsIn))
        val expanded = expandRows(norm)
        val (rows, occs) = dropWildColumns(expanded, occsIn)

        rows match {
          case Nil =>
            // this should be impossible in well-typed code
            Monad[F].pure(UnitExpr)
          case MatchRow(p0, r0, b0) :: _ if p0.forall(_ == Pattern.WildCard) =>
            Monad[F].pure(lets(b0, r0))
          case _ =>
            // Column selection: preserve left-to-right behavior by defaulting
            // to the leftmost column. (Heuristics can be added later.)
            val colIdx = 0

            val occ = occs(colIdx)

            val sigs = distinctInOrder(
              rows.flatMap(r => headSig(r.pats(colIdx)).toList)
            )

            val defaultRows =
              rows
                .filter(r => r.pats(colIdx) == Pattern.WildCard)
                .map(r => r.patchPats(colIdx, Nil))

            val defaultOccs = occs.patch(colIdx, Nil, 1)

            // Build the Matchless condition + specialized submatrix for one case.
            def buildCase(
                sig: HeadSig
            ): F[
              (
                  BoolExpr[B],
                  List[LocalAnonMut],
                  List[MatchRow],
                  List[CheapExpr[B]]
              )
            ] =
              sig match {
                case EnumSig(_, v, s, f) =>
                  val newRows = specializeRows(sig, rows, colIdx, s)
                  val fields =
                    (0 until s).toList.map(i =>
                      GetEnumElement(occ, v, i, s)
                    )
                  val newOccs = occs.patch(colIdx, fields, 1)
                  Monad[F].pure(
                    (
                      CheckVariant(occ, v, s, f),
                      Nil,
                      newRows,
                      newOccs
                    )
                  )
                case StructSig(_, s) =>
                  val newRows = specializeRows(sig, rows, colIdx, s)
                  val fields =
                    (0 until s).toList.map(i =>
                      GetStructElement(occ, i, s)
                    )
                  val newOccs = occs.patch(colIdx, fields, 1)
                  Monad[F].pure((TrueConst, Nil, newRows, newOccs))
                case LitSig(lit) =>
                  val newRows = specializeRows(sig, rows, colIdx, 0)
                  val newOccs = occs.patch(colIdx, Nil, 1)
                  Monad[F].pure((EqualsLit(occ, lit), Nil, newRows, newOccs))
                case ZeroSig =>
                  val newRows = specializeRows(sig, rows, colIdx, 0)
                  val newOccs = occs.patch(colIdx, Nil, 1)
                  Monad[F].pure(
                    (EqualsNat(occ, DataRepr.ZeroNat), Nil, newRows, newOccs)
                  )
                case SuccSig =>
                  makeAnon.map { nm =>
                    val mut = LocalAnonMut(nm)
                    val cond =
                      EqualsNat(occ, DataRepr.SuccNat) && SetMut(
                        mut,
                        PrevNat(occ)
                      )
                    val newRows = specializeRows(sig, rows, colIdx, 1)
                    val newOccs = occs.patch(colIdx, mut :: Nil, 1)
                    (cond, mut :: Nil, newRows, newOccs)
                  }
              }

            // Compile cases in order, with a default branch for wildcards.
            def compileCases(sigs: List[HeadSig]): F[Expr[B]] =
              sigs match {
                case Nil =>
                  if (defaultRows.nonEmpty) compileRows(defaultRows, defaultOccs)
                  else Monad[F].pure(UnitExpr)
                case sig :: rest =>
                  buildCase(sig).flatMap {
                    case (cond, preLets, newRows, newOccs) =>
                      if (newRows.isEmpty) compileCases(rest)
                      else {
                        compileRows(newRows, newOccs).flatMap { thenExpr =>
                          val hasElse =
                            rest.nonEmpty || defaultRows.nonEmpty

                          val branchF: F[Expr[B]] =
                            if (cond == TrueConst) Monad[F].pure(thenExpr)
                            else if (!hasElse) Monad[F].pure(always(cond, thenExpr))
                            else
                              compileCases(rest).map { elseExpr =>
                                If(cond, thenExpr, elseExpr)
                              }

                          branchF.map(letMutAll(preLets, _))
                        }
                      }
                  }
              }

            compileCases(sigs)
        }
      }

      compileRows(rows0, arg :: Nil)
    }

    def matchExprMatrix(
        arg: Expr[B],
        tmp: F[Long],
        branches: NonEmptyList[
          (Pattern[(PackageName, Constructor), Type], Expr[B])
        ]
    ): F[Expr[B]] = {
      val argFn = maybeMemo(tmp)(matchExprMatrixCheap(_, branches))
      argFn(arg)
    }

    // Use the matrix compiler for an orthogonal prefix and fall back to the
    // ordered compiler for a non-orthogonal suffix (string/list search
    // semantics). A small threshold avoids overhead on tiny matches.
    def matchExpr(
        arg: Expr[B],
        tmp: F[Long],
        branches: NonEmptyList[
          (Pattern[(PackageName, Constructor), Type], Expr[B])
        ]
    ): F[Expr[B]] = {
      val branchList = branches.toList
      val (orthoPrefix, nonOrthoSuffix) =
        branchList.span { case (p, _) => !isNonOrthogonal(p) }
      // Heuristic: only pay the matrix setup cost if we can prune a few
      // orthogonal cases before falling back.
      val orthoThreshold = 4

      val argFn = maybeMemo(tmp) { (arg: CheapExpr[B]) =>
        nonOrthoSuffix match {
          case Nil =>
            matchExprMatrixCheap(arg, branches)
          case _ if orthoPrefix.length >= orthoThreshold =>
            val suffixNel = NonEmptyList.fromListUnsafe(nonOrthoSuffix)
            matchExprOrderedCheap(arg, suffixNel).flatMap { fallbackExpr =>
              val combined = orthoPrefix :+ (Pattern.WildCard, fallbackExpr)
              val combinedNel = NonEmptyList.fromListUnsafe(combined)
              matchExprMatrixCheap(arg, combinedNel)
            }
          case _ =>
            matchExprOrderedCheap(arg, branches)
        }
      }

      argFn(arg)
    }

    loopLetVal(name, te, rec, LambdaState(None, Map.empty))
  }

  // toy matcher to see the structure
  // Left means match any number of items, like *_
  def matchList[A, B: Monoid](
      items: List[A],
      pattern: List[Either[List[A] => B, A => Option[B]]]
  ): Option[B] =
    pattern match {
      case Nil =>
        if (items.isEmpty) Some(Monoid[B].empty)
        else None
      case Right(fn) :: pt =>
        items match {
          case ih :: it =>
            fn(ih) match {
              case None    => None
              case Some(b) => matchList(it, pt).map(Monoid[B].combine(b, _))
            }
          case Nil => None
        }

      case Left(lstFn) :: Nil =>
        Some(lstFn(items))

      case Left(lstFn) :: (pt @ (Left(_) :: _)) =>
        // it is ambiguous how much to absorb
        // so, just assume lstFn gets nothing
        matchList(items, pt)
          .map(Monoid.combine(lstFn(Nil), _))

      case Left(lstFn) :: (pt @ (Right(_) :: _)) =>
        var revLeft: List[A] = Nil
        var it = items
        var result: Option[B] = None
        // this cannot match an empty list
        while (result.isEmpty && it.nonEmpty) {
          matchList(it, pt) match {
            case Some(rb) =>
              val leftB = lstFn(revLeft.reverse)
              result = Some(Monoid[B].combine(leftB, rb))
            case None =>
              revLeft = it.head :: revLeft
              it = it.tail
          }
        }
        result
      /*
       * The above should be an imperative version
       * of this code. The imperative code
       * is easier to translate into low level
       * instructions
        items
          .to(LazyList)
          .mapWithIndex { (a, idx) => afn(a).map((_, idx)) }
          .collect { case Some(m) => m }
          .flatMap { case (b, idx) =>
            val left = items.take(idx)
            val leftB = Monoid[B].combine(lstFn(left), b)

            val right = items.drop(idx + 1)
            matchList(right, pt)
              .map { br =>
                Monoid[B].combine(leftB, br)
              }
          }
          .headOption
       */
    }

  /** return the expanded product of sums
    */
  def product[A1](
      sum: NonEmptyList[NonEmptyList[A1]]
  )(prod: (A1, A1) => A1): NonEmptyList[A1] =
    sum match {
      case NonEmptyList(h, Nil) =>
        // this (a1 + a2 + a3) case
        h
      case NonEmptyList(h0, h1 :: tail) =>
        val rightProd = product(NonEmptyList(h1, tail))(prod)
        // (a0 + a1 + ...) * rightProd
        // = a0 * rightProd + a1 * rightProd + ...
        for {
          ai <- h0
          r <- rightProd
        } yield prod(ai, r)
    }

}
