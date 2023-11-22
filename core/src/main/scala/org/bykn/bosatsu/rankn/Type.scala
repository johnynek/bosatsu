package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import cats.parse.{Parser => P, Numbers}
import cats.{Applicative, Monad, Order}
import org.typelevel.paiges.{Doc, Document}
import org.bykn.bosatsu.{Kind, PackageName, Lit, TypeName, Identifier, Parser, TypeParser}
import org.bykn.bosatsu.graph.Memoize.memoizeDagHashedConcurrent
import scala.collection.immutable.SortedSet

import cats.implicits._

sealed abstract class Type {
  def sameAs(that: Type): Boolean = Type.sameType(this, that)
}

object Type {
  /**
   * A type with no top level quantification
   */
  sealed abstract class Rho extends Type
  
  object Rho {
    implicit val orderRho: Order[Rho] =
      new Order[Rho] {
        def compare(a: Rho, b: Rho): Int =
          (a, b) match {
            case (TyConst(Const.Defined(p0, n0)), TyConst(Const.Defined(p1, n1))) =>
              val c = Ordering[PackageName].compare(p0, p1)
              if (c == 0) Ordering[TypeName].compare(n0, n1) else c
            case (TyConst(_), _) => -1
            case (TyVar(v0), TyVar(v1)) =>
              Ordering[Var].compare(v0, v1)
            case (TyVar(_), TyConst(_)) => 1
            case (TyVar(_), _) => -1
            case (TyMeta(m0), TyMeta(m1)) =>
              Meta.orderingMeta.compare(m0, m1)
            case (TyMeta(_), TyApply(_, _)) => -1
            case (TyMeta(_), _) => 1
            case (TyApply(a0, b0), TyApply(a1, b1)) =>
              val c = Type.typeOrder.compare(a0, a1)
              if (c == 0) Type.typeOrder.compare(b0, b1) else c
            case (TyApply(_, _), _) => 1
        }
      }

    implicit val orderingRho: Ordering[Rho] = orderRho.toOrdering
  }

  sealed abstract class Leaf extends Rho
  type Tau = Rho // no forall or exists anywhere

  sealed abstract class Quantification {
    def vars: NonEmptyList[(Var.Bound, Kind)]
    def existList: List[(Var.Bound, Kind)]
    def forallList: List[(Var.Bound, Kind)]
    def concat(that: Quantification): Quantification

    def filter(fn: Var.Bound => Boolean): Option[Quantification] =
      Quantification.fromLists(
        forallList.filter { case (b, _) => fn(b) },
        existList.filter { case (b, _) => fn(b) })
  }

  object Quantification {
    case class ForAll(vars: NonEmptyList[(Var.Bound, Kind)]) extends Quantification {
      def existList: List[(Var.Bound, Kind)] = Nil
      def forallList: List[(Var.Bound, Kind)] = vars.toList
      def concat(that: Quantification): Quantification =
        that match {
          case ForAll(vars1) => ForAll(vars ::: vars1)
          case Exists(evars) => Dual(vars, evars)
          case Dual(f, e) => Dual(vars ::: f, e)
        }
    }
    case class Exists(vars: NonEmptyList[(Var.Bound, Kind)]) extends Quantification {
      def existList: List[(Var.Bound, Kind)] = vars.toList
      def forallList: List[(Var.Bound, Kind)] = Nil
      def concat(that: Quantification): Quantification =
        that match {
          case ForAll(vars1) => Dual(vars1, vars)
          case Exists(evars) => Exists(vars ::: evars)
          case Dual(f, e) => Dual(f, vars ::: e)
        }
    }
    case class Dual(
      foralls: NonEmptyList[(Var.Bound, Kind)],
      exists: NonEmptyList[(Var.Bound, Kind)]) extends Quantification {

      lazy val vars = foralls ::: exists
      def existList: List[(Var.Bound, Kind)] = exists.toList
      def forallList: List[(Var.Bound, Kind)] = foralls.toList
      def concat(that: Quantification): Quantification =
        that match {
          case ForAll(vars1) => Dual(foralls ::: vars1, exists)
          case Exists(evars) => Dual(foralls, exists ::: evars)
          case Dual(f, e) => Dual(foralls ::: f, exists ::: e)
        }
    }

    implicit val quantificationOrder: Order[Quantification] =
      new Order[Quantification] {
        val nelist = Order[NonEmptyList[(Var.Bound, Kind)]]

        def compare(a: Quantification, b: Quantification): Int =
          (a, b) match {
            case (ForAll(v0), ForAll(v1)) => nelist.compare(v0, v1)
            case (ForAll(_), _) => -1
            case (Exists(_), ForAll(_)) => 1
            case (Exists(v0), Exists(v1)) => nelist.compare(v0, v1)
            case (Exists(_), _) => -1
            case (Dual(fa0, ex0), Dual(fa1, ex1)) =>
              val c1 = nelist.compare(fa0, fa1)
              if (c1 != 0) c1
              else nelist.compare(ex0, ex1)
            case (Dual(_, _), _) => 1
          }
        }

    def fromLists(forallList: List[(Var.Bound, Kind)], existList: List[(Var.Bound, Kind)]): Option[Quantification] =
      forallList match {
        case Nil =>
          NonEmptyList.fromList(existList).map(Exists(_))
        case head :: tail => 
          Some(existList match {
            case Nil => ForAll(NonEmptyList(head, tail))
            case eh :: et => Dual(NonEmptyList(head, tail), NonEmptyList(eh, et))
          })
      }
  }

  case class Quantified(quant: Quantification, in: Rho) extends Type {
    def vars: NonEmptyList[(Var.Bound, Kind)] = quant.vars
    def existList: List[(Var.Bound, Kind)] = quant.existList
    def forallList: List[(Var.Bound, Kind)] = quant.forallList
  }

  object Quantified {
    implicit val quantifiedOrder: Order[Quantified] =
      new Order[Quantified] {
        def compare(a: Quantified, b: Quantified): Int = {
          val c = Order[Quantification].compare(a.quant, b.quant)
          if (c == 0) Order[Rho].compare(a.in, b.in)
          else c
        }
      }
  }

  case class TyApply(on: Type, arg: Type) extends Rho
  case class TyConst(tpe: Const) extends Leaf
  case class TyVar(toVar: Var) extends Leaf
  case class TyMeta(toMeta: Meta) extends Leaf

  def sameType(left: Type, right: Type): Boolean =
    left match {
      case leftLeaf: Leaf =>
        // a Leaf is never equal to TyApply
        right match {
          case rightLeaf: Leaf => leftLeaf == rightLeaf
          case _: TyApply => false
          case q: Quantified => leftLeaf == normalize(q)
        }
      case _: TyApply =>
        if (right.isInstanceOf[Leaf]) false
        else {
          // left and right are not leafs
          normalize(left) == normalize(right)
        }
      case _ =>
        // this is the quantified case
        normalize(left) == normalize(right)
    }

  implicit val typeOrder: Order[Type] =
    new Order[Type] {
      def compare(a: Type, b: Type): Int =
        (a, b) match {
          case (arho: Rho, brho: Rho) =>
            Rho.orderRho.compare(arho, brho)
          case (_: Rho, _) => -1
          case (aq: Quantified, bq: Quantified) =>
            Quantified.quantifiedOrder.compare(aq, bq)
          case (_: Quantified, _) => 1
        }
    }

  implicit val typeOrdering: Ordering[Type] = typeOrder.toOrdering

  @annotation.tailrec
  def applyAll(fn: Type, args: List[Type]): Type =
    args match {
      case Nil => fn
      case a :: as =>
        applyAll(TyApply(fn, a), as)
    }

  def unapplyAll(fn: Type): (Type, List[Type]) = {
    @annotation.tailrec
    def loop(fn: Type, acc: List[Type]): (Type, List[Type]) =
      fn match {
        case TyApply(fn, a) => loop(fn, a :: acc)
        case notApply => (notApply, acc)
      }

    loop(fn, Nil)
  }

  object ForAll {
    def unapply(t: Type): Option[(NonEmptyList[(Type.Var.Bound, Kind)], Type)] =
      t match {
        case _: Rho => None
        case q: Quantified =>
          q.quant match {
            case Quantification.ForAll(vars) => Some((vars, q.in))
            case Quantification.Dual(foralls, existsNel) => Some((foralls, exists(existsNel, q.in)))
            case _ => None
          }
      }
  }

  object Exists {
    def unapply(t: Type): Option[(NonEmptyList[(Type.Var.Bound, Kind)], Type)] =
      t match {
        case _: Rho => None
        case q: Quantified =>
          q.quant match {
            case Quantification.Exists(vars) => Some((vars, q.in))
            case Quantification.Dual(foralls, existsNel) => Some((existsNel, forAll(foralls, q.in)))
            case _ => None
          }
      }
  }

  def constantsOf(t: Type): List[Const] =
    t match {
      case Quantified(_, t) => constantsOf(t)
      case TyApply(on, arg) => constantsOf(on) ::: constantsOf(arg)
      case TyConst(c) => c :: Nil
      case TyVar(_) | TyMeta(_) => Nil
    }

  def hasNoVars(t: Type): Boolean =
    t match {
      case TyConst(_) => true
      case TyVar(_) | TyMeta(_) => false
      case TyApply(on, arg) => hasNoVars(on) && hasNoVars(arg)
      case q: Quantified => freeTyVars(q :: Nil).isEmpty
    }

  final def forAll(vars: List[(Var.Bound, Kind)], in: Type): Type =
    NonEmptyList.fromList(vars) match {
      case None => in
      case Some(ne) => forAll(ne, in)
    }

  final def forAll(vars: NonEmptyList[(Var.Bound, Kind)], in: Type): Type.Quantified =
    in match {
      case rho: Rho =>
        Quantified(Quantification.ForAll(vars), rho)
      case q: Quantified =>
        q.quant match {
          case Quantification.ForAll(ne1) =>
            Quantified(Quantification.ForAll(vars ::: ne1), q.in)
          case Quantification.Exists(ne1) =>
            Quantified(Quantification.Dual(foralls = vars, exists = ne1), q.in)
          case Quantification.Dual(fa0, e) =>
            Quantified(Quantification.Dual(foralls = vars ::: fa0, exists = e), q.in)
        }
    }

  final def exists(vars: NonEmptyList[(Var.Bound, Kind)], in: Type): Type.Quantified =
    in match {
      case rho: Rho =>
        Quantified(Quantification.Exists(vars), rho)
      case q: Quantified =>
        q.quant match {
          case Quantification.Exists(ne1) =>
            Quantified(Quantification.Exists(vars ::: ne1), q.in)
          case Quantification.ForAll(ne1) =>
            Quantified(Quantification.Dual(foralls = ne1, exists = vars), q.in)
          case Quantification.Dual(fa0, e) =>
            Quantified(Quantification.Dual(foralls = fa0, exists = vars ::: e), q.in)
        }
    }

  final def exists(vars: List[(Var.Bound, Kind)], in: Type): Type =
    vars match {
      case h :: t => exists(NonEmptyList(h, t), in)
      case Nil => in
    }

  final def quantify(forallList: List[(Var.Bound, Kind)], existList: List[(Var.Bound, Kind)], in: Type): Type =
    Quantification.fromLists(forallList = forallList, existList = existList) match {
      case Some(q) => quantify(q, in)
      case None => in
    }

  final def quantify(quantification: Quantification, tpe: Type): Type.Quantified =
    quantification match {
      case Quantification.ForAll(vars) => forAll(vars, tpe)
      case Quantification.Exists(vars) => exists(vars, tpe)
      case Quantification.Dual(fa, ex) => forAll(fa, exists(ex, tpe))
    }

  def getTypeOf(lit: Lit): Type =
    lit match {
      case Lit.Integer(_) => Type.IntType
      case Lit.Str(_) => Type.StrType
      case Lit.Chr(_) => Type.CharType
    }

  /**
   * types are var, meta, or const, or applied or forall on one of
   * those. This returns the Type.TyConst found
   * by recursing
   */
  @annotation.tailrec
  final def rootConst(t: Type): Option[Type.TyConst] =
    t match {
      case tyc@TyConst(_) => Some(tyc)
      case TyVar(_) | TyMeta(_) => None
      case TyApply(left, _) => rootConst(left)
      case q: Quantified => rootConst(q.in)
    }

  object RootConst {
    def unapply(t: Type): Option[Type.TyConst] =
      rootConst(t)
  }

  def applicationArgs(t: Type): (Type, List[Type]) = {
    @annotation.tailrec
    def loop(t: Type, tail: List[Type]): (Type, List[Type]) =
      t match {
        case TyApply(left, right) => loop(left, right :: tail)
        case notApply => (notApply, tail)
      }
    loop(t, Nil)
  }

  /**
   * This form is often useful in Infer
   */
  def substTy(keys: NonEmptyList[Var], vals: NonEmptyList[Type]): Type => Type = {
    val env = keys.toList.iterator.zip(vals.toList.iterator).toMap

    { t => substituteVar(t, env) }
  }

  def substituteVar(t: Type, env: Map[Type.Var, Type]): Type =
    if (env.isEmpty) t
    else (t match {
      case TyApply(on, arg) => TyApply(substituteVar(on, env), substituteVar(arg, env))
      case v@TyVar(n) =>
        env.get(n) match {
          case Some(rho) => rho
          case None => v
        }
      case m@TyMeta(_) => m
      case c@TyConst(_) => c
      case q: Quantified =>
        val boundSet = q.vars.iterator.map(_._1).toSet[Type.Var]
        val env1 = env.iterator.filter { case (v, _) => !boundSet(v) }.toMap
        val subin = substituteVar(q.in, env1)
        quantify(q.quant, subin)
    })

  def substituteRhoVar(t: Type.Rho, env: Map[Type.Var, Type.Rho]): Type.Rho =
    t match {
      case TyApply(on, arg) => TyApply(substituteVar(on, env), substituteVar(arg, env))
      case v@TyVar(n) =>
        env.get(n) match {
          case Some(rho) => rho
          case None => v
        }
      case m@TyMeta(_) => m
      case c@TyConst(_) => c
    }

  /**
   * Return the Bound and Skolem variables that
   * are free in the given list of types
   */
  def freeTyVars(ts: List[Type]): List[Type.Var] = {

    // usually we can recurse in a loop, but sometimes not
    def cheat(ts: List[Type], bound: Set[Type.Var.Bound], acc: List[Type.Var]): List[Type.Var] =
      go(ts, bound, acc)

    @annotation.tailrec
    def go(ts: List[Type], bound: Set[Type.Var.Bound], acc: List[Type.Var]): List[Type.Var] =
      ts match {
        case Nil => acc
        case Type.TyVar(tv) :: rest =>
          // we only check here, we don't add
          val isBound =
            tv match {
              case b@Type.Var.Bound(_) => bound(b)
              case _: Type.Var.Skolem => false
            }
          if (isBound) go(rest, bound, acc)
          else go(rest, bound, tv :: acc)
        case Type.TyApply(a, b) :: rest => go(a :: b :: rest, bound, acc)
        case (Type.TyMeta(_) | Type.TyConst(_)) :: rest => go(rest, bound, acc)
        case (q: Quantified) :: rest =>
          val acc1 = cheat(q.in :: Nil, bound ++ q.vars.toList.iterator.map(_._1), acc)
          // note, q.vars ARE NOT bound in rest
          go(rest, bound, acc1)
      }

    go(ts, Set.empty, Nil)
      .reverse
      .distinct
  }

  /**
   * Return the Bound variables that
   * are free in the given list of types
   */
  def freeBoundTyVars(ts: List[Type]): List[Type.Var.Bound] =
    freeTyVars(ts).collect { case b@Type.Var.Bound(_) => b }

  def normalize(tpe: Type): Type =
    tpe match {
      case q: Quantified =>
        val foralls = q.forallList
        val exists = q.existList
        val in = q.in
          
        val inFree = freeBoundTyVars(in :: Nil)
        // sort the quantification by the order of appearance
        val order = inFree.iterator.zipWithIndex.toMap
        val inFreeSet = inFree.toSet
        val fa1 = foralls
          .filter { case (b, _) => inFreeSet(b) }
          .sortBy { case (b, _) => order(b) }

        val ex1 = exists
          .filter { case (b, _) => inFreeSet(b) }
          .sortBy { case (b, _) => order(b) }

        val frees = freeBoundTyVars(tpe :: Nil).toSet
        val bs = alignBinders(fa1 ::: ex1, frees)
        val subMap =
          bs.map { case ((bold, _), bnew) =>
            bold -> TyVar(bnew)
          }
          .toMap[Type.Var, Type.Rho]

        val newVars = bs.map { case ((_, k), b) => (b, k) }
        val normin = normalize(substituteRhoVar(in, subMap))
        val forAllSize = fa1.size
        val normfas = newVars.take(forAllSize)
        val normexs = newVars.drop(forAllSize)
        quantify(forallList = normfas, existList = normexs, normin)
      case TyApply(on, arg) => TyApply(normalize(on), normalize(arg))
      case _ => tpe
    }
  
  def kindOfOption(
    cons: TyConst => Option[Kind]
  ): Type => Option[Kind] = {
    val unknown: Either[Unit, Kind] = Left(())
    val consE = (tc: TyConst) => cons(tc).fold(unknown)(Right(_))
    val fn = kindOf[Unit](_ => (), _ => (), (_, _, _) => (), consE)
    
    fn.andThen {
      case Right(kind) => Some(kind)
      case Left(_) => None
    }
  }

  def kindOf[A](
    unknownVar: Var.Bound => A,
    invalidApply: TyApply => A,
    kindSubsumeError: (TyApply, Kind.Cons, Kind) => A,
    cons: TyConst => Either[A, Kind],
  ): Type => Either[A, Kind] = {

    val fn = memoizeDagHashedConcurrent[(Type, Map[Var.Bound, Kind]), Either[A, Kind]] { case ((tpe, locals), rec) =>
      tpe match {
        case Type.TyVar(b @ Type.Var.Bound(_)) =>
          locals.get(b) match {
            case Some(k) => Right(k)
            // $COVERAGE-OFF$ this should be unreachable because all vars should have a known kind
            case None => Left(unknownVar(b))
            // $COVERAGE-ON$ this should be unreachable
          }
        case Type.TyVar(Type.Var.Skolem(_, kind, _, _)) => Right(kind)
        case Type.TyMeta(Type.Meta(kind, _, _, _)) => Right(kind)
        case tc@Type.TyConst(_) => cons(tc)
        case ap@Type.TyApply(left, right) =>
          rec((left, locals))
            .product(rec((right, locals)))
            .flatMap {
              case (leftKind, rhs) =>
                Kind.validApply[A](leftKind, rhs, invalidApply(ap))(kindSubsumeError(ap, _, rhs))
            }
        case q: Quantified =>
          val varList = q.vars.toList
          rec((q.in, locals ++ varList))
      }
    }

    { t => fn((t, Map.empty)) }
  }
  /**
   * These are upper-case to leverage scala's pattern
   * matching on upper-cased vals
   */
  val BoolType: Type.TyConst = TyConst(Const.predef("Bool"))
  val DictType: Type.TyConst = TyConst(Const.predef("Dict"))

  object FnType {
    final val MaxSize = 32

    private def predefFn(n: Int) = TyConst(Const.predef(s"Fn$n")) 
    private val tpes = (1 to MaxSize).map(predefFn)

    object ValidArity {
      def unapply(n: Int): Boolean =
        (1 <= n) && (n <= MaxSize)
    }

    def apply(n: Int): Type.TyConst = {
      require(ValidArity.unapply(n), s"invalid FnType arity = $n, must be 0 < n <= $MaxSize")
      tpes(n - 1)
    }

    def maybeFakeName(n: Int): Type.TyConst =
      if (n <= MaxSize) apply(n)
      else {
        // This type doesn't exist but we will catch it in typechecking etc...
        predefFn(n)
      }

    def unapply(tpe: Type): Option[(Type.TyConst, Int)] = {
      tpe match {
        case Type.TyConst(Const.Predef(cons)) if (cons.asString.startsWith("Fn")) =>
          var idx = 0
          while (idx < MaxSize) {
            val thisTpe = tpes(idx)
            if (thisTpe == tpe) return Some((thisTpe, idx + 1))
            idx = idx + 1
          }
          None
        case _ => None
      }
    }

    // FnType -> Kind(Kind.Type.contra, Kind.Type.co),
    val FnKinds: List[(Type.TyConst, Kind)] = {
      // -* -> -* ... -> +* -> *
      def kindSize(n: Int): Kind =
        Kind((Vector.fill(n)(Kind.Type.contra) :+ Kind.Type.co): _*)

      tpes
        .iterator
        .zipWithIndex
        .map { case (t, n1) => (t, kindSize(n1 + 1)) }
        .toList
    }
  }
  val IntType: Type.TyConst = TyConst(Const.predef("Int"))
  val ListType: Type.TyConst = TyConst(Const.predef("List"))
  val OptionType: Type.TyConst = TyConst(Const.predef("Option"))
  val StrType: Type.TyConst = TyConst(Const.predef("String"))
  val CharType: Type.TyConst = TyConst(Const.predef("Char"))
  val TestType: Type.TyConst = TyConst(Const.predef("Test"))
  val TupleConsType: Type.TyConst = TyConst(Type.Const.predef("TupleCons"))
  val UnitType: Type.TyConst = TyConst(Type.Const.predef("Unit"))

  val builtInKinds: Map[Type.Const.Defined, Kind] =
    (FnType.FnKinds ::: List(
      BoolType -> Kind.Type,
      DictType -> Kind(Kind.Type.in, Kind.Type.co),
      IntType -> Kind.Type,
      ListType -> Kind(Kind.Type.co),
      StrType -> Kind.Type,
      CharType -> Kind.Type,
      UnitType -> Kind.Type,
      TupleConsType -> Kind(Kind.Type.co, Kind.Type.co),
    ))
    .map { case (t, k) => (t.tpe.toDefined, k) }
    .toMap

  def const(pn: PackageName, name: TypeName): Type =
    TyConst(Type.Const.Defined(pn, name))

  object Fun {
    def ifValid(from: NonEmptyList[Type], to: Type): Option[Type.Rho] = {
      val len = from.length
      if (len <= FnType.MaxSize)
        Some(apply(from, to))
      else None
    }

    def unapply(t: Type): Option[(NonEmptyList[Type], Type)] = {
      def check(n: Int, t: Type, applied: List[Type], last: Type): Option[(NonEmptyList[Type], Type)] =
        t match {
          case TyApply(inner, arg) =>
            check(n + 1, inner, arg :: applied, last)
          case FnType((_, arity)) if n == (arity + 1) =>
            // we need arity types and 1 result type
            // we know applied has length == n and arity in [1, MaxSize]
            val args = NonEmptyList.fromListUnsafe(applied)
            Some((args, last))
          case _ => None
        }

      t match {
        case TyApply(inner, last) =>
          check(1, inner, Nil, last)
        case _ => None
      }
    }

    def apply(from: NonEmptyList[Type], to: Type): Type.Rho = {
      val arityFn = FnType.maybeFakeName(from.length)
      val withArgs = from.foldLeft(arityFn: Type)(TyApply(_, _))
      TyApply(withArgs, to)
    }
    def apply(from: Type, to: Type): Type.Rho =
      apply(NonEmptyList.one(from), to)

    def arity(t: Type): Int =
      t match {
        case Quantified(_, t) => arity(t)
        case Fun(args, _) => args.length
        case _ => 0
      }
  }

  object Tuple {
    def unapply(t: Type): Option[List[Type]] =
      t match {
        case UnitType => Some(Nil)
        case TyApply(TyApply(TupleConsType, h), t) =>
          unapply(t) match {
            case None => None
            case Some(ts) => Some(h :: ts)
          }
        case _ => None
      }

    def apply(ts: List[Type]): Type =
      ts match {
        case Nil => UnitType
        case h :: tail =>
          val tailT = apply(tail)
          TyApply(TyApply(TupleConsType, h), tailT)
      }
  }

  object OptionT {
    def unapply(t: Type): Option[Type] =
      t match {
        case TyApply(OptionType, t) => Some(t)
        case _ => None
      }
  }

  object DictT {
    def unapply(t: Type): Option[(Type, Type)] =
      t match {
        case TyApply(TyApply(DictType, kt), vt) => Some((kt, vt))
        case _ => None
      }
  }

  object ListT {
    def unapply(t: Type): Option[Type] =
      t match {
        case TyApply(ListType, t) => Some(t)
        case _ => None
      }
  }

  sealed abstract class Const {
    def toDefined: Const.Defined
  }
  object Const {
    case class Defined(packageName: PackageName, name: TypeName) extends Const {
      def toDefined: Defined = this
    }

    def predef(name: String): Defined =
      Defined(PackageName.PredefName, TypeName(Identifier.Constructor(name)))

    object Predef {
      def unapply(c: Const): Option[Identifier.Constructor] =
        c match {
          case Defined(PackageName.PredefName, TypeName(cons)) => Some(cons)
          case _ => None
        }
    }
  }

  sealed abstract class Var {
    def name: String
  }
  object Var {
    case class Bound(name: String) extends Var
    case class Skolem(name: String, kind: Kind, existential: Boolean, id: Long) extends Var

    object Bound {
      private[this] val cache: Array[Bound] =
        ('a' to 'z').map { c => new Bound(c.toString) }.toArray

      def apply(str: String): Bound =
        if (str.length == 1) {
          val c = str.charAt(0)
          if ('a' <= c && c <= 'z') {
            cache(c - 'a')
          }
          else new Bound(str)
        }
        else new Bound(str)

      implicit val orderBound: Order[Bound] =
        Order.by[Bound, String](_.name)
    }

    implicit val varOrdering: Ordering[Var] =
      new Ordering[Var] {
        def compare(a: Var, b: Var): Int =
          (a, b) match {
            case (Bound(a), Bound(b)) => a.compareTo(b)
            case (Bound(_), _) => -1
            case (Skolem(n0, k0, ex0, i0), Skolem(n1, k1, ex1, i1)) =>
              val c = java.lang.Long.compare(i0, i1)
              if (c != 0) c
              else {
                val cn = n0.compareTo(n1)
                if (cn != 0) cn
                else {
                  val c = java.lang.Boolean.compare(ex0, ex1)
                  if (c != 0) c
                  else Order[Kind].compare(k0, k1)
                }
              }
            case (Skolem(_, _, _, _), _) => 1
          }
      }
  }

  val allBinders: LazyList[Var.Bound] = {
    val letters = ('a' to 'z').to(LazyList)
    val allIntegers = LazyList.iterate(0L)(_ + 1L)
    val lettersWithNumber =
      for {
        num <- allIntegers
        l <- letters
      } yield Var.Bound(s"$l$num")

    letters.map { c => Var.Bound(c.toString) } #::: lettersWithNumber
  }

  def alignBinders[A](items: NonEmptyList[A], avoid: Var.Bound => Boolean): NonEmptyList[(A, Var.Bound)] = {
    val sz = items.size
    // for some reason on 2.11 we need to do .iterator or this will be an infinite loop
    val bs = NonEmptyList.fromListUnsafe(allBinders.iterator.filterNot(avoid).take(sz).toList)
    NonEmptyList((items.head, bs.head), items.tail.zip(bs.tail))
  }

  def alignBinders[A](items: List[A], avoid: Var.Bound => Boolean): List[(A, Var.Bound)] =
    NonEmptyList.fromList(items) match {
      case Some(nel) => alignBinders(nel, avoid).toList
      case None => Nil
    }

  case class Meta(kind: Kind, id: Long, existential: Boolean, ref: Ref[Option[Type.Tau]])

  object Meta {
    implicit val orderingMeta: Ordering[Meta] =
      new Ordering[Meta] {
        def compare(x: Meta, y: Meta): Int =
          if (x.existential) {
            if (y.existential) java.lang.Long.compare(x.id, y.id)
            else -1
          }
          else {
            if (!y.existential) java.lang.Long.compare(x.id, y.id)
            else 1
          }
      }
  }

  /**
   * Final the set of all of Metas inside the list of given types
   */
  def metaTvs(s: List[Type]): SortedSet[Meta] = {
    @annotation.tailrec
    def go(check: List[Type], acc: SortedSet[Meta]): SortedSet[Meta] =
      check match {
        case Nil => acc
        case Quantified(_, r) :: tail => go(r :: tail, acc)
        case TyApply(a, r) :: tail => go(a :: r :: tail, acc)
        case TyMeta(m) :: tail => go(tail, acc + m)
        case _ :: tail => go(tail, acc)
      }
    go(s, SortedSet.empty)
  }

  /**
   * Report bound variables which are used in quantify. When we
   * infer a sigma type
   */
  def tyVarBinders(tpes: List[Type]): Set[Type.Var.Bound] = {
    @annotation.tailrec
    def loop(tpes: List[Type], acc: Set[Type.Var.Bound]): Set[Type.Var.Bound] =
      tpes match {
        case Nil => acc
        case (q: Quantified) :: rest =>
          loop(rest, acc ++ q.vars.iterator.map(_._1))
        case Type.TyApply(arg, res) :: rest =>
          loop(arg :: res :: rest, acc)
        case _ :: rest => loop(rest, acc)
      }
    loop(tpes, Set.empty)
  }

  /**
    * strange name, but the idea is to replace a Meta with a resolved Rho
    * value. I think the name resolve might be better, but the paper I started
    * from used zonk  
    */
  def zonk[F[_]: Monad](
    transparent: SortedSet[Meta],
    readMeta: Meta => F[Option[Rho]],
    writeMeta: (Meta, Type.Rho) => F[Unit]): Meta => F[Option[Rho]] = {

    val pureNone = Monad[F].pure(Option.empty[Rho])

    lazy val fn: Meta => F[Option[Rho]] = { (m: Meta) =>
      if (m.existential && !transparent(m)) pureNone
      else readMeta(m).flatMap {
        case None => pureNone
        case (sm @ Some(tm: Type.TyMeta)) if tm.toMeta.existential && !transparent(tm.toMeta) =>
          // don't zonk from non-existential past existential or we forget
          // that this variable is existential and can see through it
          Monad[F].pure(sm)
        case sty @ Some(ty) =>
          zonkRhoMeta(ty)(fn).flatMap { ty1 =>
            if ((ty1: Type) === ty) Monad[F].pure(sty)
            else {
              // we were able to resolve more of the inner metas
              // inside ty, so update the state
              writeMeta(m, ty1).as(Some(ty1))
            }
          }
      }
    }

    fn
  }
  /**
   * Resolve known meta variables nested inside t
   */
  def zonkMeta[F[_]: Applicative](t: Type)(m: Meta => F[Option[Type.Rho]]): F[Type] =
    t match {
      case rho: Rho => zonkRhoMeta(rho)(m).widen
      case q: Quantified =>
        zonkRhoMeta(q.in)(m).map { tpe =>
          quantify(q.quant, tpe)
        }
    }

  /**
   * Resolve known meta variables nested inside t
   */
  def zonkRhoMeta[F[_]: Applicative](t: Type.Rho)(mfn: Meta => F[Option[Type.Rho]]): F[Type.Rho] =
    t match {
      case Type.TyApply(on, arg) =>
        (zonkMeta(on)(mfn), zonkMeta(arg)(mfn)).mapN(Type.TyApply(_, _))
      case t@Type.TyMeta(m) =>
        mfn(m).map {
          case None => t
          case Some(rho) => rho
        }
      case (Type.TyConst(_) | Type.TyVar(_)) => Applicative[F].pure(t)
    }

  private object FullResolved extends TypeParser[Type] {
    lazy val parseRoot: P[Type] = {
      val tvar = Parser.lowerIdent.map { s => Type.TyVar(Type.Var.Bound(s)) }
      val name = ((PackageName.parser <* P.string("::")) ~ Identifier.consParser)
        .map { case (p, n) => Type.TyConst(Type.Const.Defined(p, TypeName(n))) }
      val longParser: P[Long] = Numbers.signedIntString.mapFilter { str =>
        try Some(str.toLong)
        catch {
          case _: NumberFormatException => None
        }
      }
      val existential = P.char('e').?
      val skolem = (P.char('$') *> Parser.lowerIdent, P.char('$') *> (longParser ~ existential))
        // TODO Kind/existential
        .mapN { case (n, (id, ex)) => Var.Skolem(n, Kind.Type, ex.isDefined, id) }
        .map(TyVar(_))

      // this null is bad, but we have no way to reallocate this
      // and won't parse before type inference anyway
      // the ideal solution is to better static type information
      // to have fully inferred types with no skolems or metas
      // TODO Kind
      val meta = (P.char('?') *> (existential ~ longParser))
        .map { case (opt, l) => TyMeta(Meta(Kind.Type, l, opt.isDefined, null)) }

      tvar.orElse(name).orElse(skolem).orElse(meta)
    }

    def makeFn(in: NonEmptyList[Type], out: Type): Type =
      // this may be an invalid function, but typechecking verifies that.
      Type.Fun(in, out)

    def applyTypes(left: Type, args: NonEmptyList[Type]) = applyAll(left, args.toList)

    def universal(vs: NonEmptyList[(String, Option[Kind])], on: Type): Type =
      Type.forAll(vs.map {
        case (s, None) => (Type.Var.Bound(s), Kind.Type)
        case (s, Some(k)) => (Type.Var.Bound(s), k)
      }, on) 

    def existential(vs: NonEmptyList[(String, Option[Kind])], on: Type): Type =
      Type.exists(vs.map {
        case (s, None) => (Type.Var.Bound(s), Kind.Type)
        case (s, Some(k)) => (Type.Var.Bound(s), k)
      }, on) 

    def makeTuple(lst: List[Type]) = Type.Tuple(lst)

    private[this] val coloncolon = Doc.text("::")

    def unapplyRoot(a: Type): Option[Doc] =
      a match {
        case TyConst(Const.Defined(p, n)) =>
          Some(Document[PackageName].document(p) + coloncolon + Document[Identifier].document(n.ident))
        case TyVar(Var.Bound(s)) => Some(Doc.text(s))
        case TyVar(Var.Skolem(n, _, e, i)) =>
          // TODO Kind
          val dol = "$"
          val ex = if (e) "e" else ""
          Some(Doc.text(s"$dol$n$dol$i$ex"))
        case TyMeta(Meta(_, i, ex, _)) =>
          // TODO Kind and if it is existential
          val exstr = if (ex) "e" else ""
          Some(Doc.text(s"?$exstr$i"))
        case _ => None
      }

    def unapplyFn(a: Type): Option[(NonEmptyList[Type], Type)] =
      a match {
        case Fun(as, b) => Some((as, b))
        case _ => None
      }

    def unapplyUniversal(a: Type): Option[(List[(String, Option[Kind])], Type)] =
      a match {
        case _: Rho => None
        case q: Quantified =>
          q.quant match {
            case Quantification.ForAll(vs) =>
              Some((vs.map { 
                case (v, k) => (v.name, Some(k))
              }.toList, q.in))
            case Quantification.Dual(forall, ex) =>
              Some((forall.map { 
                case (v, k) => (v.name, Some(k))
              }.toList, exists(ex, q.in)))
            case _ => None
          }
      }

    def unapplyExistential(a: Type): Option[(List[(String, Option[Kind])], Type)] =
      a match {
        case _: Rho => None
        case q: Quantified =>
          q.quant match {
            case Quantification.Exists(vs) =>
              Some((vs.map { 
                case (v, k) => (v.name, Some(k))
              }.toList, q.in))
            case Quantification.Dual(forall, exists) =>
              Some((exists.map { 
                case (v, k) => (v.name, Some(k))
              }.toList, forAll(forall, q.in)))
            case _ => None
          }
      }

    def unapplyTypeApply(a: Type): Option[(Type, List[Type])] =
      a match {
        case ta@TyApply(_, _) => Some(unapplyAll(ta))
        case _ => None
      }

    def unapplyTuple(a: Type): Option[List[Type]] =
      a match {
        case Tuple(as) => Some(as)
        case _ => None
      }
  }
  /**
   * Parse fully resolved types: package::type
   */
  def fullyResolvedParser: P[Type] = FullResolved.parser
  def fullyResolvedDocument: Document[Type] = FullResolved.document
  def typeParser: TypeParser[Type] = FullResolved
}
