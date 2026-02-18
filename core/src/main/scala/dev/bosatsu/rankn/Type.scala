package dev.bosatsu.rankn

import cats.data.NonEmptyList
import cats.parse.{Parser => P, Numbers}
import cats.{Applicative, Monad, Order}
import org.typelevel.paiges.{Doc, Document}
import dev.bosatsu.{
  Kind,
  PackageName,
  Lit,
  TypeName,
  Identifier,
  Parser,
  TypeParser,
  Require
}
import dev.bosatsu.hashing.{Algo, Hashable}
import dev.bosatsu.graph.Memoize.memoizeDagHashedConcurrent
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.util.hashing.MurmurHash3

import cats.implicits._

sealed abstract class Type extends Product derives CanEqual {
  // Type nodes are immutable and heavily used as map/set keys; cache once.
  final override val hashCode: Int =
    MurmurHash3.caseClassHash(this)

  def sameAs(that: Type): Boolean = Type.sameType(this, that)

  def normalize: Type
}

object Type {

  private final case class ExitBound(vars: List[Type.Var.Bound])

  /** A type with no top level universal quantification.
    *
    * Existential quantification is allowed in Rho.
    */
  sealed abstract class Rho extends Type {
    override def normalize: Rho
  }

  object Rho {
    implicit val orderRho: Order[Rho] =
      new Order[Rho] {
        def compare(a: Rho, b: Rho): Int =
          (a, b) match {
            case (TyConst(a), TyConst(b)) =>
              Ordering[Const].compare(a, b)
            case (TyConst(_), _)        => -1
            case (TyVar(v0), TyVar(v1)) =>
              Ordering[Var].compare(v0, v1)
            case (TyVar(_), TyConst(_))   => 1
            case (TyVar(_), _)            => -1
            case (TyMeta(m0), TyMeta(m1)) =>
              Meta.orderingMeta.compare(m0, m1)
            case (TyMeta(_), TyApply(_, _))         => -1
            case (TyMeta(_), Exists(_, _))          => -1
            case (TyMeta(_), _)                     => 1
            case (TyApply(a0, b0), TyApply(a1, b1)) =>
              val c = Type.typeOrder.compare(a0, a1)
              if (c == 0) Type.typeOrder.compare(b0, b1) else c
            case (TyApply(_, _), Exists(_, _))      => -1
            case (TyApply(_, _), _)                 => 1
            case (Exists(v0, in0), Exists(v1, in1)) =>
              val c = Order[NonEmptyList[(Var.Bound, Kind)]].compare(v0, v1)
              if (c == 0) compare(in0, in1) else c
            case (Exists(_, _), _) => 1
          }
      }

    implicit val orderingRho: Ordering[Rho] = orderRho.toOrdering
  }

  sealed abstract class Leaf extends Rho {
    override def normalize: Leaf = this
  }
  // no forall or exists anywhere
  // Any Rho that is not an TyApply is always a Tau
  // TyApply is Tau
  opaque type Tau <: Rho = Rho

  object Tau {
    given Order[Tau] = Rho.orderRho

    def tauConst(c: Const): Tau = TyConst(c)
    def tauVar(v: Var): Tau = TyVar(v)
    def tauMeta(m: Meta): Tau = TyMeta(m)

    inline def apply(l: Leaf): Tau = l

    opaque type TauApply <: Tau = TyApply
    implicit class TauApplyMethods(private val ta: TauApply) extends AnyVal {
      inline def on: Tau = ta.on
      inline def arg: Tau =
        // This cast is safe because TauApply is a Tau
        ta.arg.asInstanceOf[Rho]

      inline def toTyApply: TyApply = ta
    }

    object TauApply {
      def apply(on: Tau, arg: Tau): TauApply = {
        // TODO: when exists becomes tau, this needs to change:
        val onLA = on.asInstanceOf[Leaf | TyApply]
        TyApply(onLA, arg)
      }

      // a scala3 zero allocation match result
      class TauApplyMatch(tau: Tau) extends AnyVal {
        def isEmpty: Boolean = !tau.isInstanceOf[TyApply]
        def get: TauApply = tau.asInstanceOf[TyApply]
      }

      def unapply(t: Tau): TauApplyMatch = new TauApplyMatch(t)
    }

    opaque type TauExists <: Tau = Exists
    implicit class TauExistsMethods(private val te: TauExists) extends AnyVal {
      inline def vars: NonEmptyList[(Var.Bound, Kind)] = te.vars
      inline def in: Tau = te.in
      inline def toExists: Exists = te
    }

    object TauExists {
      def apply(vars: NonEmptyList[(Var.Bound, Kind)], in: Tau): TauExists =
        existsRho(vars, in)

      // a scala3 zero allocation match result
      class TauExistsMatch(tau: Tau) extends AnyVal {
        def isEmpty: Boolean = !tau.isInstanceOf[Exists]
        def get: TauExists = tau.asInstanceOf[Exists]
      }

      def unapply(t: Tau): TauExistsMatch = new TauExistsMatch(t)
    }

    def isTau(t: Type): Boolean = {
      @annotation.tailrec
      def loop(ts: List[Type]): Boolean =
        ts match {
          case (_: Leaf) :: rest        => loop(rest)
          case TyApply(on, arg) :: rest =>
            loop(on :: arg :: rest)
          case (Exists(_, in) :: rest) =>
            loop(in :: rest)
          case (_: ForAll) :: _ => false
          case Nil              => true
        }

      t match {
        case _: Leaf         => true
        case TyApply(t1, t2) => loop(t1 :: t2 :: Nil)
        case Exists(_, in)   => loop(in :: Nil)
        case _: ForAll       => false
      }
    }

    // a scala3 zero allocation match result
    class TauMatch(t: Type) extends AnyVal {
      def isEmpty: Boolean = !isTau(t)
      def get: Tau = t.asInstanceOf[Rho]
    }

    def unapply(t: Type): TauMatch = new TauMatch(t)
  }

  case class TyConst(tpe: Const) extends Leaf
  case class TyVar(toVar: Var) extends Leaf
  case class TyMeta(toMeta: Meta) extends Leaf

  // apply doesn't have nested Exists (apply binds tighter)
  case class TyApply(on: Leaf | TyApply, arg: Type) extends Rho {
    lazy val normalize: Rho = {
      // normalization can't make a rho into an exists
      val onLA = on.normalize.asInstanceOf[Leaf | TyApply]
      TyApply(onLA, arg.normalize)
    }
  }

  case class ForAll(vars: NonEmptyList[(Var.Bound, Kind)], in: Rho)
      extends Type {
    lazy val normalize: Type = Type.runNormalize(this)
  }

  // exists aren't nested, so in must be a Leaf or TyApply
  case class Exists(vars: NonEmptyList[(Var.Bound, Kind)], in: Leaf | TyApply)
      extends Rho {
    lazy val normalize: Rho = Type.runNormalize(this).asInstanceOf[Rho]
  }

  def unshadow[V >: Type.Var.Bound](
      nel: NonEmptyList[(Type.Var.Bound, Kind)],
      otherVars: Set[V]
  ): (Map[Type.Var, Type.TyVar], NonEmptyList[(Type.Var.Bound, Kind)]) = {
    val remap: Map[Type.Var, Type.Var.Bound] =
      if (otherVars.isEmpty) Map.empty
      else {
        val collisions = nel.toList.filter { case (b, _) => otherVars(b) }
        if (collisions.isEmpty) Map.empty
        else {
          val nonCollisions = nel.iterator.filterNot { case (b, _) =>
            otherVars(b)
          }
          val colMap =
            Type.alignBinders(collisions, otherVars ++ nonCollisions.map(_._1))
          colMap.iterator.map { case ((b, _), b1) => (b, b1) }.toMap
        }
      }

    if (remap.isEmpty) (Map.empty[Type.Var, Type.TyVar], nel)
    else {
      val nel1 = nel.map { case bk @ (b, k) =>
        remap.get(b) match {
          case None     => bk
          case Some(b1) => (b1, k)
        }
      }
      (remap.view.mapValues(Type.TyVar(_)).toMap, nel1)
    }
  }

  def sameType(left: Type, right: Type): Boolean =
    left match {
      case leftLeaf: Leaf =>
        // a Leaf is never equal to TyApply
        right match {
          case rightLeaf: Leaf => leftLeaf == rightLeaf
          case _: TyApply      => false
          case _               => leftLeaf == normalize(right)
        }
      case _: TyApply if right.isInstanceOf[Leaf] => false
      case _                                      =>
        // at least one is quantified or both are TyApply
        normalize(left) == normalize(right)
    }

  implicit val typeOrder: Order[Type] =
    new Order[Type] {
      def compare(a: Type, b: Type): Int =
        (a, b) match {
          case (arho: Rho, brho: Rho) =>
            Rho.orderRho.compare(arho, brho)
          case (_: Rho, _)                        => -1
          case (ForAll(v0, in0), ForAll(v1, in1)) =>
            val c = Order[NonEmptyList[(Var.Bound, Kind)]].compare(v0, v1)
            if (c == 0) Rho.orderRho.compare(in0, in1) else c
          case (_: ForAll, _) => 1
        }
    }

  implicit val typeOrdering: Ordering[Type] = typeOrder.toOrdering

  private def hasQuantifiers(t: Type): Boolean =
    t match {
      case _: ForAll => true
      case _: Exists => true
      case _         => false
    }

  private def splitForAll(t: Type): (List[(Var.Bound, Kind)], Rho) =
    t match {
      case ForAll(vars, rho) => (vars.toList, rho)
      case r: Rho            => (Nil, r)
    }

  private def splitExists(
      rho: Rho
  ): (List[(Var.Bound, Kind)], Leaf | TyApply) = {
    @annotation.tailrec
    def loop(
        rho: Rho,
        accRev: List[(Var.Bound, Kind)]
    ): (List[(Var.Bound, Kind)], Leaf | TyApply) =
      rho match {
        case Exists(vars, in) =>
          loop(in, vars.toList.reverse_:::(accRev))
        case other: (Leaf | TyApply) => (accRev.reverse, other)
      }

    loop(rho, Nil)
  }

  def splitQuantifiers(
      t: Type
  ): (List[(Var.Bound, Kind)], List[(Var.Bound, Kind)], Leaf | TyApply) = {
    val (fas, rho0) = splitForAll(t)
    val (exs, rho1) = splitExists(rho0)
    (fas, exs, rho1)
  }

  def forallList(t: Type): List[(Var.Bound, Kind)] =
    splitForAll(t)._1

  def existList(t: Type): List[(Var.Bound, Kind)] = {
    val (_, rho) = splitForAll(t)
    splitExists(rho)._1
  }

  def quantVars(t: Type): List[(Var.Bound, Kind)] = {
    val (fa, ex, _) = splitQuantifiers(t)
    fa ::: ex
  }

  @annotation.tailrec
  private def applyAllRho(rho: Leaf | TyApply, args: List[Type]): Rho =
    args match {
      case Nil     => rho
      case a :: as => applyAllRho(TyApply(rho, a), as)
    }

  def apply1Rho(fn: Rho, arg: Type): Rho =
    fn match {
      case rho: (Leaf | TyApply) => TyApply(rho, arg)
      case ex: Exists            =>
        applyAll(ex, arg :: Nil) match {
          case rho: Rho   => rho
          case _: ForAll =>
            sys.error(s"internal error: applyAll on Rho produced ForAll: $fn")
        }
    }

  def apply1(fn: Type, arg: Type): Type =
    fn match {
      case rho: Rho => apply1Rho(rho, arg)
      case q        => applyAll(q, arg :: Nil)
    }

  def applyAll(fn: Type, args: List[Type]): Type =
    if args.isEmpty then fn
    else
      (fn match {
        case (_: ForAll) | (_: Exists) =>
          val (foralls, exists, rho) = splitQuantifiers(fn)
          val freeBound = freeBoundTyVars(fn :: args)
          if (freeBound.isEmpty) {
            quantify(foralls, exists, applyAllRho(rho, args))
          } else {
            val freeBoundSet: Set[Var.Bound] = freeBound.toSet
            val collisions =
              (foralls.iterator ++ exists.iterator).exists { case (b, _) =>
                freeBoundSet(b)
              }
            if (!collisions) {
              // we don't need to rename the vars
              quantify(foralls, exists, applyAllRho(rho, args))
            } else {
              // we have to to rename the collisions so the free set
              // is unchanged
              val fa1 = alignBinders(foralls, freeBoundSet)
              val ex1 = alignBinders(exists, freeBoundSet ++ fa1.map(_._2))
              val subMap = (fa1.iterator ++ ex1.iterator)
                .map { case ((b0, _), b1) =>
                  (b0, TyVar(b1))
                }
                .toMap[Var, Leaf]

              val rho1 = substituteLeafApplyVar(rho, subMap)

              val fa2 = fa1.map { case ((_, k), b) => (b, k) }
              val ex2 = ex1.map { case ((_, k), b) => (b, k) }

              quantify(fa2, ex2, applyAllRho(rho1, args))
            }
          }
        case lt: (Leaf | TyApply) => applyAllRho(lt, args)
      }
    )

  def unapplyAll(fn: Type): (Type, List[Type]) = {
    @annotation.tailrec
    def loop(fn: Type, acc: List[Type]): (Type, List[Type]) =
      fn match {
        case TyApply(fn, a) => loop(fn, a :: acc)
        case notApply       => (notApply, acc)
      }

    loop(fn, Nil)
  }

  def constantsOf(t: Type): List[Const] =
    t match {
      case ForAll(_, t)         => constantsOf(t)
      case Exists(_, t)         => constantsOf(t)
      case TyApply(on, arg)     => constantsOf(on) ::: constantsOf(arg)
      case TyConst(c)           => c :: Nil
      case TyVar(_) | TyMeta(_) => Nil
    }

  def hasNoVars(t: Type): Boolean =
    t match {
      case TyConst(_)           => true
      case TyVar(_) | TyMeta(_) => false
      case TyApply(on, arg)     => hasNoVars(on) && hasNoVars(arg)
      case fa: ForAll           => freeTyVars(fa :: Nil).isEmpty
      case ex: Exists           => freeTyVars(ex :: Nil).isEmpty
    }

  def hasNoUnboundVars(t: Type): Boolean = {
    def loop(t: Type, bound: Set[Var.Bound]): Boolean =
      t match {
        case TyVar(b: Var.Bound) => bound(b)
        case _: Leaf             => true
        case TyApply(on, arg)    => loop(on, bound) && loop(arg, bound)
        case ForAll(vars, in)    =>
          loop(in, bound ++ vars.iterator.map(_._1))
        case Exists(vars, in) =>
          loop(in, bound ++ vars.iterator.map(_._1))
      }

    loop(t, Set.empty)
  }

  final def forAll(vars: List[(Var.Bound, Kind)], in: Type): Type =
    NonEmptyList.fromList(vars) match {
      case None     => in
      case Some(ne) => forAll(ne, in)
    }

  final def forAll(
      vars: NonEmptyList[(Var.Bound, Kind)],
      in: Type
  ): Type =
    in match {
      case ForAll(ne1, rho) => ForAll(vars ::: ne1, rho)
      case rho: Rho         => ForAll(vars, rho)
    }

  final def existsRho(
      vars: NonEmptyList[(Var.Bound, Kind)],
      in: Rho
  ): Exists =
    in match {
      case Exists(ne1, inner)   => Exists(vars ::: ne1, inner)
      case la: (Leaf | TyApply) => Exists(vars, la)
    }

  final def exists(
      vars: NonEmptyList[(Var.Bound, Kind)],
      in: Type
  ): Type =
    in match {
      case ForAll(fa, rho) =>
        ForAll(fa, existsRho(vars, rho))
      case rho: Rho =>
        existsRho(vars, rho)
    }

  final def exists(vars: List[(Var.Bound, Kind)], in: Type): Type =
    vars match {
      case h :: t => exists(NonEmptyList(h, t), in)
      case Nil    => in
    }

  final def quantify(
      forallList: List[(Var.Bound, Kind)],
      existList: List[(Var.Bound, Kind)],
      in: Type
  ): Type =
    forAll(forallList, exists(existList, in))

  def getTypeOf(lit: Lit): Type =
    lit match {
      case Lit.Integer(_) => Type.IntType
      case Lit.Str(_)     => Type.StrType
      case Lit.Chr(_)     => Type.CharType
      case _: Lit.Float64 => Type.Float64Type
    }

  /** types are var, meta, or const, or applied or forall on one of those. This
    * returns the Type.TyConst found by recursing
    */
  @annotation.tailrec
  final def rootConst(t: Type): Option[Type.TyConst] =
    t match {
      case tyc @ TyConst(_)     => Some(tyc)
      case TyVar(_) | TyMeta(_) => None
      case TyApply(left, _)     => rootConst(left)
      case ForAll(_, in)        => rootConst(in)
      case Exists(_, in)        => rootConst(in)
    }

  def allConsts(ts: List[Type]): List[TyConst] = {
    @annotation.tailrec
    def loop(ts: List[Type], acc: List[TyConst]): List[TyConst] =
      ts match {
        case (tyc @ TyConst(_)) :: tail =>
          loop(tail, tyc :: acc)
        case (TyVar(_) | TyMeta(_)) :: tail =>
          loop(tail, acc)
        case TyApply(left, right) :: tail =>
          loop(left :: right :: tail, acc)
        case ForAll(_, in) :: tail =>
          loop(in :: tail, acc)
        case Exists(_, in) :: tail =>
          loop(in :: tail, acc)
        case Nil =>
          acc.reverse.distinct
      }

    loop(ts, Nil)
  }

  object RootConst {
    def unapply(t: Type): Option[Type.TyConst] =
      rootConst(t)
  }

  /** This form is often useful in Infer
    */
  def substTy(
      keys: NonEmptyList[Var],
      vals: NonEmptyList[Type]
  ): Type => Type = {
    val env = keys.iterator.zip(vals.iterator).toMap

    { t => substituteVar(t, env) }
  }

  def substituteVar(t: Type, env: Map[Type.Var, Type]): Type =
    if (env.isEmpty) t
    else {
      // Drop identity substitutions to avoid unnecessary binder renames.
      val env1 = env.filterNot {
        case (v, Type.TyVar(v1)) => v1 === v
        case _                   => false
      }
      if (env1.isEmpty) t
      else
        (t match {
          case TyApply(on, arg) =>
            apply1(substituteVar(on, env1), substituteVar(arg, env1))
          case v @ TyVar(n) =>
            env1.get(n) match {
              case Some(rho) => rho
              case None      => v
            }
          case m @ TyMeta(_)                     => m
          case c @ TyConst(_)                    => c
          case t @ (ForAll(_, _) | Exists(_, _)) =>
            val (foralls, exists, in0) = splitQuantifiers(t)
            val boundSet =
              (foralls.iterator ++ exists.iterator).map(_._1).toSet
            val env2 = env1.iterator.filter {
              case (v: Type.Var.Bound, _) => !boundSet(v)
              case _                      => true
            }.toMap

            // only care about substitutions that could apply in this body
            val freeInBody = freeTyVars(t :: Nil).toSet
            val envUsed = env2.iterator.filter { case (v, _) =>
              freeInBody(v)
            }.toMap

            if (envUsed.isEmpty) t
            else {

              // avoid capture: if any bound var is free in the substitution range,
              // rename the binder before substituting
              // use all env values to keep renaming stable under repeated substitution
              val freeInEnv =
                freeBoundTyVars(env1.values.toList).toSet
              val envBoundKeys =
                env1.keys.collect { case b: Type.Var.Bound => b }.toSet
              val collisions =
                (foralls ::: exists).collect {
                  case pair @ (b, _) if freeInEnv(b) => pair
                }

              val (fa1, ex1, in1) =
                NonEmptyList.fromList(collisions) match {
                  case None =>
                    (foralls, exists, in0)
                  case Some(nel) =>
                    // we must avoid any free vars in the body or the substitution env,
                    // as well as any existing binders in this quantification.
                    val avoid =
                      freeBoundTyVars(
                        t :: Nil
                      ).toSet ++ freeInEnv ++ boundSet ++ envBoundKeys

                    val renamed = alignBinders(nel, avoid)
                    val renMap =
                      renamed.iterator.map { case ((b, _), b1) =>
                        (b, b1)
                      }.toMap
                    val subMap =
                      renMap.view.mapValues(TyVar(_)).toMap[Type.Var, Type.Rho]

                    val in1 = substituteRhoVar(in0, subMap)
                    val fa1 = foralls.map { case (b, k) =>
                      (renMap.getOrElse(b, b), k)
                    }
                    val ex1 = exists.map { case (b, k) =>
                      (renMap.getOrElse(b, b), k)
                    }

                    (fa1, ex1, in1)
                }

              val subin = substituteVar(in1, envUsed)
              quantify(fa1, ex1, subin)
            }
        })
    }

  def packageNamesIn(t: Type): List[PackageName] =
    allConsts(t :: Nil).map(_.tpe.toDefined.packageName).distinct

  def substituteLeafApplyVar(
      t: Leaf | TyApply,
      env: Map[Type.Var, Leaf | TyApply]
  ): Leaf | TyApply =
    t match {
      case v @ TyVar(n) =>
        env.get(n) match {
          case Some(la) => la
          case None     => v
        }
      case TyApply(on, arg) =>
        TyApply(substituteLeafApplyVar(on, env), substituteVar(arg, env))
      case l: Leaf => l
    }

  def substituteRhoVar(t: Type.Rho, env: Map[Type.Var, Type.Rho]): Type.Rho =
    t match {
      case TyApply(on, arg) =>
        val arg1 = substituteVar(arg, env)
        substituteRhoVar(on, env) match {
          case la: (Leaf | TyApply) => TyApply(la, arg1)
          case e: Exists            =>
            // we need (exists x, y, ... ff)[arg1]
            // so if we lift the exists out, we need to rename so we don't shadow
            // the rho substitution can't introduce ForAll, so the cast is safe:
            applyAll(e, arg1 :: Nil).asInstanceOf[Rho]
        }

      case Exists(vars, in) =>
        val boundSet = vars.iterator.map(_._1).toSet
        val env1 = env.iterator.filter {
          case (v: Type.Var.Bound, _) => !boundSet(v)
          case _                      => true
        }.toMap
        existsRho(vars, substituteRhoVar(in, env1))
      case v @ TyVar(n) =>
        env.get(n) match {
          case Some(rho) => rho
          case None      => v
        }
      case m @ TyMeta(_)  => m
      case c @ TyConst(_) => c
    }

  /** Kind of the opposite of substitute: given a Map of vars, can we set those
    * vars to some Type and get from to match to exactly
    */
  def instantiate(
      vars: Map[Var.Bound, Kind],
      from: Type,
      to: Type,
      env: Map[Var.Bound, Kind]
  ): Option[
    (
        SortedMap[Var.Bound, (Kind, Var.Bound)],
        SortedMap[Var.Bound, (Kind, Type)]
    )
  ] = {

    sealed abstract class BoundState derives CanEqual
    case object Unknown extends BoundState
    case class Fixed(tpe: Type) extends BoundState
    case class Free(rightName: Var.Bound) extends BoundState

    case class State(
        fixed: Map[Var.Bound, (Kind, BoundState)],
        rightFrees: Map[Var.Bound, Kind]
    ) {

      def get(b: Var.Bound): Option[(Kind, BoundState)] = fixed.get(b)

      def updated(b: Var.Bound, kindBound: (Kind, BoundState)): State =
        copy(fixed = fixed.updated(b, kindBound))

      def --(keys: IterableOnce[Var.Bound]): State =
        copy(fixed = fixed -- keys)

      def ++(keys: IterableOnce[(Var.Bound, (Kind, BoundState))]): State =
        copy(fixed = fixed ++ keys)
    }

    def loop(from: Type, to: Type, state: State): Option[State] =
      from match {
        case TyVar(b: Var.Bound) =>
          state.get(b) match {
            case Some((kind, opt)) =>
              opt match {
                case Unknown =>
                  to match {
                    case tv @ TyVar(toB: Var.Bound) =>
                      state.rightFrees.get(toB) match {
                        case Some(toBKind) =>
                          if (Kind.leftSubsumesRight(kind, toBKind)) {
                            Some(state.updated(b, (toBKind, Free(toB))))
                          } else None
                        case None =>
                          env.get(toB) match {
                            case Some(toBKind)
                                if (Kind.leftSubsumesRight(kind, toBKind)) =>
                              Some(state.updated(b, (toBKind, Fixed(tv))))
                            case _ => None
                          }
                        // don't set to vars to non-free bound variables
                        // this shouldn't happen in real inference
                      }
                    case _
                        if freeBoundTyVars(to :: Nil)
                          .filterNot(env.keySet)
                          .isEmpty =>
                      Some(state.updated(b, (kind, Fixed(to))))
                    case _ => None
                  }
                case Fixed(set) =>
                  if (set.sameAs(to)) Some(state)
                  else None
                case Free(rightName) =>
                  to match {
                    case TyVar(toB: Var.Bound) if rightName === toB =>
                      Some(state)
                    case _ => None
                  }
              }
            case None =>
              // not a variable, we can use, but also not the same as the right
              None
          }
        case TyApply(a, b) =>
          to match {
            case TyApply(ta, tb) =>
              loop(a, ta, state).flatMap { s1 =>
                loop(b, tb, s1)
              }
            case ForAll(rightFrees, rightT) =>
              val collisions = rightFrees.toList.filter { case (b, _) =>
                state.rightFrees.contains(b)
              }

              val (rightFrees1, rightT1) =
                if (collisions.isEmpty) (rightFrees, rightT)
                else {
                  val avoidSet =
                    state.rightFrees.keySet ++ env.keySet ++
                      rightFrees.iterator.map(_._1) ++
                      freeBoundTyVars(rightT :: Nil)
                  val aligned = alignBinders(collisions, avoidSet)
                  val subMap = aligned.iterator
                    .map { case ((b, _), b1) =>
                      (b, TyVar(b1))
                    }
                    .toMap[Var, Type]
                  val remap = aligned.iterator.map { case ((b, _), b1) =>
                    (b, b1)
                  }.toMap
                  val rightFrees1 = rightFrees.map { case (b, k) =>
                    (remap.getOrElse(b, b), k)
                  }
                  val rightT1 = substituteVar(rightT, subMap)
                  (rightFrees1, rightT1)
                }

              loop(
                from,
                rightT1,
                state.copy(rightFrees =
                  state.rightFrees ++ rightFrees1.iterator
                )
              )
                .map { s1 =>
                  s1.copy(rightFrees = state.rightFrees)
                }
            case _ => None
          }
        case ForAll(shadows, from1) =>
          val noShadow = state -- shadows.iterator.map(_._1)
          loop(from1, to, noShadow).map { s1 =>
            s1 ++ shadows.iterator.flatMap { case (v, _) =>
              state.get(v).map(v -> _)
            }
          }
        case _ =>
          // We can't use sameAt to compare Var.Bound since we know the variances
          // there.
          // If we only matched via sameAs, unresolved Unknown vars that are free
          // in `from` would be unsound (they later round-trip as quantified frees).
          if (from.sameAs(to)) {
            var hasUnknown = false
            val unknownIt = state.fixed.iterator
            while (!hasUnknown && unknownIt.hasNext) {
              unknownIt.next() match {
                case (_, (_, Unknown)) => hasUnknown = true
                case _                 => ()
              }
            }

            if (!hasUnknown) Some(state)
            else {
              val free = freeBoundTyVars(from :: Nil).toSet
              var bad = false
              val fixedIt = state.fixed.iterator
              while (!bad && fixedIt.hasNext) {
                fixedIt.next() match {
                  case (b, (_, Unknown)) if free(b) => bad = true
                  case _                            => ()
                }
              }
              if (bad) None else Some(state)
            }
          } else None
      }

    val initState = State(
      vars.iterator.map { case (v, a) => (v, (a, Unknown)) }.toMap,
      Map.empty
    )

    loop(from, to, initState)
      .map { state =>
        (
          state.fixed.iterator
            .collect {
              case (t, (k, Free(t1))) => (t, (k, t1))
              case (t, (k, Unknown))  => (t, (k, t))
            }
            .to(SortedMap),
          state.fixed.iterator
            .collect { case (t, (k, Fixed(f))) =>
              (t, (k, f))
            }
            .to(SortedMap)
        )
      }
  }

  /** Return the Bound and Skolem variables that are free in the given list of
    * types
    */
  def freeTyVars(ts: List[Type]): List[Type.Var] = {
    val seen = scala.collection.mutable.HashSet.empty[Type.Var]
    val acc = List.newBuilder[Type.Var]
    val check = scala.collection.mutable.ArrayDeque.empty[Type | ExitBound]
    val boundCount = scala.collection.mutable.HashMap.empty[Type.Var.Bound, Int]

    inline def foreach[A](iter: Iterator[A])(inline fn: A => Unit): Unit = {
      while (iter.hasNext) {
        fn(iter.next())
      }
    }

    inline def record(tv: Type.Var): Unit =
      if (seen.add(tv)) acc += tv

    def bind(vars: List[Type.Var.Bound]): Unit =
      foreach(vars.iterator) { v =>
        boundCount.updateWith(v) {
          case Some(i) => Some(i + 1)
          case None    => Some(1)
        }: Unit
      }

    inline def unbind(vars: List[Type.Var.Bound]): Unit =
      foreach(vars.iterator) { v =>
        boundCount.updateWith(v) {
          case Some(1) => None
          case Some(i) => Some(i - 1)
          case None    => None
        }: Unit
      }

    inline def isBound(v: Type.Var.Bound): Boolean =
      boundCount.get(v) match {
        case Some(c) => c > 0
        case None    => false
      }

    foreach(ts.iterator)(check.append(_))

    while (check.nonEmpty) {
      check.removeHead() match {
        case t: Type =>
          t match {
            case Type.TyVar(tv) =>
              tv match {
                case b: Type.Var.Bound if !isBound(b) =>
                  record(b)
                case _: Type.Var.Bound =>
                  ()
                case sk: Type.Var.Skolem =>
                  record(sk)
              }
            case Type.TyApply(a, b) =>
              check.prepend(b)
              check.prepend(a)
            case Type.ForAll(vars, in) =>
              val vs = vars.toList.map(_._1)
              bind(vs)
              check.prepend(ExitBound(vs))
              check.prepend(in)
            case Type.Exists(vars, in) =>
              val vs = vars.toList.map(_._1)
              bind(vs)
              check.prepend(ExitBound(vs))
              check.prepend(in)
            case _: (Type.TyMeta | Type.TyConst) =>
              ()
          }
        case ExitBound(vars) =>
          unbind(vars)
      }
    }

    acc.result()
  }

  /** Return the Bound variables that are free in the given list of types
    */
  def freeBoundTyVars(ts: List[Type]): List[Type.Var.Bound] =
    freeTyVars(ts).collect { case b @ Type.Var.Bound(_) => b }

  final inline def normalize(tpe: Type): Type = tpe.normalize

  private def runNormalize(tpe: Type): Type =
    tpe match {
      case t: (ForAll | Exists) =>
        @inline def removeDups[A, B](lst: List[(A, B)]): List[(A, B)] = {
          def loop(lst: List[(A, B)]): (List[(A, B)], Set[A]) =
            lst match {
              case (pair @ (b, _)) :: rest =>
                val res @ (r1, back) = loop(rest)
                if (back(b)) res
                else {
                  val b1 = back + b
                  // if rest eq r1, then pair :: rest == lst
                  if (r1 eq rest) (lst, b1)
                  else (pair :: r1, b1)
                }
              case Nil => (Nil, Set.empty)
            }

          loop(lst)._1
        }
        val (foralls0, exists0, in0) = splitQuantifiers(t)
        val foralls = removeDups(foralls0)
        val exists = removeDups(exists0)

        val inFree = freeBoundTyVars(in0 :: Nil)
        // sort the quantification by the order of appearance
        val order = inFree.iterator.zipWithIndex.toMap
        val inFreeSet = inFree.toSet
        val ex1 = exists
          .filter { case (b, _) => inFreeSet(b) }
          .sortBy { case (b, _) => order(b) }

        val inFreeFa = inFreeSet -- ex1.iterator.map(_._1)
        val fa1 = foralls
          .filter { case (b, _) => inFreeFa(b) }
          .sortBy { case (b, _) => order(b) }

        val frees = inFreeFa -- fa1.iterator.map(_._1)
        val bs = alignBinders(fa1 ::: ex1, frees)

        if (bs.nonEmpty) {
          val subMap =
            bs.iterator
              .map { case ((bold, _), bnew) =>
                bold -> TyVar(bnew)
              }
              .toMap[Type.Var, Type.Rho]

          val newVars = bs.map { case ((_, k), b) => (b, k) }
          // subMap is nonEmpty, so this is a new type so runNormalize
          val normin = runNormalize(substituteRhoVar(in0, subMap))
          val forAllSize = fa1.size
          val (normfas, normexs) = newVars.splitAt(forAllSize)
          quantify(forallList = normfas, existList = normexs, normin)
        } else {
          // there is nothing to substitute, so we have nothing
          // to quantify
          in0.normalize
        }
      case ta @ TyApply(_, _) => ta.normalize
      case leaf: Leaf         => leaf
    }

  def kindOfOption(
      cons: TyConst => Option[Kind]
  ): Type => Option[Kind] = {
    val unknown: Either[Unit, Kind] = Left(())
    val consE = (tc: TyConst) => cons(tc).fold(unknown)(Right(_))
    val fn = kindOf[Unit](_ => (), _ => (), (_, _, _) => (), consE)

    fn.andThen {
      case Right(kind) => Some(kind)
      case Left(_)     => None
    }
  }

  def kindOf[A](
      unknownVar: Var.Bound => A,
      invalidApply: TyApply => A,
      kindSubsumeError: (TyApply, Kind.Cons, Kind) => A,
      cons: TyConst => Either[A, Kind]
  ): Type => Either[A, Kind] = {

    val fn = memoizeDagHashedConcurrent[(Type, Map[Var.Bound, Kind]), Either[
      A,
      Kind
    ]] { case ((tpe, locals), rec) =>
      tpe match {
        case Type.TyVar(b @ Type.Var.Bound(_)) =>
          locals.get(b) match {
            case Some(k) => Right(k)
            // $COVERAGE-OFF$ this should be unreachable because all vars should have a known kind
            case None => Left(unknownVar(b))
            // $COVERAGE-ON$ this should be unreachable
          }
        case Type.TyVar(Type.Var.Skolem(_, kind, _, _)) => Right(kind)
        case Type.TyMeta(Type.Meta(kind, _, _, _))      => Right(kind)
        case tc @ Type.TyConst(_)                       => cons(tc)
        case ap @ Type.TyApply(left, right)             =>
          rec((left, locals))
            .product(rec((right, locals)))
            .flatMap { case (leftKind, rhs) =>
              Kind.validApply[A](leftKind, rhs, invalidApply(ap))(
                kindSubsumeError(ap, _, rhs)
              )
            }
        case ForAll(vars, in) =>
          rec((in, locals ++ vars.toList))
        case Exists(vars, in) =>
          rec((in, locals ++ vars.toList))
      }
    }

    { t => fn((t, Map.empty)) }
  }

  /** These are upper-case to leverage scala's pattern matching on upper-cased
    * vals
    */
  val BoolType: Type.TyConst = TyConst(Const.predef("Bool"))
  val DictType: Type.TyConst = TyConst(Const.predef("Dict"))

  object FnType {
    final val MaxSize = 32

    private def predefFn(n: Int) = TyConst(Const.predef(s"Fn$n"))
    private val tpes = (1 to MaxSize).map(predefFn)
    private val fnMap: Map[TyConst, (TyConst, Int)] =
      (1 to MaxSize).iterator.map { idx =>
        val tyconst = tpes(idx - 1)
        tyconst -> (tyconst, idx)
      }.toMap

    object ValidArity {
      def unapply(n: Int): Boolean =
        (1 <= n) && (n <= MaxSize)
    }

    def apply(n: Int): Type.TyConst =
      if (ValidArity.unapply(n)) tpes(n - 1)
      else {
        throw new IllegalArgumentException(
          s"invalid FnType arity = $n, must be 0 < n <= $MaxSize"
        )
      }

    def maybeFakeName(n: Int): Type.TyConst =
      if (n <= MaxSize) apply(n)
      else {
        // This type doesn't exist but we will catch it in typechecking etc...
        predefFn(n)
      }

    def unapply(tpe: Type): Option[(Type.TyConst, Int)] =
      tpe match {
        case tyConst @ Type.TyConst(_) => fnMap.get(tyConst)
        case _                         => None
      }

    // FnType -> Kind(Kind.Type.contra, Kind.Type.co),
    val FnKinds: List[(Type.TyConst, Kind)] = {
      // -* -> -* ... -> +* -> *
      def kindSize(n: Int): Kind =
        Kind((Vector.fill(n)(Kind.Type.contra) :+ Kind.Type.co)*)

      tpes.iterator.zipWithIndex.map { case (t, n1) =>
        (t, kindSize(n1 + 1))
      }.toList
    }
  }
  val IntType: Type.TyConst = TyConst(Const.predef("Int"))
  val Float64Type: Type.TyConst = TyConst(Const.predef("Float64"))
  val ListType: Type.TyConst = TyConst(Const.predef("List"))
  val OptionType: Type.TyConst = TyConst(Const.predef("Option"))
  val StrType: Type.TyConst = TyConst(Const.predef("String"))
  val CharType: Type.TyConst = TyConst(Const.predef("Char"))
  val TestType: Type.TyConst = TyConst(Const.predef("Test"))
  val UnitType: Type.TyConst = TyConst(Type.Const.predef("Unit"))

  def const(pn: PackageName, name: TypeName): Type.Leaf =
    TyConst(Type.Const.Defined(pn, name))

  object Fun {
    def ifValid(from: NonEmptyList[Type], to: Type): Option[Type.Rho] = {
      val len = from.length
      if (len <= FnType.MaxSize)
        Some(apply(from, to))
      else None
    }

    def unapply(t: Type): Option[(NonEmptyList[Type], Type)] = {
      def check(
          n: Int,
          t: Type,
          applied: List[Type],
          last: Type
      ): Option[(NonEmptyList[Type], Type)] =
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

    /** Match if a type is a simple universal function, which is to say forall
      * a, b. C -> D where the result type is a Rho type.
      */
    object SimpleUniversal {
      def unapply(t: Type): Option[
        (NonEmptyList[(Type.Var.Bound, Kind)], NonEmptyList[Type], Type)
      ] =
        t match {
          case ForAll(univ, Fun(args, resT)) =>
            resT match {
              case ForAll(univR, res) =>
                // we need to relabel univR if it intersects univ
                val firstSet = univ.iterator.map(_._1).toSet
                val intersects = univR.filter { case (b, _) => firstSet(b) }

                NonEmptyList.fromList(intersects) match {
                  case None =>
                    Some((univ ::: univR, args, res))
                  case Some(interNel) =>
                    val good = univR.collect {
                      case pair @ (b, _) if !firstSet(b) => pair
                    }
                    val avoid = firstSet ++ good.iterator.map(_._1)
                    val rename = alignBinders(interNel, avoid)
                    val subMap =
                      rename.iterator
                        .map { case ((oldB, _), newB) =>
                          (oldB, Type.TyVar(newB))
                        }
                        .toMap[Type.Var, Type.Rho]

                    val bounds = univ.concat(good) ::: rename.map {
                      case ((_, k), b) => (b, k)
                    }
                    val newRes = Type.substituteVar(res, subMap)
                    Some((bounds, args, newRes))
                }
              case res => Some((univ, args, res))
            }
          case _ => None
        }
    }

    def apply(from: NonEmptyList[Type], to: Type): TyApply = {
      val arityFn = FnType.maybeFakeName(from.length)
      val withArgs = from.foldLeft(arityFn: (Leaf | TyApply))(TyApply(_, _))
      TyApply(withArgs, to)
    }
    def apply(from: Type, to: Type): TyApply =
      apply(NonEmptyList.one(from), to)

    def arity(t: Type): Int =
      t match {
        case ForAll(_, t) => arity(t)
        case Exists(_, t) => arity(t)
        case Fun(args, _) => args.length
        case _            => 0
      }
  }

  object Tuple {
    object Arity {
      private def alloc(i: Int): Type.TyConst =
        TyConst(Type.Const.predef(s"Tuple$i"))

      private val tupTypes: Array[Type.TyConst] =
        (Iterator.single(UnitType) ++ (1 to 32).iterator.map(alloc)).toArray

      def apply(n: Int): Type.TyConst =
        if (n <= 32) tupTypes(n)
        else alloc(n)

      def unapply(t: Type): Option[Int] =
        t match {
          case Type.UnitType => Some(0)
          case Type.TyConst(Const.Predef(cons))
              if cons.asString.startsWith("Tuple") =>
            Some(cons.asString.drop(5).toInt)
          case _ => None
        }
    }

    def unapply(t: Type): Option[List[Type]] = {
      def loop(idx: Int, t: Type, acc: List[Type]): Option[List[Type]] =
        t match {
          case Arity(a) if idx == a => Some(acc)
          case TyApply(left, right) =>
            loop(idx + 1, left, right :: acc)
          case _ => None
        }

      loop(0, t, Nil)
    }

    def apply(ts: List[Type]): Leaf | TyApply = {
      val sz = ts.size
      val root: Leaf | TyApply = Arity(sz)
      ts.foldLeft(root)((acc, t) => TyApply(acc, t))
    }

    val Kinds: List[(Type.TyConst, Kind)] = {
      // +* -> +* ... -> +* -> *
      def kindSize(n: Int): Kind =
        Kind(Vector.fill(n)(Kind.Type.co)*)

      (1 to 32).iterator.map(n => (Arity(n), kindSize(n))).toList
    }
  }

  object OptionT {
    def unapply(t: Type): Option[Type] =
      t match {
        case TyApply(OptionType, t) => Some(t)
        case _                      => None
      }
  }

  object DictT {
    def unapply(t: Type): Option[(Type, Type)] =
      t match {
        case TyApply(TyApply(DictType, kt), vt) => Some((kt, vt))
        case _                                  => None
      }
  }

  object ListT {
    def unapply(t: Type): Option[Type] =
      t match {
        case TyApply(ListType, t) => Some(t)
        case _                    => None
      }
  }

  sealed abstract class Const derives CanEqual {
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
          case _                                               => None
        }
    }

    implicit def orderTyConst[A <: Const]: Order[A] =
      new Order[A] {
        def compare(a: A, b: A) = {
          val Const.Defined(p0, n0) = a.toDefined
          val Const.Defined(p1, n1) = b.toDefined
          val c = Ordering[PackageName].compare(p0, p1)
          if (c == 0) Ordering[TypeName].compare(n0, n1) else c
        }
      }

    implicit val orderingTyConst: Ordering[Const] = orderTyConst.toOrdering
  }

  sealed abstract class Var {
    def name: String
  }
  object Var {
    case class Bound(name: String) extends Var
    case class Skolem(name: String, kind: Kind, existential: Boolean, id: Long)
        extends Var

    object Bound {
      private val cache: Array[Bound] =
        ('a' to 'z').map(c => new Bound(c.toString)).toArray

      def apply(str: String): Bound =
        if (str.length == 1) {
          val c = str.charAt(0)
          if ('a' <= c && c <= 'z') {
            cache(c - 'a')
          } else new Bound(str)
        } else new Bound(str)

      implicit val orderBound: Order[Bound] =
        Order.by[Bound, String](_.name)
    }

    implicit val varOrdering: Ordering[Var] =
      new Ordering[Var] {
        def compare(a: Var, b: Var): Int =
          (a, b) match {
            case (Bound(a), Bound(b)) => a.compareTo(b)
            case (Bound(_), _)        => -1
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

    implicit val orderVar: Order[Var] =
      Order.fromOrdering(using varOrdering)
  }

  val allBinders: LazyList[Var.Bound] = {
    val letters = ('a' to 'z').to(LazyList)
    val allIntegers = LazyList.iterate(0L)(_ + 1L)
    val lettersWithNumber =
      for {
        num <- allIntegers
        l <- letters
      } yield Var.Bound(s"$l$num")

    letters.map(c => Var.Bound(c.toString)) #::: lettersWithNumber
  }

  def alignBinders[A](
      items: NonEmptyList[A],
      avoid: Var.Bound => Boolean
  ): NonEmptyList[(A, Var.Bound)] = {
    val sz = items.size
    // for some reason on 2.11 we need to do .iterator or this will be an infinite loop
    val bs = NonEmptyList.fromListUnsafe(
      allBinders.iterator.filterNot(avoid).take(sz).toList
    )
    NonEmptyList((items.head, bs.head), items.tail.zip(bs.tail))
  }

  def alignBinders[A](
      items: List[A],
      avoid: Var.Bound => Boolean
  ): List[(A, Var.Bound)] =
    NonEmptyList.fromList(items) match {
      case Some(nel) => alignBinders(nel, avoid).toList
      case None      => Nil
    }

  case class Meta(
      kind: Kind,
      id: Long,
      existential: Boolean,
      ref: Ref[Option[Type.Tau]]
  )

  object Meta {
    implicit val orderingMeta: Ordering[Meta] =
      new Ordering[Meta] {
        def compare(x: Meta, y: Meta): Int =
          if (x.existential) {
            if (y.existential) java.lang.Long.compare(x.id, y.id)
            else -1
          } else {
            if (!y.existential) java.lang.Long.compare(x.id, y.id)
            else 1
          }
      }
  }

  given Hashable[Type] with {
    private def loopVar[B](
        v: Type.Var,
        bound: List[Type.Var.Bound],
        algo: Algo[B]
    )(hasher: algo.Hasher): algo.Hasher =
      v match {
        case b: Type.Var.Bound =>
          val idx = bound.indexOf(b)
          if (idx >= 0) {
            val withTag = Hashable[Int].addHash(0, algo)(hasher)
            Hashable[Int].addHash(idx, algo)(withTag)
          } else {
            val withTag = Hashable[Int].addHash(1, algo)(hasher)
            Hashable[Type.Var.Bound].addHash(b, algo)(withTag)
          }
        case s: Type.Var.Skolem =>
          val withTag = Hashable[Int].addHash(2, algo)(hasher)
          Hashable[Type.Var.Skolem].addHash(s, algo)(withTag)
      }

    private def loopType[B](
        t: Type,
        bound: List[Type.Var.Bound],
        algo: Algo[B]
    )(hasher: algo.Hasher): algo.Hasher =
      t match {
        case Type.TyConst(Type.Const.Predef(cons)) =>
          val withTag = Hashable[Int].addHash(3, algo)(hasher)
          val withCons =
            Hashable[Identifier.Constructor].addHash(cons, algo)(withTag)
          withCons
        case Type.TyConst(Type.Const.Defined(pack, name)) =>
          val withTag = Hashable[Int].addHash(4, algo)(hasher)
          val withPack = Hashable[PackageName].addHash(pack, algo)(withTag)
          Hashable[TypeName].addHash(name, algo)(withPack)
        case Type.TyVar(v) =>
          loopVar(v, bound, algo)(hasher)
        case Type.TyMeta(meta) =>
          val withTag = Hashable[Int].addHash(5, algo)(hasher)
          val withId = Hashable[Long].addHash(meta.id, algo)(withTag)
          val withExistential =
            Hashable[Boolean].addHash(meta.existential, algo)(withId)
          val withKind =
            Hashable[Kind].addHash(meta.kind, algo)(withExistential)
          withKind
        case Type.TyApply(on, arg) =>
          val withTag = Hashable[Int].addHash(6, algo)(hasher)
          val withOn = loopType(on, bound, algo)(withTag)
          loopType(arg, bound, algo)(withOn)
        case Type.ForAll(vars, in) =>
          val varsList = vars.toList
          val withTag = Hashable[Int].addHash(7, algo)(hasher)
          val withSize = Hashable[Int].addHash(varsList.size, algo)(withTag)
          val withKinds = varsList.foldLeft(withSize) { case (h, (_, kind)) =>
            Hashable[Kind].addHash(kind, algo)(h)
          }
          val bound1 = varsList.reverse.map(_._1) ::: bound
          loopType(in, bound1, algo)(withKinds)
        case Type.Exists(vars, in) =>
          val varsList = vars.toList
          val withTag = Hashable[Int].addHash(8, algo)(hasher)
          val withSize = Hashable[Int].addHash(varsList.size, algo)(withTag)
          val withKinds = varsList.foldLeft(withSize) { case (h, (_, kind)) =>
            Hashable[Kind].addHash(kind, algo)(h)
          }
          val bound1 = varsList.reverse.map(_._1) ::: bound
          loopType(in, bound1, algo)(withKinds)
      }

    def addHash[B](tpe: Type, algo: Algo[B])(
        hasher: algo.Hasher
    ): algo.Hasher =
      loopType(tpe.normalize, Nil, algo)(hasher)
  }

  /** Final the set of all of Metas inside the list of given types
    */
  def metaTvs(s: List[Type]): SortedSet[Meta] = {
    val metas = scala.collection.mutable.HashSet.empty[Meta]
    val check = scala.collection.mutable.ArrayDeque.empty[Type]

    val initIter = s.iterator
    while (initIter.hasNext) {
      check.append(initIter.next())
    }

    while (check.nonEmpty) {
      check.removeHead() match {
        case ForAll(_, r)  => check.prepend(r)
        case Exists(_, r)  => check.prepend(r)
        case TyApply(a, r) =>
          check.prepend(r)
          check.prepend(a)
        case TyMeta(m) => metas.add(m): Unit
        case _         => ()
      }
    }

    SortedSet.from(metas)
  }

  /** Report bound variables which are used in quantify. When we infer a sigma
    * type
    */
  def tyVarBinders(tpes: List[Type]): Set[Type.Var.Bound] = {
    @annotation.tailrec
    def loop(tpes: List[Type], acc: Set[Type.Var.Bound]): Set[Type.Var.Bound] =
      tpes match {
        case Nil                            => acc
        case t :: rest if hasQuantifiers(t) =>
          loop(rest, acc ++ quantVars(t).iterator.map(_._1))
        case Type.TyApply(arg, res) :: rest =>
          loop(arg :: res :: rest, acc)
        case _ :: rest => loop(rest, acc)
      }
    loop(tpes, Set.empty)
  }

  /** strange name, but the idea is to replace a Meta with a resolved Rho value.
    * I think the name resolve might be better, but the paper I started from
    * used zonk
    */
  def zonk[F[_]: Monad](
      transparent: SortedSet[Meta],
      readMeta: Meta => F[Option[Tau]],
      writeMeta: (Meta, Tau) => F[Unit]
  ): Meta => F[Option[Tau]] = {

    val pureNone = Monad[F].pure(Option.empty[Rho])

    lazy val fn: Meta => F[Option[Rho]] = { (m: Meta) =>
      if (m.existential && !transparent(m)) pureNone
      else
        readMeta(m).flatMap {
          case None => pureNone
          case (sm @ Some(tm: Type.TyMeta))
              if tm.toMeta.existential && !transparent(tm.toMeta) =>
            // don't zonk from non-existential past existential or we forget
            // that this variable is existential and can see through it
            Monad[F].pure(sm)
          case sty @ Some(ty) =>
            zonkRhoMeta(ty)(fn).flatMap { ty1 =>
              if ((ty1: Type) == ty) Monad[F].pure(sty)
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

  /** Resolve known meta variables nested inside t
    */
  def zonkMeta[F[_]: Applicative](
      t: Type
  )(m: Meta => F[Option[Type.Tau]]): F[Type] =
    t match {
      case rho: Rho         => zonkRhoMeta(rho)(m).widen
      case fa @ ForAll(vars, in) =>
        zonkRhoMeta(in)(m).map { tpe =>
          if (tpe eq in) fa
          else forAll(vars, tpe)
        }
    }

  /** Resolve known meta variables nested inside t
    */
  def zonkRhoMeta[F[_]: Applicative](
      t: Type.Rho
  )(mfn: Meta => F[Option[Type.Tau]]): F[Type.Rho] =
    val app = Applicative[F]
    t match {
      case ex @ Exists(vars, in) =>
        zonkRhoMeta(in)(mfn).map { in1 =>
          if (in1 eq in) ex
          else existsRho(vars, in1)
        }
      case ta @ Type.TyApply(on, arg) =>
        app.map2(zonkRhoMeta(on)(mfn), zonkMeta(arg)(mfn)) {
          case (la: (Leaf | TyApply), arg1) =>
            if ((la eq on) && (arg1 eq arg)) ta
            else TyApply(la, arg1)
          case (e: Exists, arg1)            =>
            // zonking replaced an inner Leaf | TyApply with an exists, but
            // it may shadow values in arg. We need to lift it out
            // but without pulling arg into the exists.
            val frees = freeTyVars(arg1 :: Nil)
            val (subst, newVars) =
              if (frees.isEmpty) (Map.empty[Type.Var, Type.TyVar], e.vars)
              else unshadow(e.vars, frees.toSet)
            val newIn =
              if (subst.isEmpty) e.in
              else substituteLeafApplyVar(e.in, subst)
            existsRho(newVars, TyApply(newIn, arg1))
        }
      case t @ Type.TyMeta(m) =>
        mfn(m).map {
          case None      => t
          case Some(rho) => rho
        }
      case (Type.TyConst(_) | Type.TyVar(_)) => app.pure(t)
    }

  private object FullResolved extends TypeParser[Type] {
    lazy val parseRoot: P[Type] = {
      val tvar = Parser.lowerIdent.map(s => Type.TyVar(Type.Var.Bound(s)))
      val name =
        ((PackageName.parser <* P.string("::")) ~ Identifier.consParser)
          .map { case (p, n) =>
            Type.TyConst(Type.Const.Defined(p, TypeName(n)))
          }
      val longParser: P[Long] = Numbers.signedIntString.mapFilter { str =>
        try Some(str.toLong)
        catch {
          case _: NumberFormatException => None
        }
      }
      val existential = P.char('e').?
      val skolem = (
        P.char('$') *> Parser.lowerIdent,
        P.char('$') *> (longParser ~ existential)
      )
        // TODO Kind/existential
        .mapN { case (n, (id, ex)) =>
          Var.Skolem(n, Kind.Type, ex.isDefined, id)
        }
        .map(TyVar(_))

      val meta = (P.char('?') *> (existential ~ longParser))
        .map { case (opt, l) =>
          // TODO Kind is wrong
          // this constRef is safe because it
          // won't parse before type inference anyway
          // the ideal solution is to better static type information
          // to have fully inferred types with no skolems or metas
          TyMeta(
            Meta(Kind.Type, l, opt.isDefined, RefSpace.constRef(Option.empty))
          )
        }

      tvar.orElse(name).orElse(skolem).orElse(meta)
    }

    def makeFn(in: NonEmptyList[Type], out: Type): Type =
      // this may be an invalid function, but typechecking verifies that.
      Type.Fun(in, out)

    def applyTypes(left: Type, args: NonEmptyList[Type]) =
      applyAll(left, args.toList)

    def universal(vs: NonEmptyList[(String, Option[Kind])], on: Type): Type =
      Type.forAll(
        vs.map {
          case (s, None)    => (Type.Var.Bound(s), Kind.Type)
          case (s, Some(k)) => (Type.Var.Bound(s), k)
        },
        on
      )

    def existential(vs: NonEmptyList[(String, Option[Kind])], on: Type): Type =
      Type.exists(
        vs.map {
          case (s, None)    => (Type.Var.Bound(s), Kind.Type)
          case (s, Some(k)) => (Type.Var.Bound(s), k)
        },
        on
      )

    def makeTuple(lst: List[Type]) = Type.Tuple(lst)

    private val coloncolon = Doc.text("::")

    def unapplyRoot(a: Type): Option[Doc] =
      a match {
        case TyConst(Const.Defined(p, n)) =>
          Some(
            Document[PackageName]
              .document(p) + coloncolon + Document[Identifier].document(n.ident)
          )
        case TyVar(Var.Bound(s))           => Some(Doc.text(s))
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
        case _          => None
      }

    def unapplyUniversal(
        a: Type
    ): Option[(List[(String, Option[Kind])], Type)] =
      a match {
        case ForAll(vs, in) =>
          Some(
            (
              vs.map { case (v, k) =>
                (v.name, Some(k))
              }.toList,
              in
            )
          )
        case _ => None
      }

    def unapplyExistential(
        a: Type
    ): Option[(List[(String, Option[Kind])], Type)] =
      a match {
        case Exists(vs, in) =>
          Some(
            (
              vs.map { case (v, k) =>
                (v.name, Some(k))
              }.toList,
              in
            )
          )
        case _ => None
      }

    def unapplyTypeApply(a: Type): Option[(Type, List[Type])] =
      a match {
        case ta @ TyApply(_, _) => Some(unapplyAll(ta))
        case _                  => None
      }

    def unapplyTuple(a: Type): Option[List[Type]] =
      a match {
        case Tuple(as) => Some(as)
        case _         => None
      }
  }

  /** Parse fully resolved types: package::type
    */
  def fullyResolvedParser: P[Type] = FullResolved.parser
  def fullyResolvedDocument: Document[Type] = FullResolved.document
  def typeParser: TypeParser[Type] = FullResolved

  def tupleCodeGen(n: Int): String = {
    Require(n > 0)

    val tpes = alignBinders((1 to n).toList, Set.empty)

    val tpesDecl = tpes.iterator
      .map { case (_, tpe) =>
        s"${tpe.name}: +*"
      }
      .mkString("[", ", ", "]")

    tpes.iterator
      .map { case (i, v) => s"item$i: ${v.name}" }
      .mkString(s"struct Tuple$n$tpesDecl(", ", ", ")")
  }

  def allTupleCode: String =
    (1 to 32).iterator.map(i => s"Tuple$i()").mkString("", ",\n", ",\n") +
      (1 to 32).iterator.map(tupleCodeGen).mkString("\n")

  val builtInKinds: Map[Type.Const.Defined, Kind] =
    (FnType.FnKinds ::: Tuple.Kinds ::: List(
      BoolType -> Kind.Type,
      DictType -> Kind(Kind.Type.in, Kind.Type.co),
      IntType -> Kind.Type,
      Float64Type -> Kind.Type,
      ListType -> Kind(Kind.Type.co),
      StrType -> Kind.Type,
      CharType -> Kind.Type,
      UnitType -> Kind.Type
    )).map { case (t, k) => (t.tpe.toDefined, k) }.toMap

}
