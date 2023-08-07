package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import cats.parse.{Parser => P, Numbers}
import cats.{Applicative, Order}
import org.typelevel.paiges.{Doc, Document}
import org.bykn.bosatsu.{
  Kind,
  PackageName,
  Lit,
  TypeName,
  Identifier,
  Parser,
  TypeParser
}
import scala.collection.immutable.SortedSet

import cats.implicits._

sealed abstract class Type {
  def sameAs(that: Type): Boolean = Type.sameType(this, that)
}

object Type {

  /** A type with no top level ForAll
    */
  sealed abstract class Rho extends Type
  sealed abstract class Leaf extends Rho
  type Tau = Rho // no forall anywhere

  case class ForAll(vars: NonEmptyList[(Var.Bound, Kind)], in: Rho) extends Type
  case class TyApply(on: Type, arg: Type) extends Rho
  case class TyConst(tpe: Const) extends Leaf
  case class TyVar(toVar: Var) extends Leaf
  case class TyMeta(toMeta: Meta) extends Leaf

  def sameType(left: Type, right: Type): Boolean =
    if (left.isInstanceOf[Leaf] && right.isInstanceOf[Leaf]) {
      left == right
    } else {
      normalize(left) == normalize(right)
    }

  implicit val typeOrder: Order[Type] =
    new Order[Type] {
      val boundOrd: Ordering[Var.Bound] =
        Ordering[String].on[Var.Bound] { case Var.Bound(v) => v }

      val list = org.bykn.bosatsu.ListOrdering.onType(
        Ordering.Tuple2(boundOrd, Kind.orderingKind)
      )

      def compare(a: Type, b: Type): Int =
        (a, b) match {
          case (ForAll(v0, i0), ForAll(v1, i1)) =>
            val c = list.compare(v0.toList, v1.toList)
            if (c == 0) compare(i0, i1) else c
          case (ForAll(_, _), _) => -1
          case (
                TyConst(Const.Defined(p0, n0)),
                TyConst(Const.Defined(p1, n1))
              ) =>
            val c = Ordering[PackageName].compare(p0, p1)
            if (c == 0) Ordering[TypeName].compare(n0, n1) else c
          case (TyConst(_), ForAll(_, _)) => 1
          case (TyConst(_), _)            => -1
          case (TyVar(v0), TyVar(v1)) =>
            Ordering[Var].compare(v0, v1)
          case (TyVar(_), ForAll(_, _) | TyConst(_)) => 1
          case (TyVar(_), _)                         => -1
          case (TyMeta(Meta(_, i0, _)), TyMeta(Meta(_, i1, _))) =>
            java.lang.Long.compare(i0, i1)
          case (TyMeta(_), TyApply(_, _)) => -1
          case (TyMeta(_), _)             => 1
          case (TyApply(a0, b0), TyApply(a1, b1)) =>
            val c = compare(a0, a1)
            if (c == 0) compare(b0, b1) else c
          case (TyApply(_, _), _) => 1
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
        case notApply       => (notApply, acc)
      }

    loop(fn, Nil)
  }

  def constantsOf(t: Type): List[Const] =
    t match {
      case ForAll(_, t)         => constantsOf(t)
      case TyApply(on, arg)     => constantsOf(on) ::: constantsOf(arg)
      case TyConst(c)           => c :: Nil
      case TyVar(_) | TyMeta(_) => Nil
    }

  def hasNoVars(t: Type): Boolean =
    t match {
      case TyConst(_)           => true
      case TyVar(_) | TyMeta(_) => false
      case TyApply(on, arg)     => hasNoVars(on) && hasNoVars(arg)
      case fa @ ForAll(_, _)    => freeTyVars(fa :: Nil).isEmpty
    }

  final def forAll(vars: List[(Var.Bound, Kind)], in: Type): Type =
    NonEmptyList.fromList(vars) match {
      case None     => in
      case Some(ne) => forAll(ne, in)
    }

  final def forAll(vars: NonEmptyList[(Var.Bound, Kind)], in: Type): Type =
    in match {
      case rho: Rho              => Type.ForAll(vars, rho)
      case Type.ForAll(ne1, rho) => Type.ForAll(vars ::: ne1, rho)
    }

  def getTypeOf(lit: Lit): Type =
    lit match {
      case Lit.Integer(_) => Type.IntType
      case Lit.Str(_)     => Type.StrType
    }

  /** types are var, meta, or const, or applied or forall on one of those. This
    * returns the Type.TyConst found by recursing
    */
  @annotation.tailrec
  final def rootConst(t: Type): Option[Type.TyConst] =
    t match {
      case tyc @ TyConst(_)     => Some(tyc)
      case TyVar(_) | TyMeta(_) => None
      case ForAll(_, r)         => rootConst(r)
      case TyApply(left, _)     => rootConst(left)
    }

  def applicationArgs(t: Type): (Type, List[Type]) = {
    @annotation.tailrec
    def loop(t: Type, tail: List[Type]): (Type, List[Type]) =
      t match {
        case TyApply(left, right) => loop(left, right :: tail)
        case notApply             => (notApply, tail)
      }
    loop(t, Nil)
  }

  /** This form is often useful in Infer
    */
  def substTy(
      keys: NonEmptyList[Var],
      vals: NonEmptyList[Type]
  ): Type => Type = {
    val env = keys.toList.iterator.zip(vals.toList.iterator).toMap

    { t => substituteVar(t, env) }
  }

  def substituteVar(t: Type, env: Map[Type.Var, Type]): Type =
    t match {
      case TyApply(on, arg) =>
        TyApply(substituteVar(on, env), substituteVar(arg, env))
      case v @ TyVar(n) => env.getOrElse(n, v)
      case ForAll(ns, rho) =>
        val boundSet: Set[Var] = ns.toList.iterator.map(_._1).toSet
        val env1 = env.iterator.filter { case (v, _) => !boundSet(v) }.toMap
        forAll(ns.toList, substituteVar(rho, env1))
      case m @ TyMeta(_)  => m
      case c @ TyConst(_) => c
    }

  def substituteRhoVar(t: Type.Rho, env: Map[Type.Var, Type.Rho]): Type.Rho =
    t match {
      case TyApply(on, arg) =>
        TyApply(substituteVar(on, env), substituteVar(arg, env))
      case v @ TyVar(n)   => env.getOrElse(n, v)
      case m @ TyMeta(_)  => m
      case c @ TyConst(_) => c
    }

  /** Return the Bound and Skolem variables that are free in the given list of
    * types
    */
  def freeTyVars(ts: List[Type]): List[Type.Var] = {

    // usually we can recurse in a loop, but sometimes not
    def cheat(
        ts: List[Type],
        bound: Set[Type.Var.Bound],
        acc: List[Type.Var]
    ): List[Type.Var] =
      go(ts, bound, acc)

    @annotation.tailrec
    def go(
        ts: List[Type],
        bound: Set[Type.Var.Bound],
        acc: List[Type.Var]
    ): List[Type.Var] =
      ts match {
        case Nil                    => acc
        case Type.TyVar(tv) :: rest =>
          // we only check here, we don't add
          val isBound =
            tv match {
              case b @ Type.Var.Bound(_)    => bound(b)
              case Type.Var.Skolem(_, _, _) => false
            }
          if (isBound) go(rest, bound, acc)
          else go(rest, bound, tv :: acc)
        case Type.TyApply(a, b) :: rest => go(a :: b :: rest, bound, acc)
        case Type.ForAll(tvs, ty) :: rest =>
          val acc1 =
            cheat(ty :: Nil, bound ++ tvs.toList.iterator.map(_._1), acc)
          // note, tvs ARE NOT bound in rest
          go(rest, bound, acc1)
        case (Type.TyMeta(_) | Type.TyConst(_)) :: rest => go(rest, bound, acc)
      }

    go(ts, Set.empty, Nil).reverse.distinct
  }

  /** Return the Bound variables that are free in the given list of types
    */
  def freeBoundTyVars(ts: List[Type]): List[Type.Var.Bound] =
    freeTyVars(ts).collect { case b @ Type.Var.Bound(_) => b }

  def normalize(tpe: Type): Type =
    tpe match {
      case ForAll(vars0, in) =>
        val inFree = freeBoundTyVars(in :: Nil)
        val inFreeSet = inFree.toSet
        val vars1 = vars0.filter { case (b, _) => inFreeSet(b) }

        NonEmptyList.fromList(vars1) match {
          case Some(vars2) =>
            val vars =
              if (vars2.tail.isEmpty) {
                // already sorted
                vars2
              } else {
                // sort the quantification by the order of appearance
                val order = inFree.iterator.zipWithIndex.toMap
                vars2.sortBy { case (b, _) => order(b) }
              }
            val frees = freeBoundTyVars(tpe :: Nil).toSet
            val bs = alignBinders(vars, frees)
            val subMap = bs.toList
              .map { case ((bold, _), bnew) =>
                bold -> TyVar(bnew)
              }
              .toMap[Type.Var, Type.Rho]

            forAll(
              bs.toList.map { case ((_, k), b) => (b, k) },
              normalize(substituteRhoVar(in, subMap))
            )
          case None => normalize(in)
        }

      case TyApply(on, arg) => TyApply(normalize(on), normalize(arg))
      case _                => tpe
    }

  /** These are upper-case to leverage scala's pattern matching on upper-cased
    * vals
    */
  val BoolType: Type.TyConst = TyConst(Const.predef("Bool"))
  val DictType: Type.TyConst = TyConst(Const.predef("Dict"))
  val FnType: Type.TyConst = TyConst(Const.predef("Fn"))
  val IntType: Type.TyConst = TyConst(Const.predef("Int"))
  val ListType: Type.TyConst = TyConst(Const.predef("List"))
  val OptionType: Type.TyConst = TyConst(Const.predef("Option"))
  val StrType: Type.TyConst = TyConst(Const.predef("String"))
  val TestType: Type.TyConst = TyConst(Const.predef("Test"))
  val TupleConsType: Type.TyConst = TyConst(Type.Const.predef("TupleCons"))
  val UnitType: Type.TyConst = TyConst(Type.Const.predef("Unit"))

  val builtInKinds: Map[Type.Const.Defined, Kind] =
    List(
      BoolType -> Kind.Type,
      DictType -> Kind(Kind.Type.in, Kind.Type.co),
      FnType -> Kind(Kind.Type.contra, Kind.Type.co),
      IntType -> Kind.Type,
      ListType -> Kind(Kind.Type.co),
      StrType -> Kind.Type,
      UnitType -> Kind.Type,
      TupleConsType -> Kind(Kind.Type.co, Kind.Type.co)
    ).map { case (t, k) => (t.tpe.toDefined, k) }.toMap

  def const(pn: PackageName, name: TypeName): Type =
    TyConst(Type.Const.Defined(pn, name))

  object Fun {
    def unapply(t: Type): Option[(Type, Type)] =
      t match {
        case TyApply(TyApply(FnType, from), to) =>
          Some((from, to))
        case _ => None
      }

    def apply(from: Type, to: Type): Type.Rho =
      TyApply(TyApply(FnType, from), to)

    def arity(t: Type): Int =
      t match {
        case ForAll(_, t) => arity(t)
        case fn @ Fun(_, _) =>
          uncurry(fn).fold(0)(_._1.length)
        case _ => 0
      }

    /** a -> b -> c .. -> d to [a, b, c, ..] -> d
      */
    def uncurry(t: Type): Option[(NonEmptyList[Type], Type)] =
      t match {
        case Fun(a, b) =>
          uncurry(b) match {
            case Some((ne1, t)) => Some((ne1.prepend(a), t))
            case None           => Some((NonEmptyList(a, Nil), b))
          }
        case _ => None
      }

    def curry(args: NonEmptyList[Type], res: Type): Type.Rho =
      args match {
        case NonEmptyList(h, Nil) => Fun(h, res)
        case NonEmptyList(h1, h2 :: tail) =>
          Fun(h1, curry(NonEmptyList(h2, tail), res))
      }
  }

  object Tuple {
    def unapply(t: Type): Option[List[Type]] =
      t match {
        case UnitType => Some(Nil)
        case TyApply(TyApply(TupleConsType, h), t) =>
          unapply(t) match {
            case None     => None
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

  sealed abstract class Const {
    def toDefined: Const.Defined
  }
  object Const {
    case class Defined(packageName: PackageName, name: TypeName) extends Const {
      def toDefined: Defined = this
    }

    def predef(name: String): Defined =
      Defined(PackageName.PredefName, TypeName(Identifier.Constructor(name)))
  }

  sealed abstract class Var {
    def name: String
  }
  object Var {
    case class Bound(name: String) extends Var
    case class Skolem(name: String, kind: Kind, id: Long) extends Var

    object Bound {
      private[this] val cache: Array[Bound] =
        ('a' to 'z').map { c => new Bound(c.toString) }.toArray

      def apply(str: String): Bound =
        if (str.length == 1) {
          val c = str.charAt(0)
          if ('a' <= c && c <= 'z') {
            cache(c - 'a')
          } else new Bound(str)
        } else new Bound(str)
    }

    implicit val varOrdering: Ordering[Var] =
      new Ordering[Var] {
        def compare(a: Var, b: Var): Int =
          (a, b) match {
            case (Bound(a), Bound(b)) => a.compareTo(b)
            case (Bound(_), _)        => -1
            case (Skolem(n0, k0, i0), Skolem(n1, k1, i1)) =>
              val c = java.lang.Long.compare(i0, i1)
              if (c != 0) c
              else {
                val cn = n0.compareTo(n1)
                if (cn != 0) cn
                else Order[Kind].compare(k0, k1)
              }
            case (Skolem(_, _, _), _) => 1
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

  def alignBinders[A](
      items: NonEmptyList[A],
      avoid: Set[Var.Bound]
  ): NonEmptyList[(A, Var.Bound)] = {
    val sz = items.size
    // for some reason on 2.11 we need to do .iterator or this will be an infinite loop
    val bs = NonEmptyList.fromListUnsafe(
      allBinders.iterator.filterNot(avoid).take(sz).toList
    )
    NonEmptyList((items.head, bs.head), items.tail.zip(bs.tail))
  }

  case class Meta(kind: Kind, id: Long, ref: Ref[Option[Type.Tau]])

  object Meta {
    implicit val orderingMeta: Ordering[Meta] =
      Ordering.by { (m: Meta) => m.id }
  }

  /** Final the set of all of Metas inside the list of given types
    */
  def metaTvs(s: List[Type]): SortedSet[Meta] = {
    @annotation.tailrec
    def go(check: List[Type], acc: SortedSet[Meta]): SortedSet[Meta] =
      check match {
        case Nil                   => acc
        case ForAll(_, r) :: tail  => go(r :: tail, acc)
        case TyApply(a, r) :: tail => go(a :: r :: tail, acc)
        case TyMeta(m) :: tail     => go(tail, acc + m)
        case _ :: tail             => go(tail, acc)
      }
    go(s, SortedSet.empty)
  }

  /** Report bound variables which are used in quantify. When we infer a sigma
    * type
    */
  def tyVarBinders(tpes: List[Type]): Set[Type.Var.Bound] = {
    @annotation.tailrec
    def loop(tpes: List[Type], acc: Set[Type.Var.Bound]): Set[Type.Var.Bound] =
      tpes match {
        case Nil => acc
        case Type.ForAll(tvs, _) :: rest =>
          loop(rest, acc ++ tvs.toList.iterator.map(_._1))
        case Type.TyApply(arg, res) :: rest =>
          loop(arg :: res :: rest, acc)
        case _ :: rest => loop(rest, acc)
      }
    loop(tpes, Set.empty)
  }

  /** Transform meta variables in some way
    */
  def zonkMeta[F[_]: Applicative](
      t: Type
  )(m: Meta => F[Option[Type.Rho]]): F[Type] =
    t match {
      case rho: Rho => zonkRhoMeta(rho)(m).widen
      case ForAll(ns, ty) =>
        zonkRhoMeta(ty)(m).map(Type.ForAll(ns, _))
    }

  /** Transform meta variables in some way
    */
  def zonkRhoMeta[F[_]: Applicative](
      t: Type.Rho
  )(mfn: Meta => F[Option[Type.Rho]]): F[Type.Rho] =
    t match {
      case Type.TyApply(on, arg) =>
        (zonkMeta(on)(mfn), zonkMeta(arg)(mfn)).mapN(Type.TyApply(_, _))
      case t @ Type.TyMeta(m) =>
        mfn(m).map {
          case None      => t
          case Some(rho) => rho
        }
      case (Type.TyConst(_) | Type.TyVar(_)) => Applicative[F].pure(t)
    }

  private object FullResolved extends TypeParser[Type] {
    lazy val parseRoot = {
      val tvar = Parser.lowerIdent.map { s => Type.TyVar(Type.Var.Bound(s)) }
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
      val skolem = (P.char('$') *> Parser.lowerIdent, P.char('$') *> longParser)
        // TODO Kind
        .mapN(Var.Skolem(_, Kind.Type, _))
        .map(TyVar(_))

      // this null is bad, but we have no way to reallocate this
      // and won't parse before type inference anyway
      // the ideal solution is to better static type information
      // to have fully inferred types with no skolems or metas
      // TODO Kind
      val meta = (P.char('?') *> longParser).map { l =>
        TyMeta(Meta(Kind.Type, l, null))
      }

      tvar.orElse(name).orElse(skolem).orElse(meta)
    }

    def makeFn(in: Type, out: Type) = Type.Fun(in, out)
    def applyTypes(left: Type, args: NonEmptyList[Type]) =
      applyAll(left, args.toList)

    def universal(vs: NonEmptyList[(String, Option[Kind])], on: Type) =
      Type.forAll(
        vs.map {
          case (s, None)    => (Type.Var.Bound(s), Kind.Type)
          case (s, Some(k)) => (Type.Var.Bound(s), k)
        },
        on
      )

    def makeTuple(lst: List[Type]) = Type.Tuple(lst)

    private[this] val coloncolon = Doc.text("::")

    def unapplyRoot(a: Type): Option[Doc] =
      a match {
        case TyConst(Const.Defined(p, n)) =>
          Some(
            Document[PackageName]
              .document(p) + coloncolon + Document[Identifier].document(n.ident)
          )
        case TyVar(Var.Bound(s))        => Some(Doc.text(s))
        case TyVar(Var.Skolem(n, _, i)) =>
          // TODO Kind
          val dol = "$"
          Some(Doc.text(dol + n + dol + i.toString))
        case TyMeta(Meta(_, i, _)) =>
          // TODO Kind
          Some(Doc.text("?" + i.toString))
        case _ => None
      }

    def unapplyFn(a: Type): Option[(Type, Type)] =
      a match {
        case Fun(a, b) => Some((a, b))
        case _         => None
      }

    def unapplyUniversal(
        a: Type
    ): Option[(List[(String, Option[Kind])], Type)] =
      a match {
        case ForAll(vs, arg) =>
          Some(
            (
              vs.map { case (v, k) =>
                (v.name, Some(k))
              }.toList,
              arg
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
}
