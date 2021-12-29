package org.bykn.bosatsu

import cats.Applicative
import cats.data.{NonEmptyList, State}
import cats.implicits._
import cats.parse.{Parser => P}
import org.bykn.bosatsu.rankn.Type
import org.bykn.bosatsu.{TypeName => Name}
import org.typelevel.paiges.{ Doc, Document }

import Parser.maybeSpace

/**
 * This AST is the syntactic version of Type
 * it is shaped slightly differently to match the way
 * the syntax looks (nested non empty lists are explicit
 * whereas we use a recursion/cons style in Type
 */
sealed abstract class TypeRef {
  import TypeRef._

  def toDoc: Doc =
    TypeRef.document.document(this)

  /**
   * Nested TypeLambda can be combined, and should be generally
   */
  def normalizeForAll: TypeRef =
    this match {
      case TypeVar(_) | TypeName(_) => this
      case TypeArrow(a, b) => TypeArrow(a.normalizeForAll, b.normalizeForAll)
      case TypeApply(a, bs) =>
        TypeApply(a.normalizeForAll, bs.map(_.normalizeForAll))
      case TypeLambda(pars0, TypeLambda(pars1, e)) =>
        // we normalize to lifting all the foralls to the outside
        TypeLambda(pars0 ::: pars1, e).normalizeForAll
      case TypeLambda(pars, e) =>
        TypeLambda(pars, e.normalizeForAll)
      case TypeTuple(ts) =>
        TypeTuple(ts.map(_.normalizeForAll))
    }
}

object TypeRef {
  private val colonSpace = Doc.text(": ")


  def argDoc[A: Document](st: (A, Option[TypeRef])): Doc =
    st match {
      case (s, None) => Document[A].document(s)
      case (s, Some(tr)) => Document[A].document(s) + colonSpace + (tr.toDoc)
    }

  case class TypeVar(asString: String) extends TypeRef {
    def toBoundVar: Type.Var.Bound = Type.Var.Bound(asString)
  }
  case class TypeName(name: Name) extends TypeRef
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  case class TypeLambda(params: NonEmptyList[TypeVar], in: TypeRef) extends TypeRef
  case class TypeTuple(params: List[TypeRef]) extends TypeRef

  def fromTypeA[F[_]: Applicative](
    tpe: Type,
    onSkolem: Type.Var.Skolem => F[TypeRef],
    onMeta: Long => F[TypeRef],
    onConst: Type.Const.Defined => F[TypeRef]): F[TypeRef] = {
    import Type._
    def loop(tpe: Type) = fromTypeA(tpe, onSkolem, onMeta, onConst)

    tpe match {
      case ForAll(vs, in) =>
        val args = vs.map { case Type.Var.Bound(b) => TypeVar(b) }
        loop(in).map(TypeLambda(args, _))
      case Type.Tuple(ts) =>
        // this needs to be above TyConst
        ts.traverse(loop(_)).map(TypeTuple(_))
      case TyConst(defined@Type.Const.Defined(_, _)) =>
        onConst(defined)
      case Type.Fun(from, to) =>
        (loop(from), loop(to)).mapN { (ftr, ttr) =>
          TypeArrow(ftr, ttr)
        }
      case ta@TyApply(_, _) =>
        val (on, args) = unapplyAll(ta)
        (loop(on), args.traverse(loop)).mapN {
          (of, arg1) =>
            TypeApply(of, NonEmptyList.fromListUnsafe(arg1))
        }
      case TyVar(tv) =>
        tv match {
          case Type.Var.Bound(v) =>
            Applicative[F].pure(TypeVar(v))
          case sk@Type.Var.Skolem(_, _) =>
            onSkolem(sk)
        }
      case TyMeta(Type.Meta(id, _)) =>
        onMeta(id)
      // $COVERAGE-OFF$
      case other =>
        // the extractors mess this up
        sys.error(s"unreachable: $other")
        // $COVERAGE-ON$
    }
  }

  implicit val typeRefOrdering: Ordering[TypeRef] =
    new Ordering[TypeRef] {
      val list = ListOrdering.onType(this)

      def compare(a: TypeRef, b: TypeRef): Int =
        (a, b) match {
          case (TypeVar(v0), TypeVar(v1)) => v0.compareTo(v1)
          case (TypeVar(_), _) => -1
          case (TypeName(v0), TypeName(v1)) => Ordering[Name].compare(v0, v1)
          case (TypeName(_), TypeVar(_)) => 1
          case (TypeName(_), _) => -1
          case (TypeArrow(a0, b0), TypeArrow(a1, b1)) =>
            val c = compare(a0, a1)
            if (c == 0) compare(b0, b1) else c
          case (TypeArrow(_, _), TypeVar(_) | TypeName(_)) => 1
          case (TypeArrow(_, _), _) => -1
          case (TypeApply(o0, a0), TypeApply(o1, a1)) =>
            val c = compare(o0, o1)
            if (c != 0) c
            else list.compare(a0.toList, a1.toList)
          case (TypeApply(_, _), TypeVar(_) | TypeName(_) | TypeArrow(_, _)) => 1
          case (TypeApply(_, _), _) => -1
          case (TypeLambda(p0, in0), TypeLambda(p1, in1)) =>
            // TODO, we could normalize the parmeters here
            val c = list.compare(p0.toList, p1.toList)
            if (c == 0) compare(in0, in1) else c
          case (TypeLambda(_, _), TypeVar(_) | TypeName(_) | TypeArrow(_, _) | TypeApply(_, _)) => 1
          case (TypeLambda(_, _), _) => -1
          case (TypeTuple(t0), TypeTuple(t1)) => list.compare(t0, t1)
          case (TypeTuple(_), _) => 1
        }
    }

  private object TypeRefParser extends TypeParser[TypeRef] {
    override lazy val makeVar: String => TypeVar = TypeVar(_)
    lazy val parseName = Identifier.consParser.map { cn => TypeName(Name(cn)) }
    def makeFn(in: TypeRef, out: TypeRef) = TypeArrow(in, out)

    def applyTypes(cons: TypeRef, args: NonEmptyList[TypeRef]) = TypeApply(cons, args)
    def universal(vars: NonEmptyList[String], in: TypeRef) =
      TypeLambda(vars.map(makeVar), in)

    def makeTuple(items: List[TypeRef]) = TypeTuple(items)

    def unapplyVar(a: TypeRef): Option[String] =
      a match {
        case TypeVar(s) => Some(s)
        case _ => None
      }

    def unapplyName(a: TypeRef): Option[Doc] =
      a match {
        case TypeName(n) => Some(Document[Identifier].document(n.ident))
        case _ => None
      }

    def unapplyFn(a: TypeRef): Option[(TypeRef, TypeRef)] =
      a match {
        case TypeArrow(a, b) => Some((a, b))
        case _ => None
      }

    def unapplyUniversal(a: TypeRef): Option[(List[String], TypeRef)] =
      a match {
        case TypeLambda(vs, a) => Some((vs.map(_.asString).toList, a))
        case _ => None
      }

    def unapplyTypeApply(a: TypeRef): Option[(TypeRef, List[TypeRef])] =
      a match {
        case TypeApply(a, args) => Some((a, args.toList))
        case _ => None
      }

    def unapplyTuple(a: TypeRef): Option[List[TypeRef]] =
      a match {
        case TypeTuple(as) => Some(as)
        case _ => None
      }
  }

  implicit def document: Document[TypeRef] = TypeRefParser.document
  def parser: P[TypeRef] = TypeRefParser.parser

  val annotationParser: P[TypeRef] =
    maybeSpace.with1.soft *> P.char(':') *> maybeSpace *> parser

  /**
   * In a given PackageName, convert the
   * Type back to a TypeRef, which should parse correctly for non-meta
   * types
   *
   * A common use case it to build up all the types you are going to work
   * with into the map, then consult the map as needed to convert them back
   * to TypeRef
   */
  def fromTypes(pack: Option[PackageName], tpes: List[Type]): Map[Type, TypeRef] = {
    type S = (Map[Long, TypeRef], LazyList[String])
    def encodeSkolem(sk: Type.Var.Skolem): State[S, TypeRef] =
      // Make use a typevar
      State.pure(TypeRef.TypeVar("$" + s"${sk.name}${sk.id}"))

    def encodeMeta(id: Long): State[S, TypeRef] =
      State { s: S =>
        val (idMap, vars) = s
        idMap.get(id) match {
          case Some(tr) => (s, tr)
          case None =>
            val nextId = vars.head
            val tr = TypeRef.TypeVar("?" + nextId)
            val nextMap = idMap.updated(id, tr)
            ((nextMap, vars.tail), tr)
        }
      }

    def onConst(c: Type.Const.Defined): State[S, TypeRef] = {
      val Type.Const.Defined(pn, n) = c
      val tn: TypeName =
        if (Some(pn) == pack) TypeName(n)
        else TypeName(Name(Identifier.Constructor(s"${pn.asString}::${n.ident.asString}")))

      State.pure(tn)
    }

    val state0: S = (Map.empty, Type.allBinders.map(_.name))
    tpes.traverse { tpe =>
      TypeRef.fromTypeA[State[S, *]](
        tpe,
        encodeSkolem _,
        encodeMeta _,
        onConst _).map { tr => (tpe, tr) }
    }
    .runA(state0)
    .value
    .toMap
  }
}

