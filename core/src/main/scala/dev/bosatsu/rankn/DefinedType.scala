package dev.bosatsu.rankn

import cats.{Applicative, Eval, Foldable, Traverse}
import dev.bosatsu.{Kind, TypeName, PackageName, Identifier}
import scala.collection.immutable.SortedMap

import Identifier.Constructor

import cats.implicits._
import cats.data.NonEmptyList

final case class DefinedType[+A](
    packageName: PackageName,
    name: TypeName,
    annotatedTypeParams: List[(Type.Var.Bound, A)],
    constructors: List[ConstructorFn]
) {

  def isOpaque: Boolean = constructors.isEmpty
  def toOpaque: DefinedType[A] = copy(constructors = Nil)

  val typeParams: List[Type.Var.Bound] =
    annotatedTypeParams.map(_._1)

  require(typeParams.distinct === typeParams, typeParams.toString)

  /** This is not the full type, since the full type has a ForAll(typeParams,
    * ... in front if the typeParams is nonEmpty
    */
  val toTypeConst: Type.Const.Defined =
    DefinedType.toTypeConst(packageName, name)

  val toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  lazy val dependsOn: List[Type.TyConst] =
    Type.allConsts(
      for {
        cfn <- constructors
        (_, t) <- cfn.args
      } yield t
    )

  /** A type with exactly one constructor is a struct
    */
  def isStruct: Boolean = dataFamily == DataFamily.Struct

  val dataRepr: Constructor => DataRepr =
    constructors match {
      case cf :: Nil =>
        if (cf.isSingleArg) Function.const(DataRepr.NewType)
        else Function.const(DataRepr.Struct(cf.arity))
      case c0 :: c1 :: Nil =>
        // exactly two constructor functions
        if (c0.isZeroArg && c1.hasSingleArgType(toTypeTyConst)) {
          val zero = c0.name

          { cons => if (cons == zero) DataRepr.ZeroNat else DataRepr.SuccNat }
        } else if (c1.isZeroArg && c0.hasSingleArgType(toTypeTyConst)) {
          val zero = c1.name

          { cons => if (cons == zero) DataRepr.ZeroNat else DataRepr.SuccNat }
        } else {
          val famArities = c0.arity :: c1.arity :: Nil
          val zero = c0.name
          val zrep = DataRepr.Enum(0, c0.arity, famArities)
          val orep = DataRepr.Enum(1, c1.arity, famArities)

          { cons => if (cons == zero) zrep else orep }
        }
      case cons =>
        val famArities = cons.map(_.arity)
        val mapping = cons.zipWithIndex.map { case (c, idx) =>
          c.name -> DataRepr.Enum(idx, c.arity, famArities)
        }.toMap

        mapping
    }

  val dataFamily: DataFamily =
    constructors match {
      case cf :: Nil =>
        if (cf.isSingleArg) DataFamily.NewType
        else DataFamily.Struct
      case c0 :: c1 :: Nil =>
        // exactly two constructor functions
        if (c0.isZeroArg && c1.hasSingleArgType(toTypeTyConst)) DataFamily.Nat
        else if (c1.isZeroArg && c0.hasSingleArgType(toTypeTyConst))
          DataFamily.Nat
        else DataFamily.Enum
      case _ => DataFamily.Enum
    }

  private def toAnnotatedKinds(implicit
      ev: A <:< Kind.Arg
  ): List[(Type.Var.Bound, Kind.Arg)] = {
    type L[+X] = List[(Type.Var.Bound, X)]
    ev.substituteCo[L](annotatedTypeParams)
  }

  def fnTypeOf(cf: ConstructorFn)(implicit ev: A <:< Kind.Arg): Type = {
    // evidence to prove that we only ask for this after inference
    val tc: Type.Leaf | Type.TyApply = Type.const(packageName, name)

    val res = typeParams.foldLeft(tc) { (res, v) =>
      Type.TyApply(res, Type.TyVar(v))
    }
    val resT = NonEmptyList.fromList(cf.args.map(_._2)) match {
      case Some(nel) => Type.Fun(nel, res)
      case None      => res
    }
    val typeArgs = toAnnotatedKinds.map { case (b, ka) => (b, ka.kind) }
    Type.forAll(typeArgs, resT)
  }

  def kindOf(implicit ev: A <:< Kind.Arg): Kind =
    Kind(toAnnotatedKinds.map(_._2)*)

  def depPackages: List[PackageName] =
    (packageName :: constructors.flatMap(_.depPackages)).distinct
}

object DefinedType {
  def toTypeConst(pn: PackageName, nm: TypeName): Type.Const.Defined =
    Type.Const.Defined(pn, nm)

  def listToMap[A](
      dts: List[DefinedType[A]]
  ): SortedMap[(PackageName, TypeName), DefinedType[A]] =
    SortedMap(dts.map(dt => (dt.packageName, dt.name) -> dt)*)

  def toKindMap[F[_]: Foldable](
      dts: F[DefinedType[Kind.Arg]]
  ): Map[Type.Const.Defined, Kind] =
    dts
      .foldLeft(
        Map.newBuilder[Type.Const.Defined, Kind]
      )((b, dt) => b += ((dt.toTypeConst.toDefined, dt.kindOf)))
      .result()

  implicit val definedTypeTraverse: Traverse[DefinedType] =
    new Traverse[DefinedType] {
      val listTup = Traverse[List].compose[[X] =>> (Type.Var.Bound, X)]
      def traverse[F[_]: Applicative, A, B](
          da: DefinedType[A]
      )(fn: A => F[B]): F[DefinedType[B]] =
        listTup.traverse(da.annotatedTypeParams)(fn).map { ap =>
          DefinedType(
            packageName = da.packageName,
            name = da.name,
            annotatedTypeParams = ap,
            constructors = da.constructors
          )
        }

      def foldRight[A, B](fa: DefinedType[A], b: Eval[B])(
          fn: (A, Eval[B]) => Eval[B]
      ): Eval[B] =
        listTup.foldRight(fa.annotatedTypeParams, b)(fn)

      def foldLeft[A, B](fa: DefinedType[A], b: B)(fn: (B, A) => B): B =
        listTup.foldLeft(fa.annotatedTypeParams, b)(fn)

      override def map[A, B](fa: DefinedType[A])(fn: A => B): DefinedType[B] =
        DefinedType(
          packageName = fa.packageName,
          name = fa.name,
          annotatedTypeParams = listTup.map(fa.annotatedTypeParams)(fn),
          constructors = fa.constructors
        )
    }
}
