package dev.bosatsu.rankn

import cats.{Applicative, Eval, Foldable, Traverse}
import cats.data.NonEmptyList
import cats.implicits._
import dev.bosatsu.{Identifier, Kind, PackageName, Require, TypeName}
import scala.collection.immutable.SortedMap

import Identifier.Constructor

sealed trait TypeDecl[+A] {
  def packageName: PackageName
  def name: TypeName
  def annotatedTypeParams: List[(Type.Var.Bound, A)]
  def typeParams: List[Type.Var.Bound]
  def toTypeConst: Type.Const.Defined
  def toTypeTyConst: Type.TyConst
  def dependsOn: List[Type.TyConst]
  def kindOf(implicit ev: A <:< Kind.Arg): Kind
  def mapAnnotations[B](fn: A => B): TypeDecl[B]
}

final case class DefinedType[+A](
    packageName: PackageName,
    name: TypeName,
    annotatedTypeParams: List[(Type.Var.Bound, A)],
    constructors: List[ConstructorFn[A]]
) extends TypeDecl[A] {

  def isOpaque: Boolean = constructors.isEmpty
  def toOpaque: DefinedType[A] = copy(constructors = Nil)

  val typeParams: List[Type.Var.Bound] =
    annotatedTypeParams.map(_._1)

  Require(typeParams.distinct === typeParams, typeParams.toString)

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
        arg <- cfn.args
      } yield arg.tpe
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

  def fnTypeOf[B](cf: ConstructorFn[B])(implicit
      evA: A <:< Kind.Arg,
      evB: B <:< Kind.Arg
  ): Type = {
    // evidence to prove that we only ask for this after inference
    val tc: Type.Leaf | Type.TyApply = Type.const(packageName, name)

    val res = typeParams.foldLeft(tc) { (res, v) =>
      Type.TyApply(res, Type.TyVar(v))
    }
    val resT = NonEmptyList.fromList(cf.args.map(_.tpe)) match {
      case Some(nel) => Type.Fun(nel, res)
      case None      => res
    }
    type Exists[+X] = List[(Type.Var.Bound, X)]
    val cExists: List[(Type.Var.Bound, Kind.Arg)] =
      evB.substituteCo[Exists](cf.exists)
    val typeArgs =
      toAnnotatedKinds.map { case (b, ka) => (b, ka.kind) } ::: cExists.map {
        case (b, ka) => (b, ka.kind)
      }
    Type.forAll(typeArgs, resT)
  }

  def kindOf(implicit ev: A <:< Kind.Arg): Kind =
    Kind(toAnnotatedKinds.map(_._2)*)

  def mapAnnotations[B](fn: A => B): DefinedType[B] =
    copy(
      annotatedTypeParams = annotatedTypeParams.map { case (tv, a) =>
        (tv, fn(a))
      },
      constructors = constructors.map { cfn =>
        cfn.copy(
          exists = cfn.exists.map { case (tv, a) =>
            (tv, fn(a))
          }
        )
      }
    )

  def extractTypeArgs(targetType: Type): Option[List[Type]] = {
    val (root, args) = Type.unapplyAll(targetType)
    root match {
      case Type.TyConst(rootConst)
          if rootConst == toTypeConst &&
            (typeParams.lengthCompare(args.length) == 0) =>
        Some(args)
      case _ =>
        None
    }
  }

  def instantiateConstructorFieldTypes(
      ctor: ConstructorFn[?],
      targetArgs: List[Type]
  ): Option[List[Type]] =
    if (typeParams.lengthCompare(targetArgs.length) != 0) None
    else {
      val substitutions: Map[Type.Var, Type] =
        typeParams.iterator
          .zip(targetArgs.iterator)
          .map { case (tv, targ) => (tv: Type.Var) -> targ }
          .toMap
      Some(
        ctor.args.map { field =>
          Type.substituteVar(field.tpe, substitutions)
        }
      )
    }

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
      private def traverseCons[F[_]: Applicative, A, B](
          cs: List[ConstructorFn[A]]
      )(fn: A => F[B]): F[List[ConstructorFn[B]]] =
        cs.traverse { cfn =>
          listTup.traverse(cfn.exists)(fn).map(ex => cfn.copy(exists = ex))
        }

      def traverse[F[_]: Applicative, A, B](
          da: DefinedType[A]
      )(fn: A => F[B]): F[DefinedType[B]] =
        (
          listTup.traverse(da.annotatedTypeParams)(fn),
          traverseCons(da.constructors)(fn)
        )
          .mapN { (ap, cons) =>
            DefinedType(
              packageName = da.packageName,
              name = da.name,
              annotatedTypeParams = ap,
              constructors = cons
            )
          }

      def foldRight[A, B](fa: DefinedType[A], b: Eval[B])(
          fn: (A, Eval[B]) => Eval[B]
      ): Eval[B] =
        listTup.foldRight(
          fa.annotatedTypeParams,
          fa.constructors.foldRight(b) { (cfn, acc) =>
            listTup.foldRight(cfn.exists, acc)(fn)
          }
        )(fn)

      def foldLeft[A, B](fa: DefinedType[A], b: B)(fn: (B, A) => B): B =
        fa.constructors.foldLeft(
          listTup.foldLeft(fa.annotatedTypeParams, b)(fn)
        ) { (acc, cfn) =>
          listTup.foldLeft(cfn.exists, acc)(fn)
        }

      override def map[A, B](fa: DefinedType[A])(fn: A => B): DefinedType[B] =
        DefinedType(
          packageName = fa.packageName,
          name = fa.name,
          annotatedTypeParams = listTup.map(fa.annotatedTypeParams)(fn),
          constructors = fa.constructors.map { cfn =>
            cfn.copy(exists = listTup.map(cfn.exists)(fn))
          }
        )
    }
}

final case class TypeAlias[+A](
    packageName: PackageName,
    name: TypeName,
    annotatedTypeParams: List[(Type.Var.Bound, A)],
    rhs: Type
) extends TypeDecl[A] {
  val typeParams: List[Type.Var.Bound] =
    annotatedTypeParams.map(_._1)

  val toTypeConst: Type.Const.Defined =
    Type.Const.Defined(packageName, name)

  val toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  lazy val dependsOn: List[Type.TyConst] =
    Type.allConsts(rhs :: Nil).filterNot(_ == toTypeTyConst)

  def kindOf(implicit ev: A <:< Kind.Arg): Kind =
    Kind(annotatedTypeParams.map { case (_, a) => ev(a) }*)

  def mapAnnotations[B](fn: A => B): TypeAlias[B] =
    copy(
      annotatedTypeParams = annotatedTypeParams.map { case (tv, a) =>
        (tv, fn(a))
      }
    )

  def expandWith(args: List[Type]): Option[Type] =
    if (args.lengthCompare(typeParams.length) < 0) None
    else {
      val (appliedArgs, rest) = args.splitAt(typeParams.length)
      val substitutions =
        typeParams.iterator
          .zip(appliedArgs.iterator)
          .map { case (tv, arg) => (tv: Type.Var) -> arg }
          .toMap
      Some(Type.applyAll(Type.substituteVar(rhs, substitutions), rest))
    }

  def depPackages: List[PackageName] =
    (packageName :: Type.packageNamesIn(rhs)).distinct
}

object TypeAlias {
  def listToMap[A](
      aliases: List[TypeAlias[A]]
  ): SortedMap[(PackageName, TypeName), TypeAlias[A]] =
    SortedMap(aliases.map(ta => (ta.packageName, ta.name) -> ta)*)

  def toKindMap[F[_]: Foldable](
      aliases: F[TypeAlias[Kind.Arg]]
  ): Map[Type.Const.Defined, Kind] =
    aliases
      .foldLeft(
        Map.newBuilder[Type.Const.Defined, Kind]
      )((b, ta) => b += ((ta.toTypeConst, ta.kindOf)))
      .result()

  given Traverse[TypeAlias] with {
    private val listTup = Traverse[List].compose[[X] =>> (Type.Var.Bound, X)]

    def traverse[F[_]: Applicative, A, B](
        fa: TypeAlias[A]
    )(fn: A => F[B]): F[TypeAlias[B]] =
      listTup.traverse(fa.annotatedTypeParams)(fn).map { params =>
        TypeAlias(fa.packageName, fa.name, params, fa.rhs)
      }

    def foldRight[A, B](fa: TypeAlias[A], b: Eval[B])(
        fn: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      listTup.foldRight(fa.annotatedTypeParams, b)(fn)

    def foldLeft[A, B](fa: TypeAlias[A], b: B)(fn: (B, A) => B): B =
      listTup.foldLeft(fa.annotatedTypeParams, b)(fn)
  }
}
