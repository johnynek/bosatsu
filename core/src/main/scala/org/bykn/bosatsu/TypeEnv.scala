package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.Functor
import cats.implicits._

/**
 * This is a mapping of variable names to their Schemes
 */
case class TypeEnv(
  packageName: PackageName,
  values: Map[String, Scheme],
  constructors: Map[(PackageName, ConstructorName), DefinedType],
  definedTypes: Map[(PackageName, TypeName), DefinedType],
  imported: Map[String, Either[(ConstructorName, DefinedType), DefinedType]]
  ) {

  def schemeOf(name: String): Option[Scheme] =
    values.get(name)
      .orElse {
        val cons = ConstructorName(name)
        for {
          dt <- constructors.get((packageName, cons))
          scheme <- dt.toScheme(cons)
        } yield scheme
      }
      .orElse {
        // this could be an imported constructor
        imported.get(name).flatMap {
          case Left((cons, dt)) => dt.toScheme(cons)
          case Right(_) => None
        }
      }

  def updated(v: String, scheme: Scheme): TypeEnv =
    copy(values = values.updated(v, scheme))

  def addDefinedType(d: DefinedType): TypeEnv =
    d.constructors.toList.foldLeft(this) { case (te, (nm, _)) =>
      // TODO make sure this is not duplicated
      te.copy(constructors = te.constructors + ((d.packageName, nm) -> d))
    }.copy(definedTypes = definedTypes + ((d.packageName, d.name) -> d))

  def addImportedConstructor(local: String, remote: ConstructorName, in: DefinedType): TypeEnv =
    copy(imported = imported + (local -> Left((remote, in))))

  def addImportedType(packageName: PackageName, local: String, dt: DefinedType): TypeEnv = {
    /**
     * When we add an imported type, some of our defined types may refer to it
     */
    val replaced = Type.Declared(dt.packageName, dt.name.asString)

    def translate(dec: Type.Declared): Type.Declared =
      if (dec.packageName === packageName && dec.name === local) replaced
      else dec

    val functor = Functor[List]
      .compose(Functor[(ConstructorName, ?)])
      .compose(Functor[List])
      .compose(Functor[(ParamName, ?)])

    def fixDT(dt: DefinedType): DefinedType = {
      val fixedCons = functor.map(dt.constructors)(Type.transformDeclared(_)(translate _))
      dt.copy(constructors = fixedCons)
    }

    val fixedValues = Functor[Map[String, ?]].map(values) { scm =>
      Scheme(scm.vars, Type.transformDeclared(scm.result)(translate _))
    }

    copy(
      values = fixedValues,
      imported = imported + (local -> Right(dt)),
      constructors = constructors.map { case (c, d) => c -> fixDT(d) }.toMap,
      definedTypes = definedTypes.map { case (k, d) => k -> fixDT(d) }.toMap
      )
      .addDefinedType(dt)
  }

  def getDefinedType[T: HasRegion](matches: NonEmptyList[((PackageName, ConstructorName), T)]): Either[TypeError, DefinedType] = {
    def getCons(p: PackageName, c: ConstructorName): Option[Either[(ConstructorName, DefinedType), DefinedType]] =
      constructors.get((p, c)) match {
        case Some(dt) => Some(Right(dt))
        case _ =>
          imported.get(c.asString) match {
            case Some(Left(pair)) if packageName == p => Some(Left(pair))
            case _ => None
          }
      }

    // Find all the constructor, dt pairs
    val matches1 = matches.traverse { case ((p, cn), t) =>
      getCons(p, cn) match {
        case None => Left(TypeError.UnknownConstuctor(cn, HasRegion.region(t)))
        case Some(Right(dt)) => Right((cn, dt))
        case Some(Left(pair)) => Right(pair)
      }
    }

    matches1.flatMap { nePairs =>
      val distinctMatch = nePairs.distinct
      val dts = distinctMatch.map(_._2).distinct
      dts match {
        case NonEmptyList(dt, Nil) =>
          // this is the good case
          dt.checkTotality(distinctMatch.map(_._1),
            matches.reduceMap { case (_, t) => HasRegion.region(t) }).map(_ => dt)
        case _ =>
          // we have at least 2 DefinedTypes
          Left(TypeError.TypeConstructorCollision(matches.map { case (m, t) => (m, HasRegion.region(t)) }))
      }
    }
  }
}

object TypeEnv {
  def empty(pn: PackageName): TypeEnv =
    TypeEnv(pn, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit val forTypeEnv: Substitutable[TypeEnv] =
    new Substitutable[TypeEnv] {
      implicit def eSub[A: Substitutable, B: Substitutable]: Substitutable[Either[A, B]] =
        new Substitutable[Either[A, B]] {
          def apply(sub: Subst, eab: Either[A, B]) =
            eab match {
              case Right(b) => Right(Substitutable[B].apply(sub, b))
              case Left(a) => Left(Substitutable[A].apply(sub, a))
            }
          def typeVars(eab: Either[A, B]) =
            eab match {
              case Right(b) => Substitutable[B].typeVars(b)
              case Left(a) => Substitutable[A].typeVars(a)
            }
        }

      def apply(sub: Subst, te: TypeEnv): TypeEnv =
        TypeEnv(
          te.packageName,
          Substitutable[Map[String, Scheme]].apply(sub, te.values),
          Substitutable[Map[(PackageName, ConstructorName), DefinedType]].apply(sub, te.constructors),
          Substitutable[Map[(PackageName, TypeName), DefinedType]].apply(sub, te.definedTypes),
          Substitutable[Map[String, Either[(ConstructorName, DefinedType), DefinedType]]].apply(sub, te.imported)
        )

      def typeVars(te: TypeEnv) =
        Substitutable[Map[String, Scheme]].typeVars(te.values) |
        Substitutable[Map[(PackageName, ConstructorName), DefinedType]].typeVars(te.constructors) |
        Substitutable[Map[(PackageName, TypeName), DefinedType]].typeVars(te.definedTypes) |
        Substitutable[Map[String, Either[(ConstructorName, DefinedType), DefinedType]]].typeVars(te.imported)
    }
}
