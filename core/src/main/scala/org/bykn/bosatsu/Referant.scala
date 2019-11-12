package org.bykn.bosatsu

import cats.data.NonEmptyList

import rankn.{ConstructorFn, DefinedType, Type, TypeEnv}

import Identifier.{Constructor => ConstructorName}

/**
 * A Referant is something that can be exported or imported after resolving
 * Before resolving, imports and exports are just names.
 */
sealed abstract class Referant[+A] {
  // if this is a Constructor or DefinedT, return the associated DefinedType
  def definedType: Option[DefinedType[A]] =
    this match {
      case Referant.Value(_) => None
      case Referant.DefinedT(dt) => Some(dt)
      case Referant.Constructor(dt, _) => Some(dt)
    }

  def addTo[A1 >: A](packageName: PackageName, name: Identifier, te: TypeEnv[A1]): TypeEnv[A1] =
    this match {
      case Referant.Value(t) =>
        te.addExternalValue(packageName, name, t)
      case Referant.Constructor(dt, cf) =>
        te.addConstructor(packageName, dt, cf)
      case Referant.DefinedT(dt) =>
        te.addDefinedType(dt)
    }
}

object Referant {
  case class Value(scheme: Type) extends Referant[Nothing]
  case class DefinedT[A](dtype: DefinedType[A]) extends Referant[A]
  case class Constructor[A](dtype: DefinedType[A], fn: ConstructorFn) extends Referant[A]

  private def imported[A, B, C](imps: List[Import[A, NonEmptyList[Referant[C]]]])(fn: PartialFunction[Referant[C], B]): Map[Identifier, B] =
    imps.foldLeft(Map.empty[Identifier, B]) { (m0, imp) =>
      m0 ++ Import.locals(imp)(fn)
    }

  def importedTypes[A, B](imps: List[Import[A, NonEmptyList[Referant[B]]]]): Map[Identifier, (PackageName, TypeName)] =
    imported(imps) {
      case Referant.DefinedT(dt) => (dt.packageName, dt.name)
    }

  /**
   * These are all the imported items that may be used in a match
   */
  def importedConsNames[A, B](imps: List[Import[A, NonEmptyList[Referant[B]]]]): Map[Identifier, (PackageName, ConstructorName)] =
    imported(imps) {
      case Referant.Constructor(dt, fn) => (dt.packageName, fn.name)
    }
  /**
   * There are all the imported values, including the constructor functions
   */
  def importedValues[A, B](imps: List[Import[A, NonEmptyList[Referant[B]]]]): Map[Identifier, Type] =
    imported(imps) {
      case Referant.Value(t) => t
      case Referant.Constructor(_, fn) => fn.fnType
    }
  /**
   * Fully qualified original names
   */
  def fullyQualifiedImportedValues[A, B](
    imps: List[Import[A, NonEmptyList[Referant[B]]]])(nameOf: A => PackageName): Map[(PackageName, Identifier), Type] =
    imps.iterator.flatMap { item =>
      val pn = nameOf(item.pack)
      item.items.toList.iterator.flatMap { i =>
        val orig = i.originalName
        val key = (pn, orig)
        i.tag.toList.iterator.collect {
          case Referant.Value(t) => (key, t)
          case Referant.Constructor(_, fn) => (key, fn.fnType)
        }
      }
    }
    .toMap

  def typeConstructors[A, B](
    imps: List[Import[A, NonEmptyList[Referant[B]]]]):
      Map[(PackageName, ConstructorName), (List[(Type.Var, B)], List[Type], Type.Const.Defined)] = {
    val refs: Iterator[Referant[B]] = imps.iterator.flatMap(_.items.toList.iterator.flatMap(_.tag.toList))
    refs.collect { case Constructor(dt, fn) =>
      ((dt.packageName, fn.name), (dt.annotatedTypeParams, fn.args.map(_._2), dt.toTypeConst))
    }
    .toMap
  }

  /**
   * Build the TypeEnv view of the given imports
   */
  def importedTypeEnv[A, B](inps: List[Import[A, NonEmptyList[Referant[B]]]])(nameOf: A => PackageName): TypeEnv[B] =
    inps.foldLeft((TypeEnv.empty): TypeEnv[B]) {
      case (te, imps) =>
        val pack = nameOf(imps.pack)
        imps.items.foldLeft(te) { (te, imp) =>
          val nm = imp.localName
          imp.tag.foldLeft(te) {
            case (te1, Referant.Value(t)) =>
              te1.addExternalValue(pack, nm, t)
            case (te1, Referant.Constructor(dt, cf)) =>
              te1.addConstructor(pack, dt, cf)
            case (te1, Referant.DefinedT(dt)) =>
              te1.addDefinedType(dt)
          }
        }
    }
}
