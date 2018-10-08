package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}

import Parser.{lowerIdent, upperIdent}

sealed abstract class ExportedName[+T] {
  def name: String
  def tag: T

 /**
  * Given name, in the current type environment and fully typed lets
  * what does it correspond to?
  */
  def toReferants(
    letValue: Option[rankn.Type],
    definedType: Option[rankn.DefinedType]): Option[NonEmptyList[ExportedName[Referant]]] =
     this match {
       case ExportedName.Binding(n, _) =>
         letValue.map { tpe =>
           NonEmptyList(ExportedName.Binding(n, Referant.Value(tpe)), Nil)
         }
       case ExportedName.TypeName(nm, _) =>
         // export the opaque type
         definedType.map { dt =>
           NonEmptyList(ExportedName.TypeName(nm, Referant.DefinedT(dt.toOpaque)), Nil)
         }
       case ExportedName.Constructor(nm, _) =>
         // export the type and all constructors
         definedType.map { dt =>
           val cons = dt.constructors.map { case (n, _, tpe) =>
             ExportedName.Constructor(n.asString, Referant.Constructor(n, dt, tpe))
           }
           val t = ExportedName.TypeName(nm, Referant.DefinedT(dt))
           NonEmptyList(t, cons)
         }
     }
}
object ExportedName {
  case class Binding[T](name: String, tag: T) extends ExportedName[T]
  case class TypeName[T](name: String, tag: T) extends ExportedName[T]
  case class Constructor[T](name: String, tag: T) extends ExportedName[T]

  private[this] val consDoc = Doc.text("()")

  implicit val document: Document[ExportedName[Unit]] =
    Document.instance[ExportedName[Unit]] {
      case Binding(n, _) => Doc.text(n)
      case TypeName(n, _) => Doc.text(n)
      case Constructor(n, _) => Doc.text(n) + consDoc
    }

  val parser: P[ExportedName[Unit]] =
    lowerIdent.map(Binding(_, ())) |
      P(upperIdent ~ "()".!.?).map {
        case (n, None) => TypeName(n, ())
        case (n, Some(_)) => Constructor(n, ())
      }

  private[bosatsu] def buildExportMap[T](exs: List[ExportedName[T]]): Map[String, NonEmptyList[ExportedName[T]]] =
    exs match {
      case Nil => Map.empty
      case h :: tail => NonEmptyList(h, tail).groupBy(_.name)
    }


  /**
   * Build exports into referants given a typeEnv
   * The only error we have have here is if we name an export we didn't define
   * Note a name can be two things:
   *   1. a type
   *   2. a value (e.g. a let or a constructor function)
   */
  def buildExports[E](
    nm: PackageName,
    exports: List[ExportedName[E]],
    typeEnv: rankn.TypeEnv,
    lets: List[(String, TypedExpr[Declaration])]): ValidatedNel[ExportedName[E], List[ExportedName[Referant]]] = {

     val letMap = lets.toMap

     def expName[A](ename: ExportedName[A]): Option[NonEmptyList[ExportedName[Referant]]] = {
       import ename.name
       val letValue: Option[rankn.Type] =
         letMap.get(name)
           .map(_.getType)
           .orElse {
             // It could be an external or imported value in the TypeEnv
             typeEnv.values.get((nm, name))
           }
       val optDT = typeEnv
         .definedTypes
         .get((nm, org.bykn.bosatsu.TypeName(name)))

       ename.toReferants(letValue, optDT)
     }

     def expName1[A](ename: ExportedName[A]): ValidatedNel[ExportedName[A], List[ExportedName[Referant]]] =
       expName(ename) match {
         case None => Validated.invalid(NonEmptyList.of(ename))
         case Some(v) => Validated.valid(v.toList)
       }

     exports.traverse(expName1).map(_.flatten)
    }
}

