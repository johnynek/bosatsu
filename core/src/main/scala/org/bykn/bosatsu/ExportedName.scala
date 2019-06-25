package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import scala.util.hashing.MurmurHash3

sealed abstract class ExportedName[+T] { self: Product =>
  def name: Identifier
  def tag: T

  // It is really important to cache the hashcode and these large dags if
  // we use them as hash keys
  final override val hashCode: Int =
    MurmurHash3.productHash(this)
 /**
  * Given name, in the current type environment and fully typed lets
  * what does it correspond to?
  */
  private def toReferants[A](
    letValue: Option[rankn.Type],
    definedType: Option[rankn.DefinedType[A]]): Option[NonEmptyList[ExportedName[Referant[A]]]] =
     this match {
       case ExportedName.Binding(n, _) =>
         letValue.map { tpe =>
           NonEmptyList(ExportedName.Binding(n, Referant.Value(tpe)), Nil)
         }
       case ExportedName.TypeName(nm, _) =>
         definedType.map { dt =>
           NonEmptyList(ExportedName.TypeName(nm, Referant.DefinedT(dt)), Nil)
         }
       case ExportedName.Constructor(nm, _) =>
         // export the type and all constructors
         definedType.map { dt =>
           val cons = dt.constructors.map { case (n, params, tpe) =>
             ExportedName.Constructor(n, Referant.Constructor(n, dt, params, tpe))
           }
           val t = ExportedName.TypeName(nm, Referant.DefinedT(dt))
           NonEmptyList(t, cons)
         }
     }
}
object ExportedName {
  case class Binding[T](name: Identifier.Bindable, tag: T) extends ExportedName[T]
  case class TypeName[T](name: Identifier.Constructor, tag: T) extends ExportedName[T]
  case class Constructor[T](name: Identifier.Constructor, tag: T) extends ExportedName[T]

  private[this] val consDoc = Doc.text("()")

  implicit val document: Document[ExportedName[Unit]] = {
    val di = Document[Identifier]
    Document.instance[ExportedName[Unit]] {
      case Binding(n, _) => di.document(n)
      case TypeName(n, _) => di.document(n)
      case Constructor(n, _) => di.document(n) + consDoc
    }
  }

  val parser: P[ExportedName[Unit]] =
    Identifier.bindableParser.map(Binding(_, ())) |
      P(Identifier.consParser ~ "()".!.?).map {
        case (n, None) => TypeName(n, ())
        case (n, Some(_)) => Constructor(n, ())
      }

  private[bosatsu] def buildExportMap[T](exs: List[ExportedName[T]]): Map[Identifier, NonEmptyList[ExportedName[T]]] =
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
  def buildExports[E, V, R, D](
    nm: PackageName,
    exports: List[ExportedName[E]],
    typeEnv: rankn.TypeEnv[V],
    lets: List[(Identifier.Bindable, R, TypedExpr[D])]): ValidatedNel[ExportedName[E], List[ExportedName[Referant[V]]]] = {

     val letMap = lets.iterator.map { case (n, _, t) => (n, t) }.toMap

     def expName[A](ename: ExportedName[A]): Option[NonEmptyList[ExportedName[Referant[V]]]] = {
       import ename.name
       val letValue: Option[rankn.Type] =
         name.toBindable
           .flatMap { bn =>
             letMap.get(bn)
               .map(_.getType)
               .orElse {
                 // It could be an external or imported value in the TypeEnv
                 typeEnv.getValue(nm, bn)
               }
           }
       val optDT =
         name.toConstructor
           .flatMap { cn =>
             typeEnv.getType(nm, org.bykn.bosatsu.TypeName(cn))
           }

       ename.toReferants(letValue, optDT)
     }

     def expName1[A](ename: ExportedName[A]): ValidatedNel[ExportedName[A], List[ExportedName[Referant[V]]]] =
       expName(ename) match {
         case None => Validated.invalid(NonEmptyList.of(ename))
         case Some(v) => Validated.valid(v.toList)
       }

     exports.traverse(expName1).map(_.flatten)
    }
}

