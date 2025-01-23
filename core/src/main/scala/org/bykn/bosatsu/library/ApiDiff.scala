package org.bykn.bosatsu.library

import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.{Type, TypeEnv}
import org.bykn.bosatsu.{Identifier, Kind, PackageName, Referant, ExportedName}
import org.typelevel.paiges.{Doc, Document}
import scala.collection.immutable.SortedMap

import cats.syntax.all._

sealed abstract class Diff {
  def pack: PackageName
  def isValidWhen(dk: Version.DiffKind): Boolean
  def toDoc: Doc
}

object ApiDiff {
  type R = Referant[Kind.Arg]

  case class AddedName(pack: PackageName, name: Identifier, added: NonEmptyList[ExportedName[R]]) extends Diff {
    def isValidWhen(dk: Version.DiffKind) =
      dk.isMajor | dk.isMinor

    def toDoc = Doc.text(show"in $pack added $name: ${added.map(_.nameKind).toList.mkString(", ")}")
  }
  case class RemovedName(pack: PackageName, name: Identifier, removed: NonEmptyList[ExportedName[R]]) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(show"in $pack removed $name: ${removed.map(_.nameKind).toList.mkString(", ")}")
  }

  case class ChangedValueType(pack: PackageName, bindable: Identifier.Bindable, oldTpe: Type, newTpe: Type) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = {
      val message =
        Doc.text("original type is ") + Type.fullyResolvedDocument.document(oldTpe) + Doc.text(" and new type is ") +
          Type.fullyResolvedDocument.document(newTpe)

      Doc.text(show"in $pack the value $bindable has a type that reaches a changed type:") + (
      Doc.line + message).nested(4).grouped
    }
  }

  case class ChangedValueTransitiveType(pack: PackageName, bindable: Identifier.Bindable, tpe: Type, transitive: Type.TyConst, oldKind: Kind, newKind: Kind) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = {
      def tdoc(t: Type): Doc = Type.fullyResolvedDocument.document(t)

      Doc.text(show"in $pack the value $bindable has a type ") + tdoc(tpe) + Doc.text(" that reaches type ") + tdoc(transitive) +
        Doc.text(" that changes from kind ") + Document[Kind].document(oldKind) + Doc.text(" to kind ") +
        Document[Kind].document(newKind)
    }
  }

  case class AddedPackage(pack: PackageName, exports: List[ExportedName[R]]) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor | dk.isMinor
    def toDoc = Doc.text(show"added package $pack")
  }

  case class RemovedPackage(pack: PackageName, exports: List[ExportedName[R]]) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(show"removed package $pack")
  }

  case class Diffs(toMap: SortedMap[PackageName, NonEmptyList[Diff]]) {
    def badDiffs[A](dk: Version.DiffKind)(fn: (PackageName, Diff) => A): Vector[A] =
      toMap.iterator.flatMap { case (pack, diffs) =>
        diffs.iterator.flatMap { diff =>
          if (diff.isValidWhen(dk)) Nil
          else (fn(pack, diff) :: Nil)
        }
      }
      .toVector
  }

  def apply(
    prevIface: SortedMap[PackageName, List[ExportedName[R]]],
    prevEnv: TypeEnv[Kind.Arg],
    current: SortedMap[PackageName, List[ExportedName[R]]],
    curEnv: TypeEnv[Kind.Arg]): Diffs = {
    val allPacks = prevIface.keySet | current.keySet

    val map = allPacks.iterator.flatMap { pack =>
      (prevIface.get(pack), current.get(pack)) match {
        case (Some(es), None) => RemovedPackage(pack, es) :: Nil
        case (None, Some(es)) => AddedPackage(pack, es) :: Nil
        case (Some(oldE), Some(newE)) =>
          apply(pack, oldE, prevEnv, newE, curEnv)
        case (None, None) => Nil
      }  
    }
    .toList
    .groupByNel(_.pack)

    Diffs(map)
  }

  private def apply(pn: PackageName, prev: List[ExportedName[R]], prevEnv: TypeEnv[Kind.Arg], current: List[ExportedName[R]], curEnv: TypeEnv[Kind.Arg]): List[Diff] = {
    val prevMap = prev.groupByNel(_.name)
    val currMap = current.groupByNel(_.name)

    val allNames = prevMap.keySet | currMap.keySet

    allNames.toList.flatMap { name =>
      (prevMap.get(name), currMap.get(name)) match {
        case (Some(oldExp), Some(newExp)) =>
          name match {
            case bindable: Identifier.Bindable => 
              def bindsOf(nel: NonEmptyList[ExportedName[R]]): List[ExportedName.Binding[R]] =
                nel.toList.collect { case b @ ExportedName.Binding(_, _) => b }

              // If this 
              (bindsOf(oldExp), bindsOf(newExp)) match {
                case (ExportedName.Binding(_, Referant.Value(oldTpe)) :: Nil, ExportedName.Binding(_, Referant.Value(newTpe)) :: Nil) =>
                  // this should be the only valid case, otherwise there were multiple things previously
                  if (oldTpe.sameAs(newTpe)) {
                    // these are the same so they reference the same consts
                    val consts = Type.allConsts(oldTpe :: Nil)

                    consts.flatMap { const =>
                      val oldDT = prevEnv.getType(const).getOrElse(sys.error(s"in $name tpe=$oldTpe prevEnv=$prevEnv doesn't have $const"))
                      val newDT = curEnv.getType(const).getOrElse(sys.error(s"in $name tpe=$oldTpe prevEnv=$curEnv doesn't have $const"))
                      val oldKind = oldDT.kindOf
                      val newKind = newDT.kindOf

                      if (oldKind == newKind) Nil
                      else {
                        ChangedValueTransitiveType(pn, bindable, newTpe, const, oldKind, newKind) :: Nil
                      }
                    }
                  }
                  else {
                    ChangedValueType(pn, bindable, oldTpe, newTpe) :: Nil
                  }
                case _ =>
                  // this is impossible for correctly constructed ExportedName[R] values
                  sys.error(s"invariant violation: have bindable=$bindable but unexpected bindings: oldExp=$oldExp, newExp=$newExp")
              }
            case cons: Identifier.Constructor => ???
          }
        case (None, Some(exports)) => AddedName(pn, name, exports) :: Nil
        case (Some(oldExp), None) => RemovedName(pn, name, oldExp) :: Nil
        case (None, None) =>
          sys.error(s"invariant violation: $name must be in prev or current")
      }
    }
  }
}