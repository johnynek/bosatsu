package org.bykn.bosatsu.library

import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.TypeEnv
import org.bykn.bosatsu.{Identifier, Kind, PackageName, Referant, ExportedName}
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

import cats.syntax.all._

sealed abstract class Diff {
  def isValidWhen(dk: Version.DiffKind): Boolean
  def toDoc: Doc
}

object ApiDiff {
  type R = Referant[Kind.Arg]

  case class AddedName(name: Identifier, added: NonEmptyList[ExportedName[R]]) extends Diff {
    def isValidWhen(dk: Version.DiffKind) =
      dk.isMajor | dk.isMinor

    def toDoc = Doc.text(s"added ${name.sourceCodeRepr}")
  }
  case class RemovedName(name: Identifier, removed: NonEmptyList[ExportedName[R]]) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(s"removed ${name.sourceCodeRepr}")
  }

  case class Diffs(toMap: SortedMap[PackageName, SortedMap[Identifier, Diff]]) {
    def badDiffs[A](dk: Version.DiffKind)(fn: (PackageName, Identifier, Diff) => A): Vector[A] =
      toMap.iterator.flatMap { case (pack, diffs) =>
        diffs.iterator.flatMap { case (ident, diff) =>
          if (diff.isValidWhen(dk)) Nil
          else (fn(pack, ident, diff) :: Nil)
        }
      }
      .toVector
  }

  def apply(
    prevIface: SortedMap[PackageName, List[ExportedName[R]]],
    prevEnv: TypeEnv[Kind.Arg],
    current: SortedMap[PackageName, List[ExportedName[R]]],
    curEnv: TypeEnv[Kind.Arg]): Diffs = {
    ???
  }

  private def apply(pn: PackageName, prev: List[ExportedName[R]], prevEnv: TypeEnv[Kind.Arg], current: List[ExportedName[R]], curEnv: TypeEnv[Kind.Arg]): Diffs = {
    val prevMap = prev.groupByNel(_.name)
    val currMap = current.groupByNel(_.name)

    val allNames = prevMap.keySet | currMap.keySet

    val diffs: List[(Identifier, Diff)] = allNames.toList.flatMap { name =>
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
                  ???
                case (Nil, _) | (_, Nil) =>
                  // this is impossible for correctly constructed ExportedName[R] values
                  sys.error(s"invariant violation: have bindable=$bindable but no Binding: oldExp=$oldExp, newExp=$newExp")
              }
            case cons: Identifier.Constructor => ???
          }
        case (None, Some(exports)) => (name -> AddedName(name, exports)) :: Nil
        case (Some(oldExp), None) => (name -> RemovedName(name, oldExp)) :: Nil
        case (None, None) =>
          sys.error(s"invariant violation: $name must be in prev or current")
      }
    }

    Diffs(??? /*diffs.to(SortedMap)*/)
  }
}