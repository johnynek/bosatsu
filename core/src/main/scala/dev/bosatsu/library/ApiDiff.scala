package dev.bosatsu.library

import cats.data.{Ior, NonEmptyList}
import dev.bosatsu.rankn.{ConstructorFn, DefinedType, Type, TypeEnv}
import dev.bosatsu.{Identifier, Kind, PackageName, Referant, ExportedName}
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

  case class AddedName(
      pack: PackageName,
      name: Identifier,
      added: NonEmptyList[ExportedName[R]]
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) =
      dk.isMajor | dk.isMinor

    def toDoc = Doc.text(
      show"in $pack added $name: ${added.map(_.nameKind).toList.mkString(", ")}"
    )
  }
  case class RemovedName(
      pack: PackageName,
      name: Identifier,
      removed: NonEmptyList[ExportedName[R]]
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"in $pack removed $name: ${removed.map(_.nameKind).toList.mkString(", ")}"
    )
  }

  case class ChangedType(pack: PackageName, oldTpe: Type, newTpe: Type)
      extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Doc.text(
        show"in package $pack original type is "
      ) + Type.fullyResolvedDocument.document(oldTpe) + Doc.text(
        " and new type is "
      ) +
        Type.fullyResolvedDocument.document(newTpe)
  }

  case class ChangedKind(
      pack: PackageName,
      oldDt: DefinedType[Kind.Arg],
      newDt: DefinedType[Kind.Arg]
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Type.fullyResolvedDocument.document(oldDt.toTypeTyConst) + Doc.text(
        " changes from kind "
      ) +
        Document[Kind].document(oldDt.kindOf) + Doc.text(" to kind ") +
        Document[Kind].document(newDt.kindOf)
  }

  case class ChangedTransitiveType(
      tpe: Type,
      transitive: Type.TyConst,
      diff: Diff
  ) extends Diff {
    def pack = diff.pack
    def isValidWhen(dk: Version.DiffKind) = diff.isValidWhen(dk)
    def toDoc = {
      def tdoc(t: Type): Doc = Type.fullyResolvedDocument.document(t)

      Doc.text(show"change in the type ") + tdoc(tpe) + Doc.text(
        " that reaches type "
      ) + tdoc(transitive) +
        Doc.char('.') + (Doc.line + diff.toDoc).nested(4).grouped
    }
  }

  case class ChangedValue(bindable: Identifier.Bindable, diff: Diff)
      extends Diff {
    def pack = diff.pack
    def isValidWhen(dk: Version.DiffKind) = diff.isValidWhen(dk)
    def toDoc = Doc.text(show"for value $bindable ") + diff.toDoc
  }

  case class AddedPackage(pack: PackageName, exports: List[ExportedName[R]])
      extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor | dk.isMinor
    def toDoc = Doc.text(show"added package $pack")
  }

  case class RemovedPackage(pack: PackageName, exports: List[ExportedName[R]])
      extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(show"removed package $pack")
  }

  case class ConstructorRemoved(
      pack: PackageName,
      tycons: Type.TyConst,
      cfn: ConstructorFn
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Doc.text("removed constructor of ") + Type.fullyResolvedDocument.document(
        tycons
      ) + Doc.text(show" ${cfn.name}")
  }

  case class ConstructorAdded(
      pack: PackageName,
      tycons: Type.TyConst,
      cfn: ConstructorFn
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Doc.text("added constructor of ") + Type.fullyResolvedDocument.document(
        tycons
      ) + Doc.text(show" ${cfn.name}")
  }

  case class ConstructorIndexChange(
      pack: PackageName,
      tycons: Type.TyConst,
      name: Identifier.Constructor,
      oldIdx: Int,
      newIdx: Int
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $name of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(show" changed index from $oldIdx to $newIdx")
  }

  case class ConstructorParamChange(
      name: Identifier.Constructor,
      tycons: Type.TyConst,
      paramIndex: Int,
      diff: Diff
  ) extends Diff {
    def pack = diff.pack
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $name of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(show" at parameter index $paramIndex ") + diff.toDoc
  }

  case class ConstructorParamNameChange(
      pack: PackageName,
      name: Identifier.Constructor,
      tycons: Type.TyConst,
      paramIndex: Int,
      oldName: Identifier.Bindable,
      newName: Identifier.Bindable
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $name of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(
        show" at parameter index $paramIndex changed parameter name from $oldName to $newName"
      )
  }

  case class ConstructorParamAdded(
      pack: PackageName,
      cons: Identifier.Constructor,
      tycons: Type.TyConst,
      paramIndex: Int,
      name: Identifier.Bindable,
      tpe: Type
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $cons of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(
        show" at parameter index $paramIndex added parameter $name with type "
      ) +
      Type.fullyResolvedDocument.document(tpe)
  }

  case class ConstructorParamRemoved(
      pack: PackageName,
      cons: Identifier.Constructor,
      tycons: Type.TyConst,
      paramIndex: Int,
      name: Identifier.Bindable,
      tpe: Type
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $cons of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(
        show" at parameter index $paramIndex removed parameter $name with type "
      ) +
      Type.fullyResolvedDocument.document(tpe)
  }

  case class Diffs(toMap: SortedMap[PackageName, NonEmptyList[Diff]]) {
    def isValidWhen(dk: Version.DiffKind): Boolean =
      toMap.forall(_._2.forall(_.isValidWhen(dk)))

    def badDiffs[A](dk: Version.DiffKind)(fn: Diff => A): Vector[A] =
      toMap.iterator.flatMap { case (_, diffs) =>
        diffs.iterator.flatMap { diff =>
          if (diff.isValidWhen(dk)) Nil
          else (fn(diff) :: Nil)
        }
      }.toVector

    def nextValid(v: Version): (Version.DiffKind, Version) = {
      // major bump can do anything, so, check patch then minor
      val dk = (Version.DiffKind.Patch :: Version.DiffKind.Minor :: Nil)
        .find(isValidWhen(_))
        .getOrElse(Version.DiffKind.Major)

      (dk, v.next(dk))
    }
  }

  def apply(
      prevIface: SortedMap[PackageName, List[ExportedName[R]]],
      prevEnv: TypeEnv[Kind.Arg],
      current: SortedMap[PackageName, List[ExportedName[R]]],
      curEnv: TypeEnv[Kind.Arg]
  ): Diffs =

    Diffs(
      prevIface
        .align(current)
        .transform { (pack, ior) =>
          ior match {
            case Ior.Left(es)         => RemovedPackage(pack, es) :: Nil
            case Ior.Right(es)        => AddedPackage(pack, es) :: Nil
            case Ior.Both(oldE, newE) =>
              apply(pack, oldE, prevEnv, newE, curEnv)
          }
        }
        .filter { case (_, diffs) => diffs.nonEmpty }
        .transform((_, diffs) => NonEmptyList.fromListUnsafe(diffs))
    )

  private def apply(
      pn: PackageName,
      prev: List[ExportedName[R]],
      prevEnv: TypeEnv[Kind.Arg],
      current: List[ExportedName[R]],
      curEnv: TypeEnv[Kind.Arg]
  ): List[Diff] = {
    def diffType(oldTpe: Type, newTpe: Type): List[Diff] =
      // this should be the only valid case, otherwise there were multiple things previously
      if (oldTpe.sameAs(newTpe)) {
        // these are the same so they reference the same consts
        val consts = Type.allConsts(oldTpe :: Nil)

        consts.flatMap { const =>
          val oldDt = prevEnv
            .getType(const)
            .getOrElse(
              sys.error(s"tpe=$oldTpe prevEnv=$prevEnv doesn't have $const")
            )
          val newDt = curEnv
            .getType(const)
            .getOrElse(
              sys.error(s"tpe=$oldTpe prevEnv=$curEnv doesn't have $const")
            )
          diffDT(oldDt, newDt)
            .map(diff => ChangedTransitiveType(newTpe, const, diff))
        }
      } else {
        ChangedType(pn, oldTpe, newTpe) :: Nil
      }

    def diffDT(
        oldDt: DefinedType[Kind.Arg],
        newDt: DefinedType[Kind.Arg]
    ): List[Diff] = {
      // invariant: oldDt.toTypeConst == newDt.toTypeConst
      val oldKind = oldDt.kindOf
      val newKind = newDt.kindOf

      val kinds =
        if (oldKind == newKind) Nil
        else {
          ChangedKind(pn, oldDt, newDt) :: Nil
        }

      val cons = {
        val oldConsByName =
          oldDt.constructors.zipWithIndex.groupByNel(_._1.name)
        val newConsByName =
          newDt.constructors.zipWithIndex.groupByNel(_._1.name)
        oldConsByName
          .align(newConsByName)
          .iterator
          .flatMap {
            case (_, Ior.Left(nel)) =>
              ConstructorRemoved(pn, oldDt.toTypeTyConst, nel.head._1) :: Nil
            case (_, Ior.Right(nel)) =>
              ConstructorAdded(pn, newDt.toTypeTyConst, nel.head._1) :: Nil
            case (cons, Ior.Both(oldNel, newNel)) =>
              val (oldCfn, oldIdx) = oldNel.head
              val (newCfn, newIdx) = newNel.head
              val idxDiff =
                if (oldIdx == newIdx) Nil
                else
                  (ConstructorIndexChange(
                    pn,
                    oldDt.toTypeTyConst,
                    cons,
                    oldIdx,
                    newIdx
                  ) :: Nil)

              val fnDiff =
                // ConstructorFn is a case class; equals is structural and safe here (no Eq instance in scope).
                if (oldCfn === newCfn) Nil
                else {
                  // there is some difference
                  oldCfn.args
                    .align(newCfn.args)
                    .iterator
                    .zipWithIndex
                    .flatMap {
                      case (
                            Ior.Both((oldName, oldType), (newName, newType)),
                            idx
                          ) =>
                        val tpeDiff = diffType(oldType, newType).map(
                          ConstructorParamChange(
                            cons,
                            oldDt.toTypeTyConst,
                            idx,
                            _
                          )
                        )

                        if (oldName == newName) tpeDiff
                        else
                          (ConstructorParamNameChange(
                            pn,
                            cons,
                            oldDt.toTypeTyConst,
                            idx,
                            oldName,
                            newName
                          ) :: tpeDiff)
                      case (Ior.Right((newName, newType)), idx) =>
                        ConstructorParamAdded(
                          pn,
                          cons,
                          newDt.toTypeTyConst,
                          idx,
                          newName,
                          newType
                        ) :: Nil
                      case (Ior.Left((oldName, oldType)), idx) =>
                        ConstructorParamRemoved(
                          pn,
                          cons,
                          newDt.toTypeTyConst,
                          idx,
                          oldName,
                          oldType
                        ) :: Nil
                    }
                    .toList
                }

              idxDiff ::: fnDiff
          }
          .toList
      }

      kinds ::: cons
    }

    val prevMap = prev.groupByNel(_.name)
    val currMap = current.groupByNel(_.name)

    prevMap
      .align(currMap)
      .iterator
      .flatMap { case (name, ior) =>
        ior match {
          case Ior.Both(oldExp, newExp) =>
            name match {
              case bindable: Identifier.Bindable =>
                def bindsOf(
                    nel: NonEmptyList[ExportedName[R]]
                ): List[ExportedName.Binding[R]] =
                  nel.toList.collect { case ExportedName.Binding(n, tag) =>
                    ExportedName.Binding[R](n, tag)
                  }

                (bindsOf(oldExp), bindsOf(newExp)) match {
                  case (
                        ExportedName.Binding(_, Referant.Value(oldTpe)) :: Nil,
                        ExportedName.Binding(_, Referant.Value(newTpe)) :: Nil
                      ) =>
                    diffType(oldTpe, newTpe)
                      .map(ChangedValue(bindable, _))
                  case _ =>
                    // this is impossible for correctly constructed ExportedName[R] values
                    sys.error(
                      s"invariant violation: have bindable=$bindable but unexpected bindings: oldExp=$oldExp, newExp=$newExp"
                    )
                }
              case cons: Identifier.Constructor =>
                def typesOf(
                    nel: NonEmptyList[ExportedName[R]]
                ): List[DefinedType[Kind.Arg]] =
                  nel.toList
                    .collect {
                      case ExportedName.TypeName(_, Referant.DefinedT(dt)) => dt
                      case ExportedName.Constructor(
                            _,
                            Referant.Constructor(dt, _)
                          ) =>
                        dt
                    }
                    .distinct
                    .sortBy(_.toTypeConst: Type.Const)

                (typesOf(oldExp), typesOf(newExp)) match {
                  case (oldDt :: Nil, newDt :: Nil) => diffDT(oldDt, newDt)
                  case diff                         =>
                    sys.error(
                      s"invariant violation: have Constructor=$cons but unexpected diff of not exactly one type: diff=$diff"
                    )
                }
            }
          case Ior.Right(exports) => AddedName(pn, name, exports) :: Nil
          case Ior.Left(oldExp)   => RemovedName(pn, name, oldExp) :: Nil
        }
      }
      .toList
      .distinct
  }
}
