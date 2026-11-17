package dev.bosatsu.library

import cats.data.{Ior, NonEmptyList, ValidatedNec}
import dev.bosatsu.rankn.{ConstructorFn, DefinedType, Type, TypeAlias, TypeEnv}
import dev.bosatsu.{
  ExportedName,
  Identifier,
  Kind,
  PackageError,
  PackageName,
  Referant
}
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
  type V[A] = ValidatedNec[ApiDiff.Error, A]

  sealed abstract class Error {
    def toDoc: Doc
  }
  object Error {
    enum MissingEnv derives CanEqual {
      case Previous
      case Current
      def label: String =
        this match {
          case MissingEnv.Previous => "previous"
          case MissingEnv.Current  => "current"
        }
    }

    case class MissingTransitiveType(
        pack: PackageName,
        tpe: Type,
        transitive: Type.TyConst,
        env: MissingEnv
    ) extends Error {
      def toDoc: Doc = {
        val tdocs = PackageError.showTypes(pack, tpe :: transitive :: Nil)
        Doc.text(show"while diffing $pack, type ") +
          tdocs(tpe) +
          Doc.text(" references missing transitive type ") +
          tdocs(transitive) +
          Doc.text(s" in ${env.label} environment")
      }
    }
  }

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

  case class ChangedAliasKind(
      pack: PackageName,
      oldAlias: TypeAlias[Kind.Arg],
      newAlias: TypeAlias[Kind.Arg]
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Type.fullyResolvedDocument.document(oldAlias.toTypeTyConst) + Doc.text(
        " changes from kind "
      ) +
        Document[Kind].document(oldAlias.kindOf) + Doc.text(" to kind ") +
        Document[Kind].document(newAlias.kindOf)
  }

  case class ChangedAliasRhs(
      pack: PackageName,
      oldAlias: TypeAlias[Kind.Arg],
      newAlias: TypeAlias[Kind.Arg]
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Type.fullyResolvedDocument.document(oldAlias.toTypeTyConst) + Doc.text(
        " changes from alias body "
      ) +
        Type.fullyResolvedDocument.document(oldAlias.rhs) + Doc.text(" to ") +
        Type.fullyResolvedDocument.document(newAlias.rhs)
  }

  case class ChangedTypeDefForm(
      pack: PackageName,
      tycons: Type.TyConst,
      oldForm: String,
      newForm: String
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc =
      Type.fullyResolvedDocument.document(tycons) + Doc.text(
        show" changes from $oldForm to $newForm"
      )
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
      cfn: ConstructorFn[Kind.Arg]
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
      cfn: ConstructorFn[Kind.Arg]
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

  case class ConstructorParamDefaultAdded(
      pack: PackageName,
      cons: Identifier.Constructor,
      tycons: Type.TyConst,
      paramIndex: Int,
      name: Identifier.Bindable
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMinor | dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $cons of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(
        show" at parameter index $paramIndex added default value to parameter $name"
      )
  }

  case class ConstructorParamDefaultRemoved(
      pack: PackageName,
      cons: Identifier.Constructor,
      tycons: Type.TyConst,
      paramIndex: Int,
      name: Identifier.Bindable
  ) extends Diff {
    def isValidWhen(dk: Version.DiffKind) = dk.isMajor
    def toDoc = Doc.text(
      show"constructor named $cons of "
    ) + Type.fullyResolvedDocument.document(tycons) +
      Doc.text(
        show" at parameter index $paramIndex removed default value from parameter $name"
      )
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
  ): V[Diffs] = {
    val aligned = prevIface.align(current).toList

    aligned
      .traverse { case (pack, ior) =>
        ior match {
          case Ior.Left(es)  => (pack, RemovedPackage(pack, es) :: Nil).validNec
          case Ior.Right(es) => (pack, AddedPackage(pack, es) :: Nil).validNec
          case Ior.Both(oldE, newE) =>
            apply(pack, oldE, prevEnv, newE, curEnv).map(d => (pack, d))
        }
      }
      .map { pairs =>
        val diffMap =
          pairs.iterator
            .collect {
              case (pack, diffs) if diffs.nonEmpty =>
                pack -> NonEmptyList.fromListUnsafe(diffs)
            }
            .to(SortedMap)
        Diffs(diffMap)
      }
  }

  private def apply(
      pn: PackageName,
      prev: List[ExportedName[R]],
      prevEnv: TypeEnv[Kind.Arg],
      current: List[ExportedName[R]],
      curEnv: TypeEnv[Kind.Arg]
  ): V[List[Diff]] = {
    import Error._

    sealed trait TypeTarget {
      def toTypeTyConst: Type.TyConst
    }
    object TypeTarget {
      final case class Data(dt: DefinedType[Kind.Arg]) extends TypeTarget {
        val toTypeTyConst = dt.toTypeTyConst
      }
      final case class Alias(ta: TypeAlias[Kind.Arg]) extends TypeTarget {
        val toTypeTyConst = ta.toTypeTyConst
      }

      def label(tt: TypeTarget): String =
        tt match {
          case Data(_)  => "data type"
          case Alias(_) => "type alias"
        }
    }

    def diffTA(
        oldAlias: TypeAlias[Kind.Arg],
        newAlias: TypeAlias[Kind.Arg]
    ): V[List[Diff]] = {
      val kinds =
        if (oldAlias.kindOf == newAlias.kindOf) Nil
        else ChangedAliasKind(pn, oldAlias, newAlias) :: Nil

      val rhsV =
        if (oldAlias.rhs.sameAs(newAlias.rhs)) {
          Type
            .allConsts(oldAlias.rhs :: Nil)
            .traverse { const =>
              diffConst(const).map(
                _.map(diff => ChangedTransitiveType(newAlias.rhs, const, diff))
              )
            }
            .map(_.flatten)
        } else {
          (ChangedAliasRhs(pn, oldAlias, newAlias) :: Nil).validNec
        }

      rhsV.map(kinds ::: _)
    }

    def diffTarget(
        oldTarget: TypeTarget,
        newTarget: TypeTarget
    ): V[List[Diff]] =
      (oldTarget, newTarget) match {
        case (TypeTarget.Data(oldDt), TypeTarget.Data(newDt)) =>
          diffDT(oldDt, newDt)
        case (TypeTarget.Alias(oldAlias), TypeTarget.Alias(newAlias)) =>
          diffTA(oldAlias, newAlias)
        case (oldT, newT) =>
          (ChangedTypeDefForm(
            pn,
            oldT.toTypeTyConst,
            TypeTarget.label(oldT),
            TypeTarget.label(newT)
          ) :: Nil).validNec
      }

    def diffConst(const: Type.TyConst): V[List[Diff]] = {
      val oldTarget =
        prevEnv
          .toDefinedType(const.tpe)
          .map(TypeTarget.Data(_))
          .orElse(prevEnv.toTypeAlias(const.tpe).map(TypeTarget.Alias(_)))
      val newTarget =
        curEnv
          .toDefinedType(const.tpe)
          .map(TypeTarget.Data(_))
          .orElse(curEnv.toTypeAlias(const.tpe).map(TypeTarget.Alias(_)))

      (oldTarget, newTarget) match {
        case (Some(oldT), Some(newT)) =>
          diffTarget(oldT, newT)
        case (None, _) =>
          MissingTransitiveType(
            pn,
            const,
            const,
            MissingEnv.Previous
          ).invalidNec
        case (_, None) =>
          MissingTransitiveType(
            pn,
            const,
            const,
            MissingEnv.Current
          ).invalidNec
      }
    }

    def diffType(oldTpe: Type, newTpe: Type): V[List[Diff]] =
      // this should be the only valid case, otherwise there were multiple things previously
      if (oldTpe.sameAs(newTpe)) {
        // these are the same so they reference the same consts
        val consts = Type.allConsts(oldTpe :: Nil)

        consts
          .traverse { const =>
            diffConst(const).map(
              _.map(diff => ChangedTransitiveType(newTpe, const, diff))
            )
          }
          .map(_.flatten)
      } else {
        (ChangedType(pn, oldTpe, newTpe) :: Nil).validNec
      }

    def diffDT(
        oldDt: DefinedType[Kind.Arg],
        newDt: DefinedType[Kind.Arg]
    ): V[List[Diff]] = {
      // invariant: oldDt.toTypeConst == newDt.toTypeConst
      val oldKind = oldDt.kindOf
      val newKind = newDt.kindOf

      val kinds =
        if (oldKind == newKind) Nil
        else {
          ChangedKind(pn, oldDt, newDt) :: Nil
        }

      val consV: V[List[Diff]] = {
        val oldConsByName =
          oldDt.constructors.zipWithIndex.groupByNel(_._1.name)
        val newConsByName =
          newDt.constructors.zipWithIndex.groupByNel(_._1.name)
        oldConsByName
          .align(newConsByName)
          .toList
          .traverse {
            case (_, Ior.Left(nel)) =>
              (ConstructorRemoved(
                pn,
                oldDt.toTypeTyConst,
                nel.head._1
              ) :: Nil).validNec
            case (_, Ior.Right(nel)) =>
              (ConstructorAdded(
                pn,
                newDt.toTypeTyConst,
                nel.head._1
              ) :: Nil).validNec
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

              val fnDiffV: V[List[Diff]] =
                // ConstructorFn is a case class; equals is structural and safe here (no Eq instance in scope).
                if (oldCfn === newCfn) Nil.validNec
                else {
                  oldCfn.args
                    .align(newCfn.args)
                    .iterator
                    .zipWithIndex
                    .toList
                    .traverse {
                      case (Ior.Both(oldParam, newParam), idx) =>
                        diffType(oldParam.tpe, newParam.tpe).map { tpeDiffs =>
                          val tpeDiff = tpeDiffs.map(
                            ConstructorParamChange(
                              cons,
                              oldDt.toTypeTyConst,
                              idx,
                              _
                            )
                          )

                          val nameDiff =
                            if (oldParam.name == newParam.name) Nil
                            else
                              ConstructorParamNameChange(
                                pn,
                                cons,
                                oldDt.toTypeTyConst,
                                idx,
                                oldParam.name,
                                newParam.name
                              ) :: Nil

                          val defaultDiff =
                            (
                              oldParam.defaultBinding,
                              newParam.defaultBinding
                            ) match {
                              case (None, Some(_)) =>
                                ConstructorParamDefaultAdded(
                                  pn,
                                  cons,
                                  oldDt.toTypeTyConst,
                                  idx,
                                  newParam.name
                                ) :: Nil
                              case (Some(_), None) =>
                                ConstructorParamDefaultRemoved(
                                  pn,
                                  cons,
                                  oldDt.toTypeTyConst,
                                  idx,
                                  oldParam.name
                                ) :: Nil
                              case _ =>
                                Nil
                            }

                          nameDiff ::: defaultDiff ::: tpeDiff
                        }
                      case (Ior.Right(newParam), idx) =>
                        (ConstructorParamAdded(
                          pn,
                          cons,
                          newDt.toTypeTyConst,
                          idx,
                          newParam.name,
                          newParam.tpe
                        ) :: Nil).validNec
                      case (Ior.Left(oldParam), idx) =>
                        (ConstructorParamRemoved(
                          pn,
                          cons,
                          newDt.toTypeTyConst,
                          idx,
                          oldParam.name,
                          oldParam.tpe
                        ) :: Nil).validNec
                    }
                    .map(_.flatten)
                }

              fnDiffV.map(fnDiff => idxDiff ::: fnDiff)
          }
          .map(_.flatten)
      }

      consV.map(cons => kinds ::: cons)
    }

    val prevMap = prev.groupByNel(_.name)
    val currMap = current.groupByNel(_.name)

    prevMap
      .align(currMap)
      .toList
      .traverse { case (name, ior) =>
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
                      .map(_.map(ChangedValue(bindable, _)))
                  case _ =>
                    // this is impossible for correctly constructed ExportedName[R] values
                    sys.error(
                      s"invariant violation: have bindable=$bindable but unexpected bindings: oldExp=$oldExp, newExp=$newExp"
                    )
                }
              case cons: Identifier.Constructor =>
                def typeTargetsOf(
                    nel: NonEmptyList[ExportedName[R]]
                ): List[TypeTarget] =
                  nel.toList
                    .collect {
                      case ExportedName.TypeName(_, Referant.DefinedT(dt)) =>
                        TypeTarget.Data(dt)
                      case ExportedName.TypeName(_, Referant.TypeAliasT(ta)) =>
                        TypeTarget.Alias(ta)
                      case ExportedName.Constructor(
                            _,
                            Referant.Constructor(dt, _)
                          ) =>
                        TypeTarget.Data(dt)
                    }
                    .distinct
                    .sortBy(_.toTypeTyConst: Type)

                (typeTargetsOf(oldExp), typeTargetsOf(newExp)) match {
                  case (oldTarget :: Nil, newTarget :: Nil) =>
                    diffTarget(oldTarget, newTarget)
                  case diff =>
                    sys.error(
                      s"invariant violation: have Constructor=$cons but unexpected diff of not exactly one type: diff=$diff"
                    )
                }
            }
          case Ior.Right(exports) =>
            (AddedName(pn, name, exports) :: Nil).validNec
          case Ior.Left(oldExp) =>
            (RemovedName(pn, name, oldExp) :: Nil).validNec
        }
      }
      .map(_.flatten.distinct)
  }
}
