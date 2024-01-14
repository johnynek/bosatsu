package org.bykn.bosatsu

import cats.Monad
import cats.data.{Chain, NonEmptyList, State}
import org.bykn.bosatsu.rankn.{Type, TypeEnv, DefinedType}
import scala.collection.immutable.SortedSet

import cats.syntax.all._
import org.bykn.bosatsu.Referant.Constructor
import org.bykn.bosatsu.Referant.DefinedT
import org.bykn.bosatsu.TypedExpr.Match

/**
  * This checks the imports and exports of compiled packages
  * and makes sure they are valid
  */
object PackageCustoms {
  def errors[A](pack: Package.Typed[A]): List[PackageError] = {

    val impsUsed = allImportsAreUsed(pack)
    val expCheck = checkValuesHaveExportedTypes(pack.name, pack.exports)

    (impsUsed ++ expCheck).toList
  }

  private def allImportsAreUsed[A](pack: Package.Typed[A]): Chain[PackageError] = {
    // Note, we can't import just a name or just a type when we have Structs,
    // we get both. So, we will count a Constructor used if the Identifier
    // OR the type is used
    val impValues: SortedSet[(PackageName, Identifier)] =
      (for {
        imp <- pack.imports.iterator
        impPack = imp.pack.name
        item <- imp.items.toList
        ref <- item.tag.toList
        name <- ref match {
          case Constructor(_, _) | Referant.Value(_) =>
            item.originalName :: Nil
          case DefinedT(_) => Nil
        }
      } yield (impPack, name)).to(SortedSet)

    type VSet = Set[(PackageName, Identifier)]
    type VState[X] = State[VSet, X]
    val usedValuesSt: VState[Unit] =
      pack.program.lets.traverse_ { case (_, _, te) =>
        te.traverseUp {
          case g@TypedExpr.Global(p, n, _, _) =>
            State { s => (s + ((p, n)), g) }
          case m @ Match(_, branches, _) => 
            branches.traverse_ {
              case (pat, _) =>
                pat.traverseStruct[VState, (PackageName, Identifier.Constructor)] { (n, parts) =>
                  State.modify[VSet](_ + n) *> 
                    parts.map { inner =>
                      Pattern.PositionalStruct(n, inner)
                    }
                }
                .void
            }.as(m)
          case te => Monad[VState].pure(te)
        }
      }

    val usedValues = usedValuesSt.runS(Set.empty).value
    

    val impTypes: SortedSet[(PackageName, Type.Const)] =
      (for {
        imp <- pack.imports.iterator
        item <- imp.items.toList
        ref <- item.tag.toList
        tpe <- ref match {
          case Constructor(_, _) | Referant.Value(_) =>
            Nil
          case DefinedT(dt) =>
            dt.toTypeConst :: Nil
        }
      } yield (imp.pack.name, tpe)).to(SortedSet)

    val usedTypes: Set[Type.Const] =
      pack.program.lets.iterator.flatMap(
        _._3.allTypes.flatMap(Type.constantsOf(_))
      ).toSet

    val unusedValues = impValues.filterNot { tup =>
      tup match {
        case (pn, cons @ Identifier.Constructor(_)) =>
          // deal with the ambiguity of capital names
          usedValues(tup) || usedTypes(Type.Const.Defined(pn, TypeName(cons)))
        case _ =>
          // no ambiguity for bindable
          usedValues(tup)
      }  
    }
    val unusedTypes = impTypes.filterNot { case (pn, t) =>
      // deal with the ambiguity of capital names
      usedTypes(t) || usedValues((pn, t.toDefined.name.ident))
    }

    val unusedValMap = unusedValues.groupByNes(_._1)
    val unusedTypeMap = unusedTypes.groupByNes(_._1)
    val unusedPacks = unusedValMap.keySet | unusedTypeMap.keySet

    Chain.fromIterableOnce(
      unusedPacks
      .iterator
      .filterNot(_.isPredef)
      .map { ipack =>
      val thisVals = unusedValMap.get(ipack) match {
        case Some(nes) => nes.toSortedSet.toList.map { case (_, i) =>
          ImportedName.OriginalName(i, ())
        }
        case None => Nil
      }

      val thisTpes = unusedTypeMap.get(ipack) match {
        case Some(nes) => nes.toSortedSet.toList.map { case (_, t) =>
          ImportedName.OriginalName(t.toDefined.name.ident, ())
        }
        case None => Nil
      }
      // one or the other or both of these is non-empty
      PackageError.UnusedImport(
        pack.name,
        Import(ipack, NonEmptyList.fromListUnsafe((thisVals ::: thisTpes).distinct))
      )
    })
  }

  private def checkValuesHaveExportedTypes[V](pn: PackageName, exports: List[ExportedName[Referant[V]]]): Chain[PackageError] = {
    val exportedTypes: List[DefinedType[V]] = exports
      .iterator
      .map(_.tag)
      .collect {
        case Referant.Constructor(dt, _) => dt
        case Referant.DefinedT(dt) => dt
      }
      .toList
      .distinct

    val exportedTE = TypeEnv.fromDefinitions(exportedTypes)

    type Exp = ExportedName[Referant[V]]
    val usedTypes: Iterator[(Type.Const, Exp, Type)] = exports
      .iterator
      .flatMap { n =>
        n.tag match {
          case Referant.Value(t) => Iterator.single((t, n))
          case _ => Iterator.empty
        }
      }
      .flatMap { case (t, n) => Type.constantsOf(t).map((_, n, t)) }
      .filter { case (Type.Const.Defined(p, _), _, _) => p === pn }


    def errorFor(t: (Type.Const, Exp, Type)): List[PackageError] =
      exportedTE.toDefinedType(t._1) match {
        case None =>
          PackageError.PrivateTypeEscape(t._2, t._3, pn, t._1) :: Nil
        case Some(_) => Nil
      }

    Chain.fromIterableOnce(usedTypes.flatMap(errorFor))
  }
}