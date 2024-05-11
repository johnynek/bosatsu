package org.bykn.bosatsu

import cats.data.{
  Chain,
  NonEmptyList,
  NonEmptySet,
  NonEmptyChain,
  State,
  Validated,
  ValidatedNec
}
import org.bykn.bosatsu.rankn.{Type, TypeEnv, DefinedType}
import scala.collection.immutable.SortedSet

import cats.syntax.all._
import org.bykn.bosatsu.Referant.Constructor
import org.bykn.bosatsu.Referant.DefinedT
import org.bykn.bosatsu.Identifier.Bindable
import org.bykn.bosatsu.graph.Dag

/** This checks the imports and exports of compiled packages and makes sure they
  * are valid
  */
object PackageCustoms {
  def apply[A: HasRegion](
      pack: Package.Typed[A]
  ): ValidatedNec[PackageError, Package.Typed[A]] =
    checkValuesHaveExportedTypes(pack.name, pack.exports) *>
      noUselessBinds(pack) *>
      allImportsAreUsed(pack)

  private def removeUnused[A](
      vals: Option[NonEmptySet[(PackageName, Identifier)]],
      types: Option[NonEmptySet[(PackageName, Type.Const)]],
      pack: Package.Typed[A]
  ): Package.Typed[A] =
    (vals, types) match {
      case (None, None) => pack
      case (ov, ot) =>
        val unV = ov match {
          case Some(v) => v.toSortedSet
          case None    => SortedSet.empty[(PackageName, Identifier)]
        }
        val unT = ot match {
          case Some(v) => v.toSortedSet
          case None    => SortedSet.empty[(PackageName, Type.Const)]
        }
        val i = pack.imports.flatMap { imp =>
          imp.mapFilter { (pack, item) =>
            val t1 = item.tag.toList.filter {
              case Constructor(_, _) | Referant.Value(_) =>
                !unV((pack.name, item.originalName))
              case DefinedT(dt) =>
                !unT((pack.name, dt.toTypeConst))
            }
            NonEmptyList.fromList(t1).map(item.withTag(_))
          }
        }

        val cleanImap = ImportMap.fromImportsUnsafe(i)
        pack.copy(imports = i, program = (pack.program._1, cleanImap))
    }

  private type VSet = Set[(PackageName, Identifier)]
  private type VState[X] = State[VSet, X]

  private def usedGlobals[A](
      pack: Package.Typed[A]
  ): Set[(PackageName, Identifier)] = {
    val usedValuesSt: VState[Unit] =
      pack.lets.traverse_ { case (_, _, te) =>
        TypedExpr.usedGlobals(te)
      }

    usedValuesSt.runS(Set.empty).value
  }

  private def allImportsAreUsed[A](
      pack: Package.Typed[A]
  ): ValidatedNec[PackageError, Package.Typed[A]] = {
    // Note, we can't import just a name or just a type when we have Structs,
    // we get both. So, we will count a Constructor used if the Identifier
    // OR the type is used
    val impValues: SortedSet[(PackageName, Identifier)] =
      (for {
        imp <- pack.imports.iterator
        impPack = imp.pack.name
        item <- imp.items.iterator
        ref <- item.tag.iterator
        name <- ref match {
          case Constructor(_, _) | Referant.Value(_) =>
            item.originalName :: Nil
          case DefinedT(_) => Nil
        }
      } yield (impPack, name)).to(SortedSet)

    val impTypes: SortedSet[(PackageName, Type.Const)] =
      (for {
        imp <- pack.imports.iterator
        impPack = imp.pack.name
        item <- imp.items.iterator
        ref <- item.tag.iterator
        tpe <- ref match {
          case Constructor(_, _) | Referant.Value(_) =>
            Nil
          case DefinedT(dt) =>
            dt.toTypeConst :: Nil
        }
      } yield (impPack, tpe)).to(SortedSet)

    if (impValues.isEmpty && impTypes.isEmpty) Validated.valid(pack)
    else {
      val usedValues = usedGlobals(pack)

      val usedTypes: Set[Type.Const] =
        pack.program._1.lets.iterator
          .flatMap(
            _._3.allTypes.flatMap(Type.constantsOf(_))
          )
          .toSet

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
      val unusedPacks =
        (unusedValMap.keySet | unusedTypeMap.keySet) - PackageName.PredefName

      if (unusedPacks.isEmpty) {
        // remove unused
        Validated.valid(
          removeUnused(
            unusedValMap.get(PackageName.PredefName),
            unusedTypeMap.get(PackageName.PredefName),
            pack
          )
        )
      } else {
        val badImports =
          NonEmptyList.fromListUnsafe(unusedPacks.iterator.map { ipack =>
            val thisVals = unusedValMap.get(ipack) match {
              case Some(nes) =>
                nes.toSortedSet.toList.map { case (_, i) =>
                  ImportedName.OriginalName(i, ())
                }
              case None => Nil
            }

            val thisTpes = unusedTypeMap.get(ipack) match {
              case Some(nes) =>
                nes.toSortedSet.toList.map { case (_, t) =>
                  ImportedName.OriginalName(t.toDefined.name.ident, ())
                }
              case None => Nil
            }
            // one or the other or both of these is non-empty
            Import(
              ipack,
              NonEmptyList.fromListUnsafe((thisVals ::: thisTpes).distinct)
            )
          }.toList)

        Validated.invalidNec(PackageError.UnusedImport(pack.name, badImports))
      }
    }
  }

  private def checkValuesHaveExportedTypes[V](
      pn: PackageName,
      exports: List[ExportedName[Referant[V]]]
  ): ValidatedNec[PackageError, Unit] = {
    val exportedTypes: List[DefinedType[V]] = exports
      .flatMap(_.tag.definedType)
      .distinct

    val exportedTE = TypeEnv.fromDefinitions(exportedTypes)

    type Exp = ExportedName[Referant[V]]
    val usedTypes: Iterator[(Type.Const, Exp, Type)] = exports.iterator
      .flatMap { n =>
        n.tag match {
          case Referant.Value(t) => Iterator.single((t, n))
          case _                 => Iterator.empty
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

    NonEmptyChain.fromChain(
      Chain.fromIterableOnce(usedTypes.flatMap(errorFor))
    ) match {
      case None      => Validated.valid(())
      case Some(nel) => Validated.invalid(nel)
    }
  }

  private def noUselessBinds[A: HasRegion](
      pack: Package.Typed[A]
  ): ValidatedNec[PackageError, Unit] = {
    type Node = Either[pack.exports.type, Bindable]
    implicit val ordNode: Ordering[Node] =
      new Ordering[Node] {
        def compare(x: Node, y: Node): Int =
          x match {
            case Right(bx) =>
              y match {
                case Left(_) => 1
                case Right(by) =>
                  Ordering[Identifier].compare(bx, by)
              }
            case Left(_) =>
              y match {
                case Left(_)  => 0
                case Right(_) => -1
              }
          }
      }

    val exports: Node = Left(pack.exports)
    val roots: List[Node] =
      (exports ::
        Package.testValue(pack).map { case (b, _, _) => Right(b) }.toList :::
        Package
          .mainValue(pack)
          .map { case (b, _, _) => Right(b) }
          .toList).distinct

    val bindMap: Map[Bindable, TypedExpr[A]] =
      pack.program._1.lets.iterator.map { case (b, _, te) => (b, te) }.toMap

    def internalDeps(te: TypedExpr[A]): Set[Bindable] =
      TypedExpr.usedGlobals(te).runS(Set.empty).value.collect {
        case (pn, i: Identifier.Bindable) if pn == pack.name => i
      }

    def depsOf(n: Node): Iterable[Node] =
      n match {
        case Left(_) =>
          pack.exports.flatMap {
            case ExportedName.Binding(n, _) => Right(n) :: Nil
            case _                          => Nil
          }
        case Right(value) =>
          bindMap.get(value) match {
            case None     => Nil
            case Some(te) => internalDeps(te).map(Right(_))
          }
      }
    val canReach: SortedSet[Node] = Dag.transitiveSet(roots)(depsOf _)

    val unused = pack.lets.filter { case (bn, _, _) =>
      !canReach.contains(Right(bn))
    }

    NonEmptyList.fromList(unused) match {
      case None => Validated.unit
      case Some(value) =>
        Validated.invalidNec(
          PackageError.UnusedLets(
            pack.name,
            value.map { case (b, r, te) =>
              (b, r, te, HasRegion.region(te))
            }
          )
        )
    }
  }
}
