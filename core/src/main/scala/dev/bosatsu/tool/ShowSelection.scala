package dev.bosatsu.tool

import cats.data.{Validated, ValidatedNel}
import cats.parse.{Parser => P}
import cats.syntax.all._
import com.monovore.decline.Argument
import dev.bosatsu.Identifier.Bindable
import dev.bosatsu.rankn.Type
import dev.bosatsu.rankn.TypeEnv
import dev.bosatsu.{
  ExportedName,
  Identifier,
  Import,
  ImportMap,
  Kind,
  Package,
  PackageName,
  Referant,
  TypeName
}

object ShowSelection {
  type TypeSelector = (PackageName, TypeName)
  type ValueSelector = (PackageName, Bindable)
  private type TypeConst = Type.Const.Defined

  final case class Request(
      packages: List[PackageName],
      types: List[TypeSelector],
      values: List[ValueSelector]
  ) {
    lazy val isEmpty: Boolean =
      packages.isEmpty && types.isEmpty && values.isEmpty

    lazy val requestedPackages: List[PackageName] =
      (packages ::: types.map(_._1) ::: values.map(_._1)).distinct

    def hasFullPackage(name: PackageName): Boolean =
      packages.contains(name)

    def selectedTypes(name: PackageName): Set[TypeName] =
      types.collect { case (`name`, tpe) => tpe }.toSet

    def selectedValues(name: PackageName): Set[Bindable] =
      values.collect { case (`name`, value) => value }.toSet
  }

  implicit val typeArgument: Argument[TypeSelector] =
    new Argument[TypeSelector] {
      def defaultMetavar: String = "typeIdent"

      def read(
          string: String
      ): ValidatedNel[String, TypeSelector] =
        (PackageName.parser ~ (P.string("::") *> Identifier.consParser))
          .parseAll(string) match {
          case Right((pack, cons)) =>
            Validated.valid((pack, TypeName(cons)))
          case Left(_) =>
            Validated.invalidNel(
              s"could not parse $string as package::type. Must be package::Type, e.g. Foo/Bar::Baz."
            )
        }
    }

  implicit val valueArgument: Argument[ValueSelector] =
    new Argument[ValueSelector] {
      def defaultMetavar: String = "valueIdent"

      def read(
          string: String
      ): ValidatedNel[String, ValueSelector] =
        (PackageName.parser ~ (P.string("::") *> Identifier.parser))
          .parseAll(string) match {
          case Right((pack, bindable: Bindable)) =>
            Validated.valid((pack, bindable))
          case Right((pack, cons: Identifier.Constructor)) =>
            Validated.invalidNel(
              show"${pack.asString}::${cons.asString} is a constructor or type name, not a value. A top-level value is required."
            )
          case Left(_) =>
            Validated.invalidNel(
              s"could not parse $string as package::value. Must be package::value, e.g. Foo/Bar::bippy."
            )
        }
    }

  private def hasValue(
      pack: Package.Typed[Any],
      value: Bindable
  ): Boolean =
    pack.lets.exists(_._1 == value) || pack.externalDefs.contains(value)

  private def availableValueNames(
      pack: Package.Typed[Any]
  ): List[String] = {
    val (prog, _) = pack.program
    (prog.lets.iterator.map(_._1.sourceCodeRepr) ++ prog.externalDefs.iterator
      .map(_.sourceCodeRepr)).toSet.toList.sorted
  }

  private def filteredTypeEnv(
      pack: Package.Typed[Any],
      keepLocalTypes: Set[TypeName],
      keepExternalValues: Set[Bindable]
  ): TypeEnv[Kind.Arg] = {
    val (prog, _) = pack.program
    val keptDefinitions =
      prog.types.allDefinedTypes.filter { dt =>
        (dt.packageName != pack.name) || keepLocalTypes(dt.name)
      }
    val base = TypeEnv.fromDefinitions(keptDefinitions)

    keepExternalValues.foldLeft(base) { (acc, value) =>
      prog.types.getExternalValue(pack.name, value) match {
        case Some(tpe) => acc.addExternalValue(pack.name, value, tpe)
        case None      => acc
      }
    }
  }

  private def typeConstsOf(types: List[Type]): Set[TypeConst] =
    Type.allConsts(types).iterator.map(_.tpe.toDefined).toSet

  private final case class Analysis(
      keepValues: Set[Bindable],
      keepLocalTypes: Set[TypeName],
      neededGlobals: Set[(PackageName, Identifier)],
      neededTypeConsts: Set[TypeConst]
  )

  private def analyzePartialPackage(
      pack: Package.Typed[Any],
      seedTypes: Set[TypeName],
      seedValues: Set[Bindable]
  ): Analysis = {
    val packName = pack.name
    val (prog, _) = pack.program
    val letsByName = prog.lets.iterator.map { case (name, _, expr) =>
      (name, expr)
    }.toMap
    val localExternals = prog.externalDefs.toSet

    def localTypeFromConst(tc: TypeConst): Option[TypeName] =
      if (tc.packageName == packName) Some(tc.name) else None

    def localTypeFromConstructor(cons: Identifier.Constructor): Option[TypeName] =
      prog.types.getConstructor(packName, cons).map(_._1.name)

    @annotation.tailrec
    def loop(
        pendingValues: List[Bindable],
        pendingTypes: List[TypeName],
        keepValues: Set[Bindable],
        keepTypes: Set[TypeName],
        neededGlobals: Set[(PackageName, Identifier)],
        typeConsts: Set[TypeConst]
    ): Analysis =
      pendingValues match {
        case value :: valueTail if keepValues(value) =>
          loop(
            valueTail,
            pendingTypes,
            keepValues,
            keepTypes,
            neededGlobals,
            typeConsts
          )
        case value :: valueTail =>
          val maybeExpr = letsByName.get(value)
          val globals = maybeExpr.fold(Set.empty[(PackageName, Identifier)])(_.globals)
          val localValueDeps = globals.iterator.flatMap {
            case (`packName`, ident) =>
              ident.toBindable.filter { bindable =>
                letsByName.contains(bindable) || localExternals(bindable)
              }
            case _ =>
              None
          }.toSet
          val localTypeDepsFromGlobals = globals.iterator.flatMap {
            case (`packName`, ident) =>
              ident.toConstructor.flatMap(localTypeFromConstructor)
            case _ => None
          }.toSet
          val valueTypeConsts = maybeExpr match {
            case Some(expr) =>
              typeConstsOf(expr.allTypes.toList)
            case None =>
              prog.types
                .getExternalValue(packName, value)
                .map(tpe => typeConstsOf(tpe :: Nil))
                .getOrElse(Set.empty)
          }
          val localTypeDepsFromTypes =
            valueTypeConsts.iterator.flatMap(localTypeFromConst).toSet

          loop(
            pendingValues =
              localValueDeps.toList ::: valueTail,
            pendingTypes =
              (localTypeDepsFromGlobals ++ localTypeDepsFromTypes).toList ::: pendingTypes,
            keepValues = keepValues + value,
            keepTypes = keepTypes,
            neededGlobals = neededGlobals ++ globals,
            typeConsts = typeConsts ++ valueTypeConsts
          )
        case Nil =>
          pendingTypes match {
            case typeName :: typeTail if keepTypes(typeName) =>
              loop(
                pendingValues,
                typeTail,
                keepValues,
                keepTypes,
                neededGlobals,
                typeConsts
              )
            case typeName :: typeTail =>
              val deps: Set[TypeConst] =
                prog.types
                  .getType(packName, typeName)
                  .map(dt => dt.dependsOn.iterator.map(_.tpe.toDefined).toSet)
                  .getOrElse(Set.empty)
              val localTypeDeps =
                deps.iterator.flatMap(localTypeFromConst).toSet

              loop(
                pendingValues,
                localTypeDeps.toList ::: typeTail,
                keepValues,
                keepTypes + typeName,
                neededGlobals,
                typeConsts ++ deps
              )
            case Nil =>
              Analysis(
                keepValues = keepValues,
                keepLocalTypes = keepTypes,
                neededGlobals = neededGlobals.filter(_._1 != packName),
                neededTypeConsts = typeConsts.filter(_.packageName != packName)
              )
          }
      }

    loop(
      pendingValues = seedValues.toList,
      pendingTypes = seedTypes.toList,
      keepValues = Set.empty,
      keepTypes = Set.empty,
      neededGlobals = Set.empty,
      typeConsts = Set.empty
    )
  }

  private def filterImports(
      pack: Package.Typed[Any],
      neededGlobals: Set[(PackageName, Identifier)],
      neededTypeConsts: Set[TypeConst]
  ): List[Import[
    Package.Interface,
    cats.data.NonEmptyList[Referant[Kind.Arg]]
  ]] =
    pack.imports.flatMap { imp =>
      val importPack = imp.pack.name
      val items = imp.items.toList.filter { item =>
        val neededByGlobal = neededGlobals((importPack, item.originalName))
        val neededByType = item.tag.exists {
          case Referant.DefinedT(dt) =>
            neededTypeConsts(dt.toTypeConst)
          case Referant.Constructor(dt, _) =>
            neededTypeConsts(dt.toTypeConst)
          case Referant.Value(_) =>
            false
        }
        neededByGlobal || neededByType
      }
      cats.data.NonEmptyList.fromList(items).map(i => imp.copy(items = i))
    }

  private def filterPartialPackage(
      pack: Package.Typed[Any],
      keepLocalTypes: Set[TypeName],
      keepValues: Set[Bindable],
      neededGlobals: Set[(PackageName, Identifier)],
      neededTypeConsts: Set[TypeConst]
  ): Package.Typed[Any] = {
    val (prog, _) = pack.program
    val types1 = filteredTypeEnv(pack, keepLocalTypes, keepValues)
    val imports1 = filterImports(pack, neededGlobals, neededTypeConsts)
    val importMap1 = ImportMap.fromImportsUnsafe(imports1)
    val exports1 =
      pack.exports.filter {
        case ExportedName.TypeName(name, _) =>
          keepLocalTypes(TypeName(name))
        case ExportedName.Constructor(name, _) =>
          keepLocalTypes(TypeName(name))
        case ExportedName.Binding(name, _) =>
          keepValues(name)
      }

    val program1 =
      prog.copy(
        types = types1,
        lets = prog.lets.filter { case (name, _, _) => keepValues(name) },
        externalDefs = prog.externalDefs.filter(keepValues)
      )

    pack.copy(imports = imports1, exports = exports1, program = (program1, importMap1))
  }

  def selectPackages(
      packs: List[Package.Typed[Any]],
      request: Request
  ): Either[String, List[Package.Typed[Any]]] = {
    val byName = packs.iterator.map(pack => (pack.name, pack)).toMap

    def requirePackage(pn: PackageName): Either[String, Package.Typed[Any]] =
      byName
        .get(pn)
        .toRight(show"package not found: ${pn.asString}")

    def requireType(
        pn: PackageName,
        tn: TypeName
    ): Either[String, Unit] =
      requirePackage(pn).flatMap { pack =>
        Either.cond(
          pack.types.getType(pack.name, tn).isDefined,
          (),
          show"type not found: ${pn.asString}::${tn.asString}"
        )
      }

    def requireValue(
        pn: PackageName,
        value: Bindable
    ): Either[String, Unit] =
      requirePackage(pn).flatMap { pack =>
        val candidates = availableValueNames(pack)
        val candidatesMsg =
          if (candidates.isEmpty) ""
          else
            show"\ncompiled top-level values: [${candidates.mkString(", ")}]"
        val inliningHint =
          "\nhint: fully inlined values may be absent from compiled code. Export a name to keep it from being erased, or try another name."

        Either.cond(
          hasValue(pack, value),
          (),
          show"value not found: ${pn.asString}::${value.sourceCodeRepr}${inliningHint}${candidatesMsg}"
        )
      }

    if (request.isEmpty) Right(packs)
    else
      for {
        _ <- request.requestedPackages.traverse(requirePackage)
        _ <- request.types.traverse { case (pn, tn) => requireType(pn, tn) }
        _ <- request.values.traverse { case (pn, value) =>
          requireValue(pn, value)
        }
      } yield request.requestedPackages.flatMap { pn =>
        byName.get(pn).map { pack =>
          if (request.hasFullPackage(pn)) pack
          else {
            val analysis = analyzePartialPackage(
              pack,
              request.selectedTypes(pn),
              request.selectedValues(pn)
            )
            filterPartialPackage(
              pack,
              analysis.keepLocalTypes,
              analysis.keepValues,
              analysis.neededGlobals,
              analysis.neededTypeConsts
            )
          }
        }
      }
  }

  def selectInterfaces(
      ifaces: List[Package.Interface],
      request: Request
  ): List[Package.Interface] =
    if (request.isEmpty) ifaces
    else {
      val selected = request.requestedPackages.toSet
      ifaces.filter(iface => selected(iface.name))
    }
}
