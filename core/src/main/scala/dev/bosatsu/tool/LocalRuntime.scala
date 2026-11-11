package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.Show
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.bosatsu.{Identifier, Kind, Package, PackageMap, PackageName, PackageSignatures, PlatformIO, ProtoConverter, Referant}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable
import scala.util.Try

/** All loose bundles in one executable share a scope. Keep file membership
  * separately so a runtime consumer can select roots from one specific bundle.
  */
private[bosatsu] final class LocalRuntime[P] private (
    val packages: PackageMap.Compiled,
    val members: Map[P, NonEmptyList[PackageName]],
    bindings: SortedMap[PackageName, SortedSet[Identifier.Bindable]]
) {
  def hasBinding(pack: PackageName, name: Identifier.Bindable): Boolean =
    pack.isPredef || bindings.get(pack).exists(_.contains(name))
}

private[bosatsu] object LocalRuntime {
  def load[F[_], P](platform: PlatformIO[F, P], manifest: LocalRuntimeManifest[P]): F[LocalRuntime[P]] = {
    import platform.{moduleIOMonad, showPath}
    for {
      bundles <- manifest.packages.traverse { path =>
        platform.readPackageBundle(path)
          .map(path -> _)
          .adaptError { case err =>
            CliException.Basic(show"cannot read implementation bundle $path: ${err.getMessage}")
          }
      }
      scope <- platform.canPromiseF.compute(decode(bundles, manifest.externals)).flatMap(moduleIOMonad.fromTry)
    } yield scope
  }

  private def decode[P: Show](
      bundles: NonEmptyList[(P, proto.Packages)],
      bindings: SortedMap[PackageName, SortedSet[Identifier.Bindable]]
  ): Try[LocalRuntime[P]] = Try {
    val owners = mutable.Map.empty[PackageName, P]
    val encoded = List.newBuilder[proto.Package]
    val members = bundles.toList.map { case (file, bundle) =>
      val names = bundle.packages.iterator.map { pack =>
        val raw = ProtoConverter.pname(pack)
        val name = PackageName.parse(raw).getOrElse(
          throw CliException.Basic(show"invalid package name '$raw' in implementation bundle $file"))
        if (name.isPredef)
          throw CliException.Basic(show"$file contains Bosatsu/Predef, supplied by the compiler")
        owners.put(name, file).foreach { previous =>
          throw CliException.Basic(show"duplicate implementation of $name in $previous and $file")
        }
        encoded += pack
        name
      }.toList.sorted
      file -> NonEmptyList.fromList(names).getOrElse(
        throw CliException.Basic(show"empty implementation bundle $file"))
    }.toMap

    // Decode jointly so imports can bind to actual declarations across bundle
    // boundaries. The codec uses the compiler's implicit Predef interface.
    val (_, decoded) = ProtoConverter.packagesFromProto(Nil, encoded.result()).recover { case err =>
      throw CliException.Basic(s"invalid local implementation graph: ${err.getMessage}")
    }.get
    val packages = PackageMap.fromIterable(PackageMap.predefCompiled :: decoded)
    checkImports(packages)
    checkRequirements(bundles, members, packages)
    bindings.foreach { case (name, symbols) =>
      val pack = packages.toMap.getOrElse(name,
        throw CliException.Basic(show"native bindings refer to missing package $name"))
      val declarations = pack.externalDefs.toSet
      symbols.foreach { symbol =>
        if (!declarations(symbol))
          throw CliException.Basic(show"native binding $name::${symbol.sourceCodeRepr} has no external declaration")
      }
    }
    new LocalRuntime(packages, members, bindings)
  }

  // Legacy packages also retain the caller's imported names and value types.
  // Imported type definitions can already be rebound to the implementation by
  // the codec. Check their original export names and owners here.
  private def checkImports(packages: PackageMap.Compiled): Unit = {
    def matches(a: Referant[Kind.Arg], b: Referant[Kind.Arg]): Boolean =
      (a, b) match {
        case (Referant.Value(x), Referant.Value(y)) => x.sameAs(y)
        case (Referant.DefinedT(x), Referant.DefinedT(y)) => x.toTypeConst == y.toTypeConst
        case (Referant.Constructor(x, cx), Referant.Constructor(y, cy)) =>
          x.toTypeConst == y.toTypeConst && cx.name == cy.name
        case (Referant.TypeAliasT(x), Referant.TypeAliasT(y)) => x.toTypeConst == y.toTypeConst
        case _ => false
      }

    val exports = packages.toMap.map { case (pn, pack) => pn -> pack.exports.groupMap(_.name)(_.tag) }
    packages.toMap.valuesIterator.foreach { pack =>
      pack.imports.foreach { imp =>
        val provider = imp.pack.name
        val names = exports(provider)
        imp.items.toList.foreach { item =>
          val current = names.getOrElse(item.originalName, Nil)
          if (!item.tag.forall(expected => current.exists(matches(expected, _))))
            throw CliException.Basic(
              show"stale import in ${pack.name}: $provider::${item.originalName.sourceCodeRepr} no longer exports the required declaration")
        }
      }
    }
  }

  private def checkRequirements[P: Show](
      bundles: NonEmptyList[(P, proto.Packages)],
      members: Map[P, NonEmptyList[PackageName]],
      packages: PackageMap.Compiled
  ): Unit = {
    // Cache only inside this graph load. Several callers can share a wide
    // provider and the next worker request can replace the bytes at its path.
    val verified = mutable.Map.empty[PackageName, (String, List[PackageName])]
    bundles.toList.foreach { case (file, bundle) =>
      val local = members(file).toList.toSet
      val imports = local.iterator.flatMap(packages.toMap(_).imports)
      bundle.buildRequirements match {
        case None =>
          // The legacy codec can rebind a constructor to the *current*
          // owner's layout. A match compiled against an older family can
          // have lost branches already; re-reading that owner cannot prove
          // exhaustiveness. Builtins and same-bundle types have one owner.
          val unverifiable = for {
            imp <- imports
            item <- imp.items.toList.iterator
            ref <- item.tag.toList.iterator
            pn <- ref.depPackages.iterator if !pn.isPredef && !local(pn)
          } yield pn
          val owners = unverifiable.toSet.toList.sorted
          if (owners.nonEmpty)
            throw CliException.Basic(show"recompile legacy bundle $file to verify external types from ${owners.map(_.asString).mkString(", ")}")
        case Some(requirements) =>
          val recorded = mutable.Set.empty[PackageName]
          val needed = mutable.Set.empty[PackageName]
          imports.foreach(imp => needed += imp.pack.name)
          requirements.interfaces.foreach { req =>
            val pn = PackageName.parse(req.packageName).getOrElse(
              throw CliException.Basic(show"invalid interface requirement '${req.packageName}' in $file"))
            if (pn.isPredef || local(pn) || !recorded.add(pn))
              throw CliException.Basic(show"unexpected or repeated interface requirement $pn in $file")
            val (digest, exposed) = verified.getOrElseUpdate(pn, {
              val pack = packages.toMap.getOrElse(pn,
                throw CliException.Basic(show"missing implementation of $pn required by $file"))
              val iface = Package.interfaceOf(pack)
              (PackageSignatures.requirement(iface).get.interfaceBlake3, iface.exposedDepPackages)
            })
            if (digest != req.interfaceBlake3)
              throw CliException.Basic(show"stale interface for $pn in $file: expected ${req.interfaceBlake3}, found $digest")
            needed ++= exposed
          }
          val missing = needed.iterator.filterNot(n => n.isPredef || local(n) || recorded(n)).toList.sorted
          if (missing.nonEmpty)
            throw CliException.Basic(show"missing interface requirements in $file: ${missing.map(_.asString).mkString(", ")}")
      }
    }
  }
}
