package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, Package, PackageName, PlatformIO, Referant, rankn}
import dev.bosatsu.tool.{CommandSupport, CommonOpts, FileKind, GraphOutput, Output}
import scala.collection.immutable.SortedSet

object DepsCommand {
  private def sortNonPredefPackNames(
      iter: Iterator[PackageName]
  ): List[PackageName] =
    iter
      .filterNot(_ == PackageName.PredefName)
      .to(SortedSet)
      .toList

  private def interfaceDeps(iface: Package.Interface): List[PackageName] = {
    val pn = iface.name
    sortNonPredefPackNames(
      iface.exports.iterator
        .flatMap { n =>
          n.tag match {
            case Referant.Value(t) => Iterator.single(t)
            case _                 => Iterator.empty
          }
        }
        .flatMap(rankn.Type.constantsOf)
        .collect { case rankn.Type.Const.Defined(p, _) if p != pn => p }
    )
  }

  private def packageDeps(pack: Package.Typed[Any]): List[PackageName] =
    sortNonPredefPackNames(pack.imports.iterator.map(_.pack.name))

  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize

    val depsOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.interfacePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      commonOpts.outputPathOpt.orNone,
      Colorize.optsConsoleDefault,
      GraphOutput.jsonOrDot
    ).mapN {
      (
          srcs,
          ifaces,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          output,
          errColor,
          style
      ) =>
        for {
          srcPaths <- srcs.read
          sourceDeps <- (for {
            parsed <- packageResolver.parseHeaders(srcPaths)(platformIO)
            validated <- CommandSupport.liftParseErrors(
              platformIO,
              parsed,
              errColor
            )
          } yield validated.map { case (path, (pn, imps, _)) =>
            (path, pn, FileKind.Source, sortNonPredefPackNames(imps.iterator.map(_.pack)))
          })
          depLibraries <- CommandSupport.readDepLibraries(
            platformIO,
            publicDependencies,
            privateDependencies
          )
          depEntries = depLibraries._1 ::: depLibraries._2
          ifacePaths <- ifaces.read
          ifacesWithPath <- (for {
            interfaces <- platformIO.readInterfaces(ifacePaths)
          } yield ifacePaths.zip(interfaces))
          depIfaces = depEntries.flatMap {
            case (path, dep) => dep.interfaces.map(path -> _)
          }
          allIfaces = ifacesWithPath ::: depIfaces
          packPaths <- includes.read
          packsWithPath <- (for {
            packs <- platformIO.readPackages(packPaths)
          } yield packPaths.zip(packs))
          depPacks = depEntries.flatMap {
            case (path, dep) => dep.implementations.toMap.values.map(path -> _)
          }
          allPacks = packsWithPath ::: depPacks
          ifaceInfo = allIfaces.map { case (path, iface) =>
            (path, iface.name, FileKind.Iface, interfaceDeps(iface))
          }
          packInfo = allPacks.map { case (path, pack) =>
            (path, pack.name, FileKind.Pack, packageDeps(pack))
          }
        } yield (Output.DepsOutput(sourceDeps ::: ifaceInfo ::: packInfo, output, style): Output[Path])
    }

    Opts.subcommand("deps", "emit a graph description of dependencies")(
      depsOpt
    )
  }
}
