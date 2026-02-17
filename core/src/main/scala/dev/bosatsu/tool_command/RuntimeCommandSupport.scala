package dev.bosatsu.tool_command

import cats.data.NonEmptyList
import cats.syntax.all._
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.tool.{
  CommandSupport,
  MainIdentifier,
  PackageResolver,
  PathGen
}
import dev.bosatsu.{
  CompileOptions,
  Package,
  PackageMap,
  PackageName,
  Par,
  PlatformIO
}
import dev.bosatsu.tool.CompilerApi

object RuntimeCommandSupport {
  def packMap[F[_], Path](
      platformIO: PlatformIO[F, Path],
      srcs: PathGen[F, Path],
      includes: PathGen[F, Path],
      packageResolver: PackageResolver[F, Path],
      publicDependencies: List[Path],
      privateDependencies: List[Path],
      commandName: String,
      mainIdentifiers: List[MainIdentifier[Path]],
      errColor: Colorize
  )(implicit
      ec: Par.EC
  ): F[(PackageMap.Typed[Any], List[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    for {
      inputs <- srcs.read
      includePaths <- includes.read
      includePackages <- platformIO.readPackages(includePaths)
      depLibraries <- CommandSupport.readDepLibraries(
        platformIO,
        publicDependencies,
        privateDependencies
      )
      depInterfaces = CommandSupport.dependencyInterfaces(depLibraries)
      depPackageList = CommandSupport.dependencyPackages(depLibraries)
      inputsWithMain = MainIdentifier.addAnyAbsent(mainIdentifiers, inputs)(
        using platformIO.pathOrdering
      )
      sourceWithNames <-
        if (
          includePackages.isEmpty && depPackageList.isEmpty && inputsWithMain.isEmpty
        )
          CommandSupport.noInputs(platformIO, commandName)
        else {
          NonEmptyList.fromList(inputsWithMain) match {
            case None =>
              moduleIOMonad.pure(
                (PackageMap.empty, List.empty[(Path, PackageName)])
              )
            case Some(srcNel) =>
              val baseIfaces =
                includePackages.map(Package.interfaceOf(_)) ::: depInterfaces
              CompilerApi
                .typeCheck(
                  platformIO,
                  srcNel,
                  baseIfaces,
                  errColor,
                  packageResolver,
                  CompileOptions.Default
                )
                .map { case (pm, names) =>
                  (PackageMap.toAnyTyped(pm), names.toList)
                }
          }
        }
      (srcPacks, names) = sourceWithNames
      allPacks =
        includePackages ::: depPackageList ::: srcPacks.toMap.values.toList
      _ <- CommandSupport.ensureDistinctPackages(
        platformIO,
        allPacks,
        "runtime inputs and dependencies"
      )
    } yield (PackageMap.fromIterable(allPacks), names)
  }
}
