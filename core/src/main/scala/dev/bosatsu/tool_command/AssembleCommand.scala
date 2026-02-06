package dev.bosatsu.tool_command

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{MainModule, PackageName}
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{DecodedLibrary, LibConfig, Name, Version}
import dev.bosatsu.library.LibConfig.{LibMethods, PackageFilter}
import dev.bosatsu.tool.{CliException, Output}
import org.typelevel.paiges.Doc

object AssembleCommand {

  private def libraryFileName(name: Name, version: Version): String =
    s"${name.name}-v${version.render}.bosatsu_lib"

  private def depVersion(
      dep: proto.LibDependency
  ): Option[Version] =
    dep.desc.flatMap(_.version).map(Version.fromProto(_))

  private def depVersionOrError[IO[_], Path](
      module: MainModule[IO, Path],
      dep: proto.LibDependency,
      note: String
  ): IO[Version] = {
    import module.platformIO.moduleIOMonad

    depVersion(dep) match {
      case Some(v) => moduleIOMonad.pure(v)
      case None    =>
        moduleIOMonad.raiseError(
          CliException(
            "missing dependency version",
            Doc.text(
              s"dependency ${dep.name} is missing a version while resolving $note."
            )
          )
        )
    }
  }

  def opts[IO[_], Path](
      module: MainModule[IO, Path]
  ): Opts[module.MainCommand] = {
    import module.MainCommand.FromOutput
    import module.platformIO._

    val nameOpt =
      Opts.option[Name]("name", help = "library name")
    val repoUriOpt =
      Opts
        .option[String]("repo_uri", help = "repository URI for this library")
        .withDefault("")
    val versionOpt =
      Opts.option[Version]("version", help = "library version")
    val vcsIdentOpt =
      Opts
        .option[String](
          "vcs_ident",
          help = "version-control identifier embedded in the library"
        )
        .withDefault("")
    val packagePathsOpt =
      Opts
        .options[Path](
          "package",
          help = "compiled package (.bosatsu_package)",
          short = "p"
        )
        .orEmpty
    val exportPackOpt =
      Opts
        .options[PackageName](
          "export_pack",
          help = "package exported from this library"
        )
        .orEmpty
    val privatePackOpt =
      Opts
        .options[PackageName](
          "private_pack",
          help = "package kept internal to this library"
        )
        .orEmpty
    val pubDepsOpt =
      Opts
        .options[Path](
          "pub_dep",
          help = "public dependency library (.bosatsu_lib or .bosatsu_ifacelib)"
        )
        .orEmpty
    val privDepsOpt =
      Opts
        .options[Path](
          "priv_dep",
          help =
            "private dependency library (.bosatsu_lib or .bosatsu_ifacelib)"
        )
        .orEmpty
    val prevLibOpt =
      Opts
        .option[Path](
          "previous_lib",
          help = "previous version of this library (.bosatsu_lib)"
        )
        .orNone
    val defaultMainOpt =
      Opts.option[PackageName](
        "default_main",
        help = "default main package for this library"
      ).orNone
    val outputOpt =
      Opts
        .option[Path](
          "output",
          help =
            "output path for .bosatsu_lib (default: {name}-v{version}.bosatsu_lib)",
          short = "o"
        )
        .orNone

    val args = (
      nameOpt,
      repoUriOpt,
      versionOpt,
      vcsIdentOpt,
      packagePathsOpt,
      exportPackOpt,
      privatePackOpt,
      pubDepsOpt,
      privDepsOpt,
      prevLibOpt,
      defaultMainOpt,
      outputOpt
    )

    Opts.subcommand(
      "assemble",
      "assemble a .bosatsu_lib directly from package files and explicit dependency libraries"
    ) {
      args.mapN {
        (
            name,
            repoUri,
            version,
            vcsIdent,
            packagePaths,
            exportPacks,
            privatePacks,
            pubDepPaths,
            privDepPaths,
            prevLibPath,
            defaultMain,
            outputPath
        ) =>
          val run =
            for {
              packs <- readPackages(packagePaths)
              pubDeps <- pubDepPaths.traverse(readLibrary(_).flatMap(
                DecodedLibrary.decode(_)
              ))
              privDeps <- privDepPaths.traverse(readLibrary(_).flatMap(
                DecodedLibrary.decode(_)
              ))
              prevLib <- prevLibPath.traverse(readLibrary(_).flatMap(
                DecodedLibrary.decode(_)
              ))
              out <- outputPath match {
                case Some(path) => moduleIOMonad.pure(path)
                case None       =>
                  pathF(libraryFileName(name, version))
              }
              allPackNames = packs.map(_.name).distinct.sorted
              (exportedNames, allNames) =
                if (exportPacks.nonEmpty || privatePacks.nonEmpty)
                  (
                    exportPacks.distinct.sorted,
                    (exportPacks ::: privatePacks).distinct.sorted
                  )
                else
                  (allPackNames, allPackNames)
              depMap = (pubDeps ::: privDeps).iterator
                .map(dep => ((dep.name, dep.version), dep))
                .toMap
              prevDesc = prevLib.map { prev =>
                proto.LibDescriptor(
                  version = Some(prev.version.toProto),
                  hashes = prev.hashValue.toIdent :: Nil
                )
              }
              conf = LibConfig(
                name = name,
                repoUri = repoUri,
                nextVersion = version,
                previous = prevDesc,
                exportedPackages = exportedNames.map(PackageFilter.Name(_)),
                allPackages = allNames.map(PackageFilter.Name(_)),
                publicDeps = pubDeps.map(_.toDep),
                privateDeps = privDeps.map(_.toDep),
                defaultMain = defaultMain
              )
              publicDepClosureLibs <- moduleIOMonad.fromEither(
                DecodedLibrary
                  .publicDepClosure(pubDeps ::: privDeps, depMap)
                  .leftMap { errs =>
                    CliException(
                      "invalid dependency closure",
                      DecodedLibrary.DepClosureError.toDoc(errs)
                    )
                  }
                  .toEither
              )
              prevPublicDepLibs <- prevLib match {
                case None       => moduleIOMonad.pure(Nil)
                case Some(prev) =>
                  prev.protoLib.publicDependencies.toList.traverse { dep =>
                    depVersionOrError(module, dep, "previous public deps")
                      .flatMap { v =>
                        depMap.get((Name(dep.name), v)) match {
                          case Some(lib) => moduleIOMonad.pure(lib)
                          case None      =>
                            moduleIOMonad.raiseError[DecodedLibrary[
                              Algo.Blake3
                            ]](
                              CliException(
                                "missing previous public dependency",
                                Doc.text(
                                  s"missing previous public dependency ${dep.name} $v; pass it with --pub_dep or --priv_dep."
                                )
                              )
                            )
                        }
                      }
                  }
              }
              lib <- moduleIOMonad.fromTry(
                LibConfig.Error.toTry(
                  conf.assemble(
                    vcsIdent = vcsIdent,
                    previous = prevLib,
                    packs = packs,
                    deps = pubDeps ::: privDeps,
                    publicDepClosureLibs = publicDepClosureLibs,
                    prevPublicDepLibs = prevPublicDepLibs
                  )
                )
              )
            } yield (Output.Library(lib, out): Output[Path])

          FromOutput("tool assemble", run)
      }
    }
  }
}
