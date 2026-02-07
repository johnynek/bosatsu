package dev.bosatsu.tool_command

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{PackageName, PlatformIO}
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{DecodedLibrary, LibConfig, Library, Name, Version}
import dev.bosatsu.library.LibConfig.PackageFilter
import dev.bosatsu.tool.{CliException, Output}
import org.typelevel.paiges.Doc

object AssembleCommand {
  private def requireFullDependencyLibraries[F[_], Path](
      platformIO: PlatformIO[F, Path],
      visibility: String,
      deps: List[(Path, DecodedLibrary[Algo.Blake3])]
  ): F[Unit] = {
    import platformIO.{moduleIOMonad, showPath}

    val invalid =
      deps.flatMap { case (path, dep) =>
        if (dep.isFullLibrary) Nil
        else (path, dep) :: Nil
      }

    if (invalid.isEmpty) moduleIOMonad.unit
    else {
      val details = Doc.intercalate(
        Doc.hardLine,
        invalid.map { case (path, dep) =>
          val missing = dep.missingInterfaceImplementations.map(_.asString)
          val missingDoc =
            if (missing.isEmpty) Doc.text("(unknown)")
            else Doc.text(missing.mkString(", "))
          Doc.text(
            show"${showPath.show(path)} (${dep.name.name} ${dep.version.render})"
          ) +
            (Doc.line + Doc.text("missing internal packages for exported interfaces:") +
              (Doc.line + missingDoc).nested(2)).nested(2)
        }
      )

      moduleIOMonad.raiseError(
        CliException(
          show"invalid $visibility dependency libraries",
          Doc.text(
            show"$visibility dependencies must be full .bosatsu_lib files; .bosatsu_ifacelib is not supported for assemble."
          ) + Doc.line + details
        )
      )
    }
  }

  private def depVersionOrError[F[_], Path](
      platformIO: PlatformIO[F, Path],
      dep: proto.LibDependency,
      note: String
  ): F[Version] = {
    import platformIO.moduleIOMonad

    Library.getVersion(dep) match {
      case Some(v) => moduleIOMonad.pure(v)
      case None    =>
        moduleIOMonad.raiseError(
          CliException(
            "missing dependency version",
            Doc.text(
              show"dependency ${dep.name} is missing a version while resolving $note."
            )
          )
        )
    }
  }

  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO._

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
          help = "public dependency library (.bosatsu_lib)"
        )
        .orEmpty
    val privDepsOpt =
      Opts
        .options[Path](
          "priv_dep",
          help = "private dependency library (.bosatsu_lib)"
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
          for {
            packs <- readPackages(packagePaths)
            pubDepsWithPath <- pubDepPaths.traverse { path =>
              for {
                lib <- readLibrary(path)
                decLib <- DecodedLibrary.decode(lib)
              } yield (path, decLib)
            }
            _ <- requireFullDependencyLibraries(
              platformIO,
              "public",
              pubDepsWithPath
            )
            privDepsWithPath <- privDepPaths.traverse { path =>
              for {
                lib <- readLibrary(path)
                decLib <- DecodedLibrary.decode(lib)
              } yield (path, decLib)
            }
            _ <- requireFullDependencyLibraries(
              platformIO,
              "private",
              privDepsWithPath
            )
            prevLib <- prevLibPath.traverse { path =>
              for {
                lib <- readLibrary(path)
                decLib <- DecodedLibrary.decode(lib)
              } yield decLib
            }
            out <- outputPath match {
              case Some(path) => moduleIOMonad.pure(path)
              case None       =>
                pathF(Library.defaultFileName(name, version))
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
            pubDeps = pubDepsWithPath.map(_._2)
            privDeps = privDepsWithPath.map(_._2)
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
                  depVersionOrError(platformIO, dep, "previous public deps")
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
                                show"missing previous public dependency ${dep.name} $v; pass it with --pub_dep or --priv_dep."
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
      }
    }
  }
}
