package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.PlatformIO
import dev.bosatsu.library.{
  DecodedLibraryWithDeps,
  Name,
  Version,
  VerifiedLibraryGraph,
  VerifiedLibrarySnapshot
}
import dev.bosatsu.tool.{CliException, LibraryInspection, Output}
import org.typelevel.paiges.Doc
import java.nio.charset.StandardCharsets

/** Inspect one pinned library artifact and, in full mode, its declared graph. */
object InspectLibraryCommand {
  private def serializedDoc(doc: Doc): Vector[Byte] =
    doc.render(100).getBytes(StandardCharsets.UTF_8).toVector

  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg}

    val input = Opts.option[Path]("input", "pinned Bosatsu library file")
    val expectedName = Opts.option[String]("expected-name", "expected library name")
    val expectedVersion =
      Opts.option[Version]("expected-version", "expected library version")
    val expectedHashes =
      Opts.options[String]("expected-hash", "expected ALGO:HEX digest (repeatable)").orEmpty
    val descriptorsOnly =
      Opts.flag(
        "descriptors-only",
        "authenticate only the root before reporting its dependency descriptors"
      ).orFalse
    val dependencyContext =
      Opts.option[Path]("dependency-context", "version-1 declared dependency-file context").orNone
    val signatureOut = Opts.option[Path]("sig-out", "canonical exported interface bundle").orNone
    val exposuresOut =
      Opts.option[Path]("exposures-out", "typed package exposure and provider ownership JSON").orNone
    val membershipOut = Opts.option[Path]("membership-out", "package membership JSON").orNone
    val implementationOut =
      Opts.option[Path]("implementation-out", "scoped authenticated artifact and edge JSON").orNone
    val descriptorOut = Opts.option[Path]("descriptor-out", "authenticated root descriptor JSON").orNone

    Opts.subcommand(
      "inspect-library",
      "authenticate and inspect a pinned Bosatsu library file"
    ) {
      (
        input,
        expectedName,
        expectedVersion,
        expectedHashes,
        descriptorsOnly,
        dependencyContext,
        signatureOut,
        exposuresOut,
        membershipOut,
        implementationOut,
        descriptorOut
      ).mapN {
        (
            inputPath,
            nameText,
            version,
            hashes,
            onlyDescriptors,
            contextPath,
            signaturePath,
            exposuresPath,
            membershipPath,
            implementationPath,
            descriptorPath
        ) =>
          platform.withEC {
            given CanPromise[F] = platform.canPromiseF

            val fullOutputPaths =
              signaturePath.toList ::: exposuresPath.toList ::: membershipPath.toList :::
                implementationPath.toList
            val allOutputPaths = fullOutputPaths ::: descriptorPath.toList
            val modeError =
              if (nameText.isEmpty) Some("--expected-name must be nonempty")
              else if (hashes.isEmpty) Some("at least one --expected-hash is required")
              else if (onlyDescriptors && contextPath.nonEmpty)
                Some("--descriptors-only does not accept --dependency-context")
              else if (onlyDescriptors && fullOutputPaths.nonEmpty)
                Some("--descriptors-only accepts only --descriptor-out")
              else if (onlyDescriptors && descriptorPath.isEmpty)
                Some("--descriptors-only requires --descriptor-out")
              else if (!onlyDescriptors && contextPath.isEmpty)
                Some("full inspection requires --dependency-context")
              else if (!onlyDescriptors && allOutputPaths.isEmpty)
                Some("full inspection requires at least one output")
              else None

            def validateOutputPaths(inputs: List[Path]): F[Unit] =
              OutputPathChecks.checkOutputFiles(platform, inputs, allOutputPaths)

            def descriptorOutput(
                snapshot: VerifiedLibrarySnapshot[Path],
                outputPath: Path
            ): F[Output[Path]] =
              moduleIOMonad.fromEither(
                  LibraryInspection.descriptorProjection(snapshot)
                    .leftMap(CliException.Basic(_))
              ).map { json =>
                Output.AtomicFileSet(List(outputPath -> serializedDoc(json.toDoc)))
              }

            def fullInspection(
                snapshot: VerifiedLibrarySnapshot[Path]
            ): F[Output[Path]] =
              for {
                contextFile <- moduleIOMonad.fromOption(
                  contextPath,
                  CliException.Basic("full inspection requires --dependency-context")
                )
                context <- LibraryInspection.readDependencyContext(platform, contextFile)
                graph <- VerifiedLibraryGraph.verify(platform, snapshot, context)
                decoded <- DecodedLibraryWithDeps.decodeVerifiedGraphWithLibraries(graph)
                (rootDecoded, libraries) = decoded
                signatureBytes <- signaturePath.traverse { _ =>
                  moduleIOMonad.fromTry(
                    LibraryInspection.signatureProjection(rootDecoded.lib)
                  ).map(_.toByteArray)
                }
                exposures <- exposuresPath.traverse { _ =>
                  moduleIOMonad.fromEither(
                    LibraryInspection.exposureProjection(
                      graph,
                      libraries,
                      platform.pathToString
                    ).leftMap(CliException.Basic(_))
                  ).map(_.toDoc)
                }
                membership <- membershipPath.traverse { _ =>
                  moduleIOMonad.fromEither(
                    LibraryInspection.membershipProjection(graph, libraries)
                      .leftMap(CliException.Basic(_))
                  ).map(_.toDoc)
                }
                implementation <- implementationPath.traverse { _ =>
                  moduleIOMonad.pure(
                    LibraryInspection.implementationProjection(graph, platform.pathToString).toDoc
                  )
                }
                descriptor <- descriptorPath.traverse { _ =>
                  moduleIOMonad.fromEither(
                    LibraryInspection.descriptorProjection(snapshot)
                      .leftMap(CliException.Basic(_))
                  ).map(_.toDoc)
                }
                _ <- validateOutputPaths(inputPath :: contextFile :: context.libraries.map(_.path))
              } yield {
                val outputs = List(
                  signaturePath.zip(signatureBytes).map { case (path, bytes) =>
                    path -> bytes.toVector
                  },
                  exposuresPath.zip(exposures).map { case (path, doc) =>
                    path -> serializedDoc(doc)
                  },
                  membershipPath.zip(membership).map { case (path, doc) =>
                    path -> serializedDoc(doc)
                  },
                  implementationPath.zip(implementation).map { case (path, doc) =>
                    path -> serializedDoc(doc)
                  },
                  descriptorPath.zip(descriptor).map { case (path, doc) =>
                    path -> serializedDoc(doc)
                  }
                ).flatten
                Output.AtomicFileSet(outputs)
              }

            for {
              _ <- moduleIOMonad.fromEither(
                modeError.toLeft(()).leftMap(CliException.Basic(_))
              )
              snapshot <- VerifiedLibrarySnapshot.verify(
                platform,
                inputPath,
                Name(nameText),
                version,
                hashes
              )
              output <- if (onlyDescriptors)
                for {
                  descriptorFile <- moduleIOMonad.fromOption(
                    descriptorPath,
                    CliException.Basic("--descriptors-only requires --descriptor-out")
                  )
                  prepared <- descriptorOutput(snapshot, descriptorFile)
                  _ <- validateOutputPaths(List(inputPath))
                } yield prepared
              else fullInspection(snapshot)
            } yield output
          }
      }
    }
  }
}
