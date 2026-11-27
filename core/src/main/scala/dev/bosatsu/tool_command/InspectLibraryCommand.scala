package dev.bosatsu.tool_command

import cats.data.Validated
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.{Bytes, PlatformIO}
import dev.bosatsu.library.{
  DecodedLibraryWithDeps,
  Name,
  Version,
  VerifiedLibraryGraph,
  VerifiedLibrarySnapshot
}
import dev.bosatsu.tool.{CliException, LibraryInspection, Output}
import org.typelevel.paiges.Doc

/** Inspect one pinned library artifact and, in full mode, its declared graph. */
object InspectLibraryCommand {
  private def serializedDoc(doc: Doc): Bytes =
    Bytes.fromUTF8String(doc.render(100))

  private enum Mode[Path] {
    case DescriptorsOnly(outputPath: Path)
    case Full(
        contextPath: Path,
        signaturePath: Option[Path],
        exposuresPath: Option[Path],
        membershipPath: Option[Path],
        implementationPath: Option[Path],
        descriptorPath: Option[Path]
    )
  }

  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg}

    val input = Opts.option[Path]("input", "pinned Bosatsu library file")
    val expectedName =
      Opts.option[String]("expected-name", "expected library name (nonempty)")
        .mapValidated { name =>
          if (name.isEmpty) Validated.invalidNel("--expected-name must be nonempty")
          else Validated.valid(Name(name))
        }
    val expectedVersion =
      Opts.option[Version]("expected-version", "expected library version")
    val expectedHashes =
      Opts.options[String]("expected-hash", "expected ALGO:HEX digest (repeatable)")
    val descriptorsOnly =
      Opts.flag(
        "descriptors-only",
        "authenticate only the root before reporting its dependency descriptors"
      ).orFalse
    val dependencyContext =
      Opts.option[Path]("dependency-context", ImportedScopeInputs.dependencyContextHelp).orNone
    val signatureOut = Opts.option[Path]("sig-out", "canonical exported interface bundle").orNone
    val exposuresOut =
      Opts.option[Path]("exposures-out", "typed package exposure and provider ownership JSON").orNone
    val membershipOut = Opts.option[Path]("membership-out", "package membership JSON").orNone
    val implementationOut =
      Opts.option[Path]("implementation-out", "scoped authenticated artifact and edge JSON").orNone
    val descriptorOut = Opts.option[Path]("descriptor-out", "authenticated root descriptor JSON").orNone

    val mode: Opts[Mode[Path]] = (
      descriptorsOnly,
      dependencyContext,
      signatureOut,
      exposuresOut,
      membershipOut,
      implementationOut,
      descriptorOut
    ).tupled.mapValidated {
      case (onlyDescriptors, context, signature, exposures, membership, implementation, descriptor) =>
        val hasFullOutput =
          signature.nonEmpty || exposures.nonEmpty || membership.nonEmpty || implementation.nonEmpty
        (onlyDescriptors, context, descriptor) match {
          case (true, Some(_), _) =>
            Validated.invalidNel("--descriptors-only does not accept --dependency-context")
          case (true, _, _) if hasFullOutput =>
            Validated.invalidNel("--descriptors-only accepts only --descriptor-out")
          case (true, None, Some(outputPath)) =>
            Validated.valid(Mode.DescriptorsOnly(outputPath))
          case (true, None, None) =>
            Validated.invalidNel("--descriptors-only requires --descriptor-out")
          case (false, None, _) =>
            Validated.invalidNel("full inspection requires --dependency-context")
          case (false, Some(_), None) if !hasFullOutput =>
            Validated.invalidNel("full inspection requires at least one output")
          case (false, Some(contextPath), _) =>
            Validated.valid(Mode.Full(
              contextPath, signature, exposures, membership, implementation, descriptor
            ))
        }
    }

    Opts.subcommand(
      "inspect-library",
      "authenticate and inspect a pinned Bosatsu library file"
    ) {
      (input, expectedName, expectedVersion, expectedHashes, mode).mapN {
        (inputPath, name, version, hashes, inspectionMode) =>
          platform.withEC {
            given CanPromise[F] = platform.canPromiseF

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

            for {
              snapshot <- VerifiedLibrarySnapshot.verify(
                platform, inputPath, name, version, hashes.toList
              )
              output <- inspectionMode match {
                case Mode.DescriptorsOnly(descriptorFile) =>
                  for {
                    prepared <- descriptorOutput(snapshot, descriptorFile)
                    _ <- OutputPathChecks.checkOutputFiles(
                      platform, List(inputPath), List(descriptorFile)
                    )
                  } yield prepared
                case Mode.Full(
                    contextFile, signaturePath, exposuresPath, membershipPath,
                    implementationPath, descriptorPath
                ) =>
                  val allOutputPaths =
                    signaturePath.toList ::: exposuresPath.toList ::: membershipPath.toList :::
                      implementationPath.toList ::: descriptorPath.toList
                  for {
                    context <- LibraryInspection.readDependencyContext(platform, contextFile)
                    graph <- VerifiedLibraryGraph.verify(platform, snapshot, context)
                    decoded <- DecodedLibraryWithDeps.decodeVerifiedGraphWithLibraries(graph)
                    (rootDecoded, libraries) = decoded
                    signatureBytes <- signaturePath.traverse { _ =>
                      moduleIOMonad.fromTry(
                        LibraryInspection.signatureProjection(rootDecoded.lib)
                      ).map(Bytes.fromMessage(_))
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
                    _ <- OutputPathChecks.checkOutputFiles(
                      platform,
                      inputPath :: contextFile :: context.libraries.map(_.path),
                      allOutputPaths
                    )
                  } yield {
                    val outputs = List(
                      signaturePath.zip(signatureBytes),
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
              }
            } yield output
          }
      }
    }
  }
}
