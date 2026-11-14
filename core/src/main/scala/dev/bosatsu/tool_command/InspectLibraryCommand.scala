package dev.bosatsu.tool_command

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
    case Descriptors(output: Path)
    case Full(
        context: Path,
        signature: Option[Path],
        exposures: Option[Path],
        membership: Option[Path],
        implementation: Option[Path],
        descriptor: Option[Path]
    )
  }

  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg}

    val input = Opts.option[Path]("input", "pinned Bosatsu library file")
    val expectedName = Opts.option[String]("expected-name", "expected library name (non-empty)")
      .mapValidated { name =>
        if (name.isEmpty) "--expected-name must be nonempty".invalidNel
        else Name(name).validNel
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
      Opts.option[Path]("dependency-context", "version-1 declared dependency-file context").orNone
    val signatureOut = Opts.option[Path]("sig-out", "canonical exported interface bundle").orNone
    val exposuresOut =
      Opts.option[Path]("exposures-out", "typed package exposure and provider ownership JSON").orNone
    val membershipOut = Opts.option[Path]("membership-out", "package membership JSON").orNone
    val implementationOut =
      Opts.option[Path]("implementation-out", "scoped authenticated artifact and edge JSON").orNone
    val descriptorOut = Opts.option[Path]("descriptor-out", "authenticated root descriptor JSON").orNone

    val mode = (
      descriptorsOnly,
      dependencyContext,
      signatureOut,
      exposuresOut,
      membershipOut,
      implementationOut,
      descriptorOut
    ).tupled.mapValidated {
      case (onlyDescriptors, context, signature, exposures, membership, implementation, descriptor) =>
        val fullOutputs = List(signature, exposures, membership, implementation).flatten
        if (onlyDescriptors) {
          if (context.nonEmpty)
            "--descriptors-only does not accept --dependency-context".invalidNel
          else if (fullOutputs.nonEmpty)
            "--descriptors-only accepts only --descriptor-out".invalidNel
          else descriptor match {
            case Some(output) => Mode.Descriptors(output).validNel
            case None => "--descriptors-only requires --descriptor-out".invalidNel
          }
        } else context match {
          case None => "full inspection requires --dependency-context".invalidNel
          case Some(contextFile) =>
            if (fullOutputs.isEmpty && descriptor.isEmpty)
              "full inspection requires at least one output".invalidNel
            else Mode.Full(contextFile, signature, exposures, membership, implementation, descriptor).validNel
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
                platform,
                inputPath,
                name,
                version,
                hashes.toList
              )
              output <- inspectionMode match {
                case Mode.Descriptors(descriptorFile) =>
                  for {
                    prepared <- descriptorOutput(snapshot, descriptorFile)
                    _ <- OutputPathChecks.checkOutputFiles(platform, List(inputPath), List(descriptorFile))
                  } yield prepared
                case Mode.Full(contextFile, signaturePath, exposuresPath, membershipPath, implementationPath, descriptorPath) =>
                  for {
                    context <- LibraryInspection.readDependencyContext(platform, contextFile)
                    graph <- VerifiedLibraryGraph.verify(platform, snapshot, context)
                    decoded <- DecodedLibraryWithDeps.decodeVerifiedGraphWithLibraries(graph)
                    (rootDecoded, libraries) = decoded
                    signatureBytes <- signaturePath.traverse { _ =>
                      moduleIOMonad.fromTry(
                        LibraryInspection.signatureProjection(rootDecoded.lib)
                      ).map(Bytes.fromMessage)
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
                    outputs = List(
                      signaturePath.zip(signatureBytes),
                      exposuresPath.zip(exposures).map { case (path, doc) => path -> serializedDoc(doc) },
                      membershipPath.zip(membership).map { case (path, doc) => path -> serializedDoc(doc) },
                      implementationPath.zip(implementation).map { case (path, doc) => path -> serializedDoc(doc) },
                      descriptorPath.zip(descriptor).map { case (path, doc) => path -> serializedDoc(doc) }
                    ).flatten
                    _ <- OutputPathChecks.checkOutputFiles(
                      platform,
                      inputPath :: contextFile :: context.libraries.map(_.path),
                      outputs.map(_._1)
                    )
                  } yield Output.AtomicFileSet(outputs)
              }
            } yield output
          }
      }
    }
  }
}
