package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import dev.bosatsu.PlatformIO
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.tool.CliException
import scala.util.Try

/** A library artifact whose exact bytes have been checked against external
  * pins.
  */
private[bosatsu] final class VerifiedLibrarySnapshot[Path] private (
    val path: Path,
    val name: Name,
    val version: Version,
    val blake3: HashValue[Algo.Blake3],
    val sha256: HashValue[Algo.Sha256],
    val protoLib: proto.Library,
    private val bytes: Array[Byte]
) {
  /** Compare aliases by their bytes, not only by their digest identifiers. */
  private[bosatsu] def hasSameBytes(other: VerifiedLibrarySnapshot[?]): Boolean =
    java.util.Arrays.equals(bytes, other.bytes)
}

private[bosatsu] object VerifiedLibrarySnapshot {
  private def supportedPin(
      raw: String
  ): Either[String, Option[(String, String)]] = {
    val algorithm =
      if (raw.startsWith("blake3:") || raw == "blake3") then Some("blake3")
      else if (raw.startsWith("sha256:") || raw == "sha256") then Some("sha256")
      else None

    algorithm match {
      case None => Right(None)
      case Some(name) =>
        Algo.parseIdent.parseAll(raw) match {
          case Right(parsed) => Right(Some(name -> parsed.value.hex))
          case Left(_)       => Left(s"malformed $name hash '$raw'")
        }
    }
  }

  private def checkPins(
      path: String,
      expectedHashes: List[String],
      blake3: HashValue[Algo.Blake3],
      sha256: HashValue[Algo.Sha256]
  ): Either[String, Unit] = {
    val parsed = expectedHashes.traverse(supportedPin)
      .leftMap(error => s"$error for $path")
    parsed.flatMap { all =>
      val supported = all.flatten
      if (expectedHashes.isEmpty) Left(s"no expected hashes were supplied for $path")
      else if (supported.isEmpty)
        Left(s"no supported expected hashes were supplied for $path")
      else {
        supported.traverse_ { case (name, expected) =>
          val actual = if (name == "blake3") blake3.hex else sha256.hex
          Either.cond(
            expected == actual,
            (),
            s"$name pin mismatch for $path: expected $expected, actual $actual"
          )
        }
      }
    }
  }

  /** Read once, authenticate and parse the same private byte snapshot. */
  def verify[F[_], Path](
      platform: PlatformIO[F, Path],
      path: Path,
      expectedName: Name,
      expectedVersion: Version,
      expectedHashes: List[String]
  ): F[VerifiedLibrarySnapshot[Path]] = {
    import platform.{moduleIOMonad, showPath}

    platform.readBytes(path)
      .adaptError { case error =>
        CliException.Basic(show"cannot read pinned library $path: ${error.getMessage}")
      }
      .flatMap { readBytes =>
        val snapshotBytes = readBytes.clone()
        val blake3 = Algo.hashBytes[Algo.Blake3](snapshotBytes)
        val sha256 = Algo.hashBytes[Algo.Sha256](snapshotBytes)
        val pathString = showPath.show(path)
        val result = for {
          _ <- checkPins(pathString, expectedHashes, blake3, sha256)
          library <- Try(proto.Library.parseFrom(snapshotBytes)).toEither.leftMap {
            error => s"cannot parse pinned library $pathString: ${error.getMessage}"
          }
          _ <- Either.cond(
            library.name == expectedName.name,
            (),
            s"pinned library $pathString has name '${library.name}', expected '${expectedName.name}'"
          )
          actualVersion <- Library.getVersion(library).toRight(
            s"pinned library $pathString has no version, expected ${expectedVersion.render}"
          )
          _ <- Either.cond(
            Version.versionOrder.eqv(actualVersion, expectedVersion),
            (),
            s"pinned library $pathString has version ${actualVersion.render}, expected ${expectedVersion.render}"
          )
        } yield library

        moduleIOMonad.fromEither(result.leftMap(CliException.Basic(_))).map {
          library =>
            new VerifiedLibrarySnapshot(
              path,
              expectedName,
              expectedVersion,
              blake3,
              sha256,
              library,
              snapshotBytes
            )
        }
      }
  }
}
