package dev.bosatsu.tool_command

import cats.Hash
import cats.collections.HashMap
import cats.syntax.all._
import dev.bosatsu.PlatformIO
import dev.bosatsu.tool.CliException
import java.util.Locale
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedSet

/** Shared checks for a command's requested input and output paths. */
private[bosatsu] object OutputPathChecks {
  def fileName[F[_], P](platform: PlatformIO[F, P], path: P): String =
    platform.pathToString(
      platform.parent(path).flatMap(platform.relativize(_, path)).getOrElse(path)
    )

  private def checkPendingNames[F[_], P](
      platform: PlatformIO[F, P],
      paths: List[P]
  ): F[Unit] = {
    import platform.moduleIOMonad
    given Hash[platform.FileKey] = platform.fileKeyHash
    type Location = (platform.FileKey, P, P)

    // Probe names that could alias through filesystem case folding or Unicode
    // normalization. Ordinary output names need no scratch I/O.
    val names = paths.map(path => fileName(platform, path) -> path)
    val candidates =
      if (names.exists(_._1.exists(_ > 127))) paths
      else names.groupMap(_._1.toUpperCase(Locale.ROOT))(_._2)
        .valuesIterator.filter(_.size > 1).flatten.toList

    def locate(path: P): F[Location] =
      moduleIOMonad.tailRecM(path) { current =>
        platform.parent(current) match {
          case None =>
            moduleIOMonad.raiseError(
              CliException.Basic(
                s"no output directory for ${platform.pathToString(path)}"
              )
            )
          case Some(parent) =>
            platform.existingFileKey(parent).map {
              case Some(key) =>
                Right((key, parent, platform.relativize(parent, path).get))
              case None => Left(parent)
            }
        }
      }

    if (candidates.lengthCompare(2) < 0) moduleIOMonad.unit
    else
      candidates.traverse(locate).flatMap { located =>
        val grouped = located.foldLeft(HashMap.empty[platform.FileKey, List[(P, P)]]) {
          case (groups, (key, parent, suffix)) =>
            groups.updated(key, (parent -> suffix) :: groups.getOrElse(key, Nil))
        }
        grouped.valuesIterator.filter(_.size > 1).toList.traverse_ { group =>
          // Mirror absent suffixes beneath the existing directory so the
          // destination filesystem applies its own naming rules.
          platform.withTempDirectoryIn(group.head._1) { temp =>
            group.traverse_ { case (_, suffix) =>
              val marker = platform.resolve(temp, suffix)
              platform.existingFileKey(marker).flatMap {
                case Some(_) =>
                  moduleIOMonad.raiseError[Unit](
                    CliException.Basic("multiple outputs refer to the same file")
                  )
                case None => platform.writeDoc(marker, Doc.empty)
              }
            }
          }
        }
      }
  }

  def checkOutputFiles[F[_], P](
      platform: PlatformIO[F, P],
      inputs: List[P],
      outputs: List[P]
  ): F[Unit] = {
    import platform.{moduleIOMonad, pathOrdering, showPath}
    given Hash[platform.FileKey] = platform.fileKeyHash
    type Key = Either[P, platform.FileKey]

    outputs.traverse { path =>
      platform.pendingOutputPath(path).flatMap { target =>
        platform.existingFileKey(target).map(key => (key.toRight(target): Key) -> path)
      }
    }.flatMap { files =>
      val existing = files.collect { case (Right(key), path) => key -> path }
      val byKey = HashMap.fromSeq(existing)
      val pending = files.collect { case (Left(path), _) => path }
      if (
        byKey.size != existing.size ||
          SortedSet.from(pending)(using pathOrdering).size != pending.size
      )
        moduleIOMonad.raiseError(
          CliException.Basic("multiple outputs refer to the same file")
        )
      else
        checkPendingNames(platform, pending) *> moduleIOMonad.whenA(byKey.nonEmpty) {
          inputs.traverse_ { input =>
            platform.existingFileKey(input).flatMap { inputKey =>
              inputKey.flatMap(byKey.get).traverse_ { output =>
                moduleIOMonad.raiseError[Unit](
                  CliException.Basic(show"output $output refers to runtime input $input")
                )
              }
            }
          }
        }
    }
  }
}
