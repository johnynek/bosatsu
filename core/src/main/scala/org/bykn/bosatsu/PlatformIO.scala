package org.bykn.bosatsu

import cats.MonadError
import com.monovore.decline.Argument
import org.typelevel.paiges.Doc

trait PlatformIO[F[_], Path] {
  implicit def moduleIOMonad: MonadError[F, Throwable]
  implicit def pathArg: Argument[Path]
  implicit def pathOrdering: Ordering[Path]

  def readPath(p: Path): F[String]
  def readPackages(paths: List[Path]): F[List[Package.Typed[Unit]]]

  def readInterfaces(paths: List[Path]): F[List[Package.Interface]]

  /** given an ordered list of prefered roots, if a packFile starts with one of
    * these roots, return a PackageName based on the rest
    */
  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName]

  /** Modules optionally have the capability to combine paths into a tree
    */
  def resolvePath: Option[(Path, PackageName) => F[Option[Path]]]

  /** some modules have paths that form directory trees
    *
    * if the given path is a directory, return Some and all the first children.
    */
  def unfoldDir: Option[Path => F[Option[F[List[Path]]]]]

  def hasExtension(str: String): Path => Boolean

  def writeDoc(p: Path, d: Doc): F[Unit]
  def writeStdout(doc: Doc): F[Unit]

  final def writeOut(doc: Doc, out: Option[Path]): F[Unit] =
    out match {
      case None => writeStdout(doc)
      case Some(p) => writeDoc(p, doc)
    }


  def resolve(base: Path, p: List[String]): Path

  // this is println actually
  def print(str: String): F[Unit]

  def writeInterfaces(
      interfaces: List[Package.Interface],
      path: Path
  ): F[Unit]

  def writePackages[A](packages: List[Package.Typed[A]], path: Path): F[Unit]
}
