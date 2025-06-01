package org.bykn.bosatsu.tool

import cats.{Monoid, Monad}
import cats.syntax.all._
import com.monovore.decline.Opts
import org.bykn.bosatsu.PlatformIO

sealed abstract class PathGen[IO[_], Path] {
  def read(implicit m: Monad[IO]): IO[List[Path]]
}

object PathGen {
  final case class Direct[IO[_], Path](path: Path) extends PathGen[IO, Path] {
    def read(implicit m: Monad[IO]): IO[List[Path]] =
      m.pure(path :: Nil)
  }
  final case class ChildrenOfDir[IO[_], Path](
      dir: Path,
      select: Path => Boolean,
      recurse: Boolean,
      unfold: Path => IO[Option[IO[List[Path]]]]
  ) extends PathGen[IO, Path] {
    def read(implicit m: Monad[IO]): IO[List[Path]] = {

      val pureEmpty: IO[List[Path]] = m.pure(Nil)

      lazy val rec: List[Path] => IO[List[Path]] =
        if (recurse) { (children: List[Path]) =>
          children.traverse(step).map(_.flatten)
        } else { (_: List[Path]) => pureEmpty }

      def step(path: Path): IO[List[Path]] =
        unfold(path).flatMap {
          case None          => pureEmpty
          case Some(listDir) =>
            for {
              children <- listDir
              childrenKeep = children.filter(select)
              rest <- rec(children)
            } yield childrenKeep ::: rest
        }

      step(dir)
    }
  }
  final case class Combine[IO[_], Path](gens: List[PathGen[IO, Path]])
      extends PathGen[IO, Path] {
    def read(implicit m: Monad[IO]): IO[List[Path]] =
      gens.traverse(_.read).map(_.flatten)
  }

  implicit def pathGenMonoid[IO[_], Path]: Monoid[PathGen[IO, Path]] =
    new Monoid[PathGen[IO, Path]] {
      val empty: PathGen[IO, Path] = Combine(Nil)
      def combine(a: PathGen[IO, Path], b: PathGen[IO, Path]) =
        (a, b) match {
          case (Combine(Nil), b)          => b
          case (a, Combine(Nil))          => a
          case (Combine(as), Combine(bs)) => Combine(as ::: bs)
          case (Combine(as), b)           => Combine(as :+ b)
          case (a, Combine(bs))           => Combine(a :: bs)
          case (a, b)                     => Combine(a :: b :: Nil)
        }
    }

  def path[IO[_], P](path: P): PathGen[IO, P] =
    Direct(path)

  def directChildren[IO[_], P](dir: P, extension: String)(
      platformIO: PlatformIO[IO, P]
  ): PathGen[IO, P] =
    ChildrenOfDir[IO, P](
      dir,
      platformIO.hasExtension(extension),
      false,
      platformIO.unfoldDir(_)
    )

  def recursiveChildren[IO[_], P](dir: P, extension: String)(
      platformIO: PlatformIO[IO, P]
  ): PathGen[IO, P] =
    ChildrenOfDir[IO, P](
      dir,
      platformIO.hasExtension(extension),
      true,
      platformIO.unfoldDir(_)
    )

  def opts[IO[_], Path](
      arg: String,
      help: String,
      ext: String
  )(platformIO: PlatformIO[IO, Path]): Opts[PathGen[IO, Path]] = {
    import platformIO.pathArg

    val direct = Opts
      .options[Path](arg, help = help)
      .orEmpty
      .map(paths =>
        paths.foldMap(PathGen.Direct[IO, Path](_): PathGen[IO, Path])
      )

    val child1 = Opts
      .options[Path](arg + "_dir", help = s"all $help in directory")
      .orEmpty
      .map(paths => paths.foldMap(directChildren(_, ext)(platformIO)))

    val childMany = Opts
      .options[Path](
        arg + "_all_subdir",
        help = s"all $help recursively in all directories"
      )
      .orEmpty
      .map(paths => paths.foldMap(recursiveChildren(_, ext)(platformIO)))

    (direct, child1, childMany).mapN { (a, b, c) =>
      (a :: b :: c :: Nil).combineAll
    }
  }
}
