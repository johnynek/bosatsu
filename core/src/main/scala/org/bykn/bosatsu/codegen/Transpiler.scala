package org.bykn.bosatsu.codegen

import com.monovore.decline.Opts
import org.bykn.bosatsu.{PackageMap, Par, PlatformIO}
import org.typelevel.paiges.Doc

trait Transpiler {
  type Args[F[_], P]

  // this gives the argument for reading files into strings
  // this is a bit limited, but good enough for now
  def opts[F[_], P](plat: PlatformIO[F, P]): Opts[Transpiler.Optioned[F, P]]

  // return paths to be resolved against the base output path
  def renderAll[F[_], P](
      outDir: P,
      pm: PackageMap.Typed[Any],
      args: Args[F, P]
  )(implicit ec: Par.EC): F[List[(P, Doc)]]
}

object Transpiler {
  def optioned[F[_], P](t: Transpiler)(argsP: t.Args[F, P]): Optioned[F, P] =
    new Optioned[F, P] {
      val transpiler: t.type = t
      val args = argsP
    }

  sealed abstract class Optioned[F[_], P] { self =>
    val transpiler: Transpiler
    def args: transpiler.Args[F, P]
  }
}