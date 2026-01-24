package dev.bosatsu.codegen

import com.monovore.decline.{Argument, Opts}
import dev.bosatsu.{PlatformIO, Par}
import org.typelevel.paiges.Doc

trait Transpiler {
  type Args[F[_], P]

  // this gives the argument for reading files into strings
  // this is a bit limited, but good enough for now
  def opts[F[_], P](plat: PlatformIO[F, P]): Opts[Transpiler.Optioned[F, P]]

  // return paths to be resolved against the base output path
  def renderAll[F[_], P, S](
      ns: CompilationNamespace[S],
      args: Args[F, P]
  )(implicit ec: Par.EC): F[List[(P, Doc)]]

  final def renderAll[F[_], P, S](
      ns: S,
      args: Args[F, P]
  )(implicit ec: Par.EC, cs: CompilationSource[S]): F[List[(P, Doc)]] =
    renderAll(cs.namespace(ns), args)
}

object Transpiler {
  def outDir[P: Argument]: Opts[P] =
    Opts.option[P](
      "outdir",
      help = "directory to write all output into"
    )

  def optioned[F[_], P](t: Transpiler)(argsP: t.Args[F, P]): Optioned[F, P] =
    new Optioned[F, P] {
      val transpiler: t.type = t
      val args = argsP
    }

  sealed abstract class Optioned[F[_], P] { self =>
    val transpiler: Transpiler
    def args: transpiler.Args[F, P]
    final def renderAll[S](
        ns: S
    )(implicit ec: Par.EC, cs: CompilationSource[S]): F[List[(P, Doc)]] =
      transpiler.renderAll(cs.namespace(ns), args)
  }
}
