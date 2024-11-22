package org.bykn.bosatsu.codegen

import cats.{Applicative, Traverse}
import cats.data.NonEmptyList
import com.monovore.decline.{Argument, Opts}
import org.bykn.bosatsu.{PackageMap, Par}
import org.typelevel.paiges.Doc
import scala.util.Try

import cats.syntax.all._

trait Transpiler {
  type Args[P]
  def traverseArgs: Traverse[Args]

  // this gives the argument for reading files into strings
  // this is a bit limited, but good enough for now
  def opts[P](pathArg: Argument[P]): Opts[Transpiler.Optioned[P]]

  def renderAll(
      pm: PackageMap.Typed[Any],
      args: Args[String]
  )(implicit ec: Par.EC): Try[List[(NonEmptyList[String], Doc)]]
}

object Transpiler {
  def optioned[P](t: Transpiler)(argsP: t.Args[P]): Optioned[P] =
    new Optioned[P] {
      val transpiler: t.type = t
      val args = argsP
    }

  sealed abstract class Optioned[P] { self =>
    val transpiler: Transpiler
    def args: transpiler.Args[P]
    def traverse[F[_]: Applicative](fn: P => F[String]): F[Optioned[String]] =
      self.transpiler.traverseArgs.traverse(self.args)(fn)
        .map { argsString =>
          new Optioned[String] {
            val transpiler: self.transpiler.type = self.transpiler
            val args = argsString
          }
        }
  }
}