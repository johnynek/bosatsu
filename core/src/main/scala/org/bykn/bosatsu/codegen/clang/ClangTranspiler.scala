package org.bykn.bosatsu.codegen.clang

import cats.Traverse
import cats.data.{Const, NonEmptyList}
import com.monovore.decline.{Argument, Opts}
import org.bykn.bosatsu.codegen.Transpiler
import org.bykn.bosatsu.{Identifier, MatchlessFromTypedExpr, PackageName, PackageMap, Par}
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

case object ClangTranspiler extends Transpiler {

  case class Arguments()
  type Args[P] = Const[Arguments, P]

  def traverseArgs: Traverse[Args] = implicitly

  def opts[P](pathArg: Argument[P]): Opts[Transpiler.Optioned[P]] =
    Opts.subcommand("c", "generate c code") {
      Opts(Transpiler.optioned(this)(Const[Arguments, P](Arguments())))
    }

  case class GenError(error: ClangGen.Error) extends Exception(s"clang gen error: ${error.display.render(80)}")

  case class CircularPackagesFound(loop: NonEmptyList[PackageName])
    extends Exception(s"circular dependencies found in packages: ${
      loop.map(_.asString).toList.mkString(", ")
    }")

  def externalsFor(pm: PackageMap.Typed[Any]): ClangGen.ExternalResolver =
    ClangGen.ExternalResolver.stdExternals(pm)

  def renderAll(
      pm: PackageMap.Typed[Any],
      args: Args[String]
  )(implicit ec: Par.EC): Try[List[(NonEmptyList[String], Doc)]] = {
    // we have to render the code in sorted order
    val sorted = pm.topoSort
    NonEmptyList.fromList(sorted.loopNodes) match {
      case Some(loop) => Failure(CircularPackagesFound(loop))
      case None =>
        val matchlessMap = MatchlessFromTypedExpr.compile(pm)

        val ext = externalsFor(pm)
        val doc = ClangGen.renderMain(
          sortedEnv = cats.Functor[Vector]
            .compose[NonEmptyList]
            .map(sorted.layers) { pn => pn -> matchlessMap(pn) },
          externals = ext,
          // TODO: this is currently ignored
          value = (PackageName.PredefName, Identifier.Name("todo")),
          // TODO: this is also ignored currently
          evaluator = (Code.Include(true, "eval.h"), Code.Ident("evaluator_run"))
        )

        doc match {
          case Left(err) => Failure(GenError(err))
          case Right(doc) =>
            // TODO: this name needs to be an option
            val outputName = NonEmptyList("output.c", Nil)

            val externalHeaders = ext.generateExternalsStub
              .iterator.map { case (n, d) =>
                NonEmptyList.one(n) -> d  
              }
              .toList

            // TODO: always outputing the headers may not be right, maybe an option
            Success((outputName -> doc) :: externalHeaders)
        }
    }
  }
}