package dev.bosatsu.codegen.python

import cats.data.NonEmptyList
import cats.implicits.catsKernelOrderingForOrder
import com.monovore.decline.{Argument, Opts}
import dev.bosatsu.CollectionUtils.listToUnique
import dev.bosatsu.codegen.{CompilationNamespace, Transpiler}
import dev.bosatsu.{Par, Parser, PlatformIO}
import org.typelevel.paiges.Doc
import scala.util.Try

import cats.syntax.all._

case object PythonTranspiler extends Transpiler {
  case class Arguments[F[_], P](
      externals: List[P],
      evaluators: List[P],
      outDir: P,
      platformIO: PlatformIO[F, P]
  ) {
    def read: F[(List[String], List[String])] = {
      import platformIO._

      externals
        .traverse(readUtf8)
        .product(
          evaluators.traverse(readUtf8)
        )
    }
  }

  type Args[F[_], P] = Arguments[F, P]

  // this gives the argument for reading files into strings
  // this is a bit limited, but good enough for now
  def opts[F[_], P](
      platformIO: PlatformIO[F, P]
  ): Opts[Transpiler.Optioned[F, P]] =
    Opts.subcommand("python", "generate python code") {
      implicit val impPathArg: Argument[P] = platformIO.pathArg
      (
        Opts
          .options[P](
            "externals",
            help =
              "external descriptors the transpiler uses to rewrite external defs"
          )
          .orEmpty,
        Opts
          .options[P](
            "evaluators",
            help = "evaluators which run values of certain types"
          )
          .orEmpty,
        Transpiler.outDir
      )
        .mapN(Arguments(_, _, _, platformIO))
        .map(arg => Transpiler.optioned(this)(arg))
    }

  def renderAll[F[_], P, S](
      ns: CompilationNamespace[S],
      args: Args[F, P]
  )(implicit ec: Par.EC): F[List[(P, Doc)]] = {

    import args.platformIO._

    args.read.flatMap { case (externals, evaluators) =>
      moduleIOMonad.fromTry(Try {
        val parsedExt =
          externals.map(Parser.unsafeParse(PythonGen.externalParser, _))

        val extMap = listToUnique(parsedExt.flatten)(
          { case (p, b, _, _) => (p, b) },
          { case (_, _, m, f) => (m, f) },
          "expected each package/name to map to just one file"
        ).get

        val exts = extMap.keySet
        val intrinsic = PythonGen.intrinsicValues

        val allExternals = ns.externals
        val missingExternals =
          allExternals.iterator.flatMap { case (_, pmap) =>
            pmap.iterator.flatMap { case (p, names) =>
              val missing = names.filterNot { case (n, _) =>
                exts((p, n)) || intrinsic.get(p).exists(_(n))
              }

              if (missing.isEmpty) Nil
              else (p, missing.sorted) :: Nil
            }
          }.toList

        if (missingExternals.isEmpty) {
          val parsedEvals =
            evaluators.map(Parser.unsafeParse(PythonGen.evaluatorParser, _))
          // TODO, we don't check that these types even exist in the fully
          // universe, we should. If you have a typo in a type or package name
          // you just get silently ignored
          val typeEvalMap = listToUnique(parsedEvals.flatten)(
            t => t._1,
            t => t._2,
            "expected each type to have to just one evaluator"
          ).get

          val evalMap =
            ns.mainValues(typeEvalMap.contains(_))
              .view
              .mapValues { case (b, t) =>
                val (m, i) = typeEvalMap(t)
                (b, m, i)
              }
              .toMap

          def toPath(ns: NonEmptyList[String]): P =
            resolve(args.outDir, ns.toList)

          val docs = PythonGen
            .renderAll(ns, extMap, evalMap)
            .iterator
            .flatMap { case (_, packs) =>
              packs.iterator.map { case (_, (path, doc)) =>
                (path.map(_.name), doc)
              }
            }
            .toList

          // python also needs empty __init__.py files in every package directory
          def prefixes[A](
              paths: List[(NonEmptyList[String], A)]
          ): List[(P, Doc)] = {
            val existing = paths.iterator.map(_._1).toSet
            val initPaths = paths.iterator.flatMap { case (path, _) =>
              val parts = path.toList
              (1 until parts.length).iterator.map { i =>
                NonEmptyList.ofInitLast(parts.take(i), "__init__.py")
              }
            }.toSet

            (initPaths -- existing).toList.sorted.map(p => (toPath(p), Doc.empty))
          }

          prefixes(docs) ::: docs.map { case (p, d) => (toPath(p), d) }
        } else {
          // we need to render this nicer
          val missingDoc =
            missingExternals
              .sortBy(_._1)
              .map { case (p, names) =>
                (Doc.text("package") + Doc.lineOrSpace + Doc.text(
                  p.asString
                ) + Doc.lineOrSpace +
                  Doc.char('[') +
                  Doc.intercalate(
                    Doc.comma + Doc.lineOrSpace,
                    names.map { case (b, _) => Doc.text(b.sourceCodeRepr) }
                  ) + Doc.char(']')).nested(4)
              }

          val message = Doc.text(
            "Missing external values:"
          ) + (Doc.line + Doc.intercalate(Doc.line, missingDoc)).nested(4)

          throw new IllegalArgumentException(message.renderTrim(80))
        }
      })
    }
  }
}
