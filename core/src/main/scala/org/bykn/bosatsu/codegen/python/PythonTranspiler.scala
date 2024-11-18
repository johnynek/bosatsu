package org.bykn.bosatsu.codegen.python

import cats.data.NonEmptyList
import cats.implicits.catsKernelOrderingForOrder
import org.bykn.bosatsu.{Package, PackageMap, Par, Parser, MatchlessFromTypedExpr}
import org.bykn.bosatsu.codegen.Transpiler
import org.typelevel.paiges.Doc
import scala.util.Try

import org.bykn.bosatsu.CollectionUtils.listToUnique

case object PythonTranspiler extends Transpiler {
  val name: String = "python"

  def renderAll(
      pm: PackageMap.Typed[Any],
      externals: List[String],
      evaluators: List[String]
  )(implicit ec: Par.EC): Try[List[(NonEmptyList[String], Doc)]] = {

    val cmp = MatchlessFromTypedExpr.compile(pm)
    Try {
      val parsedExt =
        externals.map(Parser.unsafeParse(PythonGen.externalParser, _))

      val extMap = listToUnique(parsedExt.flatten)(
        { case (p, b, _, _) => (p, b) },
        { case (_, _, m, f) => (m, f) },
        "expected each package/name to map to just one file"
      ).get

      val exts = extMap.keySet
      val intrinsic = PythonGen.intrinsicValues

      val allExternals = pm.allExternals
      val missingExternals =
        allExternals.iterator.flatMap { case (p, names) =>
          val missing = names.filterNot { case (n, _) =>
            exts((p, n)) || intrinsic.get(p).exists(_(n))
          }

          if (missing.isEmpty) Nil
          else (p, missing.sorted) :: Nil
        }.toList

      if (missingExternals.isEmpty) {
        val tests = pm.toMap.iterator.flatMap { case (n, pack) =>
          Package.testValue(pack).iterator.map { case (bn, _, _) =>
            (n, bn)
          }
        }.toMap

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

        val evalMap = pm.toMap.iterator.flatMap { case (n, p) =>
          val optEval = p.lets.findLast { case (_, _, te) =>
            // TODO this should really e checking that te.getType <:< a key
            // in the map.
            typeEvalMap.contains(te.getType)
          }
          optEval.map { case (b, _, te) =>
            val (m, i) = typeEvalMap(te.getType)
            (n, (b, m, i))
          }
        }.toMap

        val docs = PythonGen
          .renderAll(cmp, extMap, tests, evalMap)
          .iterator
          .map { case (_, (path, doc)) =>
            (path.map(_.name), doc)
          }
          .toList

        // python also needs empty __init__.py files in every parent directory
        def prefixes[A](
            paths: List[(NonEmptyList[String], A)]
        ): List[(NonEmptyList[String], Doc)] = {
          val inits =
            paths.map { case (path, _) =>
              val parent = path.init
              val initPy = parent :+ "__init__.py"
              NonEmptyList.fromListUnsafe(initPy)
            }.toSet

          inits.toList.sorted.map(p => (p, Doc.empty))
        }

        prefixes(docs) ::: docs
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
    }
  }
}