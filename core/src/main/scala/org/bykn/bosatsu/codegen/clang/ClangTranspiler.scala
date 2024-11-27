package org.bykn.bosatsu.codegen.clang

import cats.Traverse
import cats.data.{Const, NonEmptyList, Validated}
import com.monovore.decline.{Argument, Opts}
import java.util.regex.{Pattern => RegexPat}
import org.bykn.bosatsu.codegen.Transpiler
import org.bykn.bosatsu.{Identifier, MatchlessFromTypedExpr, Package, PackageName, PackageMap, Par, TypedExpr}
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

import Identifier.Bindable

import cats.syntax.all._

case object ClangTranspiler extends Transpiler {

  sealed abstract class EmitMode {
    def apply[A](pm: PackageMap.Typed[A], roots: Set[(PackageName, Identifier)]): PackageMap.Typed[A]
  }
  object EmitMode {
    case object Shake extends EmitMode {
      def apply[A](pm: PackageMap.Typed[A], roots: Set[(PackageName, Identifier)]): PackageMap.Typed[A] =
        PackageMap.treeShake(pm, roots)
    }
    case object All extends EmitMode {
      def apply[A](pm: PackageMap.Typed[A], roots: Set[(PackageName, Identifier)]): PackageMap.Typed[A] = pm
    }

    implicit val argumentEmitMode: Argument[EmitMode] =
      new Argument[EmitMode] {
        def defaultMetavar: String = "emitmode"
        def read(string: String) =
          string match {
            case "shake" => Validated.valid(Shake)
            case "all" => Validated.valid(All)
            case other => Validated.invalidNel(s"expected (shake|all) got $other")
          }
      }

      val opts: Opts[EmitMode] =
        Opts.option[EmitMode]("emitmode", "emit mode: shake|all, default = all").withDefault(All)
  }
  sealed abstract class Mode(val name: String)
  object Mode {
    case class Main(pack: PackageName) extends Mode("main")
    case class Test(filter: Option[PackageName => Boolean], filterRegex: String) extends Mode("test") {
      def values(p: PackageMap.Typed[Any]): List[(PackageName, Bindable)] =
        (filter match {
          case None =>
            p.testValues.toList
          case Some(k) =>
            p.testValues.iterator.filter { case (p, _) => k(p) }.toList
        }).sortBy(_._1)
    }

    val opts: Opts[Mode] =
      Opts.option[PackageName]("main", "the package to use as an entry point")
        .map(Main(_))
        .orElse(
          Opts.flag("test", "compile the tests") *>
            (Opts.option[String]("filter", "regular expression to filter package names").orNone)
            .mapValidated {
              case None => Validated.valid(Test(None, ".*"))
              case Some(pat) =>
                Try(RegexPat.compile(pat)) match {
                  case Success(p) => Validated.valid(Test(Some(pn => p.matcher(pn.asString).matches()), pat))
                  case Failure(e) => Validated.invalidNel(s"could not parse pattern: $pat\n\n${e.getMessage}")
                }
            }
        )
  }

  case class Arguments(mode: Mode, emit: EmitMode)
  type Args[P] = Const[Arguments, P]

  def traverseArgs: Traverse[Args] = implicitly

  def opts[P](pathArg: Argument[P]): Opts[Transpiler.Optioned[P]] =
    Opts.subcommand("c", "generate c code") {
      (Mode.opts, EmitMode.opts).mapN { (m, e) =>
        Transpiler.optioned(this)(Const[Arguments, P](Arguments(m, e)))
      }
    }

  case class GenError(error: ClangGen.Error) extends Exception(s"clang gen error: ${error.display.render(80)}")

  private def spacePackList(ps: Iterable[PackageName]): Doc =
    (Doc.line + Doc.intercalate(Doc.comma + Doc.line, ps.map(p => Doc.text(p.asString))))
      .nested(4)
      .grouped

  case class CircularPackagesFound(loop: NonEmptyList[PackageName])
    extends Exception(
      (Doc.text("circular dependencies found in packages:") + spacePackList(loop.toList)).render(80)
    )

  case class InvalidMainValue(pack: PackageName, message: String) extends
    Exception(s"invalid main ${pack.asString}: $message.")

  case class NoTestsFound(packs: List[PackageName], regex: String) extends
    Exception(
      (Doc.text("no tests found in:") + spacePackList(packs)).render(80)
    )

  def externalsFor(pm: PackageMap.Typed[Any]): ClangGen.ExternalResolver =
    ClangGen.ExternalResolver.stdExternals(pm)

  def validMain[A](te: TypedExpr[A]): Either[String, Unit] =
    // TODO: need to check this
    Right(())

  def renderAll(
      pm: PackageMap.Typed[Any],
      args: Args[String]
  )(implicit ec: Par.EC): Try[List[(NonEmptyList[String], Doc)]] = {
    // we have to render the code in sorted order
    val sorted = pm.topoSort
    NonEmptyList.fromList(sorted.loopNodes) match {
      case Some(loop) => Failure(CircularPackagesFound(loop))
      case None =>
        val ext = externalsFor(pm)
        val doc = args.getConst.mode match {
          case Mode.Main(p) =>
            pm.toMap.get(p).flatMap(Package.mainValue(_)) match {
              case Some((b, _, t)) =>
                validMain(t) match {
                  case Right(_) =>
                    val pm1 = args.getConst.emit(pm, Set((p, b)))
                    val matchlessMap = MatchlessFromTypedExpr.compile(pm1)
                    val sortedEnv = cats.Functor[Vector]
                        .compose[NonEmptyList]
                        .map(sorted.layers) { pn => pn -> matchlessMap(pn) }

                    ClangGen.renderMain(
                      sortedEnv = sortedEnv,
                      externals = ext,
                      // TODO: this is currently ignored
                      value = (p, b))
                  case Left(invalid) =>
                    return Failure(InvalidMainValue(p, invalid))
                }
              case None =>
                return Failure(InvalidMainValue(p, "empty package"))
            }
          case test @ Mode.Test(_, re) =>
            test.values(pm) match {
              case Nil =>
                return Failure(NoTestsFound(pm.toMap.keySet.toList.sorted, re))
              case nonEmpty =>
                val pm1 = args.getConst.emit(pm, nonEmpty.toSet)
                val matchlessMap = MatchlessFromTypedExpr.compile(pm1)
                val sortedEnv = cats.Functor[Vector]
                    .compose[NonEmptyList]
                    .map(sorted.layers) { pn => pn -> matchlessMap(pn) }

                ClangGen.renderTests(
                  sortedEnv = sortedEnv,
                  externals = ext,
                  values = nonEmpty)
            }
        }

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