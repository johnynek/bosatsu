package dev.bosatsu.codegen.js

import cats.data.{NonEmptyList, Validated}
import com.monovore.decline.{Argument, Opts}
import dev.bosatsu.codegen.{CompilationNamespace, Transpiler}
import dev.bosatsu.{Identifier, PackageName, Par, PlatformIO}
import dev.bosatsu.tool.{CliException, ExitCode}
import org.typelevel.paiges.Doc

import cats.syntax.all._

/**
 * JavaScript transpiler for Bosatsu.
 *
 * Generates JavaScript code from Matchless IR, suitable for
 * browser or Node.js execution.
 */
case object JsTranspiler extends Transpiler {

  /**
   * Output mode: ES modules or CommonJS.
   */
  sealed abstract class OutputMode(val name: String)
  object OutputMode {
    case object ESModule extends OutputMode("esm")
    case object CommonJS extends OutputMode("cjs")

    implicit val argumentOutputMode: Argument[OutputMode] =
      new Argument[OutputMode] {
        def defaultMetavar: String = "outputmode"
        def read(string: String) =
          string match {
            case "esm" | "es" | "module" => Validated.valid(ESModule)
            case "cjs" | "commonjs"      => Validated.valid(CommonJS)
            case other =>
              Validated.invalidNel(s"expected (esm|cjs) got $other")
          }
      }

    val opts: Opts[OutputMode] =
      Opts
        .option[OutputMode](
          "format",
          help = "output format: esm (ES modules) or cjs (CommonJS), default = esm"
        )
        .withDefault(ESModule)
  }

  /**
   * Execution mode: main entry point or library.
   */
  sealed abstract class Mode
  object Mode {
    case class Main(pack: PackageName, entryPoint: Option[String]) extends Mode
    case object Library extends Mode

    def opts: Opts[Mode] =
      Opts
        .option[PackageName]("main", "the package to use as an entry point")
        .product(
          Opts
            .option[String](
              "entry",
              "the binding name to call as main (default: main)"
            )
            .orNone
        )
        .map { case (pn, entry) => Main(pn, entry) }
        .orElse(
          Opts
            .flag("library", "compile as a library (no entry point)")
            .as(Library)
        )
        .withDefault(Library)
  }

  case class Arguments[F[_], P](
      mode: Mode,
      outputMode: OutputMode,
      outDir: P,
      platformIO: PlatformIO[F, P]
  )

  type Args[F[_], P] = Arguments[F, P]

  def opts[F[_], P](
      platformIO: PlatformIO[F, P]
  ): Opts[Transpiler.Optioned[F, P]] =
    Opts.subcommand("js", "generate JavaScript code") {
      implicit val impPathArg: Argument[P] = platformIO.pathArg

      (
        Mode.opts,
        OutputMode.opts,
        Transpiler.outDir
      ).mapN { (mode, outputMode, outDir) =>
        Transpiler.optioned(this)(
          Arguments(mode, outputMode, outDir, platformIO)
        )
      }
    }

  case class CircularPackagesFound(loop: NonEmptyList[(Any, PackageName)])
      extends Exception("circular deps in packages")
      with CliException {
    def errDoc: Doc =
      Doc.text("circular dependencies found in packages:") +
        (Doc.line + Doc.intercalate(
          Doc.comma + Doc.line,
          loop.toList.map { case (_, pn) => Doc.text(pn.asString) }
        )).nested(4).grouped
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  case class InvalidMainValue(pack: PackageName, message: String)
      extends Exception(s"invalid main ${pack.asString}: $message.")
      with CliException {
    def errDoc = Doc.text(getMessage())
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  def renderAll[F[_], P, S](
      ns: CompilationNamespace[S],
      args: Args[F, P]
  )(implicit ec: Par.EC): F[List[(P, Doc)]] = {
    import args.platformIO._

    // Check for circular dependencies
    NonEmptyList.fromList(ns.topoSort.loopNodes) match {
      case Some(loop) => moduleIOMonad.raiseError(CircularPackagesFound(loop))
      case None =>
        // ns.compiled is SortedMap[S, Map[PackageName, List[(Bindable, Matchless.Expr[S])]]]
        // We iterate over all packages and generate JS for each
        val allPackages: Set[PackageName] = ns.compiled.values.flatMap(_.keys).toSet

        // Render each package to a JS file
        val packageDocs: List[(P, Doc)] = allPackages.toList.sorted.flatMap { pack =>
          // Collect all bindings for this package from all compiled sources
          val bindings: List[(Identifier.Bindable, dev.bosatsu.Matchless.Expr[S])] =
            ns.compiled.values.flatMap(_.get(pack).toList.flatten).toList

          if (bindings.isEmpty) Nil
          else {
            // Convert to path: PackageName -> directory structure + index.js
            val pathParts = pack.parts.toList :+ "index.js"
            NonEmptyList.fromList(pathParts) match {
              case None => Nil
              case Some(_) =>
                val outPath = resolve(args.outDir, pathParts)
                val jsCode = JsGen.renderModule(bindings)
                val doc = Doc.text(jsCode)
                List((outPath, doc))
            }
          }
        }

        // Add entry point if in main mode
        val withEntry: F[List[(P, Doc)]] = args.mode match {
          case Mode.Main(mainPack, entryOpt) =>
            val entryName = entryOpt.getOrElse("main")
            val entryBindable = Identifier.Name(entryName)

            // Check if the package exists
            val packageBindings: List[(Identifier.Bindable, dev.bosatsu.Matchless.Expr[S])] =
              ns.compiled.values.flatMap(_.get(mainPack).toList.flatten).toList

            if (packageBindings.isEmpty) {
              moduleIOMonad.raiseError(
                InvalidMainValue(mainPack, s"package not found")
              )
            } else {
              val hasEntry = packageBindings.exists { case (name, _) =>
                name == entryBindable
              }
              if (!hasEntry) {
                moduleIOMonad.raiseError(
                  InvalidMainValue(
                    mainPack,
                    s"binding '$entryName' not found"
                  )
                )
              } else {
                // Create main.js entry point
                val mainPath = resolve(args.outDir, List("main.js"))
                val modulePath =
                  "./" + mainPack.parts.toList.mkString("/") + "/index.js"
                val mainDoc = renderEntryPoint(
                  modulePath,
                  entryName,
                  args.outputMode
                )
                moduleIOMonad.pure(packageDocs :+ (mainPath -> mainDoc))
              }
            }
          case Mode.Library =>
            moduleIOMonad.pure(packageDocs)
        }

        withEntry
    }
  }

  private def renderEntryPoint(
      modulePath: String,
      entryName: String,
      outputMode: OutputMode
  ): Doc = {
    outputMode match {
      case OutputMode.ESModule =>
        Doc.text(s"""import { $entryName } from "$modulePath";""") +
          Doc.hardLine +
          Doc.hardLine +
          Doc.text(s"$entryName();")
      case OutputMode.CommonJS =>
        Doc.text(s"""const { $entryName } = require("$modulePath");""") +
          Doc.hardLine +
          Doc.hardLine +
          Doc.text(s"$entryName();")
    }
  }
}
