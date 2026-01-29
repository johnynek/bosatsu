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
  sealed abstract class OutputMode(val name: String) derives CanEqual
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
  sealed abstract class Mode derives CanEqual
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
        // We iterate over all packages and generate JS for each, in topological order
        // so that dependencies are defined before dependents
        val allPackages: Set[PackageName] = ns.compiled.values.flatMap(_.keys).toSet

        // Get packages in topological order (dependencies first)
        val topoOrderedPackages: List[PackageName] =
          ns.topoSort.layers.flatMap(_.toList).map(_._2).filter(allPackages.contains).toList

        // First, write the runtime library
        val runtimePath = resolve(args.outDir, List("_runtime.js"))
        val runtimeDoc = Doc.text(JsGen.renderRuntime)

        // Render each package to a JS file
        val packageDocs: List[(P, Doc)] = topoOrderedPackages.flatMap { pack =>
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
                // Prepend runtime import
                val depth = pack.parts.length
                val runtimeImport = "../" * depth + "_runtime.js"
                // Use CommonJS require - for ESM, use: import "./$runtimeImport"
                val importLine = s"""// Runtime import (for standalone module use)\nif (typeof require !== 'undefined') { require("./$runtimeImport"); }\n\n"""
                val doc = Doc.text(importLine + jsCode)
                List((outPath, doc))
            }
          }
        }

        // Add runtime to the output
        val allDocs = (runtimePath -> runtimeDoc) :: packageDocs

        // Also generate a bundled file with all code for testing
        val bundlePath = resolve(args.outDir, List("_bundle.js"))

        // Functions provided by the JS runtime - skip generating these from Bosatsu source
        val runtimeProvidedFunctions: Set[String] = Set(
          "foldl_List", "range", "flat_map_List"
        )

        // Use 'var' for all bindings - allows redefinition and creates true globals
        // This is necessary because:
        // 1. Multiple packages may define the same name (e.g., 'not' in BinInt and Bool)
        // 2. Generated code references names directly without qualification
        // 3. 'var' at top level creates real global variables accessible anywhere
        //
        // Later definitions shadow earlier ones, which is acceptable since:
        // - Same-named functions in different packages are usually unrelated
        // - Tests use qualified names in _tests registry
        val packageBlocks: List[String] = topoOrderedPackages.flatMap { pack =>
          val bindings: List[(Identifier.Bindable, dev.bosatsu.Matchless.Expr[S])] =
            ns.compiled.values.flatMap(_.get(pack).toList.flatten).toList

          if (bindings.isEmpty) Nil
          else {
            bindings.flatMap { case (name, expr) =>
              val escapedBase = JsGen.escape(name).name
              // Use package-qualified name to avoid collisions
              val qualifiedName = JsGen.qualifiedName(pack, name).name

              // Skip Predef functions that are provided by the runtime
              val isRuntimeProvided = pack == PackageName.PredefName && runtimeProvidedFunctions.contains(escapedBase)
              if (isRuntimeProvided) None
              else {
                // Use exprToJsWithTopLevel to pre-bind the qualified name for recursive references
                val qualifiedIdent = JsGen.qualifiedName(pack, name)
                val (_, jsExpr) = JsGen.Env.run(JsGen.exprToJsWithTopLevel(expr, name, qualifiedIdent))
                val rendered = Code.render(jsExpr)

                // Use globalThis to ensure variables are accessible when loaded via ES module import()
                // (var declarations become module-scoped in ES modules)
                val varDef = s"globalThis.$qualifiedName = $rendered;"

                // Check if this is a test binding and register it
                val n = name match {
                  case Identifier.Name(s) => s
                  case Identifier.Backticked(s) => s
                  case op: Identifier.Operator => op.asString
                }
                val isTest = n == "test" || n == "tests" || n.startsWith("test_") || n.endsWith("_test")
                val testReg = if (isTest) {
                  // Register test immediately after definition so we capture the right value
                  // even if a later package redefines the same name
                  s"""\nglobalThis._tests["${pack.asString}::$n"] = globalThis.$qualifiedName;"""
                } else ""

                Some(varDef + testReg)
              }
            }
          }
        }

        val testsHeader = "// Test registry\nglobalThis._tests = {};\n\n"
        val bundledCode = JsGen.renderRuntime + "\n\n" + testsHeader + "// Generated code\n" + packageBlocks.mkString("\n")
        val bundleDoc = Doc.text(bundledCode)

        // Add bundle to docs
        val withBundle = allDocs :+ (bundlePath -> bundleDoc)

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
                moduleIOMonad.pure(withBundle :+ (mainPath -> mainDoc))
              }
            }
          case Mode.Library =>
            moduleIOMonad.pure(withBundle)
        }

        withEntry
    }
  }

  private def renderEntryPoint(
      modulePath: String,
      entryName: String,
      outputMode: OutputMode
  ): Doc = {
    // Escape the entry name to match how it's exported from the module
    val escapedName = JsGen.escape(Identifier.unsafeBindable(entryName)).name
    outputMode match {
      case OutputMode.ESModule =>
        Doc.text(s"""import { $escapedName } from "$modulePath";""") +
          Doc.hardLine +
          Doc.hardLine +
          Doc.text(s"$escapedName();")
      case OutputMode.CommonJS =>
        Doc.text(s"""const { $escapedName } = require("$modulePath");""") +
          Doc.hardLine +
          Doc.hardLine +
          Doc.text(s"$escapedName();")
    }
  }
}
