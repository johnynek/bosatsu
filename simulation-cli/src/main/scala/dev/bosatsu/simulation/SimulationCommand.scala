package dev.bosatsu.simulation

import cats.effect.IO
import cats.implicits._
import cats.data.{NonEmptyList, ValidatedNel, Validated}
import com.monovore.decline._
import java.nio.file.{Files, Path, Paths}

import dev.bosatsu.{
  Package,
  PackageMap,
  PackageName,
  LocationMap,
  Identifier,
  Pattern,
  Par,
  TypedExpr,
  HasRegion,
  Region,
  MatchlessFromTypedExpr
}
import dev.bosatsu.codegen.js.JsGen
import dev.bosatsu.ui.EmbedGenerator
import dev.bosatsu.Parser
import dev.bosatsu.analysis.{ProvenanceAnalyzer, DerivationGraph}

/**
 * Command-line interface for bosatsu-sim.
 *
 * Uses the principled Bosatsu compilation pipeline:
 * 1. Parse to Package.Parsed
 * 2. Type-check via PackageMap.typeCheckParsed (produces TypedExpr with analysis)
 * 3. Analyze with ProvenanceAnalyzer for derivation tracking
 * 4. Compile to Matchless IR via MatchlessFromTypedExpr.compile
 * 5. Generate JavaScript via JsGen.renderModule
 */
case class SimulationCommand(
    input: Path,
    output: Path,
    title: Option[String],
    theme: SimulationCommand.Theme,
    showWhy: Boolean,
    showWhatIf: Boolean,
    showSweeps: Boolean
) {

  def run: IO[Unit] = {
    given ec: Par.EC = Par.ecFromExecutionContext(
      using scala.concurrent.ExecutionContext.global
    )

    for {
      // Read input file
      inputContent <- IO(Files.readString(input))
      fileName = input.getFileName.toString
      packageNameStr = fileName.stripSuffix(".bosatsu").replace("-", "_")
      packageName = PackageName.parse(packageNameStr).getOrElse(
        PackageName.parse("Main").get
      )

      // Parse the Bosatsu file (packageName hint is optional, file declares its own package)
      parsed <- IO.fromEither(
        Parser.parse(Package.parser(None), inputContent).toEither.leftMap { errs =>
          val errMsg = errs.toList.map(_.showContext(LocationMap.Colorize.None).renderTrim(80)).mkString("; ")
          new RuntimeException(s"Parse errors: $errMsg")
        }
      )
      parsedPackage = parsed._2
      actualPackageName = parsedPackage.name  // Use the declared package name
      locationMap = LocationMap(inputContent)

      // Type check using the principled pipeline
      // This gives us TypedExpr with full type information and analysis
      typeChecked <- IO.fromEither {
        val packWithLoc = NonEmptyList.one(((packageNameStr, locationMap), parsedPackage))
        PackageMap.typeCheckParsed(packWithLoc, Nil, packageNameStr).toEither.leftMap { errs =>
          // Build source map for error messages
          val sourceMap: Map[PackageName, (LocationMap, String)] = Map(
            actualPackageName -> (locationMap, packageNameStr)
          )
          val errMsg = errs.toList.map(_.message(sourceMap, LocationMap.Colorize.None)).mkString("; ")
          new RuntimeException(s"Type check errors: $errMsg")
        }
      }

      // Get the typed package using the actual declared name
      typedPackage = typeChecked.toMap.get(actualPackageName).getOrElse(
        throw new RuntimeException(s"Package $actualPackageName not found after type checking")
      )

      // Get TypedExpr bindings for analysis
      typedBindings = typedPackage.lets.map { case (name, _, expr) => (name, expr) }

      // Extract display formulas from parsed AST (for human-readable formulas)
      bindingsMap = extractBindingValues(parsedPackage)

      // Check if this is an animation
      isAnimation = bindingsMap.get("simulation_type").exists(_.contains("animation")) ||
                   (bindingsMap.contains("canvas_width") && bindingsMap.contains("canvas_height"))

      // Generate HTML based on simulation type
      htmlContent = if (isAnimation) {
        generateAnimationHtml(bindingsMap, fileName)
      } else {
        generateCalculatorHtml(typedBindings, typedPackage, typeChecked, bindingsMap, fileName)
      }

      // Write output
      _ <- IO(Files.writeString(output, htmlContent))
      _ <- IO(println(s"Generated: $output"))
    } yield ()
  }

  /**
   * Extract binding values from parsed statements for UI display.
   * This is needed for things like canvas dimensions, physics expressions, etc.
   */
  private def extractBindingValues(parsed: Package.Parsed): Map[String, String] = {
    import dev.bosatsu.Statement
    import dev.bosatsu.Declaration

    parsed.program.flatMap {
      case Statement.Bind(bind) =>
        bind.name match {
          case Pattern.Var(name) =>
            val formula = exprToDisplayString(bind.value)
            Some(name.asString -> formula)
          case _ => None
        }
      case _ => None
    }.toMap
  }

  /**
   * Convert a declaration to a display string (for formulas, UI labels, etc.)
   */
  private def exprToDisplayString(expr: dev.bosatsu.Declaration.NonBinding): String = {
    import dev.bosatsu.Declaration._

    expr match {
      case Literal(lit) =>
        lit match {
          case dev.bosatsu.Lit.Integer(i) => i.toString
          case dev.bosatsu.Lit.Str(s) => s""""$s""""
          case dev.bosatsu.Lit.Chr(c) => s"'$c'"
        }
      case Var(name) => name.asString
      case Apply(fn, args, _) =>
        val fnStr = exprToDisplayString(fn)
        val argsStr = args.toList.map(exprToDisplayString).mkString(", ")
        s"$fnStr($argsStr)"
      case ApplyOp(left, op, right) =>
        val leftStr = exprToDisplayString(left)
        val rightStr = exprToDisplayString(right)
        s"$leftStr ${op.asString} $rightStr"
      case Parens(inner) =>
        inner match {
          case nb: NonBinding => s"(${exprToDisplayString(nb)})"
          case _ => inner.toDoc.renderTrim(80)
        }
      case IfElse(_, _) => "if-else"
      case ann @ Annotation(term, _) => exprToDisplayString(term)
      case _ => expr.toDoc.renderTrim(80)
    }
  }

  /**
   * Generate animation HTML - physics and rendering are defined in the .bosatsu file.
   */
  private def generateAnimationHtml(
      params: Map[String, String],
      fileName: String
  ): String = {
    val canvasWidth = params.getOrElse("canvas_width", "552").toIntOption.getOrElse(552)
    val canvasHeight = params.getOrElse("canvas_height", "300").toIntOption.getOrElse(300)

    AnimationTemplates.generate(
      title = title.getOrElse(fileName.stripSuffix(".bosatsu").replace("_", " ").capitalize),
      theme = theme,
      params = params,
      canvasWidth = canvasWidth,
      canvasHeight = canvasHeight
    )
  }

  /**
   * Generate calculator HTML using the principled Bosatsu compilation pipeline.
   *
   * Uses:
   * - TypedExpr for analysis (via ProvenanceAnalyzer)
   * - MatchlessFromTypedExpr for IR compilation
   * - JsGen for JavaScript generation
   *
   * This is the proper way to go from Bosatsu to JavaScript, using
   * existing infrastructure rather than reimplementing code generation.
   */
  private def generateCalculatorHtml(
      typedBindings: List[(Identifier.Bindable, TypedExpr[Any])],
      typedPackage: Package.Typed[Any],
      fullPackageMap: PackageMap.Inferred,
      displayFormulas: Map[String, String],
      fileName: String
  )(implicit ec: Par.EC): String = {
    import dev.bosatsu.codegen.js.Code

    // Use displayFormulas (from parsed AST) as the source of all bindings
    // since typedBindings may have constant-folded some bindings away
    val allBindingNames = displayFormulas.keySet
    val typedBindingsMap = typedBindings.map { case (name, expr) => name.asString -> expr }.toMap

    // Extract dependencies from formula strings
    def extractDepsFromFormula(formula: String, scope: Set[String]): Set[String] = {
      val identPattern = """[a-z_][a-z0-9_]*""".r
      val foundNames = identPattern.findAllIn(formula).toSet
      foundNames.intersect(scope)
    }

    val analyses = allBindingNames.toList.sorted.map { nameStr =>
      val name = Identifier.Name(nameStr)
      val formula = displayFormulas(nameStr)
      val deps = extractDepsFromFormula(formula, allBindingNames) - nameStr
      val kind = if (deps.isEmpty) DerivationAnalyzer.Assumption else DerivationAnalyzer.Computation
      val depBindables = deps.map(n => Identifier.Name(n): Identifier.Bindable).toSet
      val valueType = typedBindingsMap.get(nameStr).map(inferTypeFromTypedExpr).getOrElse("number")

      DerivationAnalyzer.AnalyzedBinding(
        name,
        kind,
        depBindables,
        formula,
        valueType
      )
    }

    // Compile to Matchless IR using the full PackageMap (includes Predef with type info)
    // This handles all operators (add, sub, times, div) properly via PredefExternal
    val matchlessCompiled = MatchlessFromTypedExpr.compile((),
      PackageMap.toAnyTyped(fullPackageMap)
    )

    val packageBindings = matchlessCompiled.getOrElse(typedPackage.name, Nil)
    val rawComputeJs = JsGen.renderModule(packageBindings)

    // Post-process to remove package prefix for standalone HTML
    // JsGen generates qualified names like "LoanCalculator$years" but we need just "years"
    val packagePrefix = JsGen.escapePackage(typedPackage.name) + "\\$"
    val computeJs = rawComputeJs.replaceAll(packagePrefix, "")

    val config = SimulationGen.SimConfig(
      title = title.getOrElse(fileName.stripSuffix(".bosatsu")),
      theme = theme match {
        case SimulationCommand.Theme.Light => EmbedGenerator.LightTheme
        case SimulationCommand.Theme.Dark  => EmbedGenerator.DarkTheme
      },
      showWhy = showWhy,
      showWhatIf = showWhatIf,
      showSweeps = showSweeps
    )

    SimulationGen.generate(analyses, computeJs, config)
  }

  /**
   * Infer type from TypedExpr for UI purposes.
   * Uses the actual type information from the type checker.
   */
  private def inferTypeFromTypedExpr(expr: TypedExpr[Any]): String = {
    val tpe = expr.getType
    // Check if it's an Int type
    tpe match {
      case dev.bosatsu.rankn.Type.TyConst(tc) if tc.toDefined.name.asString == "Int" => "number"
      case dev.bosatsu.rankn.Type.TyConst(tc) if tc.toDefined.name.asString == "String" => "string"
      case dev.bosatsu.rankn.Type.TyConst(tc) if tc.toDefined.name.asString == "Bool" => "boolean"
      case _ => "any"
    }
  }

}

object SimulationCommand {

  sealed abstract class Theme(val name: String)
  object Theme {
    case object Light extends Theme("light")
    case object Dark extends Theme("dark")

    def fromString(s: String): Theme = s.toLowerCase match {
      case "dark" => Dark
      case _      => Light
    }
  }

  implicit val pathArgument: Argument[Path] = new Argument[Path] {
    def read(string: String): ValidatedNel[String, Path] =
      try {
        Validated.valid(Paths.get(string))
      } catch {
        case e: Exception => Validated.invalidNel(e.getMessage)
      }
    def defaultMetavar: String = "path"
  }

  val opts: Opts[SimulationCommand] = {
    val input = Opts.argument[Path]("input")
    val output = Opts
      .option[Path]("output", "Output HTML file", "o")
      .withDefault(Paths.get("output.html"))
    val titleOpt = Opts.option[String]("title", "Page title", "t").orNone
    val themeOpt = Opts
      .option[String]("theme", "Theme: light or dark")
      .withDefault("light")
      .map(Theme.fromString)
    val noWhy = Opts.flag("no-why", "Disable Why? buttons").orFalse
    val noWhatIf = Opts.flag("no-what-if", "Disable What if? toggles").orFalse
    val sweeps = Opts.flag("sweeps", "Enable parameter sweeps").orFalse

    (input, output, titleOpt, themeOpt, noWhy, noWhatIf, sweeps).mapN {
      (i, o, t, th, nw, nwi, s) =>
        SimulationCommand(i, o, t, th, !nw, !nwi, s)
    }
  }

  val command: Command[SimulationCommand] =
    Command("bosatsu-sim", "Generate interactive simulation HTML from .bosatsu files")(opts)

  def parse(args: List[String]): Either[Help, SimulationCommand] =
    command.parse(args)
}
