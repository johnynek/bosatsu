package dev.bosatsu.simulation

import cats.effect.IO
import cats.implicits._
import cats.data.{ValidatedNel, Validated}
import com.monovore.decline._
import java.nio.file.{Files, Path, Paths}

import dev.bosatsu.{
  Package,
  PackageName,
  LocationMap,
  Matchless,
  Identifier,
  RecursionKind,
  Pattern
}
import dev.bosatsu.codegen.js.JsGen
import dev.bosatsu.ui.EmbedGenerator
import dev.bosatsu.Parser

/**
 * Command-line interface for bosatsu-sim.
 *
 * Parses arguments and runs the simulation HTML generation.
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
    for {
      // Read input file
      inputContent <- IO(Files.readString(input))
      fileName = input.getFileName.toString
      packageName = fileName.stripSuffix(".bosatsu").replace("-", "_")

      // Parse the Bosatsu file
      parsed <- IO.fromEither(
        Parser.parse(Package.parser(Some(PackageName.parse(packageName).getOrElse(
          PackageName.parse("Main").get
        ))), inputContent).toEither.leftMap { errs =>
          val errMsg = errs.toList.map(_.showContext(LocationMap.Colorize.None).renderTrim(80)).mkString("; ")
          new RuntimeException(s"Parse errors: $errMsg")
        }
      )

      // Generate HTML from the parsed package
      htmlContent = generateSimulationHtml(parsed._2, fileName)

      // Write output
      _ <- IO(Files.writeString(output, htmlContent))
      _ <- IO(println(s"Generated: $output"))
    } yield ()
  }

  private def generateSimulationHtml(
      parsed: Package.Parsed,
      fileName: String
  ): String = {
    // Extract bindings from the parsed statements
    // For a simple simulation, we extract let bindings directly from the statements
    val bindings = extractBindings(parsed)

    // Generate analyzed bindings (simplified - just extract names and formulas from source)
    val analyses = bindings.map { case (name, formula) =>
      val deps = extractDepsFromFormula(formula, bindings.map(_._1).toSet)
      val kind = if (deps.isEmpty) DerivationAnalyzer.Assumption else DerivationAnalyzer.Computation
      val valueType = inferTypeFromFormula(formula)

      DerivationAnalyzer.AnalyzedBinding(
        name,
        kind,
        deps,
        formula,
        valueType
      )
    }

    // Generate JS code from the source (simplified version)
    val jsBindings = bindings.map { case (name, formula) =>
      val jsName = JsGen.escape(name).name
      val jsValue = formulaToJs(formula, bindings.map(_._1).toSet)
      s"const $jsName = $jsValue;"
    }

    // Build the full JS with derivation tracking
    val computeJs = jsBindings.mkString("\n")

    // Use SimulationGen to create the full HTML
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
   * Extract top-level let bindings from parsed statements.
   */
  private def extractBindings(parsed: Package.Parsed): List[(Identifier.Bindable, String)] = {
    import dev.bosatsu.Statement
    import dev.bosatsu.Declaration

    parsed.program.flatMap {
      case Statement.Bind(bind) =>
        // Extract the name from the pattern
        bind.name match {
          case Pattern.Var(name) =>
            // Get the formula as a string representation
            val formula = exprToString(bind.value)
            Some((name, formula))
          case _ =>
            // Skip complex patterns
            None
        }
      case Statement.Def(_) =>
        // Skip function definitions for now
        None
      case _ => None
    }
  }

  /**
   * Convert a Declaration expression to a string formula.
   */
  private def exprToString(expr: dev.bosatsu.Declaration.NonBinding): String = {
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
        val fnStr = exprToString(fn)
        val argsStr = args.toList.map(exprToString).mkString(", ")
        s"$fnStr($argsStr)"
      case ApplyOp(left, op, right) =>
        val leftStr = exprToString(left)
        val rightStr = exprToString(right)
        s"($leftStr ${op.asString} $rightStr)"
      case Parens(inner) =>
        inner match {
          case nb: NonBinding => exprToString(nb)
          case _ => inner.toDoc.renderTrim(80)
        }
      case IfElse(_, _) =>
        "if-else"  // Simplified
      case ann @ Annotation(term, _) =>
        exprToString(term)
      case _ =>
        expr.toDoc.renderTrim(80)
    }
  }

  /**
   * Extract dependencies from a formula string.
   */
  private def extractDepsFromFormula(
      formula: String,
      knownBindings: Set[Identifier.Bindable]
  ): Set[Identifier.Bindable] = {
    // Simple heuristic: find identifiers that match known bindings
    knownBindings.filter { b =>
      val nameStr = b.asString
      formula.contains(nameStr) && formula != nameStr
    }
  }

  /**
   * Infer type from formula string.
   */
  private def inferTypeFromFormula(formula: String): String = {
    if (formula.matches("-?\\d+")) "number"
    else if (formula.matches("-?\\d+\\.\\d+")) "number"
    else if (formula == "true" || formula == "false") "boolean"
    else if (formula.startsWith("\"")) "string"
    else "any"
  }

  /**
   * Convert a formula to JavaScript.
   */
  private def formulaToJs(
      formula: String,
      knownBindings: Set[Identifier.Bindable]
  ): String = {
    // Handle Bosatsu method-style arithmetic: x.add(y), x.sub(y), x.times(y), x.div(y)
    var js = formula

    // Pattern: x.add(y) -> (x + y)
    js = """\b(\w+)\.add\(([^)]+)\)""".r.replaceAllIn(js, m => s"(${m.group(1)} + ${m.group(2)})")
    js = """\b(\w+)\.sub\(([^)]+)\)""".r.replaceAllIn(js, m => s"(${m.group(1)} - ${m.group(2)})")
    js = """\b(\w+)\.times\(([^)]+)\)""".r.replaceAllIn(js, m => s"(${m.group(1)} * ${m.group(2)})")
    js = """\b(\w+)\.div\(([^)]+)\)""".r.replaceAllIn(js, m => s"Math.trunc(${m.group(1)} / ${m.group(2)})")

    // Handle chained operations like income.sub(deductions) where income is a binding
    // Replace binding names with their escaped JS names
    knownBindings.foreach { b =>
      val nameStr = b.asString
      val escaped = JsGen.escape(b).name
      if (nameStr != escaped) {
        js = js.replaceAll(s"\\b${java.util.regex.Pattern.quote(nameStr)}\\b", escaped)
      }
    }

    js
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

  // Implicit for Path parsing
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
    val titleOpt =
      Opts.option[String]("title", "Page title", "t").orNone
    val themeOpt = Opts
      .option[String]("theme", "Theme: light or dark")
      .withDefault("light")
      .map(Theme.fromString)
    val noWhy =
      Opts.flag("no-why", "Disable Why? buttons").orFalse
    val noWhatIf =
      Opts.flag("no-what-if", "Disable What if? toggles").orFalse
    val sweeps =
      Opts.flag("sweeps", "Enable parameter sweeps").orFalse

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
