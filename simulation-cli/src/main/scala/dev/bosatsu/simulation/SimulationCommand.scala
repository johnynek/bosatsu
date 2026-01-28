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
  Par,
  TypedExpr,
  MatchlessFromTypedExpr,
  Evaluation,
  Predef,
  Numeric
}
import dev.bosatsu.codegen.js.JsGen
import dev.bosatsu.ui.EmbedGenerator
import dev.bosatsu.Parser

/**
 * Command-line interface for bosatsu-sim.
 *
 * Generates interactive HTML simulations from Bosatsu files using function-based
 * simulations with Bosatsu config files.
 *
 * Architecture:
 * - Simulation file (.bosatsu): Contains a pure function from inputs to outputs
 * - Config file (.sim.bosatsu): Contains UI metadata (labels, ranges, widgets)
 *
 * The function parameters ARE the inputs. The return type fields ARE the outputs.
 * TypedExpr.freeVarsSet provides dependency analysis.
 */
case class SimulationCommand(
    input: Path,           // Simulation file (.bosatsu)
    config: Path,          // Config file (.sim.bosatsu)
    output: Path,          // Output HTML file
    title: Option[String],
    theme: SimulationCommand.Theme,
    showWhy: Boolean,
    showWhatIf: Boolean,
    showSweeps: Boolean,
    showCanvas: Boolean
) {

  def run: IO[Unit] = {
    given ec: Par.EC = Par.ecFromExecutionContext(
      using scala.concurrent.ExecutionContext.global
    )

    for {
      // Read both files
      inputContent <- IO(Files.readString(input))
      configContent <- IO(Files.readString(config))

      // Parse both files
      inputParsed <- IO.fromEither(parseFile(inputContent, input.getFileName.toString))
      configParsed <- IO.fromEither(parseFile(configContent, config.getFileName.toString))

      // Create location maps
      inputLM = LocationMap(inputContent)
      configLM = LocationMap(configContent)

      // Type check both files together
      inputFileName = input.getFileName.toString.stripSuffix(".bosatsu").replace("-", "_")
      configFileName = config.getFileName.toString.stripSuffix(".bosatsu").replace("-", "_")

      typeChecked <- IO.fromEither {
        val packs = NonEmptyList.of(
          ((inputFileName, inputLM), inputParsed._2),
          ((configFileName, configLM), configParsed._2)
        )
        PackageMap.typeCheckParsed(packs, Nil, inputFileName).toEither.leftMap { errs =>
          val sourceMap: Map[PackageName, (LocationMap, String)] = Map(
            inputParsed._2.name -> (inputLM, inputFileName),
            configParsed._2.name -> (configLM, configFileName)
          )
          val errMsg = errs.toList.map(_.message(sourceMap, LocationMap.Colorize.None)).mkString("; ")
          new RuntimeException(s"Type check errors: $errMsg")
        }
      }

      // Get the typed simulation package
      simPackage = typeChecked.toMap.get(inputParsed._2.name).getOrElse(
        throw new RuntimeException(s"Package ${inputParsed._2.name} not found after type checking")
      )

      // Get the config package
      configPackage = typeChecked.toMap.get(configParsed._2.name).getOrElse(
        throw new RuntimeException(s"Config package ${configParsed._2.name} not found after type checking")
      )

      // Evaluate the 'config' binding from the config package to get runtime Value
      ev = Evaluation(PackageMap.toAnyTyped(typeChecked), Predef.jvmExternals ++ Numeric.jvmExternals)
      configValue <- IO.fromEither {
        ev.evaluateName(configParsed._2.name, Identifier.Name("config")) match {
          case Some((evalValue, _)) => Right(evalValue.value)  // Force evaluation
          case None => Left(new RuntimeException(
            s"No 'config' binding found in ${configParsed._2.name}. " +
            "Config file must have: config = SimConfig(...)"
          ))
        }
      }

      // Extract SimConfig from Value
      simConfig = ConfigExtractor.extractSimConfig(configValue)
      _ <- IO(println(s"Config loaded: ${simConfig.name} (function: ${simConfig.functionName})"))
      _ <- IO(println(s"  Inputs: ${simConfig.inputs.map(_._1).mkString(", ")}"))
      _ <- IO(println(s"  Outputs: ${simConfig.outputs.map(_._1).mkString(", ")}"))

      // Find the function definition in the simulation package
      funcDefs = simPackage.lets.filter { case (name, _, expr) =>
        // Look for function definitions (TypedExpr.AnnotatedLambda)
        expr match {
          case _: TypedExpr.AnnotatedLambda[_] => true
          case _ => false
        }
      }

      _ <- IO(println(s"Found ${funcDefs.size} function(s) in ${inputParsed._2.name}:"))
      _ <- IO(funcDefs.foreach { case (name, _, _) => println(s"  - ${name.asString}") })

      // For now, use the first function as the simulation
      mainFunc = funcDefs.headOption.getOrElse(
        throw new RuntimeException(s"No function definitions found in ${inputParsed._2.name}")
      )
      funcName = mainFunc._1.asString
      funcExpr = mainFunc._3

      // Extract function parameter names and types
      funcParams = extractFunctionParams(funcExpr)
      _ <- IO(println(s"Function '$funcName' parameters: ${funcParams.map(_._1).mkString(", ")}"))

      // Use TypedExpr.freeVarsSet to get dependencies within the function body
      // (This is the principled approach - no regex parsing needed)
      funcBody = getFunctionBody(funcExpr)
      bodyDeps = TypedExpr.freeVarsSet(List(funcBody))
      _ <- IO(println(s"Function body dependencies: ${bodyDeps.map(_.asString).mkString(", ")}"))

      // Generate HTML using the function-based approach with config values
      htmlContent = generateFunctionBasedHtml(
        simPackage,
        typeChecked,
        funcName,
        funcParams,
        simConfig
      )

      // Write output
      _ <- IO(Files.writeString(output, htmlContent))
      _ <- IO(println(s"Generated: $output"))
    } yield ()
  }

  private def parseFile(content: String, fileName: String): Either[Throwable, (LocationMap, Package.Parsed)] = {
    Parser.parse(Package.parser(None), content).toEither.leftMap { errs =>
      val lm = LocationMap(content)
      val errMsg = errs.toList.map(_.showContext(LocationMap.Colorize.None).renderTrim(80)).mkString("; ")
      new RuntimeException(s"Parse error in $fileName: $errMsg")
    }.map { case (_, parsed) =>
      (LocationMap(content), parsed)
    }
  }

  /**
   * Extract function parameter names and types from a TypedExpr.AnnotatedLambda.
   * AnnotatedLambda has: args: NonEmptyList[(Bindable, Type)], expr: TypedExpr[T], tag: T
   */
  private def extractFunctionParams(expr: TypedExpr[Any]): List[(String, String)] = {
    expr match {
      case TypedExpr.AnnotatedLambda(params, _, _) =>
        params.toList.map { case (name, tpe) =>
          (name.asString, typeToString(tpe))
        }
      case _ => Nil
    }
  }

  /**
   * Get the body of a function (the part inside the lambda).
   */
  private def getFunctionBody(expr: TypedExpr[Any]): TypedExpr[Any] = {
    expr match {
      case TypedExpr.AnnotatedLambda(_, body, _) => body
      case other => other
    }
  }

  /**
   * Convert a Type to a human-readable string.
   */
  private def typeToString(tpe: dev.bosatsu.rankn.Type): String = {
    tpe match {
      case dev.bosatsu.rankn.Type.TyConst(tc) => tc.toDefined.name.asString
      case _ => tpe.toString
    }
  }

  /**
   * Generate HTML for a function-based simulation.
   */
  private def generateFunctionBasedHtml(
      simPackage: Package.Typed[Any],
      fullPackageMap: PackageMap.Inferred,
      funcName: String,
      funcParams: List[(String, String)],
      simConfig: ConfigExtractor.SimConfig
  )(implicit ec: Par.EC): String = {
    // Compile to Matchless IR
    val matchlessCompiled = MatchlessFromTypedExpr.compile((),
      PackageMap.toAnyTyped(fullPackageMap)
    )

    val packageBindings = matchlessCompiled.getOrElse(simPackage.name, Nil)

    // Use renderStatements (not renderModule) to avoid ES6 exports
    // This is the principled approach - we generate the correct code structure
    // rather than stripping exports with regex afterwards
    //
    // For proper module bundling (imports/exports), use a bundler like esbuild
    // as a separate build step - that's not our job here.
    val computeJs = JsGen.renderStatements(packageBindings)

    // Build analysis for UI generation - use config inputs
    val analyses = simConfig.inputs.map { case (paramName, inputConfig) =>
      DerivationAnalyzer.AnalyzedBinding(
        Identifier.Name(paramName),
        DerivationAnalyzer.Assumption,  // Function params are inputs (assumptions)
        Set.empty,
        paramName,  // Display formula is just the name for inputs
        "number"    // All inputs are numeric for now
      )
    }

    val genConfig = SimulationGen.SimConfig(
      title = title.getOrElse(simConfig.name),
      theme = theme match {
        case SimulationCommand.Theme.Light => EmbedGenerator.LightTheme
        case SimulationCommand.Theme.Dark  => EmbedGenerator.DarkTheme
      },
      showWhy = showWhy,
      showWhatIf = showWhatIf,
      showSweeps = showSweeps,
      showCanvas = showCanvas
    )

    // Generate with function call pattern using config for UI metadata
    SimulationGen.generateFunctionBasedWithConfig(
      funcName,
      funcParams,
      simConfig,
      analyses,
      computeJs,
      genConfig
    )
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
    val configOpt = Opts.argument[Path]("config")
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
    val canvas = Opts.flag("canvas", "Enable canvas visualization").orFalse

    (input, configOpt, output, titleOpt, themeOpt, noWhy, noWhatIf, sweeps, canvas).mapN {
      (i, c, o, t, th, nw, nwi, s, cv) =>
        SimulationCommand(i, c, o, t, th, !nw, !nwi, s, cv)
    }
  }

  val command: Command[SimulationCommand] =
    Command("bosatsu-sim", "Generate interactive simulation HTML from .bosatsu files")(opts)

  def parse(args: List[String]): Either[Help, SimulationCommand] =
    command.parse(args)
}
