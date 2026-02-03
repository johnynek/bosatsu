package dev.bosatsu.service

import cats.data.NonEmptyList
import cats.Show
import dev.bosatsu._
import dev.bosatsu.codegen.js.JsGen
import dev.bosatsu.Identifier.Bindable

/**
 * Compiles Bosatsu service handlers to JavaScript.
 *
 * Uses the standard Bosatsu compilation pipeline:
 * TypedExpr -> MatchlessFromTypedExpr -> JsGen
 */
object ServiceBuilder {

  /**
   * Compile handlers from source code.
   */
  def compileHandlers(
    source: String,
    sourceFile: String
  )(implicit ec: Par.EC): Either[String, List[CompiledHandler]] = {
    // Parse the source file
    parseFile(source, sourceFile) match {
      case Left(err) => Left(err.getMessage)
      case Right((lm, parsed)) =>
        // Type check
        val fileName = sourceFile.stripSuffix(".bosatsu").replace("-", "_")
        val packs = NonEmptyList.one(((fileName, lm), parsed))

        PackageMap.typeCheckParsed(packs, Nil, fileName).toEither match {
          case Left(errors) =>
            val sourceMap: Map[PackageName, (LocationMap, String)] = Map(
              parsed.name -> (lm, fileName)
            )
            Left(errors.toList.map(_.message(sourceMap, LocationMap.Colorize.None)).mkString("\n"))

          case Right(typeChecked) =>
            // Get the typed package
            typeChecked.toMap.get(parsed.name) match {
              case None =>
                Left(s"Package ${parsed.name} not found after type checking")
              case Some(typedPackage) =>
                // Compile to Matchless IR
                val matchlessCompiled = MatchlessFromTypedExpr.compile((),
                  PackageMap.toAnyTyped(typeChecked)
                )

                // Get bindings for this package
                val packageBindings = matchlessCompiled.getOrElse(parsed.name, Nil)

                // Find all handlers (functions - AnnotatedLambda)
                val handlers = typedPackage.lets.flatMap { case (name, _, typedExpr) =>
                  // Only include functions (lambdas)
                  typedExpr match {
                    case _: TypedExpr.AnnotatedLambda[?] | _: TypedExpr.Generic[?] =>
                      // Find the matchless binding
                      val matchlessBinding = packageBindings.find(_._1 == name)

                      // Generate JS code for this handler
                      val jsCode = matchlessBinding match {
                        case Some((_, matchlessExpr)) =>
                          JsGen.renderBinding(name, matchlessExpr)
                        case None =>
                          s"// Handler ${name.asString} not found in compiled output"
                      }

                      // Analyze the handler
                      val analysis = ServiceAnalyzer.analyzeHandler(name.asString, sourceFile, typedExpr, BatchConfig.default)

                      // Extract parameters
                      val params = extractParams(typedExpr)

                      Some(CompiledHandler(
                        name = name.asString,
                        params = params,
                        jsCode = jsCode,
                        analysis = analysis
                      ))
                    case _ => None
                  }
                }

                if (handlers.isEmpty) {
                  Left("No handlers (functions) found")
                } else {
                  Right(handlers)
                }
            }
        }
    }
  }

  /**
   * Build source code to deployable JS.
   */
  def build(
    source: String,
    sourceFile: String,
    target: BuildTarget
  )(implicit ec: Par.EC): Either[String, BuildResult] = {
    compileHandlers(source, sourceFile).map { handlers =>
      val runtime = JsGen.renderRuntime

      val handlerCode = handlers.map { h =>
        s"""// Handler: ${h.name}
${h.jsCode}

module.exports.${h.name} = ${h.name};
"""
      }.mkString("\n")

      val targetWrapper = target match {
        case BuildTarget.Standalone =>
          s"""$runtime

$handlerCode
"""
        case BuildTarget.Vercel =>
          s"""$runtime

$handlerCode

// Vercel serverless handler wrapper
module.exports.default = async (req, res) => {
  const { handler, ...args } = req.body;
  const fn = module.exports[handler];
  if (!fn) {
    res.status(404).json({ error: 'Handler not found' });
    return;
  }
  try {
    const result = fn(args);
    res.status(200).json({ result });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
};
"""
        case BuildTarget.AwsLambda =>
          s"""$runtime

$handlerCode

// AWS Lambda handler wrapper
exports.handler = async (event) => {
  const { handler, ...args } = JSON.parse(event.body || '{}');
  const fn = module.exports[handler];
  if (!fn) {
    return {
      statusCode: 404,
      body: JSON.stringify({ error: 'Handler not found' })
    };
  }
  try {
    const result = fn(args);
    return {
      statusCode: 200,
      body: JSON.stringify({ result })
    };
  } catch (error) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: error.message })
    };
  }
};
"""
      }

      BuildResult(
        handlers = handlers.map(_.name),
        jsCode = targetWrapper,
        target = target
      )
    }
  }

  /**
   * Parse a Bosatsu source file.
   */
  private def parseFile(content: String, fileName: String): Either[Throwable, (LocationMap, Package.Parsed)] = {
    Parser.parse(Package.parser(None), content).toEither.left.map { errs =>
      val lm = LocationMap(content)
      val errMsg = errs.toList.map(_.showContext(LocationMap.Colorize.None).renderTrim(80)).mkString("; ")
      new RuntimeException(s"Parse error in $fileName: $errMsg")
    }.map { case (_, parsed) =>
      (LocationMap(content), parsed)
    }
  }

  /**
   * Extract parameters from a typed expression (lambda).
   */
  private def extractParams(expr: TypedExpr[Any]): List[HandlerParam] = {
    expr match {
      case TypedExpr.AnnotatedLambda(args, body, _) =>
        // args is NonEmptyList[(Bindable, Type)]
        // For now we just extract the names - real implementation would check types
        args.toList.map { case (name, _) =>
          HandlerParam(name, isInterface = false)
        }
      case TypedExpr.Generic(_, inner) =>
        extractParams(inner)
      case _ =>
        // For now, return empty list - we'd need to analyze the type to extract params
        Nil
    }
  }
}
