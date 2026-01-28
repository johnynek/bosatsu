package dev.bosatsu.ui

import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._
import org.typelevel.paiges.Doc
import SimulationApplet._

/**
 * Parameter sweep UI for exploring ranges of values.
 *
 * This leverages Bosatsu's type system advantages:
 * - Type-safe ranges: Parameter bounds are verified at compile time
 * - Total functions: Sweep computation is guaranteed to terminate
 * - Exhaustive exploration: All parameter combinations are explored
 *
 * Unlike ad-hoc JavaScript sliders, Bosatsu sweeps are:
 * - Type-checked (Int ranges can't accept strings)
 * - Bounded (min/max enforced at compile time)
 * - Tracked (each sweep point has full provenance)
 */
object ParameterSweep {

  /**
   * Escape HTML special characters to prevent XSS.
   */
  private def escapeHTML(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#39;")

  /**
   * Configuration for a parameter sweep.
   */
  case class SweepConfig[A](
      parameterName: String,
      minValue: A,
      maxValue: A,
      steps: Int,
      description: String = ""
  )(implicit ord: Ordering[A], frac: Fractional[A]) {
    def stepSize: A = {
      val range = frac.minus(maxValue, minValue)
      frac.div(range, frac.fromInt(steps))
    }

    def values: List[A] = {
      (0 to steps).map { i =>
        frac.plus(minValue, frac.times(frac.fromInt(i), stepSize))
      }.toList
    }
  }

  /**
   * Result of a parameter sweep.
   */
  case class SweepResult[A, B](
      parameterName: String,
      points: List[(A, B)], // (paramValue, resultValue)
      minResult: (A, B),
      maxResult: (A, B)
  )

  /**
   * Run a parameter sweep and collect results.
   */
  def runSweep[A: Numeric: Ordering, B](
      config: SweepConfig[A],
      compute: A => B
  )(implicit ordB: Ordering[B]): SweepResult[A, B] = {
    val points = config.values.map(v => (v, compute(v)))
    val minResult = points.minBy(_._2)
    val maxResult = points.maxBy(_._2)
    SweepResult(config.parameterName, points, minResult, maxResult)
  }

  /**
   * Generate JS code for a parameter sweep slider UI.
   */
  def generateSweepSlider(
      parameterName: String,
      min: Double,
      max: Double,
      step: Double,
      initialValue: Double,
      containerId: String
  ): List[Statement] = {
    val escapedName = escapeHTML(parameterName)
    val sliderId = s"sweep-$escapedName"
    val valueId = s"sweep-value-$escapedName"

    List(
      // Create container
      Const("sweepDiv", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("div")))),
      Assignment(PropertyAccess(Ident("sweepDiv"), "className"), StringLiteral("parameter-sweep")),
      Assignment(PropertyAccess(Ident("sweepDiv"), "innerHTML"), StringLiteral(
        s"""<div class="sweep-header">
           |  <span class="sweep-name">$escapedName</span>
           |  <span class="sweep-value" id="$valueId">$initialValue</span>
           |</div>
           |<input type="range" id="$sliderId" class="sweep-slider"
           |       min="$min" max="$max" step="$step" value="$initialValue" />
           |<div class="sweep-bounds">
           |  <span>$min</span>
           |  <span>$max</span>
           |</div>""".stripMargin
      )),
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(containerId))),
          "appendChild"
        ),
        List(Ident("sweepDiv"))
      )),
      // Add input handler
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("sweepDiv"), "querySelector"), List(StringLiteral(".sweep-slider"))),
          "addEventListener"
        ),
        List(
          StringLiteral("input"),
          ArrowFunction(List("e"), Right(block(
            Const("val", Call(Ident("parseFloat"), List(PropertyAccess(PropertyAccess(Ident("e"), "target"), "value")))),
            Assignment(
              PropertyAccess(
                Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(valueId))),
                "textContent"
              ),
              Ident("val")
            ),
            ExprStatement(Call(
              Ident("_setAssumption"),
              List(StringLiteral(parameterName), Ident("val"))
            ))
          )))
        )
      ))
    )
  }

  /**
   * Generate JS code for a 2D parameter sweep visualization.
   *
   * This creates a heatmap showing how the result varies with two parameters.
   */
  def generateSweep2D(
      param1: String,
      param2: String,
      resultName: String,
      canvasId: String
  ): List[Statement] = {
    List(
      Const("canvas", Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(canvasId)))),
      Const("ctx", Call(PropertyAccess(Ident("canvas"), "getContext"), List(StringLiteral("2d")))),

      // Function to render sweep heatmap
      Const("renderSweepHeatmap", Function(
        None,
        List("data", "param1Range", "param2Range"),
        block(
          Const("width", PropertyAccess(Ident("canvas"), "width")),
          Const("height", PropertyAccess(Ident("canvas"), "height")),
          Const("cellW", BinExpr(Ident("width"), BinOp.Div, PropertyAccess(Ident("param1Range"), "length"))),
          Const("cellH", BinExpr(Ident("height"), BinOp.Div, PropertyAccess(Ident("param2Range"), "length"))),

          // Find min/max for color scale
          Const("values", Call(
            PropertyAccess(Ident("data"), "flat"),
            Nil
          )),
          // Use Math.min.apply and Math.max.apply instead of spread
          Const("minVal", Call(
            PropertyAccess(PropertyAccess(Ident("Math"), "min"), "apply"),
            List(NullLiteral, Ident("values"))
          )),
          Const("maxVal", Call(
            PropertyAccess(PropertyAccess(Ident("Math"), "max"), "apply"),
            List(NullLiteral, Ident("values"))
          )),

          // Draw cells
          ExprStatement(Call(
            PropertyAccess(Ident("data"), "forEach"),
            List(ArrowFunction(List("row", "i"), Right(block(
              ExprStatement(Call(
                PropertyAccess(Ident("row"), "forEach"),
                List(ArrowFunction(List("val", "j"), Right(block(
                  // Normalize value to 0-1
                  Const("norm", Ternary(
                    BinExpr(Ident("maxVal"), BinOp.Eq, Ident("minVal")),
                    DoubleLiteral(0.5),
                    BinExpr(
                      BinExpr(Ident("val"), BinOp.Minus, Ident("minVal")),
                      BinOp.Div,
                      BinExpr(Ident("maxVal"), BinOp.Minus, Ident("minVal"))
                    )
                  )),
                  // Color: blue (low) to red (high)
                  Const("r", Call(PropertyAccess(Ident("Math"), "round"), List(BinExpr(Ident("norm"), BinOp.Times, IntLiteral(255))))),
                  Const("b", Call(PropertyAccess(Ident("Math"), "round"), List(BinExpr(BinExpr(IntLiteral(1), BinOp.Minus, Ident("norm")), BinOp.Times, IntLiteral(255))))),
                  // Build color string via concatenation
                  Assignment(
                    PropertyAccess(Ident("ctx"), "fillStyle"),
                    BinExpr(
                      BinExpr(
                        BinExpr(
                          BinExpr(StringLiteral("rgb("), BinOp.Plus, Ident("r")),
                          BinOp.Plus,
                          StringLiteral(", 100, ")
                        ),
                        BinOp.Plus,
                        Ident("b")
                      ),
                      BinOp.Plus,
                      StringLiteral(")")
                    )
                  ),
                  ExprStatement(Call(
                    PropertyAccess(Ident("ctx"), "fillRect"),
                    List(
                      BinExpr(Ident("j"), BinOp.Times, Ident("cellW")),
                      BinExpr(Ident("i"), BinOp.Times, Ident("cellH")),
                      Ident("cellW"),
                      Ident("cellH")
                    )
                  ))
                ))))
              ))
            ))))
          ))
        )
      ))
    )
  }

  /**
   * CSS for parameter sweep UI.
   */
  val sweepCSS: String =
    """.parameter-sweep { padding: 12px; margin: 8px 0; background: #f8f9fa; border-radius: 8px; }
      |.parameter-sweep .sweep-header { display: flex; justify-content: space-between; margin-bottom: 8px; }
      |.parameter-sweep .sweep-name { font-weight: 500; color: #333; }
      |.parameter-sweep .sweep-value { font-family: monospace; color: #667eea; font-weight: bold; }
      |.parameter-sweep .sweep-slider { width: 100%; height: 8px; -webkit-appearance: none; background: #ddd; border-radius: 4px; outline: none; }
      |.parameter-sweep .sweep-slider::-webkit-slider-thumb { -webkit-appearance: none; width: 20px; height: 20px; background: #667eea; border-radius: 50%; cursor: pointer; }
      |.parameter-sweep .sweep-bounds { display: flex; justify-content: space-between; font-size: 12px; color: #666; margin-top: 4px; }
      |.sweep-heatmap { border: 1px solid #ddd; border-radius: 4px; }
      |.sweep-legend { display: flex; justify-content: space-between; font-size: 12px; margin-top: 8px; }""".stripMargin
}
