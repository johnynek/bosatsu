package dev.bosatsu.ui

import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._
import org.typelevel.paiges.Doc
import SimulationApplet._

/**
 * "What if?" assumption toggling UI component.
 *
 * This leverages Bosatsu's type system advantages:
 * - Type-safe assumptions: Each assumption has a known type
 * - Exhaustive recomputation: All derived values are recalculated
 * - Verified dependencies: Changes propagate correctly through the derivation chain
 *
 * Unlike React where state updates are fire-and-forget,
 * Bosatsu tracks the full derivation chain so we can show
 * exactly what changed when an assumption is toggled.
 */
object WhatIfToggle {

  /**
   * Configuration for a "What if?" toggle.
   */
  case class ToggleConfig[A](
      assumptionName: String,
      currentValue: A,
      alternativeValue: A,
      description: String
  )

  /**
   * Result of a "What if?" analysis.
   */
  case class WhatIfResult[A](
      assumptionName: String,
      originalValue: A,
      newValue: A,
      changedDerivations: List[(String, Any, Any)] // (name, oldValue, newValue)
  )

  /**
   * Extract all toggleable assumptions from a derivation.
   */
  def extractToggles[A](d: Derivation[A]): List[Assumption[?]] =
    SimulationApplet.getAssumptions(d).distinctBy(_.name)

  /**
   * Analyze what would change if an assumption had a different value.
   *
   * This is a compile-time analysis that can be done without actually
   * running the simulation, thanks to Bosatsu's total functions.
   */
  def analyzeWhatIf[A, B](
      d: Derivation[A],
      assumptionName: String,
      newValue: B,
      recompute: Map[String, Any] => A
  ): Option[WhatIfResult[B]] = {
    val assumptions = extractToggles(d)
    assumptions.find(_.name == assumptionName) match {
      case Some(a) =>
        val originalValue = a.value.asInstanceOf[B]
        val originalAssumptions = assumptions.map(a => a.name -> a.value).toMap
        val newAssumptions = originalAssumptions + (assumptionName -> newValue)

        // Get original derived values
        val originalDerived = collectDerivedValues(d)

        // This would need the actual recomputation function from the simulation
        // For now, we just track the assumption change
        Some(WhatIfResult(
          assumptionName,
          originalValue,
          newValue,
          Nil // Would be populated by actual recomputation
        ))
      case None => None
    }
  }

  private def collectDerivedValues(d: Derivation[?]): Map[String, Any] = {
    val self = d match {
      case a: Assumption[?] => Map(a.name -> a.value)
      case c: Computed[?] => Map(c.name -> c.value)
      case c: Conditional[?] => Map(c.name -> c.value)
    }
    val deps = d.dependencies.flatMap(collectDerivedValues).toMap
    self ++ deps
  }

  /**
   * Generate JS code for a "What if?" toggle UI.
   */
  def generateToggleUI(
      assumptionName: String,
      valueType: String,
      containerId: String
  ): List[Statement] = {
    valueType match {
      case "boolean" => generateBooleanToggle(assumptionName, containerId)
      case "number" => generateNumberToggle(assumptionName, containerId)
      case _ => generateTextToggle(assumptionName, containerId)
    }
  }

  private def generateBooleanToggle(name: String, containerId: String): List[Statement] = {
    List(
      Const("toggleDiv", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("div")))),
      Assignment(PropertyAccess(Ident("toggleDiv"), "className"), StringLiteral("what-if-toggle")),
      Assignment(PropertyAccess(Ident("toggleDiv"), "innerHTML"), StringLiteral(
        s"""<label class="toggle-label">
           |  <span class="toggle-name">What if $name were </span>
           |  <button class="toggle-btn" data-assumption="$name" data-value="true">true</button>
           |  <button class="toggle-btn" data-assumption="$name" data-value="false">false</button>
           |  <span class="toggle-hint">?</span>
           |</label>""".stripMargin
      )),
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(containerId))),
          "appendChild"
        ),
        List(Ident("toggleDiv"))
      )),
      // Add click handlers
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("toggleDiv"), "querySelectorAll"), List(StringLiteral(".toggle-btn"))),
          "forEach"
        ),
        List(ArrowFunction(List("btn"), Right(block(
          ExprStatement(Call(
            PropertyAccess(Ident("btn"), "addEventListener"),
            List(
              StringLiteral("click"),
              ArrowFunction(List("e"), Right(block(
                Const("newVal", BinExpr(
                  Call(PropertyAccess(PropertyAccess(Ident("e"), "target"), "getAttribute"), List(StringLiteral("data-value"))),
                  BinOp.Eq,
                  StringLiteral("true")
                )),
                ExprStatement(Call(
                  Ident("_setAssumption"),
                  List(StringLiteral(name), Ident("newVal"))
                ))
              )))
            )
          ))
        ))))
      ))
    )
  }

  private def generateNumberToggle(name: String, containerId: String): List[Statement] = {
    List(
      Const("toggleDiv", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("div")))),
      Assignment(PropertyAccess(Ident("toggleDiv"), "className"), StringLiteral("what-if-toggle")),
      Assignment(PropertyAccess(Ident("toggleDiv"), "innerHTML"), StringLiteral(
        s"""<label class="toggle-label">
           |  <span class="toggle-name">What if $name were </span>
           |  <input type="number" class="toggle-input" data-assumption="$name" />
           |  <span class="toggle-hint">?</span>
           |</label>""".stripMargin
      )),
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(containerId))),
          "appendChild"
        ),
        List(Ident("toggleDiv"))
      )),
      // Add input handler
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("toggleDiv"), "querySelector"), List(StringLiteral(".toggle-input"))),
          "addEventListener"
        ),
        List(
          StringLiteral("input"),
          ArrowFunction(List("e"), Right(block(
            Const("newVal", Call(Ident("parseFloat"), List(PropertyAccess(PropertyAccess(Ident("e"), "target"), "value")))),
            IfStatement(
              PrefixExpr(PrefixOp.Not, Call(Ident("isNaN"), List(Ident("newVal")))),
              block(
                ExprStatement(Call(
                  Ident("_setAssumption"),
                  List(StringLiteral(name), Ident("newVal"))
                ))
              ),
              None
            )
          )))
        )
      ))
    )
  }

  private def generateTextToggle(name: String, containerId: String): List[Statement] = {
    List(
      Const("toggleDiv", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("div")))),
      Assignment(PropertyAccess(Ident("toggleDiv"), "className"), StringLiteral("what-if-toggle")),
      Assignment(PropertyAccess(Ident("toggleDiv"), "innerHTML"), StringLiteral(
        s"""<label class="toggle-label">
           |  <span class="toggle-name">What if $name were </span>
           |  <input type="text" class="toggle-input" data-assumption="$name" />
           |  <span class="toggle-hint">?</span>
           |</label>""".stripMargin
      )),
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(containerId))),
          "appendChild"
        ),
        List(Ident("toggleDiv"))
      )),
      // Add input handler
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("toggleDiv"), "querySelector"), List(StringLiteral(".toggle-input"))),
          "addEventListener"
        ),
        List(
          StringLiteral("input"),
          ArrowFunction(List("e"), Right(block(
            ExprStatement(Call(
              Ident("_setAssumption"),
              List(StringLiteral(name), PropertyAccess(PropertyAccess(Ident("e"), "target"), "value"))
            ))
          )))
        )
      ))
    )
  }

  /**
   * CSS for "What if?" toggle UI.
   */
  val toggleCSS: String =
    """.what-if-toggle { padding: 12px; margin: 8px 0; background: #f5f5f5; border-radius: 8px; }
      |.what-if-toggle .toggle-label { display: flex; align-items: center; gap: 8px; }
      |.what-if-toggle .toggle-name { color: #666; font-style: italic; }
      |.what-if-toggle .toggle-btn { padding: 4px 12px; background: #667eea; color: white; border: none; border-radius: 4px; cursor: pointer; }
      |.what-if-toggle .toggle-btn:hover { background: #5568d8; }
      |.what-if-toggle .toggle-btn.active { background: #27ae60; }
      |.what-if-toggle .toggle-input { padding: 4px 8px; border: 1px solid #ddd; border-radius: 4px; width: 100px; }
      |.what-if-toggle .toggle-hint { color: #667eea; font-weight: bold; }""".stripMargin
}
