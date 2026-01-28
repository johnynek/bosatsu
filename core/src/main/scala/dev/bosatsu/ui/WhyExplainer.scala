package dev.bosatsu.ui

import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._
import org.typelevel.paiges.Doc
import SimulationApplet._

/**
 * "Why?" button UI component that explains derivation chains.
 *
 * This is a key provenance feature that leverages Bosatsu's advantages:
 * - Type-safe derivations: The derivation chain is tracked at compile time
 * - Exhaustive cases: All derivation types (Assumption, Computed, Conditional) are handled
 * - Total functions: Explanation generation is guaranteed to terminate
 *
 * Unlike React where you'd need runtime dependency tracking with string paths,
 * Bosatsu's type system ensures derivation chains are always valid and complete.
 */
object WhyExplainer {

  /**
   * Explanation format for different display contexts.
   */
  sealed trait ExplanationFormat
  case object Plain extends ExplanationFormat
  case object HTML extends ExplanationFormat
  case object Markdown extends ExplanationFormat

  /**
   * Generate an explanation for a derivation.
   */
  def explain(d: Derivation[?], format: ExplanationFormat = Plain): String = {
    format match {
      case Plain => explainPlain(d, 0)
      case HTML => explainHTML(d, 0)
      case Markdown => explainMarkdown(d, 0)
    }
  }

  private def explainPlain(d: Derivation[?], depth: Int): String = {
    val indent = "  " * depth
    val header = d match {
      case a: Assumption[?] => s"${a.name} = ${a.value}"
      case c: Computed[?] => s"${c.name} = ${c.formula}"
      case c: Conditional[?] =>
        val branch = if (c.condition.value) c.trueBranch else c.falseBranch
        s"${c.name}: $branch (${c.conditionName}=${c.condition.value})"
    }
    val self = s"$indent$header"
    val deps = d.dependencies.map(explainPlain(_, depth + 1))
    if (deps.isEmpty) self else (self :: deps).mkString("\n")
  }

  private def explainHTML(d: Derivation[?], depth: Int): String = {
    val cls = d match {
      case _: Assumption[?] => "assumption"
      case _: Computed[?] => "computed"
      case _: Conditional[?] => "conditional"
    }
    val header = d match {
      case a: Assumption[?] =>
        s"""<span class="name">${escapeHTML(a.name)}</span> = <span class="value">${escapeHTML(a.value.toString)}</span> <span class="tag">assumption</span>"""
      case c: Computed[?] =>
        s"""<span class="name">${escapeHTML(c.name)}</span> = <span class="formula">${escapeHTML(c.formula)}</span> → <span class="value">${escapeHTML(c.value.toString)}</span>"""
      case c: Conditional[?] =>
        val branch = if (c.condition.value) c.trueBranch else c.falseBranch
        s"""<span class="name">${escapeHTML(c.name)}</span>: <span class="branch">${escapeHTML(branch)}</span> <span class="condition">(${escapeHTML(c.conditionName)}=${c.condition.value})</span>"""
    }
    val deps = d.dependencies.map(explainHTML(_, depth + 1))
    if (deps.isEmpty) {
      s"""<div class="derivation $cls" style="margin-left: ${depth * 20}px">$header</div>"""
    } else {
      s"""<div class="derivation $cls" style="margin-left: ${depth * 20}px">$header</div>${deps.mkString("")}"""
    }
  }

  private def explainMarkdown(d: Derivation[?], depth: Int): String = {
    val indent = "  " * depth
    val bullet = if (depth == 0) "" else "- "
    val header = d match {
      case a: Assumption[?] => s"**${a.name}** = `${a.value}` _(assumption)_"
      case c: Computed[?] => s"**${c.name}** = `${c.formula}` → `${c.value}`"
      case c: Conditional[?] =>
        val branch = if (c.condition.value) c.trueBranch else c.falseBranch
        s"**${c.name}**: _${branch}_ (${c.conditionName}=${c.condition.value})"
    }
    val self = s"$indent$bullet$header"
    val deps = d.dependencies.map(explainMarkdown(_, depth + 1))
    if (deps.isEmpty) self else (self :: deps).mkString("\n")
  }

  private def escapeHTML(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")

  /**
   * Generate JS code for a "Why?" button that displays derivation explanations.
   */
  def generateWhyButton(valueName: String, elementId: String): List[Statement] = {
    List(
      // Create button element
      Const("whyBtn", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("button")))),
      Assignment(PropertyAccess(Ident("whyBtn"), "textContent"), StringLiteral("Why?")),
      Assignment(PropertyAccess(Ident("whyBtn"), "className"), StringLiteral("why-button")),
      ExprStatement(Call(
        PropertyAccess(Ident("whyBtn"), "addEventListener"),
        List(
          StringLiteral("click"),
          ArrowFunction(Nil, Right(block(
            ExprStatement(Call(
              Ident("showWhyExplanation"),
              List(StringLiteral(valueName))
            ))
          )))
        )
      )),
      ExprStatement(Call(
        PropertyAccess(
          Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(elementId))),
          "appendChild"
        ),
        List(Ident("whyBtn"))
      ))
    )
  }

  /**
   * Generate JS code for the "Why?" explanation display modal.
   */
  def generateWhyModal(): List[Statement] = {
    List(
      // CSS for modal
      Const("whyStyle", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("style")))),
      Assignment(PropertyAccess(Ident("whyStyle"), "textContent"), StringLiteral(whyModalCSS)),
      ExprStatement(Call(
        PropertyAccess(PropertyAccess(Ident("document"), "head"), "appendChild"),
        List(Ident("whyStyle"))
      )),

      // Modal container
      Const("whyModal", Call(PropertyAccess(Ident("document"), "createElement"), List(StringLiteral("div")))),
      Assignment(PropertyAccess(Ident("whyModal"), "id"), StringLiteral("why-modal")),
      Assignment(PropertyAccess(Ident("whyModal"), "className"), StringLiteral("why-modal hidden")),
      Assignment(PropertyAccess(Ident("whyModal"), "innerHTML"), StringLiteral(whyModalHTML)),
      ExprStatement(Call(
        PropertyAccess(PropertyAccess(Ident("document"), "body"), "appendChild"),
        List(Ident("whyModal"))
      ))
    )
  }

  val whyModalCSS: String =
    """.why-button { margin-left: 8px; padding: 2px 8px; font-size: 12px; background: #667eea; color: white; border: none; border-radius: 4px; cursor: pointer; }
      |.why-button:hover { background: #5568d8; }
      |.why-modal { position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5); display: flex; align-items: center; justify-content: center; z-index: 1000; }
      |.why-modal.hidden { display: none; }
      |.why-modal-content { background: white; border-radius: 12px; padding: 24px; max-width: 600px; max-height: 80vh; overflow-y: auto; box-shadow: 0 10px 40px rgba(0,0,0,0.2); }
      |.why-modal-content h3 { margin-top: 0; color: #333; }
      |.why-modal-content .derivation { padding: 8px 12px; margin: 4px 0; border-radius: 6px; font-family: monospace; }
      |.why-modal-content .assumption { background: #e8f5e9; border-left: 3px solid #27ae60; }
      |.why-modal-content .computed { background: #e3f2fd; border-left: 3px solid #2196f3; }
      |.why-modal-content .conditional { background: #fff3e0; border-left: 3px solid #ff9800; }
      |.why-modal-content .name { font-weight: bold; color: #333; }
      |.why-modal-content .value { color: #667eea; }
      |.why-modal-content .formula { color: #666; }
      |.why-modal-content .tag { font-size: 10px; background: #27ae60; color: white; padding: 2px 6px; border-radius: 4px; }
      |.why-modal-content .close-btn { margin-top: 16px; padding: 8px 16px; background: #667eea; color: white; border: none; border-radius: 6px; cursor: pointer; }
      |.why-modal-content .close-btn:hover { background: #5568d8; }""".stripMargin

  private val whyModalHTML: String =
    """<div class="why-modal-content">
      |  <h3>Why this value?</h3>
      |  <div id="why-explanation"></div>
      |  <button id="why-modal-close" class="close-btn">Close</button>
      |</div>""".stripMargin

  /**
   * Generate the showWhyExplanation JS function.
   */
  def generateShowWhyFunction(): Statement = {
    Const("showWhyExplanation", Function(
      None,
      List("valueName"),
      block(
        // Get derivation from state
        Const("derivation", Call(Ident("_getDerivation"), List(Ident("valueName")))),
        IfStatement(
          Ident("derivation"),
          block(
            // Generate explanation HTML
            Const("html", Call(Ident("_explainDerivation"), List(Ident("derivation"), IntLiteral(0)))),
            Assignment(
              PropertyAccess(
                Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral("why-explanation"))),
                "innerHTML"
              ),
              Ident("html")
            ),
            // Show modal
            ExprStatement(Call(
              PropertyAccess(
                PropertyAccess(
                  Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral("why-modal"))),
                  "classList"
                ),
                "remove"
              ),
              List(StringLiteral("hidden"))
            ))
          ),
          None
        )
      )
    ))
  }

  /**
   * Generate the _formatDerivationHeader JS helper function.
   * This formats the header text for a derivation node.
   */
  def generateFormatDerivationHeaderFunction(): Statement = {
    Const("_formatDerivationHeader", Function(
      None,
      List("d"),
      block(
        IfStatement(
          BinExpr(PropertyAccess(Ident("d"), "type"), BinOp.Eq, StringLiteral("assumption")),
          block(Return(Some(BinExpr(
            BinExpr(
              BinExpr(
                StringLiteral("<span class=\"name\">"),
                BinOp.Plus,
                PropertyAccess(Ident("d"), "name")
              ),
              BinOp.Plus,
              StringLiteral("</span> = <span class=\"value\">")
            ),
            BinOp.Plus,
            BinExpr(
              PropertyAccess(Ident("d"), "value"),
              BinOp.Plus,
              StringLiteral("</span> <span class=\"tag\">assumption</span>")
            )
          )))),
          None
        ),
        IfStatement(
          BinExpr(PropertyAccess(Ident("d"), "type"), BinOp.Eq, StringLiteral("computed")),
          block(Return(Some(BinExpr(
            BinExpr(
              BinExpr(
                BinExpr(
                  StringLiteral("<span class=\"name\">"),
                  BinOp.Plus,
                  PropertyAccess(Ident("d"), "name")
                ),
                BinOp.Plus,
                StringLiteral("</span> = <span class=\"formula\">")
              ),
              BinOp.Plus,
              PropertyAccess(Ident("d"), "formula")
            ),
            BinOp.Plus,
            BinExpr(
              StringLiteral("</span> → <span class=\"value\">"),
              BinOp.Plus,
              BinExpr(
                PropertyAccess(Ident("d"), "value"),
                BinOp.Plus,
                StringLiteral("</span>")
              )
            )
          )))),
          None
        ),
        // Default: conditional
        Return(Some(BinExpr(
          BinExpr(
            StringLiteral("<span class=\"name\">"),
            BinOp.Plus,
            PropertyAccess(Ident("d"), "name")
          ),
          BinOp.Plus,
          StringLiteral("</span>")
        )))
      )
    ))
  }

  /**
   * Generate the _explainDerivation JS helper function.
   */
  def generateExplainDerivationFunction(): Statement = {
    Const("_explainDerivation", Function(
      None,
      List("d", "depth"),
      block(
        Const("marginLeft", BinExpr(Ident("depth"), BinOp.Times, IntLiteral(20))),
        Const("cls", Ternary(
          BinExpr(PropertyAccess(Ident("d"), "type"), BinOp.Eq, StringLiteral("assumption")),
          StringLiteral("assumption"),
          Ternary(
            BinExpr(PropertyAccess(Ident("d"), "type"), BinOp.Eq, StringLiteral("computed")),
            StringLiteral("computed"),
            StringLiteral("conditional")
          )
        )),
        Const("header", Call(Ident("_formatDerivationHeader"), List(Ident("d")))),
        // Build HTML string using concatenation
        Const("self", BinExpr(
          BinExpr(
            BinExpr(
              BinExpr(
                BinExpr(
                  StringLiteral("<div class=\"derivation "),
                  BinOp.Plus,
                  Ident("cls")
                ),
                BinOp.Plus,
                StringLiteral("\" style=\"margin-left: ")
              ),
              BinOp.Plus,
              Ident("marginLeft")
            ),
            BinOp.Plus,
            StringLiteral("px\">")
          ),
          BinOp.Plus,
          BinExpr(Ident("header"), BinOp.Plus, StringLiteral("</div>"))
        )),
        Const("deps", Ternary(
          PropertyAccess(Ident("d"), "deps"),
          Call(
            PropertyAccess(
              Call(
                PropertyAccess(PropertyAccess(Ident("d"), "deps"), "map"),
                List(ArrowFunction(List("dep"), Left(
                  Call(Ident("_explainDerivation"), List(Ident("dep"), BinExpr(Ident("depth"), BinOp.Plus, IntLiteral(1))))
                )))
              ),
              "join"
            ),
            List(StringLiteral(""))
          ),
          StringLiteral("")
        )),
        Return(Some(BinExpr(Ident("self"), BinOp.Plus, Ident("deps"))))
      )
    ))
  }
}
