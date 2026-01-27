package dev.bosatsu.ui

import dev.bosatsu.codegen.js.Code
import dev.bosatsu.codegen.js.Code._
import org.typelevel.paiges.Doc
import cats.data.NonEmptyList

/**
 * Bindings that connect reactive state to DOM updates.
 *
 * StateBinding generates JavaScript code that:
 * - Reads state values
 * - Updates DOM elements when state changes
 * - Handles user input events that update state
 */
object StateBinding {

  /**
   * A binding that updates a DOM element's text content when state changes.
   */
  case class TextBinding(
      stateVar: String,
      elementId: String,
      transform: Option[String] = None
  )

  /**
   * A binding that updates a DOM element's attribute when state changes.
   */
  case class AttributeBinding(
      stateVar: String,
      elementId: String,
      attributeName: String,
      transform: Option[String] = None
  )

  /**
   * A binding that reads input value and updates state.
   */
  case class InputBinding(
      elementId: String,
      stateVar: String,
      eventType: String = "input",
      transform: Option[String] = None
  )

  /**
   * Generate JS code for a reactive state store.
   */
  def generateStateStore(stateVars: Map[String, String]): List[Statement] = {
    // Create initial state object
    val stateObj = ObjectLiteral(stateVars.map { case (name, initialValue) =>
      (name, Call(PropertyAccess(Ident("JSON"), "parse"), List(StringLiteral(initialValue))))
    }.toList)

    // Create listeners map
    val listenersObj = ObjectLiteral(stateVars.keys.map { name =>
      (name, ArrayLiteral(Nil))
    }.toList)

    List(
      // const _state = { ... }
      Const("_state", stateObj),

      // const _listeners = { ... }
      Const("_listeners", listenersObj),

      // function _getState(name) { return _state[name]; }
      Const("_getState", Function(
        None,
        List("name"),
        block(Return(Some(IndexAccess(Ident("_state"), Ident("name")))))
      )),

      // function _setState(name, value) { ... }
      Const("_setState", Function(
        None,
        List("name", "value"),
        block(
          IfStatement(
            BinExpr(IndexAccess(Ident("_state"), Ident("name")), BinOp.NotEq, Ident("value")),
            block(
              Assignment(IndexAccess(Ident("_state"), Ident("name")), Ident("value")),
              ExprStatement(Call(
                PropertyAccess(IndexAccess(Ident("_listeners"), Ident("name")), "forEach"),
                List(ArrowFunction(List("fn"), Left(Call(Ident("fn"), List(Ident("value"))))))
              ))
            ),
            None
          )
        )
      )),

      // function _subscribe(name, fn) { ... }
      Const("_subscribe", Function(
        None,
        List("name", "fn"),
        block(
          ExprStatement(Call(
            PropertyAccess(IndexAccess(Ident("_listeners"), Ident("name")), "push"),
            List(Ident("fn"))
          )),
          ExprStatement(Call(Ident("fn"), List(IndexAccess(Ident("_state"), Ident("name")))))
        )
      ))
    )
  }

  /**
   * Generate JS code for a text binding.
   */
  def generateTextBinding(binding: TextBinding): Statement = {
    val updateExpr: Expression = binding.transform match {
      case Some(t) => Call(Ident(t), List(Ident("v")))
      case None => Ident("v")
    }

    ExprStatement(Call(Ident("_subscribe"), List(
      StringLiteral(binding.stateVar),
      ArrowFunction(List("v"), Right(block(
        Assignment(
          PropertyAccess(
            Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(binding.elementId))),
            "textContent"
          ),
          updateExpr
        )
      )))
    )))
  }

  /**
   * Generate JS code for an attribute binding.
   */
  def generateAttributeBinding(binding: AttributeBinding): Statement = {
    val updateExpr: Expression = binding.transform match {
      case Some(t) => Call(Ident(t), List(Ident("v")))
      case None => Ident("v")
    }

    ExprStatement(Call(Ident("_subscribe"), List(
      StringLiteral(binding.stateVar),
      ArrowFunction(List("v"), Right(block(
        ExprStatement(Call(
          PropertyAccess(
            Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(binding.elementId))),
            "setAttribute"
          ),
          List(StringLiteral(binding.attributeName), updateExpr)
        ))
      )))
    )))
  }

  /**
   * Generate JS code for an input binding.
   */
  def generateInputBinding(binding: InputBinding): Statement = {
    val valueExpr: Expression = binding.transform match {
      case Some(t) => Call(Ident(t), List(PropertyAccess(PropertyAccess(Ident("e"), "target"), "value")))
      case None => PropertyAccess(PropertyAccess(Ident("e"), "target"), "value")
    }

    ExprStatement(Call(
      PropertyAccess(
        Call(PropertyAccess(Ident("document"), "getElementById"), List(StringLiteral(binding.elementId))),
        "addEventListener"
      ),
      List(
        StringLiteral(binding.eventType),
        ArrowFunction(List("e"), Right(block(
          ExprStatement(Call(Ident("_setState"), List(StringLiteral(binding.stateVar), valueExpr)))
        )))
      )
    ))
  }

  /**
   * Generate a complete reactive module.
   */
  def generateModule(
      moduleName: String,
      stateVars: Map[String, String],
      textBindings: List[TextBinding],
      inputBindings: List[InputBinding]
  ): Doc = {
    val storeCode = generateStateStore(stateVars)

    val bindingStatements: List[Statement] = textBindings.map(generateTextBinding) ++ inputBindings.map(generateInputBinding)

    val initFn = Const("init", Function(
      None,
      Nil,
      if (bindingStatements.isEmpty) block(ExprStatement(Ident("undefined")))
      else Block(NonEmptyList.fromListUnsafe(bindingStatements))
    ))

    val exportStmt = Assignment(
      PropertyAccess(Ident("window"), moduleName),
      ObjectLiteral(List(
        ("init", Ident("init")),
        ("getState", Ident("_getState")),
        ("setState", Ident("_setState")),
        ("subscribe", Ident("_subscribe"))
      ))
    )

    val allStatements = storeCode ++ List(initFn, exportStmt)

    Code.toDoc(Statements(allStatements))
  }
}
