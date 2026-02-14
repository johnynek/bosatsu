package dev.bosatsu

import scala.scalajs.js

object StackSafe {
  inline def onStackOverflow[A](
      inline expr: => A
  )(inline onOverflow: => A): A =
    try expr
    catch {
      case _: StackOverflowError =>
        onOverflow
      case jse: js.JavaScriptException if isJsStackOverflow(jse) =>
        onOverflow
    }

  private def isJsStackOverflow(jse: js.JavaScriptException): Boolean = {
    val ex = jse.exception.asInstanceOf[js.Any]
    val isRangeError =
      js.typeOf(js.Dynamic.global.RangeError) != "undefined" &&
        js.special
          .instanceof(ex, js.Dynamic.global.RangeError)
          .asInstanceOf[Boolean]

    if (isRangeError) true
    else {
      val msg = extractMessage(ex).toLowerCase
      msg.contains("maximum call stack size exceeded") ||
      msg.contains("too much recursion")
    }
  }

  private def extractMessage(ex: js.Any): String = {
    val dyn = ex.asInstanceOf[js.Dynamic]
    if (js.typeOf(dyn.message) == "string") dyn.message.asInstanceOf[String]
    else ""
  }
}
