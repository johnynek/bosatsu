package dev.bosatsu
package jsapi

import scala.scalajs.js.annotation.JSExportTopLevel

/** The API exported to JavaScript as `Bosatsu`, using this build's info.
  *
  * This is the only jsapi file that uses the generated `BuildInfo`, which
  * changes with every commit, so the rest of jsapi doesn't depend on it.
  */
@JSExportTopLevel("Bosatsu")
object BosatsuJsApi extends JsApi(BuildInfo)
