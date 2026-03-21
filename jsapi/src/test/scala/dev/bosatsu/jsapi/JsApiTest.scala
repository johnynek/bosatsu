package dev.bosatsu
package jsapi

import scala.scalajs.js

class JsApiTest extends munit.FunSuite {
  private val webDemoKey = "root/webdemo"
  private val webDemoPackage = JsApi.normalize(webDemoKey)
  private val demoSource =
    s"""package $webDemoPackage
      |
      |main = add(40, 2)
      |""".stripMargin

  private val files = js.Dictionary(webDemoKey -> demoSource)

  test("evaluate uses tool eval args") {
    // Pass an explicit normalized package so this regression test isolates
    // the tool-prefix command routing rather than main-file path lookup.
    JsApi.evaluate(webDemoPackage, webDemoKey, files) match {
      case success: JsApi.EvalSuccess =>
        val rendered = success.result.toString
        assert(rendered.contains("42"), rendered)
        assert(rendered.contains("Int"), rendered)
      case err: JsApi.Error =>
        fail(err.error_message)
    }
  }

  test("evaluateJson uses tool json write args") {
    JsApi.evaluateJson(webDemoPackage, webDemoKey, files) match {
      case success: JsApi.EvalSuccess =>
        assertEquals(success.result.asInstanceOf[Double], 42.0)
      case err: JsApi.Error =>
        fail(err.error_message)
    }
  }
}
