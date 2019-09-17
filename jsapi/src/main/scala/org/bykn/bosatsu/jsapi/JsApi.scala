package org.bykn.bosatsu
package jsapi

import scala.scalajs.js

import js.|
import js.annotation._

import cats.implicits._

@JSExportTopLevel("Bosatsu")
object JsApi {

  private val module = new MemoryMain[Either[Throwable, ?], String]

  private def makeInputArgs(keys: Iterable[String]): List[String] =
    keys.iterator.flatMap { key => "--input" :: key :: Nil }.toList

  class Error(val error_message: String) extends js.Object

  class EvalSuccess(val result: js.Any) extends js.Object

  /**
   * mainPackage can be null, in which case we find the package
   * in mainFile
   */
  @JSExport
  def evaluate(mainPackage: String, mainFile: String, files: js.Dictionary[String]): EvalSuccess | Error = {
    val main =
      if (mainPackage != null) "--main" :: mainPackage :: Nil
      else "--main_file" :: mainFile :: Nil
    module.runWith(files)("eval" :: main ::: makeInputArgs(files.keys)) match {
      case Left(err) =>
        new Error(s"error: $err")
      case Right(module.Output.EvaluationResult(res, tpe)) =>
        new EvalSuccess(res.value.toString)
      case Right(other) =>
        new Error(s"internal error. got unexpected result: $other")
    }
  }

  def jsonToAny(j: Json): js.Any =
    j match {
      case Json.JString(s) => s
      case Json.JNumber(d) => d
      case Json.JNumberStr(str) =>
        try str.toDouble
        catch {
          case t: Throwable => str
        }
      case Json.JBool(b) => b
      case Json.JNull => null
      case Json.JArray(items) =>
        val ary = new js.Array[js.Any]
        items.foreach { j => ary += jsonToAny(j) }
        ary
      case Json.JObject(kvs) =>
        js.Dictionary[js.Any](
          kvs.map { case (k, v) =>
            (k, jsonToAny(v))
          } :_*)
    }

  /**
   * mainPackage can be null, in which case we find the package
   * in mainFile
   */
  @JSExport
  def evaluateJson(mainPackage: String, mainFile: String, files: js.Dictionary[String]): EvalSuccess | Error = {
    val main =
      if (mainPackage != null) "--main" :: mainPackage :: Nil
      else "--main_file" :: mainFile :: Nil
    module.runWith(files)("write-json" :: "--output" :: "" :: main ::: makeInputArgs(files.keys)) match {
      case Left(err) =>
        new Error(s"error: $err")
      case Right(module.Output.JsonOutput(json, _)) =>
        new EvalSuccess(jsonToAny(json))
      case Right(other) =>
        new Error(s"internal error. got unexpected result: $other")
    }
  }
}
