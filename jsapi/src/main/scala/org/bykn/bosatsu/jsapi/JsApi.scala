package org.bykn.bosatsu
package jsapi

import org.typelevel.paiges.Doc

import scala.scalajs.js

import js.|
import js.annotation._

import cats.implicits._

@JSExportTopLevel("Bosatsu")
object JsApi {

  private def splitPath(p: String): List[String] =
    p.split("/", -1).toList.map(_.toLowerCase.capitalize)

  private val module = new MemoryMain[Either[Throwable, *], String](splitPath)

  private def makeInputArgs(keys: Iterable[String]): List[String] =
    keys.iterator.flatMap { key => "--input" :: key :: Nil }.toList

  class Error(val error_message: String) extends js.Object

  class EvalSuccess(val result: js.Any) extends js.Object

  /** mainPackage can be null, in which case we find the package in mainFile
    */
  @JSExport
  def evaluate(
      mainPackage: String,
      mainFile: String,
      files: js.Dictionary[String]
  ): EvalSuccess | Error = {
    val baseArgs = "--package_root" :: "" :: "--color" :: "html" :: Nil
    val main =
      if (mainPackage != null) "--main" :: mainPackage :: baseArgs
      else "--main_file" :: mainFile :: baseArgs
    module.runWith(files)("eval" :: main ::: makeInputArgs(files.keys)) match {
      case Left(err) =>
        new Error(s"error: ${err.getMessage}")
      case Right(module.Output.EvaluationResult(_, tpe, resDoc)) =>
        val tDoc = rankn.Type.fullyResolvedDocument.document(tpe)
        val doc =
          resDoc.value + (Doc.lineOrEmpty + Doc.text(": ") + tDoc).nested(4)
        new EvalSuccess(doc.render(80))
      case Right(other) =>
        new Error(s"internal error. got unexpected result: $other")
    }
  }

  def jsonToAny(j: Json): js.Any =
    j match {
      case Json.JString(s)      => s
      case Json.JNumberStr(str) =>
        // javascript only really has doubles
        try str.toDouble
        catch {
          case (_: NumberFormatException) => str
        }
      case Json.JBool.True  => true
      case Json.JBool.False => false
      case Json.JNull       => null
      case Json.JArray(items) =>
        val ary = new js.Array[js.Any]
        items.foreach { j => ary += jsonToAny(j) }
        ary
      case Json.JObject(kvs) =>
        js.Dictionary[js.Any](kvs.map { case (k, v) =>
          (k, jsonToAny(v))
        }: _*)
    }

  /** mainPackage can be null, in which case we find the package in mainFile
    */
  @JSExport
  def evaluateJson(
      mainPackage: String,
      mainFile: String,
      files: js.Dictionary[String]
  ): EvalSuccess | Error = {
    val baseArgs = "--package_root" :: "" :: "--color" :: "html" :: Nil
    val main =
      if (mainPackage != null) "--main" :: mainPackage :: baseArgs
      else "--main_file" :: mainFile :: baseArgs
    module.runWith(files)(
      "json" :: "write" :: "--output" :: "" :: main ::: makeInputArgs(
        files.keys
      )
    ) match {
      case Left(err) =>
        new Error(s"error: ${err.getMessage}")
      case Right(module.Output.JsonOutput(json, _)) =>
        new EvalSuccess(jsonToAny(json))
      case Right(other) =>
        new Error(s"internal error. got unexpected result: $other")
    }
  }
}
