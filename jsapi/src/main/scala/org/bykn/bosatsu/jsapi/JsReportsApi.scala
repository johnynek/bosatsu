package org.bykn.bosatsu
package jsapi

import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.{Map => CMap}
import cats.implicits._
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Future
import scala.scalajs.js.JSConverters._

import scala.scalajs.js
import js.annotation._

import cats.implicits._

import JsApi.jsonToAny

@JSExportTopLevel("BosatsuReport")
object JsReportsApi {
  class JSMap[K, V] extends CMap[K, V] {
    private val myMap = MMap.empty[K, V]
    def get(key: K) = myMap.get(key)
    def -=(k: K) = {
      myMap -= k
      this
    }
    def +=(kv: (K, V)) = {
      myMap += (kv)
      this
    }

    def iterator: Iterator[(K, V)] = myMap.iterator
    def putIfAbsent(k: K, v: V): Option[V] = myMap.get(k) match {
      case None => {
        myMap += ((k, v))
        None
      }
      case res @ Some(_) => res
    }
    def remove(k: K, v: V): Boolean = myMap.get(k) match {
      case None => false
      case Some(v2) =>
        if (v2 == v) {
          myMap -= k
          true
        } else {
          false
        }
    }
    def replace(k: K, v: V): Option[V] = myMap.get(k) match {
      case None => None
      case res @ Some(_) => {
        myMap += ((k, v))
        res
      }
    }

    def replace(k: K, oldvalue: V, newvalue: V): Boolean = myMap.get(k) match {
      case None => false
      case Some(v2) =>
        if (v2 == oldvalue) {
          myMap += ((k, newvalue))
          true
        } else {
          false
        }
    }

  }

  private def splitPath(p: String): List[String] =
    p.split("/", -1).toList.map(_.toLowerCase.capitalize)

  val module = new MemoryMain[Either[Throwable, ?], String](splitPath)

  val mainPath = "submitted"
  val inputs: module.MainCommand.PathGen =
    module.MainCommand.PathGen.Direct(mainPath)

  val valueToJson: ValueToJson = ValueToJson({
    case rankn.Type.Const.Defined(pn, t) =>
      for {
        dt <- Predef.predefCompiled.program.types.getType(pn, t)
      } yield dt
  })

  val nev = module.MainCommand.NEvaluate(
    inputs,
    module.MainCommand.MainIdentifier.FromFile(mainPath),
    PathGen.pathGenMonoid.empty,
    LocationMap.Colorize.Console,
    module.MainCommand.PackageResolver.ExplicitOnly
  )

  val cacheParam: NormalEvaluation.Cache = Some(new JSMap())
  val cache = cacheParam.get

  @JSExport
  def getReport(
      bosatsuSource: String
  ): js.Promise[js.Any] = {

    val files = List((mainPath, bosatsuSource))
    val state0 =
      files.foldLeft(SortedMap.empty[String, MemoryMain.FileContent]) {
        case (st, (k, str)) =>
          st.updated(k, MemoryMain.FileContent.Str(str))
      }
    val nres = nev.eval.run(state0)
    val fjson = nres match {
      case Right(output) => {
        val lv = NormalEvaluation.LazyValue(output.ne, Nil)
        val key = lv.toKey
        val (futureValue, _) = cache.getOrElseUpdate(
          key,
          (
            Future(
              NormalEvaluation
                .evaluate(output.ne, output.extEnv, cacheParam)
            ),
            output.tpe
          )
        )
        futureValue.map { value =>
          output.optJ(value) match {
            case Left(j) =>
              Json
                .JObject(
                  List(
                    "variant" -> Json.JString("Document"),
                    "visualization" -> j,
                    "key" -> Json.JString(
                      NormalEvaluation
                        .LazyValue(output.ne, Nil)
                        .toKey
                    ),
                    "program" -> NormalEvaluation
                      .LazyValue(output.ne, Nil)
                      .toJson
                  )
                )
            case Right(err) =>
              Json
                .JObject(
                  List(
                    "variant" -> Json.JString("Error"),
                    "error" -> Json.JString(err)
                  )
                )

          }
        }
      }
      case Left(err) =>
        Future(
          Json
            .JObject(
              List(
                "variant" -> Json.JString("Error"),
                "error" -> Json.JString(err.getMessage)
              )
            )
        )
    }
    fjson.map(jsonToAny(_)).toJSPromise
  }

  @JSExport
  def postCache(keys: js.Array[String]): js.Promise[js.Dictionary[js.Any]] = {
    Future {
      val jsArray: js.Array[(String, js.Any)] = for {
        key <- keys
        result <- cache.get(key)
        (promise, tpe) = result
        valueTry <- promise.value
        value <- valueTry.toOption
      } yield {
        val toJson = valueToJson.toJson(tpe)
        toJson match {
          case Left(unsupported) => ???
          case Right(fn) =>
            fn(value) match {
              case Left(err) => ???
              case Right(j)  => key -> jsonToAny(j)
            }
        }
      }
      val result = collection.mutable.Map() ++ jsArray.toMap
      result.toJSDictionary
    }.toJSPromise
  }

}
