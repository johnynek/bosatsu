package org.bykn.bosatsu.library

import scala.collection.immutable.SortedMap
import org.bykn.bosatsu.Json

import Json.{Writer, Reader}

import cats.syntax.all._

case class Libraries(toMap: SortedMap[String, String]) {
  def updated(name: String, path: String): Libraries =
    Libraries(toMap.updated(name, path))

  def get(name: String): Option[String] =
    toMap.get(name)
}

object Libraries {
  val empty: Libraries = Libraries(SortedMap.empty)

  implicit val libariesReader: Reader[Libraries] =
    new Reader.Obj[Libraries] {
      def describe = "Libraries"
      def readObj(from: Reader.FromObj) =
        from.j.keys.traverse { name =>
          from.field[String](name).map((name, _))
        }
        .map(items => Libraries(items.to(SortedMap)))
    }

  implicit val librariesWriter: Writer[Libraries] =
    Writer.from[Libraries] { lib =>
      Json.JObject(lib.toMap.iterator.map { case (k, v) => (k, Json.JString(v)) }.toList)  
    }
}