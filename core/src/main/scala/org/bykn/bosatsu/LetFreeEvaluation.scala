package org.bykn.bosatsu

import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}

object LetFreeEvaluation {
  sealed trait LetFreeValue {}
  case class ComputedValue(value: Value) extends LetFreeValue

  type Cache = Option[CMap[String, (Future[Value], Type)]]
  type ToLFV = Option[LetFreeValue => Future[Value]]
}
