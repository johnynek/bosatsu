package org.bykn.bosatsu

import cats.data.StateT
import scala.util.{Failure, Success, Try}

import cats.implicits._

object Indexed {
  type Tab[S, A] = StateT[Try, (Map[S, Int], Vector[S], Int), A]

  def getId[S](s: S): Tab[S, Int] =
    StateT.get[Try, (Map[S, Int], Vector[S], Int)]
      .flatMap { case (table, ss, next) =>
         table.get(s) match {
           case Some(idx) =>
             tabPure(idx + 1)
           case None =>
             // we need to insert
             val newNext = next + 1
             val newState = (table.updated(s, next), ss :+ s, newNext)
             StateT.set[Try, (Map[S, Int], Vector[S], Int)](newState).as(newNext)
         }
      }

  def lookup[S](idx: Int, context: => String): Tab[S, S] =
    StateT.get[Try, (Map[S, Int], Vector[S], Int)]
      .flatMap { case (_, vec, _) =>
        // idx is 1 based so we can see 0 as invalid
        val fixedIdx = idx - 1
        if ((fixedIdx < 0) || (fixedIdx >= vec.size))
          tabFail(new Exception(s"invalid index: $idx in $context, size: ${vec.size}"))
        else tabPure(vec(fixedIdx))
      }

  def tabFail[S, A](ex: Exception): Tab[S, A] =
    StateT.liftF(Failure(ex))

  def tabPure[S, A](a: A): Tab[S, A] =
    StateT.liftF(Success(a))

  def lift[S, A](ta: Try[A]): Tab[S, A] =
    StateT.liftF(ta)

  /**
   * Runs the table twice, first to build the list of all values S that need IDs
   * secondly with the table to insert the correct value
   */
  def runTab[S, A](tab: Tab[S, A]): Try[(Vector[S], A)] =
    tab.run((Map.empty, Vector.empty, 0))
      .map { case ((_, ss, _), a) =>
        (ss, a)
      }

  def runLookupTab[S, A](vec: Vector[S], tab: Tab[S, A]): Try[A] =
    tab.runA((vec.zipWithIndex.toMap, vec, vec.size))
}
