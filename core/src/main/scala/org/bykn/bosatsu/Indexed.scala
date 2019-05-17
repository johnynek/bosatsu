package org.bykn.bosatsu

import cats.data.StateT
import scala.util.{Failure, Success, Try}

import cats.implicits._

object Indexed {
  type Tab[S, A] = StateT[Try, (Map[S, Int], List[S], Int), A]

  def getId[S](s: S): Tab[S, Int] =
    StateT.get[Try, (Map[S, Int], List[S], Int)]
      .flatMap { case (table, ss, next) =>
         table.get(s) match {
           case Some(idx) =>
             tabPure(idx + 1)
           case None =>
             // we need to insert
             val newNext = next + 1
             val newState = (table.updated(s, next), s :: ss, newNext)
             StateT.set[Try, (Map[S, Int], List[S], Int)](newState).as(newNext)
         }
      }

  def tabFail[S, A](ex: Exception): Tab[S, A] =
    StateT.liftF(Failure(ex))

  def tabPure[S, A](a: A): Tab[S, A] =
    StateT.liftF(Success(a))

  /**
   * Runs the table twice, first to build the list of all values S that need IDs
   * secondly with the table to insert the correct value
   */
  def runTab[S, A](tab: Tab[S, A]): Try[(Vector[S], A)] =
    tab.run((Map.empty, Nil, 0))
      .map { case ((_, ss, _), a) =>
        (ss.reverse.toVector, a)
      }

}
