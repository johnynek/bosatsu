package org.bykn.bosatsu

import cats.Order
import cats.data.NonEmptyList
import scala.collection.immutable.SortedMap
import scala.util.{Success, Failure, Try}

import cats.implicits._

object CollectionUtils {
  /**
   * Either return a unique mapping from B to A, or return a pair of duplicates
   * and the unque subset
   */
  def uniqueByKey[A, B: Order](as: List[A])(fn: A => B): Either[(SortedMap[B, (A, NonEmptyList[A])], SortedMap[B, A]), SortedMap[B, A]] = {
    val m: SortedMap[B, NonEmptyList[A]] = as.groupByNel(fn)
    def check(as: NonEmptyList[A]): Either[(A, NonEmptyList[A]), A] =
      as match {
        case NonEmptyList(a, Nil) =>
          Right(a)
        case NonEmptyList(a0, a1 :: tail) =>
          Left((a0, NonEmptyList(a1, tail)))
      }

    val checked: SortedMap[B, Either[(A, NonEmptyList[A]), A]] = m.map { case (b, as) => (b, check(as)) }
    val good: SortedMap[B, A] = checked.collect { case (b, Right(a)) => (b, a) }
    val bad: SortedMap[B, (A, NonEmptyList[A])] = checked.collect { case (b, Left(a)) => (b, a) }

    if (bad.isEmpty) Right(good)
    else Left((bad, good))
  }

  def listToUnique[A, K: Order, V](l: List[A])(key: A => K, value: A => V, msg: => String): Try[SortedMap[K, V]] =
    uniqueByKey(l)(key) match {
      case Right(b) => Success(b.map { case (k, a) => (k, value(a)) })
      case Left((errMap, _)) =>
        Failure(new IllegalArgumentException(s"$msg, $errMap"))
      }

}
