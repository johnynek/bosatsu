package org.bykn.bosatsu

import cats.Order
import cats.data.{Ior, NonEmptyList, NonEmptyMap}
import scala.collection.immutable.SortedMap
import scala.util.{Success, Failure, Try}

import cats.implicits._

object CollectionUtils {

  /** Return the unique keys on the Right, and the duplicate keys on the Left
    * (and possibly Both)
    */
  def uniqueByKey[A, B: Order](as: NonEmptyList[A])(
      fn: A => B
  ): Ior[NonEmptyMap[B, (A, NonEmptyList[A])], NonEmptyMap[B, A]] = {
    def check(as: NonEmptyList[A]): Either[(A, NonEmptyList[A]), A] =
      as match {
        case NonEmptyList(a, Nil) =>
          Right(a)
        case NonEmptyList(a0, a1 :: tail) =>
          Left((a0, NonEmptyList(a1, tail)))
      }

    // We know this is nonEmpty, so good and bad can't both be empty
    val checked: SortedMap[B, Either[(A, NonEmptyList[A]), A]] =
      as.groupBy(fn).map { case (b, as) => (b, check(as)) }
    val good: SortedMap[B, A] = checked.collect { case (b, Right(a)) => (b, a) }
    val bad: SortedMap[B, (A, NonEmptyList[A])] = checked.collect {
      case (b, Left(a)) => (b, a)
    }

    (NonEmptyMap.fromMap(bad), NonEmptyMap.fromMap(good)) match {
      case (None, Some(goodNE))        => Ior.right(goodNE)
      case (Some(badNE), None)         => Ior.left(badNE)
      case (Some(badNE), Some(goodNE)) => Ior.both(badNE, goodNE)
      // $COVERAGE-OFF$
      case _ =>
        sys.error("unreachable due to as being nonempty")
      // $COVERAGE-ON$
    }
  }

  def listToUnique[A, K: Order, V](
      l: List[A]
  )(key: A => K, value: A => V, msg: => String): Try[SortedMap[K, V]] =
    NonEmptyList.fromList(l) match {
      case None => Success(SortedMap.empty[K, V])
      case Some(nel) =>
        uniqueByKey(nel)(key) match {
          case Ior.Right(b) =>
            Success(b.toSortedMap.map { case (k, a) => (k, value(a)) })
          case Ior.Left(errMap) =>
            Failure(new IllegalArgumentException(s"$msg, $errMap"))
          case Ior.Both(errMap, _) =>
            Failure(new IllegalArgumentException(s"$msg, $errMap"))
        }
    }

}
