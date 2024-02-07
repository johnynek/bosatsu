package org.bykn.bosatsu

import cats.Order

sealed abstract class FirstOrSecond[+A, +B]
object FirstOrSecond {
  case class First[A](first: A) extends FirstOrSecond[A, Nothing]
  case class Second[B](second: B) extends FirstOrSecond[Nothing, B]

  implicit def orderFS[A: Order, B: Order]: Order[FirstOrSecond[A, B]] = 
    new Order[FirstOrSecond[A, B]] {
      def compare(a: FirstOrSecond[A, B], b: FirstOrSecond[A, B]) =
        (a, b) match {
          case (First(fa), First(fb)) =>
            Order[A].compare(fa, fb)
          case (Second(sa), Second(sb)) =>
            Order[B].compare(sa, sb)
          case (First(_), Second(_)) => -1
          case (Second(_), First(_)) => 1
        }
    }

  def someFirst[A](opt: Option[A]): FirstOrSecond[A, Unit] =
    opt match {
      case Some(a) => FirstOrSecond.First(a)
      case None => FirstOrSecond.Second(())
    }
}