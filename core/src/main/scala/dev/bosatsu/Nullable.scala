package dev.bosatsu

import cats.Eq
import scala.quoted.*

opaque type Nullable[+T] = T | Null

object Nullable:

  extension [T](inline nullable: Nullable[T])
    transparent inline def fold[A](inline ifNull: => A)(inline fn: T => A): A =
      ${ foldImpl('nullable, 'ifNull, 'fn) }

    inline def isNull: Boolean =
      {
        given CanEqual[T | Null, Null] = CanEqual.derived
        nullable == null
      }

    inline def nonNull: Boolean =
      {
        given CanEqual[T | Null, Null] = CanEqual.derived
        nullable != null
      }

    inline def map[B](inline fn: T => B): Nullable[B] =
      fold(null: Null)(fn)

    inline def flatMap[B](inline fn: T => Nullable[B]): Nullable[B] =
      fold(null: Null)(fn)

    inline def toOption: Option[T] =
      fold(None)(Some(_))

    inline def iterator: Iterator[T] =
      fold(Iterator.empty)(Iterator.single(_))

  inline def apply[A](inline a: A | Null): Nullable[A] =
    a

  def fromOption[A](opt: Option[A]): Nullable[A] =
    opt match
      case Some(a) => a
      case None    => null

  given [A: Eq]: Eq[Nullable[A]] with
    def eqv(na: Nullable[A], nb: Nullable[A]): Boolean =
      val a: A | Null = na
      val b: A | Null = nb
      given CanEqual[A | Null, Null] = CanEqual.derived
      if (a == null) b == null
      else if (b == null) false
      else Eq[A].eqv(a, b)

  def empty[A]: Nullable[A] =
    null

  private def foldImpl[T: Type, A: Type](
      nullable: Expr[Nullable[T]],
      ifNull: Expr[A],
      fn: Expr[T => A]
  )(using Quotes): Expr[A] =
    val nullableUnion: Expr[T | Null] = '{ $nullable.asInstanceOf[T | Null] }

    '{
      val n = $nullableUnion
      given CanEqual[T | Null, Null] = CanEqual.derived
      if (n == null) $ifNull
      else {
        val safe: T = n.asInstanceOf[T]
        ${ Expr.betaReduce('{ $fn(safe) }) }
      }
    }
