package dev.bosatsu

import scala.quoted.*

opaque type Nullable[T] = T | Null

object Nullable:

  extension [T](inline nullable: Nullable[T])
    transparent inline def fold[A](inline ifNull: => A)(inline fn: T => A): A =
      ${ foldImpl('nullable, 'ifNull, 'fn) }

    inline def isNull: Boolean =
      fold(true)(_ => false)

    inline def nonNull: Boolean =
      fold(false)(_ => true)

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

  private def foldImpl[T: Type, A: Type](
      nullable: Expr[Nullable[T]],
      ifNull: Expr[A],
      fn: Expr[T => A]
  )(using Quotes): Expr[A] =
    '{
      val n = $nullable
      given CanEqual[T | Null, Null] = CanEqual.derived
      if (n == null) $ifNull
      else {
        val safe: T = n.asInstanceOf[T]
        ${ Expr.betaReduce('{ $fn(safe) }) }
      }
    }
