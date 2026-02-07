package dev.bosatsu

import scala.quoted.*

object NonNullFold:

  extension [A](inline arg: A | Null)
    /**
     * Extension method to fold over a nullable value.
     * Uses pattern matching to ensure compatibility with strict equality.
     */
    inline def foldNN[B](inline default: => B)(inline fn: A => B): B =
      ${ foldNNImpl('arg, 'default, 'fn) }

    inline def notNull[B]: Boolean =
      foldNN(false)(_ => true)

  private def foldNNImpl[A: Type, B: Type](
      arg: Expr[A | Null],
      default: Expr[B],
      fn: Expr[A => B]
  )(using Quotes): Expr[B] =
    '{
      // 1. Bind the argument to a val to prevent multiple evaluations
      val t = $arg
      
      // 2. 
      // This works even if 'import scala.language.strictEquality' is active.
      given CanEqual[A | Null, Null] = CanEqual.derived
      if (t == null) $default
      else {
        // CRITICAL FIX: 
        // We define a new local 'val' with the explicit type 'A'.
        // This strips away the complex intersection types (flow typing) from 't'
        // that were causing the MatchError in the unpickler.
        val safe: A = t.asInstanceOf[A]
        // 3. We cast to A safely because we are in the not-null branch.
        // 4. betaReduce inlines the function body directly here.
        ${ Expr.betaReduce('{ $fn(safe) }) }
      }
    }