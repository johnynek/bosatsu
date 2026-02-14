package dev.bosatsu

import scala.quoted.{Expr, Quotes}

object Require {

  inline def apply(inline requirement: Boolean): Unit =
    ${ applyImpl('requirement) }

  inline def apply(inline requirement: Boolean, inline message: => Any): Unit =
    ${ applyImpl('requirement, 'message) }

  private def applyImpl(
      requirement: Expr[Boolean]
  )(using Quotes): Expr[Unit] =
    '{
      if (! $requirement)
        throw new IllegalArgumentException("requirement failed")
    }

  private def applyImpl(
      requirement: Expr[Boolean],
      message: Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect._

    def literalString(term: Term): Option[String] =
      term match {
        case Inlined(_, _, inner)         => literalString(inner)
        case Block(Nil, inner)            => literalString(inner)
        case Typed(inner, _)              => literalString(inner)
        case Literal(StringConstant(str)) => Some(str)
        case _                            => None
      }

    literalString(message.asTerm) match {
      case Some(str) =>
        val errorMessage = Expr("requirement failed: " + str)
        '{
          if (! $requirement)
            throw new IllegalArgumentException($errorMessage)
        }
      case None =>
        '{
          if (! $requirement)
            throw new IllegalArgumentException(
              "requirement failed: " + $message
            )
        }
    }
  }
}
