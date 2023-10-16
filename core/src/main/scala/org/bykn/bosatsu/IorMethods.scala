package org.bykn.bosatsu

import cats.data.{Ior, Validated}

object IorMethods {
  implicit class IorExtension[A, B](val ior: Ior[A, B]) extends AnyVal {
    def strictToValidated: Validated[A, B] =
      ior match {
        case Ior.Right(b)   => Validated.valid(b)
        case Ior.Left(a)    => Validated.invalid(a)
        case Ior.Both(a, _) => Validated.invalid(a)
      }
  }
}
