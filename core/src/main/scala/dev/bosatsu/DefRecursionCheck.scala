package dev.bosatsu

import cats.data.{ValidatedNec, ValidatedNel}

/** Legacy syntax recursion checker moved to test scope.
  *
  * Production recursion validation now runs in [[TypedExprRecursionCheck]]
  * after typechecking.
  */
object DefRecursionCheck {
  type RecursionError = RecursionCheck.Error
  type Res = ValidatedNel[RecursionError, Unit]
  type ResNec = ValidatedNec[RecursionError, Unit]
}
