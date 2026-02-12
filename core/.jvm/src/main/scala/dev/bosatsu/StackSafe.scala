package dev.bosatsu

object StackSafe {
  inline def onStackOverflow[A](
      inline expr: => A
  )(inline onOverflow: => A): A =
    try expr
    catch {
      case _: StackOverflowError =>
        onOverflow
    }
}
