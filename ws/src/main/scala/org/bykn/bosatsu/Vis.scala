package org.bykn.bosatsu

import cats.data.NonEmptyList

object Vis {
  val packageName: PackageName = PackageName(NonEmptyList.of("Bosatsu", "Vis"))

  def jvmExternals[T[_]](implicit
    valueT: Evaluation.ValueT[T],
    visImpl: VisImpl[T]
  ): Externals[T] =
    Externals
      .empty
      .add(packageName, "header", FfiCall.Fn1(visImpl.header(_)))
      .add(packageName, "markdown", FfiCall.Fn1(visImpl.markdown(_)))
}

case class VisImpl[T[_]]()(implicit
  valueT: Evaluation.ValueT[T],
  tokenize: Evaluation.Value[T] => Json
) {
  import Evaluation.Value

  case class Header(content: Value[T])

  val tokenizeHeader: Any => Json = x => Json.JObject(
    List(
      "format" -> Json.JString("Header"),
      "token" -> tokenize(x.asInstanceOf[Header].content)
    )
  )

  def header(content: Value[T]): Value[T] =
    valueT.ExternalValue(Header(content), tokenizeHeader)

  case class MarkDown(content: Value[T])

  val tokenizeMarkDown: Any => Json = x => Json.JObject(
    List(
      "format" -> Json.JString("MarkDown"),
      "token" -> tokenize(x.asInstanceOf[MarkDown].content)
    )
  )

  def markdown(content: Value[T]): Value[T] =
    valueT.ExternalValue(MarkDown(content), tokenizeMarkDown)
}