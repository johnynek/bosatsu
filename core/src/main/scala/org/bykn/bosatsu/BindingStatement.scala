package org.bykn.bosatsu

import org.typelevel.paiges.{Doc, Document}

case class BindingStatement[B, V, T](name: B, value: V, in: T)

object BindingStatement {
  private[this] val eqDoc = Doc.text(" = ")

  implicit def document[A: Document, V: Document, T: Document]
      : Document[BindingStatement[A, V, T]] =
    Document.instance[BindingStatement[A, V, T]] { let =>
      import let._
      Document[A].document(name) + eqDoc + Document[V].document(
        value
      ) + Document[T].document(in)
    }
}
