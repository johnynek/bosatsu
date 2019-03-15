package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, maybeSpace, spaces }
import cats.Functor
import cats.data.NonEmptyList
import cats.implicits._
import fastparse.all._
import org.bykn.fastparse_cats.StringInstances._
import org.typelevel.paiges.{ Doc, Document }

case class DefStatement[T](
    kind: RecursionKind,
    name: String,
    args: List[(String, Option[TypeRef])],
    retType: Option[TypeRef], result: T) {


  def map[A](fn: T => A): DefStatement[A] =
    copy(result = fn(result))
  /**
   * This ignores the name completely and just returns the lambda expression here
   *
   * This could be a traversal: TypeRef => F[rankn.Type] for an Applicative F[_]
   */
  def toLambdaExpr[A](resultExpr: Expr[A], tag: A)(trFn: TypeRef => rankn.Type): Expr[A] = {
    val unTypedBody = resultExpr
    val bodyExp =
      retType.fold(unTypedBody) { t =>
        Expr.Annotation(unTypedBody, trFn(t), tag)
      }
    NonEmptyList.fromList(args) match {
      case None => bodyExp
      case Some(neargs) =>
        val deepFunctor = Functor[NonEmptyList].compose[(String, ?)].compose[Option]
        Expr.buildLambda(deepFunctor.map(neargs)(trFn), bodyExp, tag)
    }
  }
}

object DefStatement {
  private[this] val defDoc = Doc.text("def ")
  private[this] val recDefDoc = Doc.text("recursive def ")

  implicit def document[T: Document]: Document[DefStatement[T]] =
    Document.instance[DefStatement[T]] { defs =>
      import defs._
      val res = retType.fold(Doc.empty) { t => Doc.text(" -> ") + t.toDoc }
      val argDoc =
        if (args.isEmpty) Doc.empty
        else {
          Doc.char('(') +
            Doc.intercalate(Doc.text(", "), args.map(TypeRef.argDoc _)) +
            Doc.char(')')
        }
      val dd = kind match {
        case RecursionKind.NonRecursive => defDoc
        case RecursionKind.Recursive => recDefDoc
      }
      val line0 = dd + Doc.text(name) + argDoc + res + Doc.text(":")

      line0 + Document[T].document(result)
    }

    /**
     * The resultTParser should parse some indentation any newlines
     */
    def parser[T](resultTParser: P[T]): P[DefStatement[T]] = {
      val args = argParser.nonEmptyList
      val result = P(maybeSpace ~ "->" ~/ maybeSpace ~ TypeRef.parser).?
      val recKind = P("recursive" ~ spaces ~ "def").map(_ => RecursionKind.Recursive)
      val notRec = P("def").map(_ => RecursionKind.NonRecursive)
      P((recKind | notRec) ~ spaces ~/ lowerIdent ~ ("(" ~ maybeSpace ~ args ~ maybeSpace ~ ")").? ~
        result ~ maybeSpace ~ ":" ~/ resultTParser)
        .map {
          case (kind, name, optArgs, resType, res) =>
            val args = optArgs match {
              case None => Nil
              case Some(ne) => ne.toList
            }
            DefStatement(kind, name, args, resType, res)
        }
    }

    val argParser: P[(String, Option[TypeRef])] =
      P(lowerIdent ~ (":" ~/ maybeSpace ~ TypeRef.parser).?)
}
