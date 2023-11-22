package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import org.typelevel.paiges.{ Doc, Document }
import cats.syntax.all._

import Parser.{ Combinators, MaybeTupleOrParens, lowerIdent, maybeSpace, maybeSpacesAndLines, keySpace }

abstract class TypeParser[A] {
  /*
   * These are the construction methods to allow parsing
   */
  protected def parseRoot: P[A]
  protected def makeFn(in: NonEmptyList[A], out: A): A
  protected def universal(vars: NonEmptyList[(String, Option[Kind])], in: A): A
  protected def existential(vars: NonEmptyList[(String, Option[Kind])], in: A): A
  protected def applyTypes(cons: A, args: NonEmptyList[A]): A
  protected def makeTuple(items: List[A]): A

  /*
   * These are deconstruction methods to allow converting to Doc
   */
  protected def unapplyRoot(a: A): Option[Doc]
  protected def unapplyFn(a: A): Option[(NonEmptyList[A], A)]
  protected def unapplyUniversal(a: A): Option[(List[(String, Option[Kind])], A)]
  protected def unapplyExistential(a: A): Option[(List[(String, Option[Kind])], A)]
  protected def unapplyTypeApply(a: A): Option[(A, List[A])]
  protected def unapplyTuple(a: A): Option[List[A]]


  final val parser: P[A] = P.recursive[A] { recurse =>
    val univItem: P[(String, Option[Kind])] = {
      val kindP: P[Kind] =
        (maybeSpacesAndLines.soft.with1 *> (P.char(':') *> maybeSpacesAndLines *> Kind.parser))
      lowerIdent ~ kindP.?
    }

    val quantified: P[(NonEmptyList[(String, Option[Kind])], A) => A] =
      keySpace("forall").as(universal(_, _)) |
      keySpace("exists").as(existential(_, _))

    val lambda: P[MaybeTupleOrParens[A]] =
      (quantified, univItem.nonEmptyListOfWs(maybeSpacesAndLines) ~ (maybeSpacesAndLines *> P.char('.') *> maybeSpacesAndLines *> recurse))
        .mapN { case (fn, (args, e)) => MaybeTupleOrParens.Bare(fn(args, e)) }

    val tupleOrParens: P[MaybeTupleOrParens[A]] =
      MaybeTupleOrParens.tupleOrParens(recurse)

    def nonArrow(mtp: MaybeTupleOrParens[A]): A =
      mtp match {
        case MaybeTupleOrParens.Bare(a) => a
        case MaybeTupleOrParens.Parens(a) => a
        case MaybeTupleOrParens.Tuple(as) => makeTuple(as)
      }

    val appP: P[MaybeTupleOrParens[A] => MaybeTupleOrParens[A]] =
      (P.char('[') *> maybeSpacesAndLines *> recurse.nonEmptyListOfWs(maybeSpacesAndLines) <* maybeSpacesAndLines <* P.char(']'))
        .map { args =>

          { left =>
            MaybeTupleOrParens.Bare(applyTypes(nonArrow(left), args))
          }
        }

    val arrowP: P[MaybeTupleOrParens[A] => MaybeTupleOrParens[A]] =
      ((maybeSpace.with1.soft ~ P.string("->") ~ maybeSpacesAndLines) *> recurse)
        // TODO remove the flatMap when we support FunctionN
        .map { right =>
          {
            case MaybeTupleOrParens.Bare(a) =>
              MaybeTupleOrParens.Bare(makeFn(NonEmptyList.one(a), right))
            case MaybeTupleOrParens.Parens(a) =>
              MaybeTupleOrParens.Bare(makeFn(NonEmptyList.one(a), right))
            case MaybeTupleOrParens.Tuple(items) =>
              val args = NonEmptyList.fromList(items) match {
                case None => NonEmptyList.one(makeTuple(Nil))
                case Some(nel) => nel
              }
              // We know th
              MaybeTupleOrParens.Bare(makeFn(args, right))
          }
        }

    P.oneOf(lambda :: parseRoot.map(MaybeTupleOrParens.Bare(_)) :: tupleOrParens :: Nil)
      .maybeAp(appP)
      .maybeAp(arrowP)
      .map(nonArrow)
  }

  final def toDoc(a: A): Doc = {
    import TypeParser._

    def p(d: Doc): Doc = Doc.char('(') + (d + Doc.char(')'))
    /*
     * Tuples and functions have syntax that is distinct
     * from their internal representation. We have to check
     * those first
     */
    unapplyTuple(a) match {
      case None => ()
      case Some(ts) =>
        return ts match {
          case Nil => unitDoc
          case h :: Nil => Doc.char('(') + toDoc(h) + commaPar
          case twoAndMore =>
            p(Doc.intercalate(commaSpace, twoAndMore.map(toDoc)))
        }
    }

    unapplyFn(a) match {
      case None => ()
      case Some((ins, out)) =>
        val args = if (ins.tail.isEmpty) {
          val in0 = ins.head
          val din = toDoc(in0)
          unapplyFn(in0)
            .orElse(unapplyUniversal(in0))
            .orElse(unapplyExistential(in0))
            .orElse(unapplyTuple(in0)) match {
            case Some(_) => par(din)
            case None => din
          }
        }
        else {
          // there is more than 1 arg so parens are always used: (a, b) -> c
          par(Doc.intercalate(commaSpace, ins.toList.map(toDoc)))
           
        }
        return (args + (spaceArrow + toDoc(out)))
    }

    unapplyRoot(a) match {
      case None => ()
      case Some(d) => return d
    }

    unapplyTypeApply(a) match {
      case None => ()
      case Some((of, args)) =>
        val ofDoc0 = toDoc(of)
        val ofDoc = unapplyUniversal(of).orElse(unapplyExistential(of)) match {
          case None => ofDoc0
          case Some(_) => par(ofDoc0)
        }
        return ofDoc + Doc.char('[') + Doc.intercalate(commaSpace, args.map(toDoc)) + Doc.char(']')
    }

    unapplyUniversal(a) match {
      case None => ()
      case Some((vars, in)) =>
        return forAll + Doc.intercalate(commaSpace,
          vars.map {
            case (a, None) => Doc.text(a)
            case (a, Some(k)) => Doc.text(a) + TypeParser.colonSpace + k.toDoc
          }) +
          Doc.char('.') + Doc.space + toDoc(in)
    }

    unapplyExistential(a) match {
      case None => ()
      case Some((vars, in)) =>
        return exists + Doc.intercalate(commaSpace,
          vars.map {
            case (a, None) => Doc.text(a)
            case (a, Some(k)) => Doc.text(a) + TypeParser.colonSpace + k.toDoc
          }) +
          Doc.char('.') + Doc.space + toDoc(in)
    }

    Doc.text(s"<nothing matched: $a>")
  }

  def render(a: A): String = toDoc(a).renderTrim(80)

  final val document: Document[A] = Document.instance(toDoc(_))
}

object TypeParser {
  private val forAll = Doc.text("forall ")
  private val exists = Doc.text("exists ")
  private val spaceArrow = Doc.text(" -> ")
  private val commaSpace = Doc.text(", ")
  private val colonSpace = Doc.text(": ")
  private val commaPar = Doc.text(",)")
  private val unitDoc = Doc.text("()")
  private def par(d: Doc): Doc = Doc.char('(') + d + Doc.char(')')
}
