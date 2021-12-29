package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import org.typelevel.paiges.{ Doc, Document }

import Parser.{ Combinators, lowerIdent, maybeSpace, maybeSpacesAndLines, keySpace }

abstract class TypeParser[A] {
  /*
   * These are the construction methods to allow parsing
   */
  protected def makeVar: String => A
  protected def parseName: P[A]
  protected def makeFn(in: A, out: A): A
  protected def applyTypes(cons: A, args: NonEmptyList[A]): A
  protected def universal(vars: NonEmptyList[String], in: A): A
  protected def makeTuple(items: List[A]): A

  /*
   * These are deconstruction methods to allow converting to Doc
   */
  protected def unapplyVar(a: A): Option[String]
  protected def unapplyName(a: A): Option[Doc]
  protected def unapplyFn(a: A): Option[(A, A)]
  protected def unapplyUniversal(a: A): Option[(List[String], A)]
  protected def unapplyTypeApply(a: A): Option[(A, List[A])]
  protected def unapplyTuple(a: A): Option[List[A]]


  final val parser: P[A] = P.recursive[A] { recurse =>
    val tvar = lowerIdent.map(makeVar)
    val tname = parseName

    val lambda =
      (keySpace("forall") *> lowerIdent.nonEmptyListOfWs(maybeSpacesAndLines) ~ (maybeSpacesAndLines *> P.char('.') *> maybeSpacesAndLines *> recurse))
        .map { case (args, e) => universal(args, e) }

    val tupleOrParens: P[A] =
      recurse.tupleOrParens.map {
        case Left(par) => par
        case Right(tup) => makeTuple(tup)
      }

    val appP: P[A => A] =
      (P.char('[') *> maybeSpacesAndLines *> recurse.nonEmptyListOfWs(maybeSpacesAndLines) <* maybeSpacesAndLines <* P.char(']'))
        .map { args => applyTypes(_, args) }

    val arrowP: P[A => A] =
      ((maybeSpace.with1.soft ~ P.string("->") ~ maybeSpacesAndLines) *> recurse)
        .map { right => makeFn(_, right) }

    P.oneOf(lambda :: tvar :: tname :: tupleOrParens :: Nil)
      .maybeAp(appP)
      .maybeAp(arrowP)
  }

  final def toDoc(a: A): Doc = {
    import TypeParser._

    unapplyVar(a) match {
      case None => ()
      case Some(s) => return Doc.text(s)
    }

    unapplyName(a) match {
      case None => ()
      case Some(d) => return d
    }

    unapplyFn(a) match {
      case None => ()
      case Some((in, out)) =>
        val din = toDoc(in)
        val dout = spaceArrow + toDoc(out)
        return unapplyFn(in).orElse(unapplyUniversal(in)) match {
          case Some(_) => par(din) + dout
          case None => din + dout
        }
    }

    unapplyTypeApply(a) match {
      case None => ()
      case Some((of, args)) =>
        val ofDoc0 = toDoc(of)
        val ofDoc = unapplyUniversal(of) match {
          case None => ofDoc0
          case Some(_) => par(ofDoc0)
        }
        return ofDoc + Doc.char('[') + Doc.intercalate(commaSpace, args.map(toDoc)) + Doc.char(']')
    }

    unapplyUniversal(a) match {
      case None => ()
      case Some((vars, in)) =>
        return forAll + Doc.intercalate(commaSpace,
          vars.map(Doc.text(_))) +
          Doc.char('.') + Doc.space + toDoc(in)
    }

    // the last one must be a tuple
    unapplyTuple(a) match {
      case None => Doc.text(s"<nothing matched: $a>")
      case Some(ts) =>
        ts match {
          case Nil => unitDoc
          case h :: Nil => Doc.char('(') + toDoc(h) + commaPar
          case twoAndMore =>
            Doc.char('(') + Doc.intercalate(commaSpace, twoAndMore.map(toDoc)) + Doc.char(')')
        }
    }
  }

  final val document: Document[A] = Document.instance(toDoc(_))
}

object TypeParser {
  private val forAll = Doc.text("forall ")
  private val spaceArrow = Doc.text(" -> ")
  private val commaSpace = Doc.text(", ")
  private val commaPar = Doc.text(",)")
  private val unitDoc = Doc.text("()")
  private def par(d: Doc): Doc = Doc.char('(') + d + Doc.char(')')
}
