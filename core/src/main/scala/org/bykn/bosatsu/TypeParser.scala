package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.parse.{Parser => P}

import Parser.{ Combinators, lowerIdent, maybeSpace, keySpace }

abstract class TypeParser[A] {
  def makeVar: String => A
  def parseName: P[A]
  def makeFn(in: A, out: A): A

  def applyTypes(cons: A, args: NonEmptyList[A]): A
  def universal(vars: NonEmptyList[String], in: A): A
  def makeTuple(items: List[A]): A

  final val parser: P[A] = P.recursive[A] { recurse =>
    val tvar = lowerIdent.map(makeVar)
    val tname = parseName

    val lambda =
      (keySpace("forall") *> lowerIdent.nonEmptyList ~ (maybeSpace *> P.char('.') *> maybeSpace *> recurse))
        .map { case (args, e) => universal(args, e) }

    val tupleOrParens: P[A] =
      recurse.tupleOrParens.map {
        case Left(par) => par
        case Right(tup) => makeTuple(tup)
      }

    val appP: P[A => A] =
      (P.char('[') *> maybeSpace *> recurse.nonEmptyList <* maybeSpace <* P.char(']'))
        .map { args => applyTypes(_, args) }

    val arrowP: P[A => A] =
      ((maybeSpace.with1.soft ~ P.string("->") ~ maybeSpace) *> recurse)
        .map { right => makeFn(_, right) }

    P.oneOf(lambda :: tvar :: tname :: tupleOrParens :: Nil)
      .maybeAp(appP)
      .maybeAp(arrowP)
  }
}
