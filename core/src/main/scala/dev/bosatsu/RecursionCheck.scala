package dev.bosatsu

import cats.data.NonEmptyList
import org.typelevel.paiges.Doc

import Identifier.Bindable

object RecursionCheck {
  enum ArgLexOrder derives CanEqual {
    case Equal
    case Smaller
    case Other
  }

  import ArgLexOrder.*

  def isLexicographicallySmaller[A, B, C](
      target: NonEmptyList[A],
      allowedPerTarget: NonEmptyList[B],
      callArgsByTarget: NonEmptyList[C]
  )(
      classifyArg: (A, B, C) => ArgLexOrder
  ): Boolean = {
    val stepIter = target.iterator
      .zip(allowedPerTarget.iterator)
      .zip(callArgsByTarget.iterator)

    while (stepIter.hasNext) {
      val ((targetItem, allowed), arg) = stepIter.next()
      classifyArg(targetItem, allowed, arg) match {
        case Smaller => return true
        case Equal   => ()
        case Other   => return false
      }
    }

    false
  }

  def argsDoc[A](args: NonEmptyList[NonEmptyList[A]])(show: A => Doc): Doc =
    Doc.intercalate(
      Doc.empty,
      args.toList.map { group =>
        (Doc.char('(') +
          Doc.intercalate(
            Doc.comma + Doc.line,
            group.toList.map(show)
          ) +
          Doc.char(')')).grouped
      }
    )

  def renderArgs[A](args: NonEmptyList[NonEmptyList[A]])(show: A => Doc): String =
    argsDoc(args)(show).render(80)

  sealed abstract class Error {
    def region: Region
    def message: String
  }

  case class InvalidRecursion(name: Bindable, illegalPosition: Region)
      extends Error {
    def region = illegalPosition
    def message =
      s"invalid recursion on ${name.sourceCodeRepr}. Consider replacing `match` with `recur`."
  }

  case class NotEnoughRecurArgs(name: Bindable, illegalPosition: Region)
      extends Error {
    def region = illegalPosition
    def message =
      s"not enough args to ${name.sourceCodeRepr} to check recursion safety."
  }

  case class IllegalShadow(fnname: Bindable, illegalPosition: Region)
      extends Error {
    def region = illegalPosition
    def message =
      s"illegal shadowing on: ${fnname.sourceCodeRepr}. Recursive shadowing of def names disallowed"
  }

  case class UnexpectedRecur(illegalPosition: Region) extends Error {
    def region = illegalPosition
    def message = "unexpected recur: may only appear unnested inside a def"
  }

  case class RecurNotOnArg(
      illegalPosition: Region,
      fnname: Bindable,
      argsRepr: String
  ) extends Error {
    def region = illegalPosition
    def message =
      s"recur not on an argument to the def of ${fnname.sourceCodeRepr}, args: $argsRepr"
  }

  case class RecurTargetInvalid(fnname: Bindable, illegalPosition: Region)
      extends Error {
    def region = illegalPosition
    def message =
      s"recur target for ${fnname.sourceCodeRepr} must be a name or tuple of names bound to def args"
  }

  case class RecurTargetDuplicate(
      fnname: Bindable,
      duplicated: Bindable,
      illegalPosition: Region
  ) extends Error {
    def region = illegalPosition
    def message =
      s"recur target for ${fnname.sourceCodeRepr} contains duplicate parameter ${duplicated.sourceCodeRepr}"
  }

  case class RecursionNotLexicographic(
      fnname: Bindable,
      target: NonEmptyList[Bindable],
      illegalPosition: Region
  ) extends Error {
    def region = illegalPosition
    def message = {
      val targetStr = target.toList.map(_.sourceCodeRepr).mkString(", ")
      s"recursive call to ${fnname.sourceCodeRepr} is not lexicographically smaller on recur target ($targetStr)."
    }
  }

  case class RecursiveDefNoRecur(
      fnname: Bindable,
      recurRegion: Region,
      likelyRenamedCall: Option[(Bindable, Int)]
  ) extends Error {
    def region = recurRegion
    def message =
      likelyRenamedCall match {
        case Some((calledName, count)) =>
          s"Function name looks renamed: declared `${fnname.sourceCodeRepr}`, but recursive calls use `${calledName.sourceCodeRepr}`.\nDid you mean `${fnname.sourceCodeRepr}` in recursive calls? ($count occurrences)"
        case None =>
          s"recur but no recursive call to ${fnname.sourceCodeRepr}"
      }
  }
}
