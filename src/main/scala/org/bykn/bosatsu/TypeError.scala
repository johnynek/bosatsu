package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._

sealed abstract class TypeError {
  def message(sourceName: String, lm: LocationMap): String
}

object TypeError {
  case class UnificationFail(left: Type, right: Type, leftRegion: Region, rightRegion: Region) extends TypeError {
    def message(sourceName: String, lm: LocationMap) = {
      val header =
        s"in $sourceName type $left at ${lm.toLineCol(leftRegion.start)} does not match type $right at ${lm.toLineCol(rightRegion.start)}\n"
      val leftCtx = lm.showContext(leftRegion.start).getOrElse("")
      val rightCtx = lm.showContext(rightRegion.start).getOrElse("")
      List(header, leftCtx, rightCtx).mkString("\n")
    }
  }
  case class InfiniteType(name: String, tpe: Type, nameRegion: Region, tpeRegion: Region) extends TypeError {
    def message(sourceName: String, lm: LocationMap) = {
      val header =
        s"in $sourceName type variable $name at ${lm.toLineCol(nameRegion.start)} forms an infinite type $tpe at ${lm.toLineCol(tpeRegion.start)}\n"
      val leftCtx = lm.showContext(nameRegion.start).getOrElse("")
      val rightCtx = lm.showContext(tpeRegion.start).getOrElse("")
      List(header, leftCtx, rightCtx).mkString("\n")
    }
  }
  case class Unbound(name: String, region: Region) extends TypeError {
    def message(sourceName: String, lm: LocationMap) = {
      val header =
        s"in $sourceName value $name at ${lm.toLineCol(region.start)} is unknown\n"
      val ctx = lm.showContext(region.start).getOrElse("")
      List(header, ctx).mkString("\n")
    }
  }
  case class UnknownConstuctor(cname: ConstructorName, region: Region) extends TypeError {
    def message(sourceName: String, lm: LocationMap) = {
      val header =
        s"in $sourceName constructor $cname at ${lm.toLineCol(region.start)} is unknown\n"
      val ctx = lm.showContext(region.start).getOrElse("")
      List(header, ctx).mkString("\n")
    }
  }
  case class TypeConstructorCollision(matches: NonEmptyList[((PackageName, ConstructorName), Region)], env: TypeEnv) extends TypeError {
    def message(sourceName: String, lm: LocationMap) = {
      val region = matches.reduceMap { case (_, r) => r }
      val header =
        s"in $sourceName two different enum types in one match at ${lm.toLineCol(region.start)}\n"
      val ctx = lm.showContext(region.start).getOrElse("")
      List(header, ctx).mkString("\n")
    }
  }
  case class NonTotalMatch(matches: NonEmptyList[ConstructorName], expected: List[ConstructorName], region: Region) extends TypeError {
    def message(sourceName: String, lm: LocationMap) = {
      val missing = expected.filterNot(matches.toList.toSet)
      val comma = ", "
      val header =
        s"in $sourceName non-total match at ${lm.toLineCol(region.start)}. Missing: ${missing.map(_.asString).mkString(comma)}\n"
      val ctx = lm.showContext(region.start).getOrElse("")
      List(header, ctx).mkString("\n")
    }
  }
}

