package dev.bosatsu.protobuf

import dev.bosatsu.PackageName
import scala.collection.immutable.SortedMap

object ProtoNaming {

  private val bindableKeywords: Set[String] = Set(
    "as",
    "case",
    "def",
    "elif",
    "else",
    "enum",
    "export",
    "external",
    "False",
    "for",
    "from",
    "if",
    "import",
    "in",
    "match",
    "matches",
    "operator",
    "package",
    "recur",
    "struct",
    "True"
  )

  private val splitBoundary = "([a-z0-9])([A-Z])".r
  private val stripNonAlphaNum = "[^A-Za-z0-9]+".r

  private def tokens(raw: String): Vector[String] = {
    val withWordBreaks = splitBoundary.replaceAllIn(raw, "$1 $2")
    val normalized = stripNonAlphaNum.replaceAllIn(withWordBreaks, " ").trim
    if (normalized.isEmpty) Vector("value")
    else normalized.split("\\s+").toVector.filter(_.nonEmpty)
  }

  private def capitalizeToken(token: String): String =
    token.headOption match {
      case Some(ch) => ch.toUpper.toString + token.drop(1).toLowerCase
      case None     => ""
    }

  def constructorBaseName(raw: String): String = {
    val joined = tokens(raw).map(capitalizeToken).mkString
    val prefixed =
      joined.headOption match {
        case Some(ch) if ch.isDigit => s"T$joined"
        case Some(_)                => joined
        case None                   => "TValue"
      }

    if (prefixed.nonEmpty) prefixed else "TValue"
  }

  def bindableBaseName(raw: String): String = {
    val parts = tokens(raw).map(_.toLowerCase)
    val head =
      parts.headOption match {
        case Some(h) if h.headOption.exists(_.isDigit) => s"v_$h"
        case Some(h)                                   => h
        case None                                      => "value"
      }

    val joined = (head +: parts.drop(1)).mkString("_")
    if (bindableKeywords(joined)) s"${joined}_value" else joined
  }

  def dedupeNames[A: Ordering](
      items: Vector[(A, String)]
  ): SortedMap[A, String] = {
    val (_, out) =
      items.foldLeft((Map.empty[String, Int], SortedMap.empty[A, String])) {
      case ((counts, acc), (key, base0)) =>
        val base = if (base0.nonEmpty) base0 else "value"
        val nextCount = counts.getOrElse(base, 0) + 1
        val name = if (nextCount == 1) base else s"${base}_$nextCount"
        (counts.updated(base, nextCount), acc.updated(key, name))
      }

    out
  }

  def packageName(
      protoPackage: Option[String],
      fileName: String
  ): PackageName = {
    val protoParts =
      protoPackage
        .map(_.split("\\.").toVector.filter(_.nonEmpty))
        .getOrElse(Vector.empty)

    val fromFileName =
      tokens(fileName.stripSuffix(".proto")).map(capitalizeToken)

    val packageParts =
      if (protoParts.nonEmpty) protoParts.map(constructorBaseName)
      else fromFileName

    val normalizedParts =
      if (packageParts.isEmpty) Vector("Generated")
      else packageParts

    PackageName.parts("Proto", normalizedParts*)
  }

  def flattenedTypeName(path: List[String]): String =
    path.map(constructorBaseName).mkString("_")

  def outputFilePath(pkg: PackageName): String =
    pkg.parts.toList.mkString("/") + ".bosatsu"
}
