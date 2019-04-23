package org.bykn.bosatsu

import java.nio.file.{Path => JPath}

abstract class Path {
  def toJPath: JPath
  def resolve(str: String): Path
  def getParent: Option[Path]
  def isAbsolute: Boolean
  def names: List[String]
}
