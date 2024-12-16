package org.bykn.bosatsu.tool

sealed abstract class FileKind(val name: String)
object FileKind {
  case object Source extends FileKind("source")
  case object Iface extends FileKind("interface")
  case object Pack extends FileKind("package")
}
