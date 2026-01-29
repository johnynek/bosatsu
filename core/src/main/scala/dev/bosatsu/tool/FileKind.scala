package dev.bosatsu.tool

sealed abstract class FileKind(val name: String) derives CanEqual
object FileKind {
  case object Source extends FileKind("source")
  case object Iface extends FileKind("interface")
  case object Pack extends FileKind("package")

  given cats.Eq[FileKind] = cats.Eq.fromUniversalEquals
}
