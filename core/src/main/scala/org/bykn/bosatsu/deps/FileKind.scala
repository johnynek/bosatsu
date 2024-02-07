package org.bykn.bosatsu.deps

import cats.Order

sealed abstract class FileKind(val name: String)
object FileKind {
  case object Source extends FileKind("source")
  case object Iface extends FileKind("interface")
  case object Pack extends FileKind("package")

  // order such that less information comes before more information
  implicit val orderKind: cats.Order[FileKind] = 
    new cats.Order[FileKind] {
      def compare(a: FileKind, b: FileKind) =
        (a, b) match {
          case (FileKind.Iface, FileKind.Iface) => 0
          case (FileKind.Iface, _) => -1
          case (FileKind.Pack, FileKind.Iface) => 1
          case (FileKind.Pack, FileKind.Pack) => 0
          case (FileKind.Pack, FileKind.Source) => -1
          case (FileKind.Source, FileKind.Source) => 0
          case (FileKind.Source, _) => 1
        }
    }

  implicit def orderingKind: Ordering[FileKind] = orderKind.toOrdering
}
