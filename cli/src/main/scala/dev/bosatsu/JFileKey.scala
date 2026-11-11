package dev.bosatsu

import cats.Hash
import java.nio.file.attribute.BasicFileAttributes

/** A native filesystem identity whose supported contract is Hash.
  * Java returns an Object, but BasicFileAttributes.fileKey guarantees equality
  * and hashing suitable for collections while the filesystem remains static.
  * Keep this representation inside the JVM interop boundary.
  */
opaque type JFileKey = Object

object JFileKey {
  private[bosatsu] def fromAttributes(attrs: BasicFileAttributes): Option[JFileKey] =
    Option(attrs.fileKey())

  given Hash[JFileKey] with {
    def hash(key: JFileKey): Int = key.hashCode()
    def eqv(left: JFileKey, right: JFileKey): Boolean = left.equals(right)
  }
}
