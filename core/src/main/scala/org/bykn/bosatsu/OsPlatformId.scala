package org.bykn.bosatsu

sealed abstract class OsPlatformId(val name: String)

object OsPlatformId {
  case object Linux extends OsPlatformId("Linux")
  case object MacOS extends OsPlatformId("MacOS")
  case object Windows extends OsPlatformId("Windows")
  case class Unknown(n: String) extends OsPlatformId(n)
}
