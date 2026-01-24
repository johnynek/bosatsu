package dev.bosatsu.hashing

case class Hashed[A, +Of](hash: HashValue[A], arg: Of)

object Hashed {
  def viaBytes[A: Algo, B](value: B)(fn: B => Array[Byte]): Hashed[A, B] =
    Hashed(Algo[A].hashBytes(fn(value)), value)
}
