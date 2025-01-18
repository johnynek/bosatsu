package org.bykn.bosatsu.hashing

sealed abstract class Algo[A] {
  def name: String
  def hashBytesHex(bytes: Array[Byte]): HashValue[A]
}

object Algo {
  trait Sha256
  object Sha256 extends Sha256

  implicit val sha256Algo: Algo[Sha256] =
    new Algo[Sha256] {
      def name: String = "sha256"
      def hashBytesHex(bytes: Array[Byte]): HashValue[Sha256] =
        HashValue(Sha256Hash.sha256HashHex(bytes))
    }

  def hashBytesHex[A](bytes: Array[Byte])(implicit
      algo: Algo[A]
  ): HashValue[A] =
    algo.hashBytesHex(bytes)
}
