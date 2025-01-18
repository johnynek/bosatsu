package org.bykn.bosatsu.hashing

import cats.parse.Parser

sealed abstract class Algo[A] {
  def name: String
  def hexLen: Int 
  def hashBytes(bytes: Array[Byte]): HashValue[A]
}

// A represents the algorithm
case class HashValue[A](hex: String) {
  def toIdent(implicit A: Algo[A]): String =
    s"${A.name}:$hex"
}

object Algo {
  trait Sha256
  object Sha256 extends Sha256
  
  implicit val sha256Algo: Algo[Sha256] =
    new Algo[Sha256] {
      def name: String = "sha256"
      def hexLen: Int = 64
      def hashBytes(bytes: Array[Byte]): HashValue[Sha256] =
        HashValue(Sha256Hash.sha256HashHex(bytes))
    }

  sealed abstract class WithAlgo[F[_]] {
    type A
    def algo: Algo[A]
    def value: F[A]

    override def equals(obj: Any): Boolean =
      obj match {
        case e: WithAlgo[_] => (e.algo == algo) && (e.value == value)
        case _ => false
      }

    override lazy val hashCode: Int =
      31 * (31 * algo.hashCode() + algo.name.hashCode()) + value.hashCode

    override def toString: String = s"WithAlgo($algo, $value)"
  }

  object WithAlgo {
    def apply[F[_], T](alg: Algo[T], v: F[T]): WithAlgo[F] =
      new WithAlgo[F] {
        type A = T
        def algo = alg
        def value = v
      }
  }

  def hashBytes[A](bytes: Array[Byte])(implicit algo: Algo[A]): HashValue[A] =
    algo.hashBytes(bytes)


  private val hexChar = Parser.charIn(('0' to '9') ++ ('a' to 'f'))

  def parseHashValue[A](algo: Algo[A]): Parser[WithAlgo[HashValue]] =
    Parser.string(algo.name) *> Parser.char(':') *> {
      hexChar.repExactlyAs[String](algo.hexLen)
        .map(hex => WithAlgo(algo, HashValue[A](hex)))
    }

  val parseIdent: Parser[WithAlgo[HashValue]] =
    parseHashValue(sha256Algo)
}