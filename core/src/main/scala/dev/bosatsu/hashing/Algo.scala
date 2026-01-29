package dev.bosatsu.hashing

import cats.arrow.FunctionK
import cats.parse.Parser
import pt.kcry.blake3.{Blake3 => B3, Hasher => B3Hasher}

sealed abstract class Algo[A] {
  def name: String
  def hexLen: Int
  def hashBytes(bytes: Array[Byte]): HashValue[A]

  type Hasher
  def newHasher(): Hasher
  def hashBytes(h: Hasher, bytes: Array[Byte], offset: Int, len: Int): Hasher
  def finishHash(h: Hasher): HashValue[A]
}

// A represents the algorithm
case class HashValue[A](hex: String) {
  def toIdent(implicit A: Algo[A]): String =
    s"${A.name}:$hex"
}

object HashValue {
  implicit def orderHashValue[A]: cats.Order[HashValue[A]] =
    cats.Order.by(_.hex)

  implicit def orderingHashValue[A]: Ordering[HashValue[A]] =
    orderHashValue[A].toOrdering
}

object Algo {
  trait Blake3
  object Blake3 extends Blake3

  implicit def apply[A](implicit algo: Algo[A]): Algo[A] = algo

  implicit val blake3Algo: Algo[Blake3] =
    new Algo[Blake3] {
      def name: String = "blake3"
      def hexLen: Int = 64
      def hashBytes(bytes: Array[Byte]): HashValue[Blake3] =
        HashValue(
          B3.newHasher().update(bytes).doneHex(64)
        )

      type Hasher = B3Hasher
      def newHasher() = B3.newHasher()
      def hashBytes(
          h: Hasher,
          bytes: Array[Byte],
          offset: Int,
          len: Int
      ): Hasher =
        h.update(bytes, offset, len)
      def finishHash(h: Hasher): HashValue[Blake3] =
        HashValue(h.doneHex(64))
    }

  sealed abstract class WithAlgo[F[_]] {
    type A
    def algo: Algo[A]
    def value: F[A]

    def mapK[G[_]](fn: FunctionK[F, G]): WithAlgo[G] =
      WithAlgo[G, A](algo, fn(value))

    override def equals(obj: Any): Boolean =
      obj match {
        case e: WithAlgo[?] =>
          e.algo.equals(algo) && e.value.equals(value)
        case _ => false
      }

    override lazy val hashCode: Int =
      31 * (31 * algo.hashCode() + algo.name.hashCode()) + value.hashCode

    override def toString: String = s"WithAlgo($algo, $value)"
  }

  object WithAlgo {
    def apply[F[_], T](v: F[T])(implicit alg: Algo[T]): WithAlgo[F] {
      type A = T
    } =
      new WithAlgo[F] {
        type A = T
        def algo = alg
        def value = v
      }

    def apply[F[_], T](alg: Algo[T], v: F[T]): WithAlgo[F] { type A = T } =
      apply(v)(using alg)

    implicit class WithAlgoHashValue(private val hashValue: WithAlgo[HashValue])
        extends AnyVal {
      def toIdent: String = hashValue.value.toIdent(using hashValue.algo)
    }

    implicit val ordWithAlgoHash: cats.Order[WithAlgo[HashValue]] =
      cats.Order.by(_.toIdent)

    implicit val orderingWithAlgoHash: Ordering[WithAlgo[HashValue]] =
      Ordering.by(_.toIdent)
  }

  def hashBytes[A](bytes: Array[Byte])(implicit algo: Algo[A]): HashValue[A] =
    algo.hashBytes(bytes)

  private val hexChar = Parser.charIn(('0' to '9') ++ ('a' to 'f'))

  def parseHashValue[A](algo: Algo[A]): Parser[WithAlgo[HashValue]] =
    Parser.string(algo.name) *> Parser.char(':') *> {
      hexChar
        .repExactlyAs[String](algo.hexLen)
        .map(hex => WithAlgo(algo, HashValue[A](hex)))
    }

  val parseIdent: Parser[WithAlgo[HashValue]] =
    parseHashValue(blake3Algo)
}
