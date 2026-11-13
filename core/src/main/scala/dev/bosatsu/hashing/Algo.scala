package dev.bosatsu.hashing

import cats.arrow.FunctionK
import cats.parse.Parser
import dev.bosatsu.Json
import pt.kcry.blake3.{Blake3 => B3, Hasher => B3Hasher}
import pt.kcry.sha.Sha2_256

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

  trait Sha256
  object Sha256 extends Sha256

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

  implicit val sha256Algo: Algo[Sha256] =
    new Algo[Sha256] {
      def name: String = "sha256"
      def hexLen: Int = 64
      def hashBytes(bytes: Array[Byte]): HashValue[Sha256] =
        HashValue(toHex(Sha2_256.hash(bytes)))

      type Hasher = Sha2_256
      def newHasher(): Hasher = new Sha2_256()
      def hashBytes(
          h: Hasher,
          bytes: Array[Byte],
          offset: Int,
          len: Int
      ): Hasher = {
        h.update(bytes, offset, len)
        h
      }
      def finishHash(h: Hasher): HashValue[Sha256] = {
        val bytes = new Array[Byte](32)
        h.finish(bytes, 0)
        HashValue(toHex(bytes))
      }
    }

  private val hexDigits = "0123456789abcdef"

  private def toHex(bytes: Array[Byte]): String = {
    val result = new StringBuilder(bytes.length * 2)
    var idx = 0
    while (idx < bytes.length) {
      val byte = bytes(idx) & 0xff
      result.append(hexDigits.charAt(byte >>> 4))
      result.append(hexDigits.charAt(byte & 0x0f))
      idx += 1
    }
    result.result()
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

    implicit val jsonReader: Json.Reader[WithAlgo[HashValue]] =
      Json.Reader.fromParser("Algo.WithAlgo[HashValue]", Algo.parseIdent)

    implicit val jsonWriter: Json.Writer[WithAlgo[HashValue]] =
      Json.Writer.from(withAlgo => Json.JString(withAlgo.toIdent))
  }

  def hashBytes[A](bytes: Array[Byte])(implicit algo: Algo[A]): HashValue[A] =
    algo.hashBytes(bytes)

  private val hexChar = Parser.charIn(('0' to '9') ++ ('a' to 'f'))

  def parseHashHex[A](algo: Algo[A]): Parser[HashValue[A]] =
    hexChar
      .repExactlyAs[String](algo.hexLen)
      .map(hex => HashValue[A](hex))

  def parseHashValue[A](algo: Algo[A]): Parser[HashValue[A]] =
    Parser.string(algo.name) *> Parser.char(':') *> parseHashHex(algo)

  val parseIdent: Parser[WithAlgo[HashValue]] =
    parseHashValue(blake3Algo)
      .map(WithAlgo(blake3Algo, _))
      .orElse(parseHashValue(sha256Algo).map(WithAlgo(sha256Algo, _)))
}
