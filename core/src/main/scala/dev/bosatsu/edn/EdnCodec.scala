package dev.bosatsu.edn

import cats.parse.{Parser => P}
import org.typelevel.paiges.Doc

trait EdnCodec[A] {
  def encode(a: A): Edn
  def decode(edn: Edn): Either[String, A]

  final def toDoc(a: A): Doc =
    Edn.toDoc(encode(a))

  final def render(a: A, width: Int = 100): String =
    toDoc(a).render(width)

  final def parse(input: String): Either[String, A] =
    Edn.parseAll(input) match {
      case Right(edn) => decode(edn)
      case Left(err)  => Left(err.toString)
    }

  final val parser: P[A] =
    Edn.parser.mapFilter(edn => decode(edn).toOption)
}

object EdnCodec {
  def apply[A](using codec: EdnCodec[A]): EdnCodec[A] =
    codec

  def encode[A: EdnCodec](a: A): Edn =
    summon[EdnCodec[A]].encode(a)

  def decode[A: EdnCodec](edn: Edn): Either[String, A] =
    summon[EdnCodec[A]].decode(edn)

  def decode[A: EdnCodec](str: String): Either[String, A] =
    Edn.parseAll(str) match {
      case Right(edn) => decode[A](edn)
      case Left(err)  => Left(err.toString)
    }

  def toDoc[A: EdnCodec](a: A): Doc =
    summon[EdnCodec[A]].toDoc(a)

  def render[A: EdnCodec](a: A, width: Int = 100): String =
    summon[EdnCodec[A]].render(a, width)

  def parse[A: EdnCodec](input: String): Either[String, A] =
    summon[EdnCodec[A]].parse(input)
}
