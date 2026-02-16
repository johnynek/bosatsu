package dev.bosatsu.hashing

import cats.Foldable
import java.nio.charset.StandardCharsets
import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

trait Hashable[A] {
  def hash[B](a: A)(using algo: Algo[B]): Hashed[B, A] = {
    val hasher = algo.newHasher()
    Hashed(algo.finishHash(addHash(a, algo)(hasher)), a)
  }

  def addHash[B](a: A, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher

  final def hashValue[B](a: A)(using algo: Algo[B]): HashValue[B] =
    hash(a).hash
}

private[hashing] trait LowPriorityHashableInstances {
  given [F[_], A](using
      foldable: Foldable[F],
      hashA: Hashable[A]
  ): Hashable[F[A]] with {
    def addHash[B](fa: F[A], algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      foldable.foldLeft(fa, hasher) { (h, a) =>
        hashA.addHash(a, algo)(h)
      }
  }
}

object Hashable extends LowPriorityHashableInstances {
  def apply[A](using hashable: Hashable[A]): Hashable[A] =
    hashable

  final class HashPartiallyApplied[B](private val u: Unit) extends AnyVal {
    def apply[A: Hashable](a: A)(using algo: Algo[B]): Hashed[B, A] =
      summon[Hashable[A]].hash(a)
  }

  def hash[B]: HashPartiallyApplied[B] =
    new HashPartiallyApplied[B](())

  def hash[A, B](algo: Algo[B], a: A)(using hashable: Hashable[A]): Hashed[B, A] =
    hashable.hash(a)(using algo)

  private val Utf8 = StandardCharsets.UTF_8
  private val Ascii = StandardCharsets.US_ASCII

  trait AnyHashable {
    def hashValue[B](value: Any, algo: Algo[B]): HashValue[B]
  }

  final class LiftedHashable[T](tHash: () => Hashable[T]) extends AnyHashable {
    def hashValue[B](value: Any, algo: Algo[B]): HashValue[B] =
      tHash().hashValue(value.asInstanceOf[T])(using algo)
  }

  private inline def summonHashables[T <: Tuple]: List[AnyHashable] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        new LiftedHashable[t](() => summonInline[Hashable[t]]) :: summonHashables[
          ts
        ]
    }

  private inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        constValue[t].asInstanceOf[String] :: summonLabels[ts]
    }

  private inline def summonTypeNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        typeNameOf[t] :: summonTypeNames[ts]
    }

  private inline def typeNameOf[A]: String = ${ typeNameOfImpl[A] }

  private def typeNameOfImpl[A: Type](using quotes: Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[A].dealias.show)
  }

  private def addBytes[B](
      bytes: Array[Byte],
      algo: Algo[B]
  )(hasher: algo.Hasher): algo.Hasher =
    if (bytes.isEmpty) hasher
    else algo.hashBytes(hasher, bytes, 0, bytes.length)

  private def addUtf8String[B](value: String, algo: Algo[B])(
      hasher: algo.Hasher
  ): algo.Hasher = {
    val bytes = value.getBytes(Utf8)
    val withLen = addIntBE(bytes.length, algo)(hasher)
    addBytes(bytes, algo)(withLen)
  }

  private def addByte[B](value: Byte, algo: Algo[B])(
      hasher: algo.Hasher
  ): algo.Hasher =
    addBytes(Array(value), algo)(hasher)

  private def addShortBE[B](
      value: Short,
      algo: Algo[B]
  )(hasher: algo.Hasher): algo.Hasher = {
    val intVal = value.toInt & 0xffff
    addBytes(
      Array(
        ((intVal >>> 8) & 0xff).toByte,
        (intVal & 0xff).toByte
      ),
      algo
    )(hasher)
  }

  private def addIntBE[B](value: Int, algo: Algo[B])(
      hasher: algo.Hasher
  ): algo.Hasher =
    addBytes(
      Array(
        ((value >>> 24) & 0xff).toByte,
        ((value >>> 16) & 0xff).toByte,
        ((value >>> 8) & 0xff).toByte,
        (value & 0xff).toByte
      ),
      algo
    )(hasher)

  private def addLongBE[B](value: Long, algo: Algo[B])(
      hasher: algo.Hasher
  ): algo.Hasher =
    addBytes(
      Array(
        ((value >>> 56) & 0xffL).toByte,
        ((value >>> 48) & 0xffL).toByte,
        ((value >>> 40) & 0xffL).toByte,
        ((value >>> 32) & 0xffL).toByte,
        ((value >>> 24) & 0xffL).toByte,
        ((value >>> 16) & 0xffL).toByte,
        ((value >>> 8) & 0xffL).toByte,
        (value & 0xffL).toByte
      ),
      algo
    )(hasher)

  private def addHashValue[B](
      hashValue: HashValue[B],
      algo: Algo[B]
  )(hasher: algo.Hasher): algo.Hasher =
    addBytes(hashValue.hex.getBytes(Ascii), algo)(hasher)

  private def addNestedHash[A, B](
      value: A,
      hashable: Hashable[A],
      algo: Algo[B]
  )(hasher: algo.Hasher): algo.Hasher =
    addHashValue(hashable.hashValue(value)(using algo), algo)(hasher)

  private def derivedProduct[A](
      typeLabel: String,
      fieldLabels: List[String],
      fields: List[AnyHashable]
  ): Hashable[A] =
    new Hashable[A] {
      def addHash[B](a: A, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher = {
        var h = addUtf8String(typeLabel, algo)(hasher)
        h = addIntBE(fields.size, algo)(h)
        val product = a.asInstanceOf[Product]
        var idx = 0
        var labels = fieldLabels
        var fs = fields
        while (fs.nonEmpty) {
          h = addUtf8String(labels.head, algo)(h)
          h = addHashValue(
            fs.head.hashValue(product.productElement(idx), algo),
            algo
          )(h)
          idx += 1
          labels = labels.tail
          fs = fs.tail
        }
        h
      }
    }

  private def derivedSum[A](
      typeLabel: String,
      sum: Mirror.SumOf[A],
      subtypeLabels: Vector[String],
      subtypes: Vector[AnyHashable]
  ): Hashable[A] =
    new Hashable[A] {
      def addHash[B](a: A, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher = {
        var h = addUtf8String(typeLabel, algo)(hasher)
        val ord = sum.ordinal(a)
        h = addIntBE(ord, algo)(h)
        h = addUtf8String(subtypeLabels(ord), algo)(h)
        h = addHashValue(subtypes(ord).hashValue(a, algo), algo)(h)
        h
      }
    }

  private def tupleHashableImpl[T <: Tuple: Type](using
      q: Quotes
  ): Expr[Hashable[T]] = {
    import q.reflect.*

    val tupleConsSym = Symbol.requiredClass("scala.*:")
    val emptyTupleTpe = TypeRepr.of[EmptyTuple]
    val tupleClassPrefix = "scala.Tuple"

    def loopTupleElements(tpe: TypeRepr): List[TypeRepr] =
      tpe.dealias match {
        case AppliedType(tyCon, head :: tail :: Nil)
            if tyCon.typeSymbol.eq(tupleConsSym) =>
          head :: loopTupleElements(tail)
        case AppliedType(tyCon, args)
            if tyCon.typeSymbol.fullName.startsWith(tupleClassPrefix) =>
          args
        case t if t =:= emptyTupleTpe =>
          Nil
        case other =>
          report.errorAndAbort(s"Expected tuple type, found: ${other.show}")
      }

    val elems = loopTupleElements(TypeRepr.of[T])
    val tupleLabelExpr = Expr(TypeRepr.of[T].dealias.show)
    val labelsExpr =
      Expr.ofList(elems.indices.map(i => Expr(s"_${i + 1}")).toList)
    val hashablesExpr: Expr[List[AnyHashable]] = Expr.ofList(
      elems.map { elem =>
        elem.asType match {
          case '[tt] =>
            '{
              new LiftedHashable[tt](() => summonInline[Hashable[tt]]): AnyHashable
            }
          case _ =>
            report.errorAndAbort(s"Unexpected tuple element type: ${elem.show}")
        }
      }
    )

    '{
      derivedProduct[T]($tupleLabelExpr, $labelsExpr, $hashablesExpr)
    }
  }

  inline given [T <: Tuple]: Hashable[T] = ${ tupleHashableImpl[T] }

  inline given derived[A](using mirror: Mirror.Of[A]): Hashable[A] = {
    val label = typeNameOf[A]
    inline mirror match {
      case sum: Mirror.SumOf[A] =>
        derivedSum(
          label,
          sum,
          summonTypeNames[mirror.MirroredElemTypes].toVector,
          summonHashables[mirror.MirroredElemTypes].toVector
        )
      case _: Mirror.ProductOf[A] =>
        derivedProduct(
          label,
          summonLabels[mirror.MirroredElemLabels],
          summonHashables[mirror.MirroredElemTypes]
        )
    }
  }

  given Hashable[Unit] with {
    def addHash[B](a: Unit, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      hasher
  }

  given Hashable[Boolean] with {
    def addHash[B](a: Boolean, algo: Algo[B])(
        hasher: algo.Hasher
    ): algo.Hasher =
      addByte(if (a) 1.toByte else 0.toByte, algo)(hasher)
  }

  given Hashable[Byte] with {
    def addHash[B](a: Byte, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addByte(a, algo)(hasher)
  }

  given Hashable[Short] with {
    def addHash[B](a: Short, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addShortBE(a, algo)(hasher)
  }

  given Hashable[Char] with {
    def addHash[B](a: Char, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addShortBE(a.toShort, algo)(hasher)
  }

  given Hashable[Int] with {
    def addHash[B](a: Int, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addIntBE(a, algo)(hasher)
  }

  given Hashable[Long] with {
    def addHash[B](a: Long, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addLongBE(a, algo)(hasher)
  }

  given Hashable[Float] with {
    def addHash[B](a: Float, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addIntBE(java.lang.Float.floatToRawIntBits(a), algo)(hasher)
  }

  given Hashable[Double] with {
    def addHash[B](a: Double, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addLongBE(java.lang.Double.doubleToRawLongBits(a), algo)(hasher)
  }

  given Hashable[String] with {
    def addHash[B](a: String, algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      addBytes(a.getBytes(Utf8), algo)(hasher)
  }

  given Hashable[Array[Byte]] with {
    override def hash[B](a: Array[Byte])(using
        algo: Algo[B]
    ): Hashed[B, Array[Byte]] =
      Hashed[B, Array[Byte]](algo.hashBytes(a), a)

    def addHash[B](a: Array[Byte], algo: Algo[B])(
        hasher: algo.Hasher
    ): algo.Hasher =
      addBytes(a, algo)(hasher)
  }

  given [A, B](using sourceAlgo: Algo[A]): Hashable[Hashed[A, B]] with {
    def addHash[C](a: Hashed[A, B], algo: Algo[C])(
        hasher: algo.Hasher
    ): algo.Hasher = {
      val ident = a.hash.toIdent(using sourceAlgo)
      addBytes(ident.getBytes(Utf8), algo)(hasher)
    }
  }

  given [A](using hashA: Hashable[A]): Hashable[Option[A]] with {
    def addHash[B](a: Option[A], algo: Algo[B])(hasher: algo.Hasher): algo.Hasher =
      a match {
        case Some(value) =>
          val withTag = addByte(1.toByte, algo)(hasher)
          addNestedHash(value, hashA, algo)(withTag)
        case None =>
          addByte(0.toByte, algo)(hasher)
      }
  }

  given [A](using hashA: Hashable[A]): Hashable[List[A]] with {
    def addHash[B](a: List[A], algo: Algo[B])(hasher: algo.Hasher): algo.Hasher = {
      var h = addIntBE(a.length, algo)(hasher)
      val iter = a.iterator
      while (iter.hasNext) {
        h = addNestedHash(iter.next(), hashA, algo)(h)
      }
      h
    }
  }

  given [A](using hashA: Hashable[A]): Hashable[Vector[A]] with {
    def addHash[B](a: Vector[A], algo: Algo[B])(
        hasher: algo.Hasher
    ): algo.Hasher = {
      var h = addIntBE(a.length, algo)(hasher)
      val iter = a.iterator
      while (iter.hasNext) {
        h = addNestedHash(iter.next(), hashA, algo)(h)
      }
      h
    }
  }
}
