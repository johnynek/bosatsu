package dev.bosatsu.hashing

import cats.{Eq, Hash}

/** JVM equality compares `arg` with its own `equals`, which is identity for
  * array-backed values such as `Bytes`. Compare through `Hash[Hashed[A, B]]`.
  */
case class Hashed[A, +Of](hash: HashValue[A], arg: Of)

object Hashed {
  /** The digest already summarizes `arg`, so it alone is the hash. Equality
    * still checks `arg`, but only after the digests match: nothing forces a
    * digest to be the digest of its argument.
    */
  given hashedHash[A, B](using eqArg: Eq[B]): Hash[Hashed[A, B]] =
    new Hash[Hashed[A, B]] {
      def eqv(left: Hashed[A, B], right: Hashed[A, B]): Boolean =
        left.hash.hex == right.hash.hex && eqArg.eqv(left.arg, right.arg)

      def hash(value: Hashed[A, B]): Int = value.hash.hex.hashCode
    }

  // `fn` returns Array[Byte], not Bytes: its result is hashed once and dropped,
  // so the encoding never outlives this call and never pairs with the hash.
  def viaBytes[A: Algo, B](value: B)(fn: B => Array[Byte]): Hashed[A, B] =
    Hashed(Algo[A].hashBytes(fn(value)), value)
}
