package org.bykn.bosatsu

final object FixType {

  /** Use a trick in scala to give an opaque type for a fixed point recursion
    * without having to allocate wrappers at each level
    */
  type Fix[F[_]]

  final def fix[F[_]](f: F[Fix[F]]): Fix[F] =
    f.asInstanceOf[Fix[F]]

  final def unfix[F[_]](f: Fix[F]): F[Fix[F]] =
    f.asInstanceOf[F[Fix[F]]]
}
