package org.bykn.bosatsu

case class Fix[F[_]](unfix: F[Fix[F]])
