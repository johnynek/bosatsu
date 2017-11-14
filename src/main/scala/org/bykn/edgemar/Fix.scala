package org.bykn.edgemar

case class Fix[F[_]](unfix: F[Fix[F]])
