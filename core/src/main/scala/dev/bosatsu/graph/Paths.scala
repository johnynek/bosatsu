package dev.bosatsu.graph

import cats.Eq
import cats.data.NonEmptyList

object Paths {

  /** A list of cycles all terminating at node E is intended to carry state
    * about the edge in the graph
    */
  def allCycles[A: Eq, E](node: A)(
      nfn: A => List[(E, A)]
  ): List[NonEmptyList[(E, A)]] =
    allPaths(node, node)(nfn)

  /** A list of paths all terminating at to, but omitting from. E is intended to
    * carry state about the edge in the graph
    */
  def allPaths[A: Eq, E](from: A, to: A)(
      nfn: A => List[(E, A)]
  ): List[NonEmptyList[(E, A)]] = {
    def loop(from: A, to: A, avoid: Set[A]): List[NonEmptyList[(E, A)]] = {
      val newPaths = nfn(from).filterNot { case (_, a) => avoid(a) }
      val (ends, notEnds) = newPaths.partition { case (_, a) =>
        Eq[A].eqv(a, to)
      }

      val rest = notEnds.flatMap { case edge @ (_, a) =>
        // don't loop back on a, loops to a are handled by ends
        loop(a, to, avoid + a).map(edge :: _)
      }

      NonEmptyList.fromList(ends) match {
        case None         => rest
        case Some(endsNE) => endsNE :: rest
      }
    }

    loop(from, to, Set.empty)
  }

  /** Same as allPaths but without the edge annotation type
    */
  def allPaths0[A: Eq](start: A, end: A)(
      nfn: A => List[A]
  ): List[NonEmptyList[A]] =
    allPaths(start, end)(nfn.andThen(_.map(((), _)))).map(_.map(_._2))

  /** Same as allCycles but without the edge annotation type
    */
  def allCycle0[A: Eq](start: A)(nfn: A => List[A]): List[NonEmptyList[A]] =
    allPaths0(start, start)(nfn)
}
