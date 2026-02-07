package dev.bosatsu.graph

import cats.{Eq, Order}
import cats.data.NonEmptyList
import cats.syntax.eq._
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable.{Map => MMap}
import dev.bosatsu.ListOrdering

sealed trait Dag[A] {
  def nodes: SortedSet[A]
  def deps(a: A): SortedSet[A]

  private val layerCache: MMap[A, Int] = MMap.empty[A, Int]

  // 0: no deps, 1: only depend on layer 0 and less, 2: layers 1 and less ...
  def layerOf(a: A): Int =
    // we know we can't infinitely loop on a dag, so this cache is safe
    layerCache.getOrElseUpdate(
      a, {
        val d = deps(a)
        if (d.isEmpty) 0
        else (d.iterator.map(layerOf(_)).max + 1)
      }
    )

  // the exclusive upper bound of layerOf
  lazy val layers: Int =
    if (nodes.isEmpty) 0
    else nodes.iterator.map(layerOf(_)).max + 1

  def toToposorted: Toposort.Success[A] = {
    val layerMap: Map[Int, SortedSet[A]] = nodes.groupBy(layerOf(_))
    val ls = (0 until layers).iterator
      .map { idx =>
        // by construction all layers have at least 1 item
        NonEmptyList.fromListUnsafe(layerMap(idx).toList)
      }
      .to(Vector)

    Toposort.Success(ls)
  }

  override def equals(that: Any) =
    that match {
      case thatDag: Dag[?] =>
        def eqDag[B](bs: Dag[B]): Boolean =
          nodes.equals(bs.nodes) && {
            nodes.iterator.zip(bs.nodes.iterator).forall { case (a, b) =>
              deps(a).equals(bs.deps(b))
            }
          }
        eqDag(thatDag)
      case _ => false
    }

  override lazy val hashCode: Int =
    nodes.hashCode
}

object Dag {
  implicit class Dagified[A: Ordering](dag: Dag[SortedSet[A]]) {
    def unsingleton: Option[Dag[A]] =
      if (dag.nodes.forall(_.size == 1)) Some {
        new Dag[A] {
          val nodes = dag.nodes.map(_.head)
          val depsCache = MMap.empty[A, SortedSet[A]]
          def deps(a: A): SortedSet[A] =
            depsCache.getOrElseUpdate(a, dag.deps(SortedSet(a)).map(_.head))
        }
      }
      else None
  }

  def dagify[A: Ordering](nodes: Iterable[A])(
      nfn: A => IterableOnce[A]
  ): (A => Option[SortedSet[A]], Dag[SortedSet[A]]) = {

    def allReachable(
        nfn: A => IterableOnce[A]
    ): SortedMap[A, SortedSet[A]] = {
      given Order[A] = Order.fromOrdering(using summon[Ordering[A]])
      given Eq[SortedMap[A, SortedSet[A]]] = Eq.by(_.toList)

      def step(
          m: SortedMap[A, SortedSet[A]]
      ): SortedMap[A, SortedSet[A]] =
        m.iterator
          .map { case (a, current) =>
            val next = nfn(a).iterator.foldLeft(SortedSet.empty[A]) { (s, a) =>
              s | m(a)
            }
            a -> (current | next)
          }
          .to(SortedMap)

      @annotation.tailrec
      def loop(m: SortedMap[A, SortedSet[A]]): SortedMap[A, SortedSet[A]] = {
        val m1 = step(m)
        if (m1 === m) m
        else loop(m1)
      }

      loop(
        nodes.iterator
          .map(a => (a, SortedSet.empty[A] + a))
          .to(SortedMap)
      )
    }

    // two nodes are joined into one cluster if a can reach b and vice-versa
    // all edges are either within or between clusters
    val originalAR = allReachable(nfn)
    val internalNfn: A => IterableOnce[A] = { a =>
      nfn(a).iterator.filter { b =>
        originalAR(a).contains(b) && originalAR(b).contains(a)
      }
    }

    val externalNfn: A => Iterator[A] = { a =>
      nfn(a).iterator.filterNot { b =>
        originalAR(a).contains(b) && originalAR(b).contains(a)
      }
    }

    val clusterMap: SortedMap[A, SortedSet[A]] = allReachable(internalNfn)

    implicit val ordSortedSet: Ordering[SortedSet[A]] =
      ListOrdering.byIterator[SortedSet[A], A]

    val dag: Dag[SortedSet[A]] =
      new Dag[SortedSet[A]] {
        val nodes = clusterMap.iterator.map(_._2).to(SortedSet)
        val depCache = MMap.empty[SortedSet[A], SortedSet[SortedSet[A]]]
        def deps(cluster: SortedSet[A]): SortedSet[SortedSet[A]] =
          depCache.getOrElseUpdate(
            cluster,
            // if any node in the set is linked, the set is:
            cluster.iterator
              .flatMap { a =>
                externalNfn(a).map(clusterMap)
              }
              .to(SortedSet)
          )
      }

    (clusterMap.get(_), dag)
  }

  def transitiveSet[A: Ordering](
      nodes: List[A]
  )(nfn: A => Iterable[A]): SortedSet[A] = {
    def loop(
        stack: List[A],
        inStack: SortedSet[A],
        reached: SortedSet[A]
    ): SortedSet[A] =
      stack match {
        case head :: tail =>
          val next = nfn(head).iterator
            .filterNot(n => inStack(n) || reached(n))
            .toList
            .sorted
          loop(next ::: tail, inStack ++ next, reached + head)
        case Nil => reached
      }

    loop(nodes, SortedSet.empty, SortedSet.empty)
  }
}
