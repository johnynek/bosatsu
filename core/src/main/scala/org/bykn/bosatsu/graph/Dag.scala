package org.bykn.bosatsu.graph

import scala.collection.immutable.SortedSet
import scala.collection.mutable.{Map => MMap}
import org.bykn.bosatsu.ListOrdering

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

  override def equals(that: Any) =
    that match {
      case thatDag: Dag[_] =>
        def eqDag[B](bs: Dag[B]): Boolean = {
          (nodes == bs.nodes) && {
            nodes.iterator.zip(bs.nodes.iterator).forall { case (a, b) =>
              deps(a) == bs.deps(b)
            }
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
      nfn: A => Iterable[A]
  ): (A => Option[SortedSet[A]], Dag[SortedSet[A]]) = {

    val nodeVec: Vector[A] = nodes.to(Vector).sorted
    def allReachable(nfn: A => Iterable[A]): Map[A, SortedSet[A]] = {
      def step(m: Map[A, SortedSet[A]]): Map[A, SortedSet[A]] =
        nodeVec.iterator
          .map { a =>
            val current = m(a)
            val next = nfn(a).map(m).foldLeft(SortedSet.empty[A])(_ | _)
            a -> (current | next)
          }
          .to(Map)

      @annotation.tailrec
      def loop(m: Map[A, SortedSet[A]]): Map[A, SortedSet[A]] = {
        val m1 = step(m)
        if (m1 == m) m
        else loop(m1)
      }

      loop(nodeVec.iterator.map { a => (a, SortedSet.empty[A] + a) }.to(Map))
    }

    // two nodes are joined into one cluster if a can reach b and vice-versa
    // all edges are either within or between clusters
    val originalAR = allReachable(nfn)
    val internalNfn: A => Iterable[A] = { a =>
      nfn(a).filter { b =>
        originalAR(a).contains(b) && originalAR(b).contains(a)
      }
    }

    val externalNfn: A => Iterator[A] = { a =>
      nfn(a).iterator.filterNot { b =>
        originalAR(a).contains(b) && originalAR(b).contains(a)
      }
    }

    val clusterMap: Map[A, SortedSet[A]] = allReachable(internalNfn)

    implicit val ordSortedSet: Ordering[SortedSet[A]] =
      ListOrdering.byIterator[SortedSet[A], A]

    val dag: Dag[SortedSet[A]] =
      new Dag[SortedSet[A]] {
        val nodes =
          clusterMap.iterator.foldLeft(SortedSet.empty[SortedSet[A]]) {
            case (n, (_, v)) => n + v
          }
        val depCache = MMap.empty[SortedSet[A], SortedSet[SortedSet[A]]]
        def deps(cluster: SortedSet[A]): SortedSet[SortedSet[A]] =
          depCache.getOrElseUpdate(
            cluster, {
              // if any node in the set is linked, the set is:
              cluster.iterator
                .flatMap { a =>
                  externalNfn(a).map(clusterMap)
                }
                .to(SortedSet)
            }
          )
      }

    (clusterMap.get(_), dag)
  }

  def fromToposorted[A: Ordering](s: Toposort.Success[A]): Dag[A] =
    new Dag[A] {
      val nodes = s.layers.iterator.flatMap { nel => nel.toList }.to(SortedSet)
      val depCache = MMap.empty[A, SortedSet[A]]
      def deps(a: A): SortedSet[A] =
        depCache.getOrElseUpdate(a, s.nfn(a).to(SortedSet))

    }
}
