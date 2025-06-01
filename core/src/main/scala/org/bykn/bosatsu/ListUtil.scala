package org.bykn.bosatsu

import cats.data.NonEmptyList

private[bosatsu] object ListUtil {
  // filter b from a pretty short lst but try to conserve lst if possible
  def filterNot[A](lst: List[A])(b: A => Boolean): List[A] =
    lst match {
      case Nil       => lst
      case h :: tail =>
        val t1 = filterNot(tail)(b)
        if (b(h)) t1
        else if (t1 eq tail) lst
        else (h :: t1) // we only allocate here
    }

  def greedyGroup[A, G](
      list: NonEmptyList[A]
  )(one: A => G)(combine: (G, A) => Option[G]): NonEmptyList[G] = {
    def loop(g: G, tail: List[A]): NonEmptyList[G] =
      tail match {
        case Nil            => NonEmptyList.one(g)
        case tailh :: tailt =>
          combine(g, tailh) match {
            case None =>
              // can't combine into the head group, start a new group
              g :: loop(one(tailh), tailt)
            case Some(g1) =>
              // we can combine into a new group
              loop(g1, tailt)
          }
      }

    loop(one(list.head), list.tail)
  }

  def greedyGroup[A, G](
      list: List[A]
  )(one: A => G)(combine: (G, A) => Option[G]): List[G] =
    NonEmptyList.fromList(list) match {
      case None      => Nil
      case Some(nel) => greedyGroup(nel)(one)(combine).toList
    }

  def mapConserveNel[A <: AnyRef, B >: A <: AnyRef](
      nel: NonEmptyList[A]
  )(f: A => B): NonEmptyList[B] = {
    val as = nel.toList
    val bs = as.mapConserve(f)
    if (bs eq as) nel
    else NonEmptyList.fromListUnsafe(bs)
  }

  def distinctByHashSet[A](nel: NonEmptyList[A]): NonEmptyList[A] = {
    // This code leverages the scala type ::[A] which is the nonempty
    // list in order to avoid allocations building a NonEmptyList
    // since a :: tailnel will have to allocate twice vs 1 time.
    def revCons(item: ::[A], tail: List[A]): NonEmptyList[A] =
      item.tail match {
        case nel: ::[A]  => revCons(nel, item.head :: tail)
        case _: Nil.type => NonEmptyList(item.head, tail)
      }
    @annotation.tailrec
    def loop(prior: Set[A], tail: List[A], front: ::[A]): NonEmptyList[A] =
      tail match {
        case Nil          => revCons(front, Nil)
        case head :: next =>
          if (prior(head)) loop(prior, next, front)
          else loop(prior + head, next, ::(head, front))
      }

    val h = nel.head
    loop(Set.empty + h, nel.tail, ::(h, Nil))
  }
}
