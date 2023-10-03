package org.bykn.bosatsu

import cats.data.NonEmptyList

private[bosatsu] object ListUtil {
  // filter b from a pretty short lst but try to conserve lst if possible
  def filterNot[A](lst: List[A])(b: A => Boolean): List[A] =
    lst match {
      case Nil => lst
      case h :: tail =>
        val t1 = filterNot(tail)(b)
        if (b(h)) t1
        else if (t1 eq tail) lst
        else (h :: t1) // we only allocate here
    }

  def greedyGroup[A, G](list: NonEmptyList[A])(one: A => G)(combine: (G, A) => Option[G]): NonEmptyList[G] = {
    def loop(g: G, tail: List[A]): NonEmptyList[G] =
      tail match {
        case Nil => NonEmptyList.one(g)
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

  def greedyGroup[A, G](list: List[A])(one: A => G)(combine: (G, A) => Option[G]): List[G] =
    NonEmptyList.fromList(list) match {
      case None => Nil
      case Some(nel) => greedyGroup(nel)(one)(combine).toList
    }

}