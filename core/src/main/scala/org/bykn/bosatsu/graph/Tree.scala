package org.bykn.bosatsu.graph

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._

case class Tree[+A](item: A, children: List[Tree[A]])

object Tree {

  def neighborsFn[A](t: Tree[A]): A => List[A] = {
    def toMap(t: Tree[A]): Map[A, Tree[A]] =
      Map(t.item -> t) ++ (t.children.flatMap(toMap(_)))

    val mapToTree: Map[A, Tree[A]] = toMap(t)

    { (a: A) => mapToTree.get(a).fold(List.empty[A])(_.children.map(_.item)) }
  }

  /** either return a tree representation of this dag or all cycles
    *
    * Note, this could run in a monadic context if we needed that: nfn: A =>
    * F[List[A]] for some monad F[_]
    */
  def dagToTree[A](
      node: A
  )(nfn: A => List[A]): ValidatedNel[NonEmptyList[A], Tree[A]] = {
    def treeOf(
        path: NonEmptyList[A],
        visited: Set[A]
    ): ValidatedNel[NonEmptyList[A], Tree[A]] = {
      val children = nfn(path.head)
      def assumeValid(
          children: List[A]
      ): ValidatedNel[NonEmptyList[A], Tree[A]] =
        children
          .traverse { a =>
            // we grow the path out here
            treeOf(a :: path, visited + a)
          }
          .map(Tree(path.head, _))

      NonEmptyList.fromList(children.filter(visited)) match {
        case Some(loops) =>
          val paths = loops.map(_ :: path)
          val invalid = Validated.invalid(paths)
          // also search all the valid cases
          invalid *> assumeValid(children.filterNot(visited))
        case None => assumeValid(children)
      }
    }
    treeOf(NonEmptyList(node, Nil), Set(node))
      .leftMap { nelnel =>
        // remove depulicate paths
        val withSet = nelnel.map { nel => (nel, nel.toList.toSet) }
        distinctBy(withSet)(_._2).map(_._1)
      }
  }

  def distinctBy[A, B](nel: NonEmptyList[A])(fn: A => B): NonEmptyList[A] = {
    @annotation.tailrec
    def remove(seen: Set[B], items: List[A], acc: List[A]): List[A] =
      items match {
        case Nil => acc.reverse
        case h :: t =>
          val b = fn(h)
          if (seen(b)) remove(seen, t, acc)
          else remove(seen + b, t, h :: acc)
      }

    NonEmptyList(nel.head, remove(Set(fn(nel.head)), nel.tail, Nil))
  }

  def distinctBy[A, B](nel: List[A])(fn: A => B): List[A] =
    NonEmptyList.fromList(nel) match {
      case None      => Nil
      case Some(nel) => distinctBy(nel)(fn).toList
    }
}
