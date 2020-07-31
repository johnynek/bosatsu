package org.bykn.bosatsu

import Identifier.Bindable

case class Program[T, +D, +S](
  types: T,
  lets: List[(Bindable, RecursionKind, D)],
  externalDefs: List[Bindable],
  from: S) {

  private[this] lazy val letMap: Map[Bindable, (RecursionKind, D)] =
    lets.iterator.map { case (n, r, d) => (n, (r, d)) }.toMap

  def getLet(name: Bindable): Option[(RecursionKind, D)] = letMap.get(name)
}

object Program {
  /**
   * For all duplicate binds, for all but the final
   * value, rename them
   */
  def makeLetsUnique[T, D, S](
    prog: Program[T, D, S])(
    newName: (Bindable, Int) => (Bindable, D => D)): Program[T, D, S] = {
    val dups: Map[Bindable, Int] =
      prog.lets.foldLeft(Map.empty[Bindable, Int]) {
        case (bound, (b, _, _)) =>
          bound.get(b) match {
            case Some(c) => bound.updated(b, c + 1)
            case None => bound.updated(b, 1)
          }
      }
      .filter { case (_, v) => v > 1 }

    if (dups.isEmpty) prog
    else {
      type BRD = (Bindable, RecursionKind, D)

      def renameUntilNext(name: Bindable, lets: List[BRD], acc: List[BRD])(fn: D => D): List[BRD] =
        lets match {
          case (head@(b1, r, d)) :: tail if b1 == name =>
            if (r.isRecursive) {
              // the new b1 is in scope right away
              (head :: acc).reverse ::: tail
            }
            else {
              // the old b1 is in scope for this one
              ((b1, r, fn(d)) :: acc).reverse ::: tail
            }
          case Nil => acc.reverse
          case (b, r, d) :: tail =>
            // this b is different from name, but may reference it
            val d1 = fn(d)
            renameUntilNext(name, tail, (b, r, d1) :: acc)(fn)
        }

      @annotation.tailrec
      def loop(lets: List[BRD], state: Map[Bindable, (Int, Int)], acc: List[BRD]): List[BRD] =
        lets match {
          case Nil => acc.reverse
          case (l@(b, r, d)) :: tail =>
            state.get(b) match {
              case Some((cnt, sz)) if cnt < (sz - 1) =>
                val newState = state.updated(b, (cnt + 1, sz))
                // we have to rename until the next bind
                val (b1, renamer) = newName(b, cnt)
                val d1 =
                  if (r.isRecursive) renamer(d)
                  else d

                val head1 = (b1, r, d1)
                val tail1 = renameUntilNext(b, tail, Nil)(renamer)
                loop(tail1, newState, head1 :: acc)
              case _ =>
                // this is the last one or not a duplicate, we don't change it
                loop(tail, state, l :: acc)
            }
        }

      // there are duplicates
      val dupState: Map[Bindable, (Int, Int)] =
        dups.iterator.map { case (k, sz) => (k, (0, sz)) }.toMap

      prog.copy(lets = loop(prog.lets, dupState, Nil))
    }
  }
}
