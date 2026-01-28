package dev.bosatsu

import cats.{Applicative, Functor, Order, Parallel}
import scala.collection.immutable.SortedMap

import cats.syntax.all._

/** There are all the distinct imported names and the original ImportedName
  */
case class ImportMap[A, B](toMap: SortedMap[Identifier, (A, ImportedName[B])]) {
  def apply(name: Identifier): Option[(A, ImportedName[B])] =
    toMap.get(name)

  def +(that: (A, ImportedName[B])): ImportMap[A, B] =
    ImportMap(toMap.updated(that._2.localName, that))

  def toList(implicit ord: Order[A]): List[Import[A, B]] =
    toMap.iterator
      .map { case (_, ab) => ab }
      .toList
      .groupByNel(_._1)
      .iterator
      .map { case (a, bs) =>
        Import(a, bs.map(_._2))
      }
      .toList

  def traverse[F[_]: Applicative, C, D](
      fn: (A, ImportedName[B]) => F[(C, ImportedName[D])]
  ): F[ImportMap[C, D]] =
    toMap
      .traverse[F, (C, ImportedName[D])] { case (a, ib) => fn(a, ib) }
      .map(ImportMap(_))

  def parTraverse[F[_]: Parallel: Functor, C, D](
      fn: (A, ImportedName[B]) => F[(C, ImportedName[D])]
  ): F[ImportMap[C, D]] =
    toMap
      .parTraverse[F, (C, ImportedName[D])] { case (a, ib) => fn(a, ib) }
      .map(ImportMap(_))
}

object ImportMap {
  def empty[A, B]: ImportMap[A, B] = ImportMap(SortedMap.empty)

  sealed abstract class Unify derives CanEqual
  object Unify {
    case object Error extends Unify
    case object Left extends Unify
    case object Right extends Unify
  }
  // Return the list of collisions in local names along with a map
  // with the last name overwriting the import
  def fromImports[A, B](
      is: List[Import[A, B]]
  )(
      unify: ((A, ImportedName[B]), (A, ImportedName[B])) => Unify
  ): (List[(A, ImportedName[B])], ImportMap[A, B]) =
    is.iterator
      .flatMap { case Import(p, is) => is.toList.iterator.map((p, _)) }
      .foldLeft((List.empty[(A, ImportedName[B])], ImportMap.empty[A, B])) {
        case (old @ (dups, imap), pim @ (_, im)) =>
          val (dups1, imap1) = imap(im.localName) match {
            case Some(nm) =>
              unify(nm, pim) match {
                case Unify.Error =>
                  // pim and nm are a collision, add both
                  (pim :: nm :: dups, imap + pim)
                case Unify.Left  => old
                case Unify.Right => (dups, imap + pim)
              }
            case None => (dups, imap + pim)
          }

          (dups1.reverse.distinct, imap1)
      }

  // This is only safe after verifying there are not collisions
  // which has been done on compiled packages
  def fromImportsUnsafe[A, B](is: List[Import[A, B]]): ImportMap[A, B] =
    fromImports(is)((a, b) => sys.error(s"collision in $a and $b: $is"))._2
}
