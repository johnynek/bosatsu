package org.bykn.bosatsu.pattern

import cats.Monoid

trait Splitter[Elem, Item, Sequence, R] {
  def matcher: Matcher[Elem, Item, R]
  def monoidResult: Monoid[R]

  def toResult(i: Item): R

  def foldMap(s: Sequence): R

  // return all the places such that fst ++ c ++ snd == str
  def positions(c: Elem, str: Sequence): Stream[(Sequence, R, Sequence)]

  // splits skipping a single character to match AnyElem
  def anySplits(str: Sequence): Stream[(Sequence, R, Sequence)]

  def isEmpty(s: Sequence): Boolean

  def uncons(s: Sequence): Option[(Item, Sequence)]

  def anyMatch(s: Sequence): R
}
