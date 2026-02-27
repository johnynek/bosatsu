package dev.bosatsu

object NameSuggestion {
  sealed abstract class ScopePriority(val rank: Int) derives CanEqual
  object ScopePriority {
    case object Local extends ScopePriority(0)
    case object SamePackage extends ScopePriority(1)
    case object Imported extends ScopePriority(2)
  }

  final case class Candidate[+A](
      ident: Identifier,
      value: A,
      scope: ScopePriority = ScopePriority.Imported
  )

  final private case class Scored[+A](
      candidate: Candidate[A],
      distance: Int,
      longestPrefix: Int,
      relationRank: Int,
      lengthDelta: Int
  ) {
    def sortKey: (Int, Int, Int, Int, Int, String) =
      (
        candidate.scope.rank,
        relationRank,
        distance,
        -longestPrefix,
        lengthDelta,
        candidate.ident.sourceCodeRepr
      )
  }

  def nearest[A](
      ident: Identifier,
      existing: Iterable[Candidate[A]],
      count: Int
  ): List[Candidate[A]] =
    if (count <= 0) Nil
    else {
      val query = ident.sourceCodeRepr
      if (query.isEmpty) Nil
      else {
        val scored = existing.iterator
          .filterNot(_.ident == ident)
          .map(score(query, _))
          .toList

        val bestByName = scored
          .groupBy(_.candidate.ident)
          .valuesIterator
          .map(_.minBy(_.sortKey))
          .toList

        bestByName
          .filter(likelyTypo(query, _))
          .sortBy(_.sortKey)
          .take(count)
          .map(_.candidate)
      }
    }

  def best[A](
      ident: Identifier,
      existing: Iterable[Candidate[A]]
  ): Option[Candidate[A]] =
    nearest(ident, existing, 1).headOption

  private def score[A](query: String, candidate: Candidate[A]): Scored[A] = {
    val cand = candidate.ident.sourceCodeRepr
    val relationRank =
      if (cand.startsWith(query) || query.startsWith(cand)) 0
      else if (cand.contains(query) || query.contains(cand)) 1
      else 2

    Scored(
      candidate,
      EditDistance.string(query, cand),
      commonPrefix(query, cand),
      relationRank,
      (query.length - cand.length).abs
    )
  }

  private def likelyTypo(query: String, scored: Scored[?]): Boolean = {
    val cand = scored.candidate.ident.sourceCodeRepr
    if (cand.isEmpty) false
    else {
      val maxLen = query.length.max(cand.length)
      val minLen = query.length.min(cand.length)
      val normalizedDist = scored.distance.toDouble / maxLen.toDouble

      val prefixMatch = scored.relationRank == 0
      val substringMatch =
        scored.relationRank == 1 && (minLen >= 3 || scored.lengthDelta <= 2)

      prefixMatch ||
      substringMatch ||
      (scored.longestPrefix >= 3 && scored.lengthDelta <= 4) ||
      (minLen <= 3 && scored.distance <= 1) ||
      (minLen <= 6 && scored.distance <= 2 && normalizedDist <= 0.6) ||
      (scored.distance <= 3 && normalizedDist <= 0.34 && scored.lengthDelta <= 4)
    }
  }

  private def commonPrefix(a: String, b: String): Int = {
    val max = a.length.min(b.length)
    var idx = 0
    while ((idx < max) && (a.charAt(idx) == b.charAt(idx))) {
      idx += 1
    }
    idx
  }
}
