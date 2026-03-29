package dev.bosatsu.pattern

import cats.data.{NonEmptyChain, NonEmptyList}
import dev.bosatsu.{Identifier, Pattern}
import scala.collection.mutable

import Identifier.Bindable

case class NameMap private (
    alternatives: NonEmptyChain[NameMap.Deterministic]
) {
  def maybeOr(that: NameMap): NameMap =
    NameMap.maybeOr(this, that)

  def deterministic: Option[NameMap.Deterministic] =
    {
      val it = alternatives.iterator
      val head = it.next()
      if (it.hasNext) None else Some(head)
    }

  // Convenience for deterministic alignments; disjunctive maps return empty.
  def superNames: Map[Bindable, NameMap.Rename] =
    deterministic.fold(Map.empty[Bindable, NameMap.Rename])(_.superNames)

  // Convenience for deterministic alignments; disjunctive maps return empty.
  def addedInSub: Set[Bindable] =
    deterministic.fold(Set.empty[Bindable])(_.addedInSub)

  // Deterministic lookup for legacy call sites.
  def superToSub(name: Bindable): Option[Bindable] =
    deterministic match {
      case Some(det) => det.superToSub(name)
      case None      => None
    }

  // Return one substitution per viable disjunctive alignment.
  // If a required source name cannot be mapped in an alternative, that
  // alternative is dropped (conservative but sound for implication facts).
  def substitutionAlternatives(
      fromNames: Set[Bindable]
  ): List[Map[Bindable, Bindable]] =
    alternatives.iterator.flatMap { alt =>
      val resolved = fromNames.iterator.map { from =>
        alt.superToSub(from).map(from -> _)
      }.toList

      if (resolved.forall(_.nonEmpty)) Some(resolved.flatten.toMap)
      else None
    }.toList.distinct
}

object NameMap {
  enum Rename derives CanEqual {
    case Same
    case Removed
    case To(name: Bindable)
  }

  final case class Deterministic(
      superNames: Map[Bindable, Rename],
      addedInSub: Set[Bindable]
  ) derives CanEqual {
    def superToSub(name: Bindable): Option[Bindable] =
      superNames.get(name) match {
        case Some(Rename.Same)      => Some(name)
        case Some(Rename.To(name1)) => Some(name1)
        case Some(Rename.Removed) | None => None
      }
  }

  private case class Alignment(
      alternatives: NonEmptyChain[Map[Bindable, Bindable]]
  )

  def apply(
      superNames: Map[Bindable, Rename],
      addedInSub: Set[Bindable]
  ): NameMap =
    NameMap(
      NonEmptyChain.one(
        Deterministic(superNames, addedInSub)
      )
    )

  private def oneAlignment(
      aligned: Map[Bindable, Bindable]
  ): Alignment =
    Alignment(NonEmptyChain.one(aligned))

  private def alignmentFromList(
      aligned: IterableOnce[Map[Bindable, Bindable]]
  ): Option[Alignment] =
    distinctChain(aligned.iterator).map(Alignment(_))

  private def nameMapFromList(alts: IterableOnce[Deterministic]): Option[NameMap] =
    distinctChain(alts.iterator).map(NameMap(_))

  private def distinctChain[A](
      values: Iterator[A]
  ): Option[NonEmptyChain[A]] = {
    val seen = mutable.LinkedHashSet.empty[A]
    while (values.hasNext) {
      seen += values.next()
    }

    val it = seen.iterator
    if (it.hasNext) {
      val head = it.next()
      Some(NonEmptyChain.fromNonEmptyList(NonEmptyList(head, it.toList)))
    } else None
  }

  def maybeOr(left: NameMap, right: NameMap): NameMap =
    nameMapFromList(
      left.alternatives.iterator ++ right.alternatives.iterator
    ).get

  private def maybeOr(
      left: Alignment,
      right: Alignment
  ): Alignment =
    alignmentFromList(
      left.alternatives.iterator ++ right.alternatives.iterator
    ).get

  private def maybeAnd(
      left: Alignment,
      right: Alignment
  ): Option[Alignment] = {
    val merged = for {
      l <- left.alternatives.iterator.toList
      r <- right.alternatives.iterator.toList
      m <- mergeAlignedNames(l, r).toList
    } yield m

    alignmentFromList(merged)
  }

  /** Compute how binders change from `superPattern` to `subPattern`.
    *
    * This method is intended to be used only when:
    * `TotalityCheck.difference(subPattern, superPattern).isEmpty`
    * (i.e. `superPattern` subsumes `subPattern`). Without that precondition, a
    * returned map is not meaningful for implication-style reasoning.
    *
    * Even with the subsumption precondition, this can still return `None`
    * because the alignment is intentionally conservative/syntactic:
    * 1. list/string patterns may be semantically compatible but not have the
    *    same part-wise decomposition used by this aligner;
    * 2. no viable disjunctive alignment remains after merging nested parts.
    */
  def alignSubsumedPatternNames[N, T](
      superPattern: Pattern[N, T],
      subPattern: Pattern[N, T]
  ): Option[NameMap] =
    alignSubsumedPatternNameTargets(superPattern, subPattern).flatMap { aligned =>
      nameMapFromList(
        aligned.alternatives.iterator.toList.map(alignedNames =>
          build(superPattern, subPattern, alignedNames)
        )
      )
    }

  private def build[N, T](
      superPattern: Pattern[N, T],
      subPattern: Pattern[N, T],
      alignedNames: Map[Bindable, Bindable]
  ): Deterministic = {
    val superNames = superPattern.names.toSet
    val subNames = subPattern.names.toSet

    val superNameMap: Map[Bindable, Rename] =
      superNames.iterator.map { superName =>
        val rename =
          alignedNames.get(superName) match {
            case Some(subName) if subName == superName =>
              Rename.Same
            case Some(subName) =>
              Rename.To(subName)
            case None =>
              Rename.Removed
          }
        superName -> rename
      }.toMap

    val mappedTargets = superNameMap.iterator.collect {
      case (superName, Rename.Same) => superName
      case (_, Rename.To(subName))  => subName
    }.toSet

    Deterministic(superNameMap, subNames -- mappedTargets)
  }

  @annotation.tailrec
  private def unwrapNamedPattern[N, T](
      pattern: Pattern[N, T]
  ): Pattern[N, T] =
    pattern match {
      case Pattern.Annotation(inner, _) => unwrapNamedPattern(inner)
      case Pattern.Named(_, inner)      => unwrapNamedPattern(inner)
      case p                            => p
    }

  private def mergeAlignedNames(
      left: Map[Bindable, Bindable],
      right: Map[Bindable, Bindable]
  ): Option[Map[Bindable, Bindable]] =
    right.foldLeft(Option(left)) { case (accOpt, (from, to)) =>
      accOpt.flatMap { acc =>
        acc.get(from) match {
          case Some(existing) if existing != to => None
          case Some(_)                          => Some(acc)
          case None                             => Some(acc.updated(from, to))
        }
      }
    }

  private def alignSubsumedListPartNames[N, T](
      superPart: Pattern.ListPart[Pattern[N, T]],
      subPart: Pattern.ListPart[Pattern[N, T]]
  ): Option[Alignment] =
    (superPart, subPart) match {
      case (Pattern.ListPart.Item(superItem), Pattern.ListPart.Item(subItem)) =>
        alignSubsumedPatternNameTargets(superItem, subItem)
      case (Pattern.ListPart.NamedList(superName), Pattern.ListPart.NamedList(subName)) =>
        Some(oneAlignment(Map(superName -> subName)))
      case (Pattern.ListPart.NamedList(_), Pattern.ListPart.WildList) =>
        Some(oneAlignment(Map.empty))
      case (
            Pattern.ListPart.WildList,
            Pattern.ListPart.WildList | Pattern.ListPart.NamedList(_)
          ) =>
        Some(oneAlignment(Map.empty))
      case _ =>
        None
    }

  private def alignSubsumedStrPartNames(
      superPart: Pattern.StrPart,
      subPart: Pattern.StrPart
  ): Option[Alignment] =
    (superPart, subPart) match {
      case (Pattern.StrPart.NamedStr(superName), Pattern.StrPart.NamedStr(subName)) =>
        Some(oneAlignment(Map(superName -> subName)))
      case (Pattern.StrPart.NamedChar(superName), Pattern.StrPart.NamedChar(subName)) =>
        Some(oneAlignment(Map(superName -> subName)))
      case (Pattern.StrPart.NamedStr(_), Pattern.StrPart.WildStr) =>
        Some(oneAlignment(Map.empty))
      case (Pattern.StrPart.NamedChar(_), Pattern.StrPart.WildChar) =>
        Some(oneAlignment(Map.empty))
      case (
            Pattern.StrPart.WildStr,
            Pattern.StrPart.WildStr | Pattern.StrPart.NamedStr(_)
          ) =>
        Some(oneAlignment(Map.empty))
      case (
            Pattern.StrPart.WildChar,
            Pattern.StrPart.WildChar | Pattern.StrPart.NamedChar(_)
          ) =>
        Some(oneAlignment(Map.empty))
      case (Pattern.StrPart.LitStr(left), Pattern.StrPart.LitStr(right))
          if left == right =>
        Some(oneAlignment(Map.empty))
      case _ =>
        None
    }

  private def alignSubsumedPatternNameTargets[N, T](
      superPattern: Pattern[N, T],
      subPattern: Pattern[N, T]
  ): Option[Alignment] = {
    val topAligned: Alignment = {
      val subTopNames = subPattern.topNames.distinct
      val topAlternatives =
        subTopNames match {
          case Nil =>
            List(Map.empty[Bindable, Bindable])
          case _ =>
            val superTopNames = superPattern.topNames.distinct
            subTopNames.map(subTop =>
              superTopNames.iterator.map(_ -> subTop).toMap
            )
        }

      alignmentFromList(topAlternatives).get
    }

    val coreAligned =
      (unwrapNamedPattern(superPattern), unwrapNamedPattern(subPattern)) match {
        case (Pattern.Union(superHead, superTail), subPat) =>
          (superHead :: superTail.toList)
            .flatMap(alignSubsumedPatternNameTargets(_, subPat))
            .reduceOption(maybeOr(_, _))
        case (Pattern.WildCard | Pattern.Literal(_), _) =>
          Some(oneAlignment(Map.empty[Bindable, Bindable]))
        case (Pattern.Var(superName), _) =>
          // A variable pattern binds the whole matched value, so align against
          // the sub-pattern's whole-value aliases before unwrapping.
          val subTopNames = subPattern.topNames.distinct
          val topAlternatives =
            subTopNames match {
              case Nil =>
                List(Map.empty[Bindable, Bindable])
              case _ =>
                subTopNames.map(subName => Map(superName -> subName))
            }

          alignmentFromList(topAlternatives)
        case (
              Pattern.PositionalStruct(superName, superParams),
              Pattern.PositionalStruct(subName, subParams)
            )
            if superName.equals(subName) && (superParams.length == subParams.length) =>
          superParams
            .zip(subParams)
            .foldLeft(Option(oneAlignment(Map.empty[Bindable, Bindable]))) {
              case (accOpt, (superItem, subItem)) =>
                for {
                  acc <- accOpt
                  next <- alignSubsumedPatternNameTargets(superItem, subItem)
                  merged <- maybeAnd(acc, next)
                } yield merged
            }
        case (Pattern.ListPat(superParts), Pattern.ListPat(subParts))
            if superParts.length == subParts.length =>
          superParts
            .zip(subParts)
            .foldLeft(Option(oneAlignment(Map.empty[Bindable, Bindable]))) {
              case (accOpt, (superItem, subItem)) =>
                for {
                  acc <- accOpt
                  next <- alignSubsumedListPartNames(superItem, subItem)
                  merged <- maybeAnd(acc, next)
                } yield merged
            }
        case (Pattern.StrPat(superParts), Pattern.StrPat(subParts))
            if superParts.length == subParts.length =>
          superParts.toList
            .zip(subParts.toList)
            .foldLeft(Option(oneAlignment(Map.empty[Bindable, Bindable]))) {
              case (accOpt, (superItem, subItem)) =>
                for {
                  acc <- accOpt
                  next <- alignSubsumedStrPartNames(superItem, subItem)
                  merged <- maybeAnd(acc, next)
                } yield merged
            }
        case _ =>
          None
      }

    coreAligned.flatMap(maybeAnd(topAligned, _))
  }
}
