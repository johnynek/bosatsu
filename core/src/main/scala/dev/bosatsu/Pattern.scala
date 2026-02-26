package dev.bosatsu

import cats.{Applicative, Foldable, Order}
import cats.data.NonEmptyList
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{Doc, Document}
import dev.bosatsu.pattern.{NamedSeqPattern, SeqPattern, SeqPart}
import dev.bosatsu.rankn.Type
import java.util.regex.{Pattern => RegexPattern}

import Parser.{Combinators, maybeSpace, MaybeTupleOrParens}
import cats.implicits._

import Identifier.{Bindable, Constructor}

sealed abstract class Pattern[+N, +T] derives CanEqual {
  def mapName[U](fn: N => U): Pattern[U, T] =
    (new Pattern.InvariantPattern(this)).mapStruct[U] { (n, parts) =>
      Pattern.PositionalStruct(fn(n), parts)
    }

  def mapType[U](fn: T => U): Pattern[N, U] =
    (new Pattern.InvariantPattern(this)).traverseType[cats.Id, U](fn)

  /** List all the names that are bound in Vars inside this pattern in the left
    * to right order they are encountered, without any duplication
    */
  lazy val names: List[Bindable] = {
    @annotation.tailrec
    def loop(
        stack: List[Pattern[N, T]],
        seen: Set[Bindable],
        acc: List[Bindable]
    ): List[Bindable] =
      stack match {
        case Nil                                             => acc.reverse
        case (Pattern.WildCard | Pattern.Literal(_)) :: tail =>
          loop(tail, seen, acc)
        case Pattern.Var(v) :: tail =>
          if (seen(v)) loop(tail, seen, acc)
          else loop(tail, seen + v, v :: acc)
        case Pattern.Named(v, p) :: tail =>
          if (seen(v)) loop(p :: tail, seen, acc)
          else loop(p :: tail, seen + v, v :: acc)
        case Pattern.StrPat(items) :: tail =>
          val names = items
            .collect {
              case Pattern.StrPart.NamedStr(n)  => n
              case Pattern.StrPart.NamedChar(n) => n
            }
            .filterNot(seen)
          loop(tail, seen ++ names, names reverse_::: acc)
        case Pattern.ListPat(items) :: tail =>
          val globs = items
            .collect { case Pattern.ListPart.NamedList(glob) => glob }
            .filterNot(seen)
          val next = items.collect { case Pattern.ListPart.Item(inner) =>
            inner
          }
          loop(next ::: tail, seen ++ globs, globs reverse_::: acc)
        case Pattern.Annotation(p, _) :: tail => loop(p :: tail, seen, acc)
        case Pattern.PositionalStruct(_, params) :: tail =>
          loop(params ::: tail, seen, acc)
        case Pattern.Union(h, t) :: tail =>
          loop(h :: (t.toList) ::: tail, seen, acc)
      }

    loop(this :: Nil, Set.empty, Nil)
  }

  /** What are the names that will be bound to the entire pattern, Bar(x) as foo
    * would return List(foo) foo as bar as baz would return List(baz, bar, foo)
    * Bar(x) would return Nil
    */
  lazy val topNames: List[Bindable] =
    this match {
      case Pattern.Var(v)      => v :: Nil
      case Pattern.Named(v, p) => (v :: p.topNames).distinct
      case Pattern.ListPat(Pattern.ListPart.NamedList(n) :: Nil) => n :: Nil
      case Pattern.Annotation(p, _)                              => p.topNames
      case Pattern.Union(h, t)                                   =>
        // the intersection of all top level names
        // is okay
        val pats = h :: t.toList
        val patIntr = pats.map(_.topNames.toSet).reduce(_ & _)
        // put them in the same order as written:
        pats.flatMap(_.topNames).iterator.filter(patIntr).toList.distinct
      case Pattern.ListPat(_) | Pattern.WildCard | Pattern.Literal(_) |
          Pattern.StrPat(_) | Pattern.PositionalStruct(_, _) =>
        Nil
    }

  def substitute(table: Map[Bindable, Bindable]): Pattern[N, T] =
    this match {
      case Pattern.WildCard | Pattern.Literal(_) => this
      case Pattern.Var(b)                        =>
        table.get(b) match {
          case None     => this
          case Some(b1) => Pattern.Var(b1)
        }
      case Pattern.Named(n, p) =>
        val p1 = p.substitute(table)
        val n2 = table.get(n) match {
          case None     => n
          case Some(n1) => n1
        }
        if ((p1 eq p) && (n2 eq n)) this
        else Pattern.Named(n2, p1)
      case Pattern.Annotation(p, t) =>
        val p1 = p.substitute(table)
        if (p1 eq p) this
        else Pattern.Annotation(p1, t)
      case Pattern.Union(h, t) =>
        Pattern.Union(h.substitute(table), t.map(_.substitute(table)))
      case Pattern.PositionalStruct(n, pats) =>
        Pattern.PositionalStruct(n, pats.map(_.substitute(table)))
      case Pattern.ListPat(parts) =>
        Pattern.ListPat(parts.map(_.substitute(table)))
      case Pattern.StrPat(parts) =>
        Pattern.StrPat(parts.map(_.substitute(table)))
    }

  /** List all the names that strictly smaller than anything that would match
    * this pattern e.g. a top level var, would not be returned
    */
  def substructures: List[Bindable] = {
    def cheat(
        stack: List[(Pattern[N, T], Boolean)],
        seen: Set[Bindable],
        acc: List[Bindable]
    ): List[Bindable] =
      loop(stack, seen, acc)

    import Pattern.{ListPart, StrPart}

    @annotation.tailrec
    def loop(
        stack: List[(Pattern[N, T], Boolean)],
        seen: Set[Bindable],
        acc: List[Bindable]
    ): List[Bindable] =
      stack match {
        case Nil => acc.reverse
        case ((Pattern.WildCard, _) | (Pattern.Literal(_), _)) :: tail =>
          loop(tail, seen, acc)
        case (Pattern.Var(v), isTop) :: tail =>
          if (seen(v) || isTop) loop(tail, seen, acc)
          else loop(tail, seen + v, v :: acc)
        case (Pattern.Named(v, p), isTop) :: tail =>
          if (seen(v) || isTop) loop((p, isTop) :: tail, seen, acc)
          else loop((p, isTop) :: tail, seen + v, v :: acc)
        case (
              Pattern.StrPat(NonEmptyList(StrPart.NamedStr(_), Nil)),
              true
            ) :: tail =>
          // this is a total match at the top level, not a substructure
          loop(tail, seen, acc)
        case (Pattern.StrPat(items), _) :: tail =>
          val globs = items
            .collect { case StrPart.NamedStr(glob) => glob }
            .filterNot(seen)
          loop(tail, seen ++ globs, globs reverse_::: acc)
        case (Pattern.ListPat(ListPart.NamedList(_) :: Nil), true) :: tail =>
          // this is a total match at the top level, not a substructure
          loop(tail, seen, acc)
        case (Pattern.ListPat(items), _) :: tail =>
          val globs = items
            .collect { case ListPart.NamedList(glob) => glob }
            .filterNot(seen)
          val next = items.collect { case ListPart.Item(inner) =>
            (inner, false)
          }
          loop(next ::: tail, seen ++ globs, globs reverse_::: acc)
        case (Pattern.Annotation(p, _), isTop) :: tail =>
          loop((p, isTop) :: tail, seen, acc)
        case (Pattern.PositionalStruct(_, params), _) :: tail =>
          loop(params.map((_, false)) ::: tail, seen, acc)
        case (Pattern.Union(h, t), isTop) :: tail =>
          val all = (h :: t.toList).map { p =>
            cheat((p, isTop) :: tail, seen, acc)
          }
          // we need to be substructual on all:
          val intr = all.map(_.toSet).reduce(_.intersect(_))
          all.flatMap(_.filter(intr)).distinct
      }

    loop((this, true) :: Nil, Set.empty, Nil)
  }

  /** Return the pattern with all the binding names removed
    */
  def unbind: Pattern[N, T] =
    filterVars(Set.empty)

  /** replace all Var names with Wildcard that are not satifying the keep
    * predicate
    */
  def filterVars(keep: Bindable => Boolean): Pattern[N, T] =
    this match {
      case Pattern.WildCard | Pattern.Literal(_) => this
      case p @ Pattern.Var(v)                    =>
        if (keep(v)) p else Pattern.WildCard
      case Pattern.Named(v, p) =>
        val inner = p.filterVars(keep)
        if (keep(v)) Pattern.Named(v, inner)
        else inner
      case Pattern.StrPat(items) =>
        Pattern.StrPat(items.map {
          case wl @ (Pattern.StrPart.WildStr | Pattern.StrPart.WildChar |
              Pattern.StrPart.LitStr(_)) =>
            wl
          case in @ Pattern.StrPart.NamedStr(n) =>
            if (keep(n)) in
            else Pattern.StrPart.WildStr
          case in @ Pattern.StrPart.NamedChar(n) =>
            if (keep(n)) in
            else Pattern.StrPart.WildChar
        })
      case Pattern.ListPat(items) =>
        Pattern.ListPat(items.map {
          case Pattern.ListPart.WildList          => Pattern.ListPart.WildList
          case in @ Pattern.ListPart.NamedList(n) =>
            if (keep(n)) in
            else Pattern.ListPart.WildList
          case Pattern.ListPart.Item(p) =>
            Pattern.ListPart.Item(p.filterVars(keep))
        })
      case Pattern.Annotation(p, tpe) =>
        Pattern.Annotation(p.filterVars(keep), tpe)
      case Pattern.PositionalStruct(name, params) =>
        Pattern.PositionalStruct(name, params.map(_.filterVars(keep)))
      case Pattern.Union(h, t) =>
        Pattern.Union(h.filterVars(keep), t.map(_.filterVars(keep)))
    }

  /** a collision happens when the same binding happens twice not separated by a
    * union
    */
  def collisionBinds: List[Bindable] = {

    def loop(pat: Pattern[N, T]): (Set[Bindable], List[Bindable]) =
      pat match {
        case Pattern.WildCard | Pattern.Literal(_) => (Set.empty, Nil)
        case Pattern.Var(v)                        => (Set(v), Nil)
        case Pattern.Named(v, p)                   =>
          val (s1, l1) = loop(p)
          if (s1(v)) (s1, v :: l1)
          else (s1 + v, l1)
        case Pattern.StrPat(items) =>
          items.foldLeft((Set.empty[Bindable], List.empty[Bindable])) {
            case (
                  res,
                  Pattern.StrPart.WildStr | Pattern.StrPart.WildChar |
                  Pattern.StrPart.LitStr(_)
                ) =>
              res
            case ((s1, l1), Pattern.StrPart.NamedStr(v)) =>
              if (s1(v)) (s1, v :: l1)
              else (s1 + v, l1)
            case ((s1, l1), Pattern.StrPart.NamedChar(v)) =>
              if (s1(v)) (s1, v :: l1)
              else (s1 + v, l1)
          }
        case Pattern.ListPat(items) =>
          items.foldLeft((Set.empty[Bindable], List.empty[Bindable])) {
            case (res, Pattern.ListPart.WildList)          => res
            case ((s1, l1), Pattern.ListPart.NamedList(v)) =>
              if (s1(v)) (s1, v :: l1)
              else (s1 + v, l1)
            case ((s1, l1), Pattern.ListPart.Item(p)) =>
              val (s2, l2) = loop(p)
              // here are new duplicates
              val dups = ((s1 & s2) -- l2) -- l1
              (s1 | s2, dups.toList ::: l2 ::: l1)
          }
        case Pattern.Annotation(p, _)            => loop(p)
        case Pattern.PositionalStruct(_, params) =>
          params.foldLeft((Set.empty[Bindable], List.empty[Bindable])) {
            case ((s1, l1), p) =>
              val (s2, l2) = loop(p)
              val dups = ((s1 & s2) -- l2) -- l1
              (s1 | s2, dups.toList ::: l2 ::: l1)
          }
        case Pattern.Union(h, t) =>
          (h :: t.toList).foldMap(loop)
      }

    loop(this)._2.distinct.sorted
  }

  /** @return
    *   the type if we can directly see it
    */
  def simpleTypeOf: Option[T] =
    this match {
      case Pattern.Named(_, p)      => p.simpleTypeOf
      case Pattern.Annotation(_, t) => Some(t)
      case Pattern.Union(_, _) | Pattern.ListPat(_) | Pattern.Literal(_) |
          Pattern.WildCard | Pattern.Var(_) | Pattern.StrPat(_) |
          Pattern.PositionalStruct(_, _) =>
        None
    }
}

object Pattern {
  implicit def patternOrder[N: Order, T: Order]: Order[Pattern[N, T]] = {
    val ordN: Ordering[N] = Order[N].toOrdering
    val ordT: Ordering[T] = Order[T].toOrdering
    Order.fromOrdering(using patternOrdering(using ordN, ordT))
  }

  /** Represents the different patterns that are all for structs (2, 3) Foo(2,
    * 3) etc...
    */
  sealed abstract class StructKind derives CanEqual {
    def namedStyle: Option[StructKind.Style] =
      this match {
        case StructKind.Tuple                  => None
        case StructKind.Named(_, style)        => Some(style)
        case StructKind.NamedPartial(_, style) => Some(style)
      }
  }
  object StructKind {
    sealed abstract class Style derives CanEqual
    object Style {
      sealed abstract class FieldKind derives CanEqual {
        def field: Bindable
      }
      object FieldKind {
        final case class Explicit(field: Bindable) extends FieldKind
        // an implicit field can only be associated with a Var of
        // the same name
        final case class Implicit(field: Bindable) extends FieldKind
      }
      case object TupleLike extends Style
      // represents the fields like: Foo { bar: x, age }
      final case class RecordLike(fields: NonEmptyList[FieldKind]) extends Style
    }
    sealed abstract class NamedKind extends StructKind {
      def name: Constructor
      def style: Style
    }
    case object Tuple extends StructKind
    // Represents a complete tuple-like pattern Foo(a, b)
    final case class Named(name: Constructor, style: Style) extends NamedKind
    // Represents a partial tuple-like pattern Foo(a, ...)
    final case class NamedPartial(name: Constructor, style: Style)
        extends NamedKind
  }

  sealed abstract class StrPart derives CanEqual {
    import StrPart._

    def substitute(table: Map[Bindable, Bindable]): StrPart =
      this match {
        case WildStr | LitStr(_) | WildChar => this
        case NamedStr(n)                    =>
          table.get(n) match {
            case None     => this
            case Some(n1) => NamedStr(n1)
          }
        case NamedChar(n) =>
          table.get(n) match {
            case None     => this
            case Some(n1) => NamedChar(n1)
          }
      }
  }
  object StrPart {
    case object WildStr extends StrPart
    final case class NamedStr(name: Bindable) extends StrPart
    case object WildChar extends StrPart
    final case class NamedChar(name: Bindable) extends StrPart
    final case class LitStr(asString: String) extends StrPart

    // this is to circumvent scala warnings because these bosatsu
    // patterns like right.
    private val dollar = "$"
    private val wildDoc = Doc.text(s"$dollar{_}")
    private val wildCharDoc = Doc.text(s"${dollar}.{_}")
    private val prefix = Doc.text(s"$dollar{")
    private val prefixChar = Doc.text(s"${dollar}.{")

    def document(q: Char): Document[StrPart] =
      Document.instance {
        case WildStr     => wildDoc
        case WildChar    => wildCharDoc
        case NamedStr(b) =>
          prefix + Document[Bindable].document(b) + Doc.char('}')
        case NamedChar(b) =>
          prefixChar + Document[Bindable].document(b) + Doc.char('}')
        case LitStr(s) => Doc.text(StringUtil.escape(q, s))
      }
  }

  /** represents items in a list pattern
    */
  sealed abstract class ListPart[+A] derives CanEqual {
    def map[B](fn: A => B): ListPart[B]
  }
  object ListPart {
    sealed abstract class Glob extends ListPart[Nothing] {
      def map[B](fn: Nothing => B): ListPart[B] = this
    }
    case object WildList extends Glob
    final case class NamedList(name: Bindable) extends Glob
    final case class Item[A](pat: A) extends ListPart[A] {
      def map[B](fn: A => B): ListPart[B] = Item(fn(pat))
    }

    implicit class ListPartPat[N, T](val self: ListPart[Pattern[N, T]])
        extends AnyVal {
      def substitute(table: Map[Bindable, Bindable]): ListPart[Pattern[N, T]] =
        self match {
          case WildList     => WildList
          case NamedList(n) =>
            table.get(n) match {
              case None     => self
              case Some(n1) => NamedList(n1)
            }
          case Item(p) =>
            val p1 = p.substitute(table)
            if (p1 eq p) self
            else Item(p1)
        }
    }
  }

  /** This will match any list without any binding
    */
  val AnyList: Pattern[Nothing, Nothing] =
    Pattern.ListPat(ListPart.WildList :: Nil)

  type Parsed = Pattern[StructKind, TypeRef]

  def patternTypeVars(
      pat: Parsed
  ): List[Type.Var.Bound] = {
    def loop(p: Parsed): List[Type.Var.Bound] =
      p match {
        case Pattern.WildCard | Pattern.Literal(_) | Pattern.Var(_) |
            Pattern.StrPat(_) =>
          Nil
        case Pattern.Named(_, inner) =>
          loop(inner)
        case Pattern.Annotation(inner, tpe) =>
          TypeRef.freeTypeRefVars(tpe).map(_.toBoundVar) ::: loop(inner)
        case Pattern.PositionalStruct(_, params) =>
          params.flatMap(loop)
        case Pattern.ListPat(parts) =>
          parts.toList.flatMap {
            case Pattern.ListPart.Item(inner) => loop(inner)
            case _                            => Nil
          }
        case Pattern.Union(head, tail) =>
          loop(head) ::: tail.toList.flatMap(loop)
      }

    loop(pat)
  }

  /** Flatten a pattern out such that there are no top-level unions
    */
  def flatten[N, T](p: Pattern[N, T]): NonEmptyList[Pattern[N, T]] =
    p match {
      case Union(h, t) => NonEmptyList(h, t.toList).flatMap(flatten(_))
      case nonU        => NonEmptyList.one(nonU)
    }

  /** Create a normalized pattern, which doesn't have nested top level unions
    */
  def union[N, T](
      head: Pattern[N, T],
      tail: List[Pattern[N, T]]
  ): Pattern[N, T] =
    NonEmptyList(head, tail).flatMap(flatten(_)) match {
      case NonEmptyList(h, Nil)         => h
      case NonEmptyList(h0, h1 :: tail) => Union(h0, NonEmptyList(h1, tail))
    }

  implicit class InvariantPattern[N, T](val pat: Pattern[N, T]) extends AnyVal {
    def traverseType[F[_]: Applicative, T1](fn: T => F[T1]): F[Pattern[N, T1]] =
      traversePattern[F, N, T1](
        (n, args) => args.map(PositionalStruct(n, _)),
        fn,
        parts => parts.map(ListPat(_))
      )

    def traverseStruct[F[_]: Applicative, N1](
        parts: (N, F[List[Pattern[N1, T]]]) => F[Pattern[N1, T]]
    ): F[Pattern[N1, T]] =
      traversePattern[F, N1, T](
        parts,
        Applicative[F].pure(_),
        parts => parts.map(ListPat(_))
      )

    def mapStruct[N1](
        parts: (N, List[Pattern[N1, T]]) => Pattern[N1, T]
    ): Pattern[N1, T] =
      traverseStruct[cats.Id, N1](parts)

    def traversePattern[F[_]: Applicative, N1, T1](
        parts: (N, F[List[Pattern[N1, T1]]]) => F[Pattern[N1, T1]],
        tpeFn: T => F[T1],
        listFn: F[List[ListPart[Pattern[N1, T1]]]] => F[Pattern[N1, T1]]
    ): F[Pattern[N1, T1]] = {
      lazy val pwild: F[Pattern[N1, T1]] = Applicative[F].pure(Pattern.WildCard)

      def go(pat: Pattern[N, T]): F[Pattern[N1, T1]] =
        pat match {
          case Pattern.WildCard     => pwild
          case Pattern.Literal(lit) => Applicative[F].pure(Pattern.Literal(lit))
          case Pattern.Var(v)       => Applicative[F].pure(Pattern.Var(v))
          case Pattern.StrPat(s)    => Applicative[F].pure(Pattern.StrPat(s))
          case Pattern.Named(v, p)  =>
            go(p).map(Pattern.Named(v, _))
          case Pattern.ListPat(items) =>
            type L = ListPart[Pattern[N1, T1]]
            val items1 = items.traverse {
              case ListPart.WildList =>
                Applicative[F].pure(ListPart.WildList: L)
              case ListPart.NamedList(n) =>
                Applicative[F].pure(ListPart.NamedList(n): L)
              case ListPart.Item(p) =>
                go(p).map(ListPart.Item(_): L)
            }
            listFn(items1)
          case Pattern.Annotation(p, tpe) =>
            (go(p), tpeFn(tpe)).mapN(Pattern.Annotation(_, _))
          case Pattern.PositionalStruct(name, params) =>
            val p1 = params.traverse(go(_))
            parts(name, p1)
          case Pattern.Union(h, tail) =>
            (go(h), tail.traverse(go)).mapN(Pattern.Union(_, _))
        }

      go(pat)
    }
  }

  implicit class FoldablePattern[F[_], N, T](private val pats: F[Pattern[N, T]])
      extends AnyVal {
    def patternNames(implicit F: Foldable[F]): List[Bindable] =
      F.toList(pats).flatMap(_.names)
  }

  case object WildCard extends Pattern[Nothing, Nothing]
  case class Literal(toLit: Lit) extends Pattern[Nothing, Nothing]
  case class Var(name: Bindable) extends Pattern[Nothing, Nothing]
  case class StrPat(parts: NonEmptyList[StrPart])
      extends Pattern[Nothing, Nothing] {
    def isEmpty: Boolean = this === StrPat.Empty

    lazy val isTotal: Boolean = {
      import StrPart.{LitStr, WildChar, NamedChar}

      !parts.exists {
        case LitStr(_) | WildChar | NamedChar(_) => true
        case _                                   => false
      }
    }

    /** Convert this to simpler pattern, if possible (such as Literal, Wild,
      * Var)
      */
    def simplify: Option[Pattern[Nothing, Nothing]] =
      parts match {
        case NonEmptyList(StrPart.WildStr, Nil)     => Some(Pattern.WildCard)
        case NonEmptyList(StrPart.NamedStr(n), Nil) => Some(Pattern.Var(n))
        case _                                      =>
          val allStrings = parts.traverse {
            case StrPart.LitStr(s) => Some(s)
            case _                 => None
          }

          allStrings.map { strs =>
            Pattern.Literal(Lit.Str(strs.combineAll))
          }
      }

    lazy val toNamedSeqPattern: NamedSeqPattern[Int] =
      StrPat.toNamedSeqPattern(this)

    lazy val toSeqPattern: SeqPattern[Int] = toNamedSeqPattern.unname

    lazy val toLiteralString: Option[String] =
      toSeqPattern.toLiteralSeq.map(_.mkString)

    lazy val matcher = SeqPattern.stringUnitMatcher(toSeqPattern)

    def matches(str: String): Boolean =
      isTotal || matcher(str).isDefined

    /** Convert to a regular expression matching this pattern, which uses
      * reluctant modifiers
      */
    def toRegex: RegexPattern = {
      def mapPart(p: StrPart): String =
        p match {
          case StrPart.NamedStr(_)  => "(.*?)"
          case StrPart.WildStr      => ".*?"
          case StrPart.NamedChar(_) => "(.)"
          case StrPart.WildChar     => "."
          case StrPart.LitStr(s)    =>
            // we need to escape any characters that may be in regex
            RegexPattern.quote(s)
        }
      RegexPattern.compile(
        parts.iterator
          .map(mapPart(_))
          .mkString,
        RegexPattern.DOTALL
      )
    }
  }

  /** Patterns like Some(_) as foo as binds tighter than |, so use ( ) with
    * groups you want to bind
    */
  case class Named[+N, +T](name: Bindable, pat: Pattern[N, T])
      extends Pattern[N, T]
  case class ListPat[+N, +T](parts: List[ListPart[Pattern[N, T]]])
      extends Pattern[N, T] {
    lazy val toNamedSeqPattern: NamedSeqPattern[Pattern[N, T]] =
      ListPat.toNamedSeqPattern(this)

    lazy val toSeqPattern: SeqPattern[Pattern[N, T]] = toNamedSeqPattern.unname
  }
  case class Annotation[N, T](pattern: Pattern[N, T], tpe: T)
      extends Pattern[N, T]
  case class PositionalStruct[N, T](name: N, params: List[Pattern[N, T]])
      extends Pattern[N, T]
  case class Union[N, T](head: Pattern[N, T], rest: NonEmptyList[Pattern[N, T]])
      extends Pattern[N, T] {
    def split: (Pattern[N, T], Pattern[N, T]) = {
      // we have at least two patterns here
      val pats = head :: rest.flatMap(flatten(_))
      val (left, right) = pats.toList.splitAt(pats.size / 2)
      def fromL(ps: List[Pattern[N, T]]): Pattern[N, T] =
        ps match {
          case h :: t => Pattern.union(h, t)
          // $COVERAGE-OFF$ should be unreachable
          case Nil => sys.error("unreacheable since there are at least 2")
          // $COVERAGE-ON$ should be unreachable
        }

      (fromL(left), fromL(right))
    }
  }

  object ListPat {
    val Wild: ListPat[Nothing, Nothing] =
      ListPat(ListPart.WildList :: Nil)

    def fromSeqPattern[N, T](sp: SeqPattern[Pattern[N, T]]): ListPat[N, T] = {

      @annotation.tailrec
      def loop(
          ps: List[SeqPart[Pattern[N, T]]],
          front: List[ListPart[Pattern[N, T]]]
      ): List[ListPart[Pattern[N, T]]] =
        ps match {
          case Nil                    => front.reverse
          case SeqPart.Lit(p) :: tail =>
            loop(tail, ListPart.Item(p) :: front)
          case SeqPart.AnyElem :: tail =>
            loop(tail, ListPart.Item(WildCard) :: front)
          case SeqPart.Wildcard :: SeqPart.AnyElem :: tail =>
            // *_, _ is the same as _, *_
            loop(SeqPart.AnyElem :: SeqPart.Wildcard :: tail, front)
          case SeqPart.Wildcard :: tail =>
            loop(tail, ListPart.WildList :: front)
        }

      ListPat(loop(sp.toList, Nil))
    }

    def toNamedSeqPattern[N, T](
        lp: ListPat[N, T]
    ): NamedSeqPattern[Pattern[N, T]] = {
      def partToNsp(
          lp: ListPart[Pattern[N, T]]
      ): NamedSeqPattern[Pattern[N, T]] =
        lp match {
          case ListPart.Item(WildCard) => NamedSeqPattern.Any
          case ListPart.Item(p)        => NamedSeqPattern.fromLit(p)
          case ListPart.WildList       => NamedSeqPattern.Wild
          case ListPart.NamedList(n)   =>
            NamedSeqPattern.Bind(n.sourceCodeRepr, NamedSeqPattern.Wild)
        }

      def loop(
          lp: List[ListPart[Pattern[N, T]]]
      ): NamedSeqPattern[Pattern[N, T]] =
        lp match {
          case Nil      => NamedSeqPattern.NEmpty
          case h :: Nil => partToNsp(h)
          case h :: t   =>
            NamedSeqPattern.NCat(partToNsp(h), loop(t))
        }

      loop(lp.parts)
    }

    def toPositionalStruct[N, T](lp: ListPat[N, T], empty: N, cons: N): Either[
      (ListPart.Glob, NonEmptyList[ListPart[Pattern[N, T]]]),
      Pattern[N, T]
    ] = {
      def loop(
          parts: List[ListPart[Pattern[N, T]]]
      ): Either[(ListPart.Glob, NonEmptyList[ListPart[Pattern[N, T]]]), Pattern[
        N,
        T
      ]] =
        parts match {
          case Nil                      => Right(PositionalStruct(empty, Nil))
          case ListPart.WildList :: Nil => Right(WildCard)
          case ListPart.NamedList(glob) :: Nil => Right(Var(glob))
          case ListPart.Item(p) :: tail        =>
            // we can always make some progress here
            val tailPat = loop(tail).toOption.getOrElse(ListPat(tail))
            Right(PositionalStruct(cons, List(p, tailPat)))
          case (l @ ListPart.WildList) :: (r @ ListPart.Item(WildCard)) :: t =>
            // we can switch *_, _ with _, *_
            loop(r :: l :: t)
          case (glob: ListPart.Glob) :: h1 :: t =>
            // a prefixed list cannot be represented as a cons cell
            Left((glob, NonEmptyList(h1, t)))
        }

      loop(lp.parts)
    }
  }

  object StrPat {
    val Empty: StrPat = fromLitStr("")
    val Wild: StrPat = StrPat(NonEmptyList.one(StrPart.WildStr))

    given cats.Eq[StrPat] = cats.Eq.fromUniversalEquals

    def fromSeqPattern(sp: SeqPattern[Int]): StrPat = {
      def lit(rev: List[Int]): List[StrPart.LitStr] =
        if (rev.isEmpty) Nil
        else {
          val cps = rev.reverse
          val bldr = new java.lang.StringBuilder
          cps.foreach(bldr.appendCodePoint(_))
          StrPart.LitStr(bldr.toString) :: Nil
        }

      def loop(
          ps: List[SeqPart[Int]],
          front: List[Int]
      ): NonEmptyList[StrPart] =
        ps match {
          case Nil => NonEmptyList.fromList(lit(front)).getOrElse(Empty.parts)
          case SeqPart.Lit(c) :: tail =>
            loop(tail, c :: front)
          case SeqPart.AnyElem :: tail =>
            loop(tail, Nil)
              .prepend(StrPart.WildChar)
              .prependList(lit(front))
          case SeqPart.Wildcard :: SeqPart.AnyElem :: tail =>
            // *_, _ is the same as _, *_
            loop(SeqPart.AnyElem :: SeqPart.Wildcard :: tail, front)
          case SeqPart.Wildcard :: (tail @ (SeqPart.Wildcard :: _)) =>
            // unnormalized
            loop(tail, front)
          case SeqPart.Wildcard :: tail =>
            val tr = loop(tail, Nil)
            val tailRes =
              if (tr eq Empty.parts) NonEmptyList.one(StrPart.WildStr)
              else tr.prepend(StrPart.WildStr)

            NonEmptyList.fromList(lit(front)) match {
              case None    => tailRes
              case Some(h) => h ::: tailRes
            }
        }

      StrPat(loop(sp.toList, Nil))
    }

    def toNamedSeqPattern(sp: StrPat): NamedSeqPattern[Int] = {
      val empty: NamedSeqPattern[Int] = NamedSeqPattern.NEmpty

      def partToNsp(s: StrPart): NamedSeqPattern[Int] =
        s match {
          case StrPart.NamedStr(n) =>
            NamedSeqPattern.Bind(n.sourceCodeRepr, NamedSeqPattern.Wild)
          case StrPart.NamedChar(n) =>
            NamedSeqPattern.Bind(n.sourceCodeRepr, NamedSeqPattern.Any)
          case StrPart.WildStr   => NamedSeqPattern.Wild
          case StrPart.WildChar  => NamedSeqPattern.Any
          case StrPart.LitStr(s) =>
            StringUtil
              .codePoints(s)
              .foldRight(empty) { (c, tail) =>
                NamedSeqPattern.NCat(NamedSeqPattern.fromLit(c), tail)
              }
        }

      sp.parts.toList.foldRight(empty) { (h, t) =>
        NamedSeqPattern.NCat(partToNsp(h), t)
      }
    }

    def fromLitStr(s: String): StrPat =
      StrPat(NonEmptyList.one(StrPart.LitStr(s)))
  }

  /** If this pattern is: x (x: T) unnamed as x x | x | x then it is
    * "SinglyNamed"
    */
  object SinglyNamed {
    def unapply[N, T](p: Pattern[N, T]): Option[Bindable] =
      p match {
        case Var(b)                        => Some(b)
        case Annotation(SinglyNamed(b), _) => Some(b)
        case Named(b, inner)               =>
          if (inner.names.isEmpty) Some(b)
          else unapply(inner).filter(_ == b)
        case Union(SinglyNamed(b), r) =>
          r.foldM(b) { (b, pat) =>
            unapply(pat).filter(_ == b)
          }
        case _ => None
      }
  }

  implicit def patternOrdering[N: Ordering, T: Ordering]
      : Ordering[Pattern[N, T]] =
    new Ordering[Pattern[N, T]] {
      val ordN = implicitly[Ordering[N]]
      val ordT = implicitly[Ordering[T]]
      val list = ListOrdering.onType(this)
      val ordBin = implicitly[Ordering[Bindable]]
      def partOrd[A](ordA: Ordering[A]): Ordering[ListPart[A]] =
        new Ordering[ListPart[A]] {
          def compare(a: ListPart[A], b: ListPart[A]) =
            (a, b) match {
              case (ListPart.WildList, ListPart.WildList)         => 0
              case (ListPart.WildList, _)                         => -1
              case (ListPart.NamedList(_), ListPart.WildList)     => 1
              case (ListPart.NamedList(a), ListPart.NamedList(b)) =>
                ordBin.compare(a, b)
              case (ListPart.NamedList(_), ListPart.Item(_)) => -1
              case (ListPart.Item(a), ListPart.Item(b)) => ordA.compare(a, b)
              case (ListPart.Item(_), _)                => 1
            }
        }

      val listE = ListOrdering.onType(partOrd(this))

      val ordStrPart = new Ordering[StrPart] {
        import StrPart._

        def compare(a: StrPart, b: StrPart) =
          (a, b) match {
            case (WildStr, WildStr)                      => 0
            case (WildStr, _)                            => -1
            case (WildChar, WildStr)                     => 1
            case (WildChar, WildChar)                    => 0
            case (WildChar, _)                           => -1
            case (LitStr(_), WildStr | WildChar)         => 1
            case (LitStr(sa), LitStr(sb))                => sa.compareTo(sb)
            case (LitStr(_), NamedStr(_) | NamedChar(_)) => -1
            case (NamedChar(_), WildStr | WildChar | LitStr(_)) => 1
            case (NamedChar(na), NamedChar(nb)) => ordBin.compare(na, nb)
            case (NamedChar(_), NamedStr(_))    => -1
            case (NamedStr(na), NamedStr(nb))   => ordBin.compare(na, nb)
            case (NamedStr(_), _)               => 1
          }
      }
      val strOrd = ListOrdering.onType(ordStrPart)

      val compIdent: Ordering[Identifier] = implicitly[Ordering[Identifier]]

      def compare(a: Pattern[N, T], b: Pattern[N, T]): Int =
        (a, b) match {
          case (WildCard, WildCard)            => 0
          case (WildCard, _)                   => -1
          case (Literal(_), WildCard)          => 1
          case (Literal(a), Literal(b))        => Lit.litOrdering.compare(a, b)
          case (Literal(_), _)                 => -1
          case (Var(_), WildCard | Literal(_)) => 1
          case (Var(a), Var(b))                => compIdent.compare(a, b)
          case (Var(_), _)                     => -1
          case (Named(_, _), WildCard | Literal(_) | Var(_)) => 1
          case (Named(n1, p1), Named(n2, p2))                =>
            val c = compIdent.compare(n1, n2)
            if (c == 0) compare(p1, p2) else c
          case (Named(_, _), _)                                          => -1
          case (StrPat(_), WildCard | Literal(_) | Var(_) | Named(_, _)) => 1
          case (StrPat(as), StrPat(bs)) => strOrd.compare(as.toList, bs.toList)
          case (StrPat(_), _)           => -1
          case (
                ListPat(_),
                WildCard | Literal(_) | Var(_) | Named(_, _) | StrPat(_)
              ) =>
            1
          case (ListPat(as), ListPat(bs)) => listE.compare(as, bs)
          case (ListPat(_), _)            => -1
          case (Annotation(_, _), PositionalStruct(_, _) | Union(_, _)) => -1
          case (Annotation(a0, t0), Annotation(a1, t1))                 =>
            val c = compare(a0, a1)
            if (c == 0) ordT.compare(t0, t1) else c
          case (Annotation(_, _), _)                                => 1
          case (PositionalStruct(_, _), Union(_, _))                => -1
          case (PositionalStruct(n0, a0), PositionalStruct(n1, a1)) =>
            val c = ordN.compare(n0, n1)
            if (c == 0) list.compare(a0, a1) else c
          case (PositionalStruct(_, _), _)    => 1
          case (Union(h0, t0), Union(h1, t1)) =>
            list.compare(h0 :: t0.toList, h1 :: t1.toList)
          case (Union(_, _), _) => 1
        }
    }

  implicit def document[T: Document]: Document[Pattern[StructKind, T]] =
    Document.instance[Pattern[StructKind, T]] {
      case WildCard                  => Doc.char('_')
      case Literal(lit)              => Document[Lit].document(lit)
      case Var(n)                    => Document[Identifier].document(n)
      case Named(n, u @ Union(_, _)) =>
        // union is also an operator, so we need to use parens to explicitly bind | more tightly
        // than the @ on the left.
        Doc.char('(') + document.document(u) + Doc.char(')') + Doc.text(
          " as "
        ) + Document[Identifier].document(n)
      case Named(n, p) =>
        document.document(p) + Doc.text(" as ") + Document[Identifier].document(
          n
        )
      case StrPat(items) =>
        // prefer ' if possible, else use "
        val useDouble = items.exists {
          case StrPart.LitStr(str) => str.contains('\'') && !str.contains('"')
          case _                   => false
        }
        val q = if (useDouble) '"' else '\''
        val sd = StrPart.document(q)
        val inner = Doc.intercalate(Doc.empty, items.toList.map(sd.document(_)))
        Doc.char(q) + inner + Doc.char(q)
      case ListPat(items) =>
        Doc.char('[') + Doc.intercalate(
          Doc.text(", "),
          items.map {
            case ListPart.WildList        => Doc.text("*_")
            case ListPart.NamedList(glob) =>
              Doc.char('*') + Document[Identifier].document(glob)
            case ListPart.Item(p) => document.document(p)
          }
        ) + Doc.char(']')
      case Annotation(p, t) =>
        /*
         * We need to know what package we are in and what imports we depend on here.
         * This creates some challenges we need to deal with:
         *   1. how do we make sure we don't have duplicate short names
         *   2. how do we make sure we have imported the names we need
         *   3. at the top level we need parens to distinguish a: Integer from being the rhs of a
         *      case
         */
        document.document(p) + Doc.text(": ") + Document[T].document(t)
      case PositionalStruct(n, Nil) =>
        n match {
          case StructKind.Tuple =>
            Doc.text("()")
          case StructKind.Named(nm, _) =>
            Document[Identifier].document(nm)
          case StructKind.NamedPartial(nm, _) =>
            Document[Identifier].document(nm) + Doc.text("(...)")
        }
      case PositionalStruct(StructKind.Tuple, h :: Nil) =>
        // single item tuples need a comma:
        Doc.char('(') + document.document(h) + Doc.text(",)")
      case PositionalStruct(n, nonEmpty) =>
        val prefix = n match {
          case StructKind.Tuple            => Doc.empty
          case named: StructKind.NamedKind =>
            Document[Identifier].document(named.name)
        }

        val args = nonEmpty.map(document.document(_))

        val suffix = n match {
          case StructKind.NamedPartial(_, _) =>
            Doc.text(", ...")
          case StructKind.Named(_, _) | StructKind.Tuple =>
            Doc.empty
        }

        n.namedStyle match {
          case None | Some(StructKind.Style.TupleLike) =>
            prefix +
              Doc.char('(') +
              Doc.intercalate(Doc.text(", "), args) +
              suffix +
              Doc.char(')')
          case Some(StructKind.Style.RecordLike(fields)) =>
            // We assume that we have a matching set
            // of fields here
            val cspace = Doc.text(": ")
            val identDoc = Document[Identifier]
            val kvargs = Doc.intercalate(
              Doc.text(", "),
              fields.toList
                .zip(args)
                .map {
                  case (StructKind.Style.FieldKind.Explicit(n), adoc) =>
                    identDoc.document(n) + cspace + adoc
                  case (StructKind.Style.FieldKind.Implicit(_), adoc) => adoc
                }
            )
            prefix +
              Doc.text(" { ") +
              kvargs +
              suffix +
              Doc.text(" }")
        }
      case Union(head, rest) =>
        def doc(p: Pattern[StructKind, T]): Doc =
          p match {
            case Annotation(_, _) | Union(_, _) =>
              // if an annotation or union is embedded, we need to put parens for parsing
              // to round trip. Note, we will never parse a nested union, but generators or could
              // code produce one
              Doc.char('(') + document.document(p) + Doc.char(')')
            case nonParen => document.document(nonParen)
          }
        Doc.intercalate(Doc.text(" | "), (head :: rest.toList).map(doc(_)))
    }

  def recordPat[N <: StructKind.NamedKind](
      name: Constructor,
      args: NonEmptyList[Either[Bindable, (Bindable, Parsed)]]
  )(
      fn: (Constructor, StructKind.Style) => N
  ): PositionalStruct[StructKind, TypeRef] = {
    val fields = args.map {
      case Left(b)       => StructKind.Style.FieldKind.Implicit(b)
      case Right((b, _)) => StructKind.Style.FieldKind.Explicit(b)
    }
    val structArgs = args.toList.map {
      case Left(b)       => Pattern.Var(b)
      case Right((_, p)) => p
    }
    PositionalStruct(fn(name, StructKind.Style.RecordLike(fields)), structArgs)
  }

  def compiledDocument[A: Document]
      : Document[Pattern[(PackageName, Constructor), A]] = {
    lazy val doc: Document[Pattern[(PackageName, Constructor), A]] =
      compiledDocument[A]
    Document.instance[Pattern[(PackageName, Constructor), A]] {
      case WildCard                  => Doc.char('_')
      case Literal(lit)              => Document[Lit].document(lit)
      case Var(n)                    => Document[Identifier].document(n)
      case Named(n, u @ Union(_, _)) =>
        // union is also an operator, so we need to use parens to explicitly bind | more tightly
        // than the as on the left.
        Doc.char('(') + doc.document(u) + Doc.char(')') + Doc.text(
          " as "
        ) + Document[Identifier].document(n)
      case Named(n, p) =>
        doc.document(p) + Doc.text(" as ") + Document[Identifier].document(n)
      case StrPat(items)  => document.document(StrPat(items))
      case ListPat(items) =>
        Doc.char('[') + Doc.intercalate(
          Doc.text(", "),
          items.map {
            case ListPart.WildList        => Doc.text("*_")
            case ListPart.NamedList(glob) =>
              Doc.char('*') + Document[Identifier].document(glob)
            case ListPart.Item(p) => doc.document(p)
          }
        ) + Doc.char(']')
      case Annotation(p, t) =>
        /*
         * We need to know what package we are in and what imports we depend on here.
         * This creates some challenges we need to deal with:
         *   1. how do we make sure we don't have duplicate short names
         *   2. how do we make sure we have imported the names we need
         *   3. at the top level we need parens to distinguish a: Integer from being the rhs of a
         *      case
         */
        doc.document(p) + Doc.text(": ") + Document[A].document(t)
      case ps @ PositionalStruct((_, c), a) =>
        def untuple(
            p: Pattern[(PackageName, Constructor), A]
        ): Option[List[Doc]] =
          p match {
            case PositionalStruct(
                  (PackageName.PredefName, Constructor("Unit")),
                  Nil
                ) =>
              Some(Nil)
            case PositionalStruct(
                  (PackageName.PredefName, Constructor("TupleCons")),
                  a :: b :: Nil
                ) =>
              untuple(b).map(l => doc.document(a) :: l)
            case _ => None
          }
        def tup(ds: List[Doc]): Doc =
          Doc.char('(') +
            Doc.intercalate(Doc.text(", "), ds) +
            Doc.char(')')

        untuple(ps) match {
          case Some(tupDocs) => tup(tupDocs)
          case None          =>
            val args = a match {
              case Nil => Doc.empty
              case _   => tup(a.map(doc.document(_)))
            }
            Doc.text(c.asString) + args
        }
      case Union(head, rest) =>
        def inner(p: Pattern[(PackageName, Constructor), A]): Doc =
          p match {
            case Annotation(_, _) | Union(_, _) =>
              // if an annotation or union is embedded, we need to put parens for parsing
              // to round trip. Note, we will never parse a nested union, but generators or could
              // code produce one
              Doc.char('(') + doc.document(p) + Doc.char(')')
            case nonParen => doc.document(nonParen)
          }
        Doc.intercalate(Doc.text(" | "), (head :: rest.toList).map(inner(_)))
    }
  }

  /** For fully typed patterns, compute the type environment of the bindings
    * from this pattern. This will sys.error if you pass a bad pattern, which
    * you should never do (and this code will never do unless there is some
    * broken invariant)
    */
  def envOf[C, K, T](p: Pattern[C, T], env: Map[K, T])(
      kfn: Identifier => K
  ): Map[K, T] = {
    def update(env: Map[K, T], n: Identifier, typeOf: Option[T]): Map[K, T] =
      typeOf match {
        case None =>
          // $COVERAGE-OFF$ should be unreachable
          sys.error(s"no type found for $n in $p")
        // $COVERAGE-ON$ should be unreachable
        case Some(t) => env.updated(kfn(n), t)
      }
    def loop(p0: Pattern[C, T], typeOf: Option[T], env: Map[K, T]): Map[K, T] =
      p0 match {
        case WildCard     => env
        case Literal(_)   => env
        case Var(n)       => update(env, n, typeOf)
        case Named(n, p1) =>
          val e1 = loop(p1, typeOf, env)
          update(e1, n, typeOf)
        case StrPat(items) =>
          // the type annotation typeOf applies to all the current names, which must be strings
          items
            .foldLeft(env) {
              case (env, StrPart.NamedStr(n)) => update(env, n, typeOf)
              case (env, _)                   => env
            }

        case ListPat(items) =>
          items.foldLeft(env) {
            case (env, ListPart.WildList)     => env
            case (env, ListPart.NamedList(n)) =>
              // the type of a named sub-list is
              // the same as the type of the list
              update(env, n, typeOf)
            case (env, ListPart.Item(p)) =>
              loop(p, None, env)
          }
        case Annotation(p, t) =>
          loop(p, Some(t), env)
        case PositionalStruct(_, as) =>
          as.foldLeft(env)((env, p) => loop(p, None, env))
        case Union(head, rest) =>
          (head :: rest).foldLeft(env)((env, p) => loop(p, None, env))
      }

    loop(p, None, env)
  }

  private val pwild = P.char('_').as(WildCard)
  private val plit: P[Pattern[Nothing, Nothing]] = {
    val intp =
      (Lit.float64Parser.backtrack | Lit.integerParser | Lit.codePointParser)
        .map(Literal(_))
    val startStr = P.string("${").as { (opt: Option[Bindable]) =>
      opt.fold(StrPart.WildStr: StrPart)(StrPart.NamedStr(_))
    }
    val startChar = P.string("$.{").as { (opt: Option[Bindable]) =>
      opt.fold(StrPart.WildChar: StrPart)(StrPart.NamedChar(_))
    }
    val start = startStr | startChar
    val end = P.char('}')

    val pwild = P.char('_').as(None)
    val pname = Identifier.bindableParser.map(Some(_))
    val part: P[Option[Bindable]] = pwild | pname

    def strp(q: Char): P[List[StrPart]] =
      StringUtil
        .interpolatedString(q, start, part, end)
        .map(_.map {
          case Left(p)         => p
          case Right((_, str)) => StrPart.LitStr(str)
        })

    val eitherString = strp('\'') <+> strp('"')
    // don't emit complex patterns for simple strings:
    val str = eitherString.map {
      case Nil                        => Literal(Lit.EmptyStr)
      case StrPart.LitStr(str) :: Nil => Literal(Lit.Str(str))
      case h :: tail                  => StrPat(NonEmptyList(h, tail))
    }

    str <+> intp
  }

  /** This does not allow a top-level type annotation which would be ambiguous
    * with : used for ending the match case block
    */
  val matchParser: P[Parsed] =
    P.defer(matchOrNot(isMatch = true))

  /** A Pattern in a match position allows top level un-parenthesized type
    * annotation
    */
  val bindParser: P[Parsed] =
    P.defer(matchOrNot(isMatch = false))

  private val maybePartial
      : P0[(Constructor, StructKind.Style) => StructKind.NamedKind] = {
    val partial = (maybeSpace.soft ~ P.string("...")).as(
      (n: Constructor, s: StructKind.Style) => StructKind.NamedPartial(n, s)
    )

    val notPartial =
      P.pure((n: Constructor, s: StructKind.Style) => StructKind.Named(n, s))

    partial.orElse(notPartial)
  }

  private def parseRecordStruct(
      recurse: P0[Parsed]
  ): P[Constructor => PositionalStruct[StructKind, TypeRef]] = {
    // We do maybeSpace, then { } then either a Bindable or Bindable: Pattern
    // maybe followed by ...
    val item: P[Either[Bindable, (Bindable, Parsed)]] =
      (Identifier.bindableParser ~ ((maybeSpace.soft ~ P.char(
        ':'
      ) ~ maybeSpace) *> recurse).?)
        .map {
          case (b, None)      => Left(b)
          case (b, Some(pat)) => Right((b, pat))
        }

    val items = item.nonEmptyList ~ maybePartial
    ((maybeSpace.with1.soft ~ P.char(
      '{'
    ) ~ maybeSpace) *> items <* (maybeSpace ~ P.char('}')))
      .map { case (args, fn) => { (c: Constructor) => recordPat(c, args)(fn) } }
  }

  private def parseTupleStruct(
      recurse: P[Parsed]
  ): P[Constructor => PositionalStruct[StructKind, TypeRef]] = {
    // There are three cases:
    // Foo(1 or more patterns)
    // Foo(1 or more patterns, ...)
    // Foo(...)

    val oneOrMore = recurse.nonEmptyList.map(_.toList) ~ maybePartial
    val onlyPartial = P.string("...").as {
      (
        Nil,
        { (n: Constructor, s: StructKind.Style) =>
          StructKind.NamedPartial(n, s)
        }
      )
    }

    (oneOrMore <+> onlyPartial).parensCut
      .map {
        case (args, fn) => { (n: Constructor) =>
          PositionalStruct(fn(n, StructKind.Style.TupleLike), args)
        }
      }
  }

  def tuple(args: List[Parsed]): Parsed =
    PositionalStruct(StructKind.Tuple, args)

  def isNonUnitTuple(arg: Parsed): Boolean =
    arg match {
      case PositionalStruct(StructKind.Tuple, args) => args.nonEmpty
      case _                                        => false
    }

  def fromTupleOrParens(e: Either[Parsed, List[Parsed]]): Parsed =
    e match {
      case Right(tup)   => tuple(tup)
      case Left(parens) => parens
    }

  def fromMaybeTupleOrParens(p: MaybeTupleOrParens[Parsed]): Parsed =
    p match {
      case MaybeTupleOrParens.Bare(b)   => b
      case MaybeTupleOrParens.Parens(p) => p
      case MaybeTupleOrParens.Tuple(p)  => tuple(p)
    }

  private def matchOrNot(isMatch: Boolean): P[Parsed] = {
    val recurse = P.defer(bindParser)

    val positional =
      (Identifier.consParser ~ (parseTupleStruct(recurse) <+> parseRecordStruct(
        recurse
      )).?)
        .map {
          case (n, None) =>
            PositionalStruct(
              StructKind.Named(n, StructKind.Style.TupleLike),
              Nil
            )
          case (n, Some(fn)) => fn(n)
        }

    val tupleOrParens = recurse.tupleOrParens.map(fromTupleOrParens)

    val listItem: P[ListPart[Parsed]] = {
      val maybeNamed: P[ListPart[Parsed]] =
        P.char('_')
          .as(ListPart.WildList)
          .orElse(Identifier.bindableParser.map(ListPart.NamedList(_)))

      (P.char('*') *> maybeNamed) <+> recurse.map(ListPart.Item(_))
    }

    val listP = listItem.listSyntax.map(ListPat(_))

    val pvar = Identifier.bindableParser.map(Var(_))

    val nonAnnotated =
      P.defer(
        P.oneOf(
          plit :: pwild :: tupleOrParens :: positional :: listP :: pvar :: Nil
        )
      )

    val namedOp: P[Parsed => Parsed] =
      ((maybeSpace.with1 *> P.string(
        "as"
      ) <* Parser.spaces).backtrack *> Identifier.bindableParser)
        .map(n => (pat: Parsed) => Named(n, pat))

    val withAs: P[Parsed] =
      (nonAnnotated ~ namedOp.rep0)
        .map { case (p, ops) => ops.foldLeft(p)((p, fn) => fn(p)) }

    // A union can't have an annotation, we need to be inside a parens for that
    val unionOp: P[Parsed => Parsed] = {
      val bar = P.char('|')
      val unionRest = withAs
        .nonEmptyListOfWsSep(maybeSpace, bar, allowTrailing = false)

      (maybeSpace.with1.soft *> bar *> maybeSpace *> unionRest)
        .map(ne => (pat: Parsed) => union(pat, ne.toList))
    }
    val typeAnnotOp: P[Parsed => Parsed] =
      TypeRef.annotationParser
        .map(tpe => (pat: Parsed) => Annotation(pat, tpe))

    // We only allow type annotation not at the top level, must be inside
    // Struct or parens
    if (isMatch) withAs.maybeAp(unionOp)
    else withAs.maybeAp(unionOp.orElse(typeAnnotOp))
  }
}
