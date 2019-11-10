package org.bykn.bosatsu

import cats.Applicative
import cats.data.NonEmptyList
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.{ Combinators, maybeSpace }
import cats.implicits._

import Identifier.{Bindable, Constructor}

sealed abstract class Pattern[+N, +T] {
  def mapName[U](fn: N => U): Pattern[U, T] =
    (new Pattern.InvariantPattern(this)).mapStruct[U] { (n, parts) =>
      Pattern.PositionalStruct(fn(n), parts)
    }

  def mapType[U](fn: T => U): Pattern[N, U] =
    (new Pattern.InvariantPattern(this)).traverseType[cats.Id, U](fn)

  /**
   * List all the names that are bound in Vars inside this pattern
   * in the left to right order they are encountered, without any duplication
   */
  def names: List[Bindable] = {
    @annotation.tailrec
    def loop(stack: List[Pattern[N, T]], seen: Set[Bindable], acc: List[Bindable]): List[Bindable] =
      stack match {
        case Nil => acc.reverse
        case (Pattern.WildCard | Pattern.Literal(_)) :: tail => loop(tail, seen, acc)
        case Pattern.Var(v) :: tail =>
          if (seen(v)) loop(tail, seen, acc)
          else loop(tail, seen + v, v :: acc)
        case Pattern.Named(v, p) :: tail =>
          if (seen(v)) loop(p :: tail, seen, acc)
          else loop(p :: tail, seen + v, v :: acc)
        case Pattern.ListPat(items) :: tail =>
          val globs = items.collect { case Pattern.ListPart.NamedList(glob) => glob }.filterNot(seen)
          val next = items.collect { case Pattern.ListPart.Item(inner) => inner }
          loop(next ::: tail, seen ++ globs, globs reverse_::: acc)
        case Pattern.Annotation(p, _) :: tail => loop(p :: tail, seen, acc)
        case Pattern.PositionalStruct(name, params) :: tail =>
          loop(params ::: tail, seen, acc)
        case Pattern.Union(h, t) :: tail =>
          loop(h :: (t.toList) ::: tail, seen, acc)
      }

    loop(this :: Nil, Set.empty, Nil)
  }

  /**
   * List all the names that strictly smaller than anything that would match this pattern
   * e.g. a top level var, would not be returned
   */
  def substructures: List[Bindable] = {
    def cheat(stack: List[(Pattern[N, T], Boolean)], seen: Set[Bindable], acc: List[Bindable]): List[Bindable] =
      loop(stack, seen, acc)

    import Pattern.ListPart

    @annotation.tailrec
    def loop(stack: List[(Pattern[N, T], Boolean)], seen: Set[Bindable], acc: List[Bindable]): List[Bindable] =
      stack match {
        case Nil => acc.reverse
        case ((Pattern.WildCard, _) | (Pattern.Literal(_), _)) :: tail => loop(tail, seen, acc)
        case (Pattern.Var(v), isTop) :: tail =>
          if (seen(v) || isTop) loop(tail, seen, acc)
          else loop(tail, seen + v, v :: acc)
        case (Pattern.Named(v, p), isTop) :: tail =>
          if (seen(v) || isTop) loop((p, isTop) :: tail, seen, acc)
          else loop((p, isTop) :: tail, seen + v, v :: acc)
        case (Pattern.ListPat(ListPart.NamedList(_) :: Nil), true) :: tail =>
            // this is a total match at the top level, not a substructure
            loop(tail, seen, acc)
        case (Pattern.ListPat(items), _) :: tail =>
          val globs = items.collect { case ListPart.NamedList(glob) => glob }.filterNot(seen)
          val next = items.collect { case ListPart.Item(inner) => (inner, false) }
          loop(next ::: tail, seen ++ globs, globs reverse_::: acc)
        case (Pattern.Annotation(p, _), isTop) :: tail => loop((p, isTop) :: tail, seen, acc)
        case (Pattern.PositionalStruct(name, params), _) :: tail =>
          loop(params.map((_, false)) ::: tail, seen, acc)
        case (Pattern.Union(h, t), isTop) :: tail =>
          val all = (h :: t.toList).map { p => cheat((p, isTop) :: tail, seen, acc) }
          // we need to be substructual on all:
          val intr = all.map(_.toSet).reduce(_.intersect(_))
          all.flatMap(_.filter(intr)).distinct
      }

    loop((this, true) :: Nil, Set.empty, Nil)
  }

  /**
   * Return the pattern with all the binding names removed
   */
  def unbind: Pattern[N, T] =
    filterVars(Set.empty)

  /**
   * replace all Var names with Wildcard that are not
   * satifying the keep predicate
   */
  def filterVars(keep: Bindable => Boolean): Pattern[N, T] =
    this match {
      case Pattern.WildCard | Pattern.Literal(_) => this
      case p@Pattern.Var(v) =>
        if (keep(v)) p else Pattern.WildCard
      case n@Pattern.Named(v, p) =>
        val inner = p.filterVars(keep)
        if (keep(v)) Pattern.Named(v, inner)
        else inner
      case Pattern.ListPat(items) =>
        Pattern.ListPat(items.map {
          case Pattern.ListPart.WildList => Pattern.ListPart.WildList
          case in@Pattern.ListPart.NamedList(n) =>
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
}

object Pattern {

  /**
   * Represents the different patterns that are all for structs
   * (2, 3)
   * Foo(2, 3)
   * etc...
   */
  sealed abstract class StructKind {
    def namedStyle: Option[StructKind.Style] =
      this match {
        case StructKind.Tuple => None
        case StructKind.Named(_, style) => Some(style)
        case StructKind.NamedPartial(_, style) => Some(style)
      }
  }
  object StructKind {
    sealed abstract class Style
    object Style {
      sealed abstract class FieldKind {
        def field: Bindable
      }
      object FieldKind {
        final case class Explicit(field: Bindable) extends FieldKind
        // an implicit field can only be associated with a Var of
        // the same name
        final case class Implicit(field: Bindable) extends FieldKind
      }
      final case object TupleLike extends Style
      // represents the fields like: Foo { bar: x, age }
      final case class RecordLike(fields: NonEmptyList[FieldKind]) extends Style
    }
    sealed abstract class NamedKind extends StructKind {
      def name: Constructor
      def style: Style
    }
    final case object Tuple extends StructKind
    // Represents a complete tuple-like pattern Foo(a, b)
    final case class Named(name: Constructor, style: Style) extends NamedKind
    // Represents a partial tuple-like pattern Foo(a, ...)
    final case class NamedPartial(name: Constructor, style: Style) extends NamedKind
  }

  /**
   * represents items in a list pattern
   */
  sealed abstract class ListPart[+A] {
    def map[B](fn: A => B): ListPart[B]
  }
  object ListPart {
    sealed abstract class Glob extends ListPart[Nothing] {
      def map[B](fn: Nothing => B): ListPart[B] = this
    }
    final case object WildList extends Glob
    final case class NamedList(name: Bindable) extends Glob
    final case class Item[A](pat: A) extends ListPart[A] {
      def map[B](fn: A => B): ListPart[B] = Item(fn(pat))
    }
  }

  type Parsed = Pattern[StructKind, TypeRef]

  /**
   * Flatten a pattern out such that there are no top-level
   * unions
   */
  def flatten[N, T](p: Pattern[N, T]): NonEmptyList[Pattern[N, T]] =
    p match {
      case Union(h, t) => NonEmptyList(h, t.toList).flatMap(flatten(_))
      case nonU => NonEmptyList(nonU, Nil)
    }

  /**
   * Create a normalized pattern, which doesn't have nested top level unions
   */
  def union[N, T](head: Pattern[N, T], tail: List[Pattern[N, T]]): Pattern[N, T] = {
    NonEmptyList(head, tail).flatMap(flatten(_)) match {
      case NonEmptyList(h, Nil) => h
      case NonEmptyList(h0, h1 :: tail) => Union(h0, NonEmptyList(h1, tail))
    }
  }

  implicit class InvariantPattern[N, T](val pat: Pattern[N, T]) extends AnyVal {
    def traverseType[F[_]: Applicative, T1](fn: T => F[T1]): F[Pattern[N, T1]] =
      pat match {
        case Pattern.WildCard => Applicative[F].pure(Pattern.WildCard)
        case Pattern.Literal(lit) => Applicative[F].pure(Pattern.Literal(lit))
        case Pattern.Var(v) => Applicative[F].pure(Pattern.Var(v))
        case Pattern.Named(n, p) => p.traverseType(fn).map(Pattern.Named(n, _))
        case Pattern.ListPat(items) =>
          items.traverse {
            case ListPart.Item(p) =>
              p.traverseType(fn).map(ListPart.Item(_): ListPart[Pattern[N, T1]])
            case ListPart.WildList =>
              Applicative[F].pure(ListPart.WildList: ListPart[Pattern[N, T1]])
            case ListPart.NamedList(n) =>
              Applicative[F].pure(ListPart.NamedList(n): ListPart[Pattern[N, T1]])
          }.map(Pattern.ListPat(_))
        case Pattern.Annotation(p, tpe) =>
          (p.traverseType(fn), fn(tpe)).mapN(Pattern.Annotation(_, _))
        case Pattern.PositionalStruct(name, params) =>
          params.traverse(_.traverseType(fn)).map { ps =>
            Pattern.PositionalStruct(name, ps)
          }
        case Pattern.Union(h, tail) =>
          (h.traverseType(fn), tail.traverse(_.traverseType(fn))).mapN { (h, t) =>
            Pattern.Union(h, t)
          }
      }

    def mapStruct[N1](parts: (N, List[Pattern[N1, T]]) => Pattern[N1, T]): Pattern[N1, T] =
      traverseStruct[cats.Id, N1](parts)

    def traverseStruct[F[_]: Applicative, N1](parts: (N, F[List[Pattern[N1, T]]]) => F[Pattern[N1, T]]): F[Pattern[N1, T]] = {
      lazy val pwild: F[Pattern[N1, T]] = Applicative[F].pure(Pattern.WildCard)

      def go(pat: Pattern[N, T]): F[Pattern[N1, T]] =
        pat match {
          case Pattern.WildCard => pwild
          case Pattern.Literal(lit) => Applicative[F].pure(Pattern.Literal(lit))
          case Pattern.Var(v) => Applicative[F].pure(Pattern.Var(v))
          case Pattern.Named(v, p) =>
            go(p).map(Pattern.Named(v, _))
          case Pattern.ListPat(items) =>
            type L = ListPart[Pattern[N1, T]]
            val items1 = items.traverse {
              case ListPart.WildList =>
                Applicative[F].pure(ListPart.WildList: L)
              case ListPart.NamedList(n) =>
                Applicative[F].pure(ListPart.NamedList(n): L)
              case ListPart.Item(p) =>
                go(p).map(ListPart.Item(_): L)
            }
            items1.map(Pattern.ListPat(_))
          case Pattern.Annotation(p, tpe) =>
            go(p).map(Pattern.Annotation(_, tpe))
          case Pattern.PositionalStruct(name, params) =>
            val p1 = params.traverse(go(_))
            parts(name, p1)
          case Pattern.Union(h, tail) =>
            (go(h), tail.traverse(go)).mapN(Pattern.Union(_, _))
        }

      go(pat)
    }
  }

  case object WildCard extends Pattern[Nothing, Nothing]
  case class Literal(toLit: Lit) extends Pattern[Nothing, Nothing]
  case class Var(name: Bindable) extends Pattern[Nothing, Nothing]
  /**
   * Patterns like foo @ Some(_)
   * @ binds tighter than |, so use ( ) with groups you want to bind
   */
  case class Named[N, T](name: Bindable, pat: Pattern[N, T]) extends Pattern[N, T]
  case class ListPat[N, T](parts: List[ListPart[Pattern[N, T]]]) extends Pattern[N, T]
  case class Annotation[N, T](pattern: Pattern[N, T], tpe: T) extends Pattern[N, T]
  case class PositionalStruct[N, T](name: N, params: List[Pattern[N, T]]) extends Pattern[N, T]
  case class Union[N, T](head: Pattern[N, T], rest: NonEmptyList[Pattern[N, T]]) extends Pattern[N, T]

  implicit def patternOrdering[N: Ordering, T: Ordering]: Ordering[Pattern[N, T]] =
    new Ordering[Pattern[N, T]] {
      val ordN = implicitly[Ordering[N]]
      val ordT = implicitly[Ordering[T]]
      val list = ListOrdering.onType(this)
      def partOrd[A](ordA: Ordering[A]): Ordering[ListPart[A]] =
        new Ordering[ListPart[A]] {
          val ordBin = implicitly[Ordering[Bindable]]
          def compare(a: ListPart[A], b: ListPart[A]) =
            (a, b) match {
              case (ListPart.WildList, ListPart.WildList) => 0
              case (ListPart.WildList, _) => -1
              case (ListPart.NamedList(_), ListPart.WildList) => 1
              case (ListPart.NamedList(a), ListPart.NamedList(b)) =>
                ordBin.compare(a, b)
              case (ListPart.NamedList(_), ListPart.Item(_)) => -1
              case (ListPart.Item(a), ListPart.Item(b)) => ordA.compare(a, b)
              case (ListPart.Item(_), _) => 1
            }
        }

      val listE = ListOrdering.onType(partOrd(this))

      val compIdent: Ordering[Identifier] = implicitly[Ordering[Identifier]]

      def compare(a: Pattern[N, T], b: Pattern[N, T]): Int =
        (a, b) match {
          case (WildCard, WildCard) => 0
          case (WildCard, _) => -1
          case (Literal(_), WildCard) => 1
          case (Literal(a), Literal(b)) => Lit.litOrdering.compare(a, b)
          case (Literal(_), _) => -1
          case (Var(_), WildCard | Literal(_)) => 1
          case (Var(a), Var(b)) => compIdent.compare(a, b)
          case (Var(_), _) => -1
          case (Named(_, _), WildCard | Literal(_) | Var(_)) => 1
          case (Named(n1, p1), Named(n2, p2)) =>
            val c = compIdent.compare(n1, n2)
            if (c == 0) compare(p1, p2) else c
          case (Named(_, _), _) => -1
          case (ListPat(_), WildCard | Literal(_) | Var(_) | Named(_, _)) => 1
          case (ListPat(as), ListPat(bs)) => listE.compare(as, bs)
          case (ListPat(_), _) => -1
          case (Annotation(_, _), PositionalStruct(_, _) | Union(_, _)) => -1
          case (Annotation(a0, t0), Annotation(a1, t1)) =>
            val c = compare(a0, a1)
            if (c == 0) ordT.compare(t0, t1) else c
          case (Annotation(_, _), _) => 1
          case (PositionalStruct(_, _), Union(_, _)) => -1
          case (PositionalStruct(n0, a0), PositionalStruct(n1, a1)) =>
            val c = ordN.compare(n0, n1)
            if (c == 0) list.compare(a0, a1) else c
          case (PositionalStruct(_, _), _) => 1
          case (Union(h0, t0), Union(h1, t1)) =>
            list.compare(h0 :: t0.toList, h1 :: t1.toList)
          case (Union(_, _), _) => 1
        }
    }

  implicit lazy val document: Document[Parsed] =
    Document.instance[Parsed] {
      case WildCard => Doc.char('_')
      case Literal(lit) => Document[Lit].document(lit)
      case Var(n) => Document[Identifier].document(n)
      case Named(n, u@Union(_, _)) =>
        // union is also an operator, so we need to use parens to explicitly bind | more tightly
        // than the @ on the left.
        Document[Identifier].document(n) + Doc.char('@') + Doc.char('(') + document.document(u) + Doc.char(')')
      case Named(n, p) =>
        Document[Identifier].document(n) + Doc.char('@') + document.document(p)
      case ListPat(items) =>
        Doc.char('[') + Doc.intercalate(Doc.text(", "),
          items.map {
            case ListPart.WildList => Doc.text("*_")
            case ListPart.NamedList(glob) => Doc.char('*') + Document[Identifier].document(glob)
            case ListPart.Item(p) => document.document(p)
          }) + Doc.char(']')
      case Annotation(p, t) =>
        /*
         * We need to know what package we are in and what imports we depend on here.
         * This creates some challenges we need to deal with:
         *   1. how do we make sure we don't have duplicate short names
         *   2. how do we make sure we have imported the names we need
         *   3. at the top level we need parens to distinguish a: Integer from being the rhs of a
         *      case
         */
        document.document(p) + Doc.text(": ") + Document[TypeRef].document(t)
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
          case StructKind.Tuple => Doc.empty
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
            val kvargs = Doc.intercalate(Doc.text(", "),
              fields.toList.zip(args)
                .map {
                  case (StructKind.Style.FieldKind.Explicit(n), adoc) =>
                    identDoc.document(n) + cspace + adoc
                  case (StructKind.Style.FieldKind.Implicit(_), adoc) => adoc
                })
            prefix +
              Doc.text(" {") +
              kvargs +
              suffix +
              Doc.text(" }")
        }
      case Union(head, rest) =>
        def doc(p: Parsed): Doc =
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
    args: NonEmptyList[Either[Bindable, (Bindable, Parsed)]])(
      fn: (Constructor, StructKind.Style) => N): PositionalStruct[StructKind, TypeRef] = {
    val fields = args.map {
      case Left(b) => StructKind.Style.FieldKind.Implicit(b)
      case Right((b, _)) => StructKind.Style.FieldKind.Explicit(b)
    }
    val structArgs = args.toList.map {
      case Left(b) => Pattern.Var(b)
      case Right((_, p)) => p
    }
    PositionalStruct(
      fn(name, StructKind.Style.RecordLike(fields)),
      structArgs)
  }

  def compiledDocument[A: Document]: Document[Pattern[(PackageName, Constructor), A]] = {
    lazy val doc: Document[Pattern[(PackageName, Constructor), A]] = compiledDocument[A]
    Document.instance[Pattern[(PackageName, Constructor), A]] {
      case WildCard => Doc.char('_')
      case Literal(lit) => Document[Lit].document(lit)
      case Var(n) => Document[Identifier].document(n)
      case Named(n, u@Union(_, _)) =>
        // union is also an operator, so we need to use parens to explicitly bind | more tightly
        // than the @ on the left.
        Document[Identifier].document(n) + Doc.char('@') + Doc.char('(') + doc.document(u) + Doc.char(')')
      case Named(n, p) =>
        Document[Identifier].document(n) + Doc.char('@') + doc.document(p)
      case ListPat(items) =>
        Doc.char('[') + Doc.intercalate(Doc.text(", "),
          items.map {
            case ListPart.WildList => Doc.text("*_")
            case ListPart.NamedList(glob) => Doc.char('*') + Document[Identifier].document(glob)
            case ListPart.Item(p) => doc.document(p)
          }) + Doc.char(']')
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
      case ps@PositionalStruct((_, c), a) =>
        def untuple(p: Pattern[(PackageName, Constructor), A]): Option[List[Doc]] =
          p match {
            case PositionalStruct((PackageName.PredefName, Constructor("Unit")), Nil) =>
              Some(Nil)
            case PositionalStruct((PackageName.PredefName, Constructor("TupleCons")), a :: b :: Nil) =>
              untuple(b).map { l => doc.document(a) :: l }
            case _ => None
          }
        def tup(ds: List[Doc]): Doc =
          Doc.char('(') +
            Doc.intercalate(Doc.text(", "), ds) +
            Doc.char(')')

        untuple(ps) match {
          case Some(tupDocs) => tup(tupDocs)
          case None =>
            val args = a match {
              case Nil => Doc.empty
              case notEmpty => tup(a.map(doc.document(_)))
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

  /**
   * For fully typed patterns, compute the type environment of the bindings
   * from this pattern. This will sys.error if you pass a bad pattern, which
   * you should never do (and this code will never do unless there is some
   * broken invariant)
   */
  def envOf[C, K, T](p: Pattern[C, T], env: Map[K, T])(kfn: Identifier => K): Map[K, T] = {
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
        case WildCard => env
        case Literal(lit) => env
        case Var(n) => update(env, n, typeOf)
        case Named(n, p1) =>
          val e1 = loop(p1, typeOf, env)
          update(e1, n, typeOf)
        case ListPat(items) =>
          type L = ListPart[Pattern[C, T]]
          items.foldLeft(env) {
            case (env, ListPart.WildList) => env
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
          as.foldLeft(env) { (env, p) => loop(p, None, env) }
        case Union(head, rest) =>
          (head :: rest).foldLeft(env) { (env, p) => loop(p, None, env) }
      }

    loop(p, None, env)
  }

  private[this] val pwild = P("_").map(_ => WildCard)
  private[this] val plit = Lit.parser.map(Literal(_))

  /**
   * This does not allow a top-level type annotation which would be ambiguous
   * with : used for ending the match case block
   */
  val matchParser: P[Parsed] =
    P(matchOrNot(isMatch = true))

  /**
   * A Pattern in a match position allows top level un-parenthesized type annotation
   */
  val bindParser: P[Parsed] =
    P(matchOrNot(isMatch = false))

  private val maybePartial: P[(Constructor, StructKind.Style) => StructKind.NamedKind] = {
    val partial = P(maybeSpace ~ "...").map { _ =>
      { (n: Constructor, s: StructKind.Style) => StructKind.NamedPartial(n, s) }
    }
    val notPartial = PassWith(
      { (n: Constructor, s: StructKind.Style) => StructKind.Named(n, s) }
    )

    partial | notPartial
  }

  private def parseRecordStruct(recurse: P[Parsed]): P[Constructor => PositionalStruct[StructKind, TypeRef]] = {
    // We do maybeSpace, then { } then either a Bindable or Bindable: Pattern
    // maybe followed by ...
    val item: P[Either[Bindable, (Bindable, Parsed)]] =
      (Identifier.bindableParser ~ P(maybeSpace ~ ":" ~ maybeSpace ~ recurse).?)
        .map {
          case (b, None) => Left(b)
          case (b, Some(pat)) => Right((b, pat))
        }

    val items = item.nonEmptyList ~ maybePartial
    (maybeSpace ~ P("{") ~ maybeSpace ~ items ~ maybeSpace ~ P("}"))
      .map { case (args, fn) =>
        { (c: Constructor) => recordPat(c, args)(fn) }
      }
  }

  private def parseTupleStruct(recurse: P[Parsed]): P[Constructor => PositionalStruct[StructKind, TypeRef]] = {
    // There are three cases:
    // Foo(1 or more patterns)
    // Foo(1 or more patterns, ...)
    // Foo(...)

    val oneOrMore = P(recurse.listN(1) ~ maybePartial)
    val onlyPartial = P("...").map { _ =>
      (Nil, { (n: Constructor, s: StructKind.Style) => StructKind.NamedPartial(n, s) })
    }

    (oneOrMore | onlyPartial)
      .parens
      .map { case (args, fn) =>
        { (n: Constructor) => PositionalStruct(fn(n, StructKind.Style.TupleLike), args) }
      }
  }

  private def matchOrNot(isMatch: Boolean): P[Parsed] = {
    val recurse = P(bindParser)

    val positional =
      P(Identifier.consParser ~ (parseTupleStruct(recurse) | parseRecordStruct(recurse)).?)
        .map {
          case (n, None) =>
            PositionalStruct(StructKind.Named(n, StructKind.Style.TupleLike), Nil)
          case (n, Some(fn)) => fn(n)
        }

    val tupleOrParens = recurse.tupleOrParens.map {
      case Left(parens) => parens
      case Right(tup) => PositionalStruct(StructKind.Tuple, tup)
    }

    val listItem: P[ListPart[Parsed]] = {
      val maybeNamed: P[ListPart[Parsed]] =
        P("_").map(_ => ListPart.WildList) |
          Identifier.bindableParser.map(ListPart.NamedList(_))

      P("*" ~ maybeNamed) | recurse.map(ListPart.Item(_))
    }

    val listP = listItem.listSyntax.map(ListPat(_))

    lazy val named: P[Parsed] =
      P(maybeSpace ~ "@" ~ maybeSpace ~ nonAnnotated)

    lazy val pvarOrName = (Identifier.bindableParser ~ named.?)
      .map {
        case (n, None) => Var(n)
        case (n, Some(p)) => Named(n, p)
      }
    lazy val nonAnnotated = plit | pwild | tupleOrParens | positional | listP | pvarOrName
    // A union can't have an annotation, we need to be inside a parens for that
    val unionOp: P[Parsed => Parsed] = {
      val unionRest = nonAnnotated
        .nonEmptyListOfWsSep(maybeSpace, P("|"), allowTrailing = false, 1)
      ("|" ~ maybeSpace ~ unionRest)
        .map { ne =>
          { pat: Parsed => Union(pat, ne) }
        }
    }
    val typeAnnotOp: P[Parsed => Parsed] = {
      P(":" ~ maybeSpace ~ TypeRef.parser)
        .map { tpe =>
          { pat: Parsed => Annotation(pat, tpe) }
        }
    }

    def maybeOp(opP: P[Parsed => Parsed]): P[Parsed] =
      (nonAnnotated ~ (maybeSpace ~ opP).?)
        .map {
          case (p, None) => p
          case (p, Some(op)) => op(p)
        }

    // We only allow type annotation not at the top level, must be inside
    // Struct or parens
    if (isMatch) maybeOp(unionOp)
    else maybeOp(unionOp | typeAnnotOp)
  }
}

