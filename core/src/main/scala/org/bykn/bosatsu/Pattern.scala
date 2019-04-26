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
    this match {
      case Pattern.WildCard => Pattern.WildCard
      case Pattern.Literal(lit) => Pattern.Literal(lit)
      case Pattern.Var(v) => Pattern.Var(v)
      case Pattern.Named(n, p) => Pattern.Named(n, p.mapType(fn))
      case Pattern.ListPat(items) =>
        Pattern.ListPat(items.map(_.right.map(_.mapType(fn))))
      case Pattern.Annotation(p, tpe) => Pattern.Annotation(p.mapType(fn), fn(tpe))
      case Pattern.PositionalStruct(name, params) =>
        Pattern.PositionalStruct(name, params.map(_.mapType(fn)))
      case Pattern.Union(h, t) => Pattern.Union(h.mapType(fn), t.map(_.mapType(fn)))
    }

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
          val globs = items.collect { case Left(Some(glob)) => glob }.filterNot(seen)
          val next = items.collect { case Right(inner) => inner }
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
        case (Pattern.ListPat(Left(Some(_)) :: Nil), true) :: tail =>
            // this is a total match at the top level, not a substructue
            loop(tail, seen, acc)
        case (Pattern.ListPat(items), _) :: tail =>
          val globs = items.collect { case Left(Some(glob)) => glob }.filterNot(seen)
          val next = items.collect { case Right(inner) => (inner, false) }
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
          case Left(opt) => Left(opt.filter(keep))
          case Right(p) => Right(p.filterVars(keep))
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

  type Parsed = Pattern[Option[Constructor], TypeRef]

  /**
   * Create a normalized pattern, which doesn't have nested top level unions
   */
  def union[N, T](head: Pattern[N, T], tail: List[Pattern[N, T]]): Pattern[N, T] = {
    def flatten(p: Pattern[N, T]): NonEmptyList[Pattern[N, T]] =
      p match {
        case Union(h, t) => NonEmptyList(h, t.toList).flatMap(flatten(_))
        case nonU => NonEmptyList(nonU, Nil)
      }

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
            case Left(v) => Applicative[F].pure(Left(v): Either[Option[Bindable], Pattern[N, T1]])
            case Right(p) => p.traverseType(fn).map(Right(_): Either[Option[Bindable], Pattern[N, T1]])
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
      pat match {
        case Pattern.WildCard => Pattern.WildCard
        case Pattern.Literal(lit) => Pattern.Literal(lit)
        case Pattern.Var(v) => Pattern.Var(v)
        case Pattern.Named(v, p) => Pattern.Named(v, p.mapStruct(parts))
        case Pattern.ListPat(items) =>
          val items1 = items.map {
            case Left(v) =>
              Left(v): Either[Option[Bindable], Pattern[N1, T]]
            case Right(p) =>
              val p1 = p.mapStruct(parts)
              Right(p1): Either[Option[Bindable], Pattern[N1, T]]
          }
          Pattern.ListPat(items1)
        case Pattern.Annotation(p, tpe) =>
          Pattern.Annotation(p.mapStruct(parts), tpe)
        case Pattern.PositionalStruct(name, params) =>
          val p1 = params.map(_.mapStruct(parts))
          parts(name, p1)
        case Pattern.Union(h, tail) =>
          Pattern.Union(h.mapStruct(parts), tail.map(_.mapStruct(parts)))
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
  case class ListPat[N, T](parts: List[Either[Option[Bindable], Pattern[N, T]]]) extends Pattern[N, T]
  case class Annotation[N, T](pattern: Pattern[N, T], tpe: T) extends Pattern[N, T]
  case class PositionalStruct[N, T](name: N, params: List[Pattern[N, T]]) extends Pattern[N, T]
  case class Union[N, T](head: Pattern[N, T], rest: NonEmptyList[Pattern[N, T]]) extends Pattern[N, T]

  implicit def patternOrdering[N: Ordering, T: Ordering]: Ordering[Pattern[N, T]] =
    new Ordering[Pattern[N, T]] {
      val ordN = implicitly[Ordering[N]]
      val ordT = implicitly[Ordering[T]]
      val list = ListOrdering.onType(this)
      def eOrd[A, B](ordA: Ordering[A], ordB: Ordering[B]): Ordering[Either[A, B]] =
        new Ordering[Either[A, B]] {
          def compare(a: Either[A, B], b: Either[A, B]) =
            (a, b) match {
              case (Left(_), Right(_)) => -1
              case (Right(_), Left(_)) => 1
              case (Left(a), Left(b)) => ordA.compare(a, b)
              case (Right(a), Right(b)) => ordB.compare(a, b)
            }
        }

      val listE = ListOrdering.onType(eOrd[Option[Bindable], Pattern[N, T]](implicitly, this))

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

  implicit lazy val document: Document[Pattern[Option[Constructor], TypeRef]] =
    Document.instance[Pattern[Option[Constructor], TypeRef]] {
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
            case Left(None) => Doc.text("*_")
            case Left(Some(glob)) => Doc.char('*') + Document[Identifier].document(glob)
            case Right(p) => document.document(p)
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
          case None => Doc.text("()")
          case Some(nm) => Document[Identifier].document(nm)
        }
      case PositionalStruct(None, h :: Nil) =>
        // single item tuples need a comma:
        Doc.char('(') + document.document(h) + Doc.text(",)")
      case PositionalStruct(n, nonEmpty) =>
        val prefix = n match {
          case None => Doc.empty
          case Some(n) => Document[Identifier].document(n)
        }
        prefix +
          Doc.char('(') + Doc.intercalate(Doc.text(", "), nonEmpty.map(document.document(_))) + Doc.char(')')
      case Union(head, rest) =>
        def doc(p: Pattern[Option[Constructor], TypeRef]): Doc =
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
            case Left(None) => Doc.text("*_")
            case Left(Some(glob)) => Doc.char('*') + Document[Identifier].document(glob)
            case Right(p) => doc.document(p)
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
            case PositionalStruct((Predef.Name, Constructor("Unit")), Nil) =>
              Some(Nil)
            case PositionalStruct((Predef.Name, Constructor("TupleCons")), a :: b :: Nil) =>
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
            Doc.text(c.asString) + tup(a.map(doc.document(_)))
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

  private def matchOrNot(isMatch: Boolean): P[Parsed] = {
    lazy val recurse = bindParser

    val positional = P(Identifier.consParser ~ (recurse.listN(1).parens).?)
      .map {
        case (n, None) => PositionalStruct(Some(n), Nil)
        case (n, Some(ls)) => PositionalStruct(Some(n), ls)
      }

    val tupleOrParens = recurse.tupleOrParens.map {
      case Left(parens) => parens
      case Right(tup) => PositionalStruct(None, tup)
    }

    val listItem: P[Either[Option[Bindable], Pattern[Option[Constructor], TypeRef]]] = {
      val maybeNamed: P[Option[Bindable]] =
        P("_").map(_ => None) | Identifier.bindableParser.map(Some(_))

      P("*" ~ maybeNamed).map(Left(_)) | recurse.map(Right(_))
    }

    val listP = listItem.listSyntax.map(ListPat(_))

    lazy val named: P[Pattern[Option[Constructor], TypeRef]] =
      P(maybeSpace ~ "@" ~ maybeSpace ~ nonAnnotated)
    lazy val pvarOrName = (Identifier.bindableParser ~ named.?)
      .map {
        case (n, None) => Var(n)
        case (n, Some(p)) => Named(n, p)
      }
    lazy val nonAnnotated = plit | pwild | tupleOrParens | positional | listP | pvarOrName
    // A union can't have an annotation, we need to be inside a parens for that
    val unionOp: P[Pattern[Option[Constructor], TypeRef] => Pattern[Option[Constructor], TypeRef]] = {
      val unionRest = nonAnnotated
        .nonEmptyListOfWsSep(maybeSpace, P("|"), allowTrailing = false, 1)
      ("|" ~ maybeSpace ~ unionRest)
        .map { ne =>
          { pat: Pattern[Option[Constructor], TypeRef] => Union(pat, ne) }
        }
    }
    val typeAnnotOp: P[Pattern[Option[Constructor], TypeRef] => Pattern[Option[Constructor], TypeRef]] = {
      P(":" ~ maybeSpace ~ TypeRef.parser)
        .map { tpe =>
          { pat: Pattern[Option[Constructor], TypeRef] => Annotation(pat, tpe) }
        }
    }

    def maybeOp(opP: P[Pattern[Option[Constructor], TypeRef] => Pattern[Option[Constructor], TypeRef]]): P[Pattern[Option[Constructor], TypeRef]] =
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

