package org.bykn.bosatsu

import Parser.{ Combinators, Indy, maybeSpace, spaces, toEOL }
import cats.data.NonEmptyList
import cats.implicits._
import com.stripe.dagon.Memoize
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }
import scala.collection.immutable.SortedSet

import Indy.IndyMethods
import org.bykn.fastparse_cats.StringInstances._

import ListLang.{KVPair, SpliceOrItem}

import Identifier.{Bindable, Constructor}

/**
 * Represents the syntactic version of Expr
 */
sealed abstract class Declaration {
  import Declaration._

  def region: Region

  def toDoc: Doc =
    this match {
      case Apply(fn, args, kind) =>
        val fnDoc = fn match {
          case Var(n) => Identifier.document.document(n)
          case p@Parens(_) => p.toDoc
          case other => Doc.char('(') + other.toDoc + Doc.char(')')
        }

        val (prefix, body) =
          kind match {
            case ApplyKind.Parens =>
              (fnDoc, args.toList)
            case ApplyKind.Dot =>
              (args.head.toDoc + Doc.char('.') + fnDoc, args.tail)
          }

        body match {
          case Nil => prefix
          case notEmpty =>
            prefix + Doc.char('(') + Doc.intercalate(Doc.text(", "), notEmpty.map(_.toDoc)) + Doc.char(')')
        }
      case ApplyOp(left, Identifier.Operator(opStr), right) =>
        left.toDoc space Doc.text(opStr) space right.toDoc
      case Binding(b) =>
        val d0 = Document[Padding[Declaration]]
        val withNewLine = Document.instance[Padding[Declaration]] { pd =>
           Doc.line + d0.document(pd)
        }
        BindingStatement.document(Document[Pattern.Parsed], withNewLine).document(b)
      case Comment(c) =>
        CommentStatement.document[Padding[Declaration]].document(c)
      case DefFn(d) =>
        implicit val pairDoc: Document[(OptIndent[Declaration], Padding[Declaration])] =
          Document.instance {
            case (fnBody, letBody) =>
                fnBody.sepDoc +
                Document[OptIndent[Declaration]].document(fnBody) +
                Doc.line +
                Document[Padding[Declaration]].document(letBody)
          }
        DefStatement.document[Pattern.Parsed, (OptIndent[Declaration], Padding[Declaration])].document(d)
      case IfElse(ifCases, elseCase) =>
        // TODO, we could make this ternary if it is small enough
        def checkBody(cb: (Declaration, OptIndent[Declaration])): Doc = {
          val (check, optbody) = cb
          val sep = optbody.sepDoc
          val rest = sep + Document[OptIndent[Declaration]].document(optbody)
          check.toDoc + Doc.char(':') + rest
        }

        val elseDoc = elseCase.sepDoc + Document[OptIndent[Declaration]].document(elseCase)
        val tail = Doc.text("else:") + elseDoc :: Nil
        val parts = (Doc.text("if ") + checkBody(ifCases.head)) :: (ifCases.tail.map(Doc.text("elif ") + checkBody(_))) ::: tail
        Doc.intercalate(Doc.line, parts)
      case Lambda(args, body) =>
        Doc.char('\\') + Doc.intercalate(Doc.text(", "), args.toList.map(Document[Pattern.Parsed].document(_))) + Doc.text(" -> ") + body.toDoc
      case Literal(lit) => Document[Lit].document(lit)
      case Match(kind, typeName, args) =>
        val pid = Document[OptIndent[Declaration]]

        implicit val patDoc: Document[(Pattern.Parsed, OptIndent[Declaration])] =
          Document.instance[(Pattern.Parsed, OptIndent[Declaration])] {
            case (pat, decl) =>
              Document[Pattern.Parsed].document(pat) + Doc.text(":") + decl.sepDoc + pid.document(decl)
          }
        implicit def linesDoc[T: Document]: Document[NonEmptyList[T]] =
          Document.instance { ts => Doc.intercalate(Doc.line, ts.toList.map(Document[T].document _)) }

        val piPat = Document[OptIndent[NonEmptyList[(Pattern.Parsed, OptIndent[Declaration])]]]
        val kindDoc = kind match {
          case RecursionKind.NonRecursive => Doc.text("match ")
          case RecursionKind.Recursive => Doc.text("recur ")
        }
        // TODO this isn't quite right
        kindDoc + typeName.toDoc + Doc.char(':') + args.sepDoc +
          piPat.document(args)
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case TupleCons(h :: Nil) =>
        // we need a trailing comma here:
        Doc.char('(') + h.toDoc + Doc.char(',') + Doc.char(')')
      case TupleCons(items) =>
        Doc.char('(') + Doc.intercalate(Doc.text(", "),
          items.map(_.toDoc)) + Doc.char(')')
      case Var(name) => Document[Identifier].document(name)

      case ListDecl(list) =>
        ListLang.document[Declaration, Pattern.Parsed].document(list)

      case DictDecl(dict) =>
        ListLang.documentDict[Declaration, Pattern.Parsed].document(dict)

      case RecordConstructor(name, args) =>
        val idDoc = Identifier.document[Identifier]
        val colonSpace = Doc.text(": ")
        def pair(kv: (Bindable, Declaration)): Doc =
          idDoc.document(kv._1) + colonSpace + kv._2.toDoc

        val argDoc = Doc.char('{') +
          Doc.intercalate(Doc.char(',') + Doc.line,
          args.toList.map(pair)).grouped.nested(2) + Doc.char('}')

        idDoc.document(name) + Doc.space + argDoc
    }

  /**
   * Get the set of free variables in this declaration.
   * These are variables that must be defined at an outer
   * lexical scope in order to typecheck
   */
  def freeVars: SortedSet[Bindable] = {
    def loop(decl: Declaration, bound: Set[Bindable], acc: SortedSet[Bindable]): SortedSet[Bindable] =
      decl match {
        case Apply(fn, args, _) =>
          (fn :: args).foldLeft(acc) { (acc0, d) => loop(d, bound, acc0) }
        case ApplyOp(left, _, right) =>
          val acc0 = loop(left, bound, acc)
          loop(right, bound, acc0)
        case Binding(BindingStatement(n, v, in)) =>
          val acc0 = loop(v, bound, acc)
          val bound1 = bound ++ n.names
          loop(in.padded, bound1, acc0)
        case Comment(c) => loop(c.on.padded, bound, acc)
        case DefFn(d) =>
          // def sets up a binding to itself, which
          // may or may not be recursive
          val bound1 = bound + d.name
          val bound2 = bound1 ++ d.args.flatMap(_.names)
          val (body, rest) = d.result
          val acc1 = loop(body.get, bound2, acc)
          loop(rest.padded, bound1, acc1)
        case IfElse(ifCases, elseCase) =>
          val acc2 = ifCases.foldLeft(acc) { case (acc0, (cond, v)) =>
            val acc1 = loop(cond, bound, acc0)
            loop(v.get, bound, acc1)
          }
          loop(elseCase.get, bound, acc2)
        case Lambda(args, body) =>
          val bound1 = bound ++ args.toList.flatMap(_.names)
          loop(body, bound1, acc)
        case Literal(lit) => SortedSet.empty
        case Match(_, typeName, args) =>
          val acc1 = loop(typeName, bound, acc)
          args.get.foldLeft(acc1) { case (acc0, (pat, res)) =>
            val bound1 = bound ++ pat.names
            loop(res.get, bound1, acc0)
          }
        case Parens(p) => loop(p, bound, acc)
        case TupleCons(items) =>
          items.foldLeft(acc) { (acc0, d) => loop(d, bound, acc0) }
        case Var(name: Bindable) if !bound(name) => acc + name
        case Var(_) => acc
        case ListDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, sori) =>
            loop(sori.value, bound, acc0)
          }
        case ListDecl(ListLang.Comprehension(ex, b, in, _)) =>
          val acc1 = loop(in, bound, acc)
          val bound1 = bound ++ b.names
          loop(ex.value, bound1, acc1)
        case DictDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, kv) =>
            val acc1 = loop(kv.key, bound, acc0)
            loop(kv.value, bound, acc1)
          }
        case DictDecl(ListLang.Comprehension(ex, b, in, _)) =>
          val acc1 = loop(in, bound, acc)
          val bound1 = bound ++ b.names
          val acc2 = loop(ex.key, bound1, acc1)
          loop(ex.value, bound1, acc2)
      }

    loop(this, Set.empty, SortedSet.empty)
  }

  /**
   * This returns *all* names in the declaration, bound or not
   */
  def allNames: SortedSet[Bindable] = {
    def loop(decl: Declaration, acc: SortedSet[Bindable]): SortedSet[Bindable] =
      decl match {
        case Apply(fn, args, _) =>
          (fn :: args).foldLeft(acc) { (acc0, d) => loop(d, acc0) }
        case ApplyOp(left, _, right) =>
          val acc0 = loop(left, acc)
          loop(right, acc0)
        case Binding(BindingStatement(n, v, in)) =>
          val acc0 = loop(v, acc ++ n.names)
          loop(in.padded, acc0)
        case Comment(c) => loop(c.on.padded, acc)
        case DefFn(d) =>
          // def sets up a binding to itself, which
          // may or may not be recursive
          val acc1 = (acc + d.name) ++ d.args.flatMap(_.names)
          val (body, rest) = d.result
          val acc2 = loop(body.get, acc1)
          loop(rest.padded, acc2)
        case IfElse(ifCases, elseCase) =>
          val acc2 = ifCases.foldLeft(acc) { case (acc0, (cond, v)) =>
            val acc1 = loop(cond, acc0)
            loop(v.get, acc1)
          }
          loop(elseCase.get, acc2)
        case Lambda(args, body) =>
          val acc1 = acc ++ args.toList.flatMap(_.names)
          loop(body, acc1)
        case Literal(lit) => SortedSet.empty
        case Match(_, typeName, args) =>
          val acc1 = loop(typeName, acc)
          args.get.foldLeft(acc1) { case (acc0, (pat, res)) =>
            loop(res.get, acc0 ++ pat.names)
          }
        case Parens(p) => loop(p, acc)
        case TupleCons(items) =>
          items.foldLeft(acc) { (acc0, d) => loop(d, acc0) }
        case Var(name: Bindable) => acc + name
        case Var(_) => acc
        case ListDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, sori) =>
            loop(sori.value, acc0)
          }
        case ListDecl(ListLang.Comprehension(ex, b, in, _)) =>
          val acc1 = loop(in, acc)
          loop(ex.value, acc1 ++ b.names)
        case DictDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, kv) =>
            val acc1 = loop(kv.key, acc0)
            loop(kv.value, acc1)
          }
        case DictDecl(ListLang.Comprehension(ex, b, in, _)) =>
          val acc1 = loop(in, acc)
          val acc2 = loop(ex.key, acc1 ++ b.names)
          loop(ex.value, acc2)
        case RecordConstructor(_, args) =>
          args.foldLeft(acc) { case (acc, (_, arg)) =>
            loop(arg, acc)
          }
      }
    loop(this, SortedSet.empty)
  }
}

object Declaration {
  implicit val document: Document[Declaration] = Document.instance[Declaration](_.toDoc)
  implicit val hasRegion: HasRegion[Declaration] =
    HasRegion.instance[Declaration](_.region)

  sealed abstract class ApplyKind
  object ApplyKind {
    case object Dot extends ApplyKind
    case object Parens extends ApplyKind
  }

  sealed abstract class RecordArg
  object RecordArg {
    final case class Pair(field: Bindable, arg: Declaration) extends RecordArg
    // for cases like:
    // age = 47
    // Person { name: "Frank", age }
    final case class Simple(field: Bindable) extends RecordArg
  }
  //
  // We use the pattern of an implicit region for two reasons:
  // 1. we don't want the region to play a role in pattern matching or equality, since it is about
  //    error reporting, and if we include it in equality it massively complicates tests.
  // 2. we want to be able to construct these for tests dummy values, so we can set up an implicit
  //    value in tests and construct them.
  // These reasons are a bit abusive, and we may revisit this in the future
  //

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration], kind: ApplyKind)(implicit val region: Region) extends Declaration
  case class ApplyOp(left: Declaration, op: Identifier.Operator, right: Declaration) extends Declaration {
    val region = left.region + right.region
    def opVar: Var = Var(op)(Region(left.region.end, right.region.start))
  }
  case class Binding(binding: BindingStatement[Pattern.Parsed, Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class Comment(comment: CommentStatement[Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class DefFn(deffn: DefStatement[Pattern.Parsed, (OptIndent[Declaration], Padding[Declaration])])(implicit val region: Region) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(Declaration, OptIndent[Declaration])],
    elseCase: OptIndent[Declaration])(implicit val region: Region) extends Declaration
  case class Lambda(args: NonEmptyList[Pattern.Parsed], body: Declaration)(implicit val region: Region) extends Declaration
  case class Literal(lit: Lit)(implicit val region: Region) extends Declaration
  case class Match(
    kind: RecursionKind,
    arg: Declaration,
    cases: OptIndent[NonEmptyList[(Pattern.Parsed, OptIndent[Declaration])]])(
    implicit val region: Region) extends Declaration
  case class Parens(of: Declaration)(implicit val region: Region) extends Declaration
  case class TupleCons(items: List[Declaration])(implicit val region: Region) extends Declaration
  case class Var(name: Identifier)(implicit val region: Region) extends Declaration

  /**
   * This represents code like:
   * Foo { bar: 12 }
   */
  case class RecordConstructor(cons: Constructor, arg: NonEmptyList[(Bindable, Declaration)])(implicit val region: Region) extends Declaration
  /**
   * This represents the list construction language
   */
  case class ListDecl(list: ListLang[SpliceOrItem, Declaration, Pattern.Parsed])(implicit val region: Region) extends Declaration
  /**
   * Here are dict constructors and comprehensions
   */
  case class DictDecl(list: ListLang[KVPair, Declaration, Pattern.Parsed])(implicit val region: Region) extends Declaration

  val matchKindParser: P[RecursionKind] =
    (P("match").map(_ => RecursionKind.NonRecursive) | P("recur").map(_ => RecursionKind.Recursive))

  /**
   * A pattern can also be a declaration in some cases
   *
   * TODO, patterns don't parse with regions, so we lose track of precise position information
   * if we want to point to an inner portion of it
   */
  def toPattern(d: Declaration): Option[Pattern.Parsed] =
    d match {
      case Var(nm@Identifier.Constructor(_)) =>
        Some(Pattern.PositionalStruct(Some(nm), Nil))
      case Var(v: Bindable) => Some(Pattern.Var(v))
      case Literal(lit) => Some(Pattern.Literal(lit))
      case ListDecl(ListLang.Cons(elems)) =>
        val optParts: Option[List[Either[Option[Bindable], Pattern.Parsed]]] =
          elems.traverse {
            case SpliceOrItem.Splice(Var(bn: Bindable)) =>
              Some(Left(Some(bn)))
            case SpliceOrItem.Item(p) =>
              toPattern(p).map(Right(_))
            case _ => None
          }
        optParts.map(Pattern.ListPat(_))
      case ApplyOp(left, Identifier.Operator("|"), right) =>
        // this could be a pattern
        (toPattern(left), toPattern(right)).mapN { (l, r) =>
          Pattern.union(l, r :: Nil)
        }
      case Apply(Var(nm@Identifier.Constructor(_)), args, ApplyKind.Parens) =>
        args.traverse(toPattern(_)).map { argPats =>
          Pattern.PositionalStruct(Some(nm), argPats.toList)
        }
      case TupleCons(ps) =>
        ps.traverse(toPattern(_)).map { argPats =>
          Pattern.PositionalStruct(None, argPats.toList)
        }
      case Parens(p) => toPattern(p)
      case _ => None
    }

  private val restP: Indy[Padding[Declaration]] =
    (Indy.parseIndent *> parser).mapF(Padding.parser(_))

  // This is something we check after variables
  private val bindingOp: Indy[(Pattern.Parsed, Region) => Binding] = {
    BindingStatement.bindingParser[Pattern.Parsed, Padding[Declaration]](parser <* Indy.lift(toEOL), restP)
      .region
      .map { case (region, fn) =>
        { (pat: Pattern.Parsed, r: Region) => Binding(fn(pat))(r + region) }
      }
  }

  val commentP: Parser.Indy[Comment] =
    CommentStatement.parser(Parser.Indy { indent => Padding.parser(P(indent ~ parser(indent)))})
      .region
      .map { case (r, c) => Comment(c)(r) }

  val defP: Indy[DefFn] = {
    val restParser: Indy[(OptIndent[Declaration], Padding[Declaration])] =
      OptIndent.indy(parser).product(Indy.lift(toEOL) *> restP)

    restParser.mapF { rp =>
      DefStatement.parser(Pattern.bindParser, maybeSpace ~ rp)
        .region
        .map { case (r, d) => DefFn(d)(r) }
    }
  }

  def ifElseP(expr: Indy[Declaration]): Indy[IfElse] = {

    def ifelif(str: String): Indy[(Declaration, OptIndent[Declaration])] =
      Indy.block(Indy.lift(P(str ~ spaces ~ maybeSpace)) *> expr, expr)

    /*
     * We don't need to parse the else term to the end,
     * EOL is a kind of a separator we only have in between
     */
    val elseTerm: Indy[OptIndent[Declaration]] =
      Indy.block(Indy.lift(P("else" ~ maybeSpace)), expr).map(_._2)

    val elifs1 = ifelif("elif").rep(1, sepIndy = Indy.toEOLIndent) <* Indy.toEOLIndent

    (ifelif("if") <* Indy.toEOLIndent)
      .product(elifs1.?)
      .product(elseTerm)
      .region
      .map {
        case (region, ((ifcase, optElses), elseBody)) =>
          val elses =
            optElses match {
              case None => Nil
              case Some(s) => s.toList // type inference works better than fold sadly
            }
          IfElse(NonEmptyList(ifcase, elses), elseBody)(region)
      }
  }

  val lambdaP: Indy[Lambda] = {
    val params = Indy.lift(P("\\" ~/ maybeSpace ~ Pattern.bindParser.nonEmptyList))

    Indy.blockLike(params, parser, P(maybeSpace ~ "->"))
      .region
      .map { case (r, (args, body)) => Lambda(args, body.get)(r) }
  }

  def matchP(expr: Indy[Declaration]): Indy[Match] = {
    val withTrailing = expr <* Indy.lift(maybeSpace)
    val branch = Indy.block(Indy.lift(Pattern.matchParser), withTrailing)

    Indy.block(Indy.lift(P(matchKindParser ~ spaces)).product(withTrailing), branch.nonEmptyList(Indy.toEOLIndent))
      .region
      .map { case (r, ((kind, exp), branches)) =>
        Match(kind, exp, branches)(r)
      }
  }

  val varP: P[Var] =
    Identifier.bindableParser.region.map { case (r, i) => Var(i)(r) }

  val constructorP: P[Var] =
    Identifier.consParser.region.map { case (r, i) => Var(i)(r) }

  private val patternBind: Indy[Declaration] =
    Indy.lift(Pattern.bindParser.region).product(bindingOp)
      .map {
        case ((reg, pat), fn) => fn(pat, reg)
      }

  private def decOrBind(p: P[Declaration], indent: String): P[Declaration] =
    (p ~ bindingOp(indent).?)
      .flatMap {
        case (d, None) => PassWith(d)
        case (d, Some(op)) =>
          toPattern(d) match {
            case None => Fail
            case Some(dpat) =>
              PassWith(op(dpat, d.region))
          }
      }

  private def listP(p: P[Declaration]): P[ListDecl] =
    ListLang.parser(p, Pattern.bindParser)
      .region
      .map { case (r, l) => ListDecl(l)(r) }

  private def dictP(p: P[Declaration]): P[DictDecl] =
    ListLang.dictParser(p, Pattern.bindParser)
      .region
      .map { case (r, l) => DictDecl(l)(r) }

  private[this] val parserCache: String => P[Declaration] =
    Memoize.function[String, P[Declaration]] { (indent, rec) =>

      val recurse = P(rec(indent)) // needs to be inside a P for laziness

      val recIndy = Indy(rec)

      val lits = Lit.parser.region.map { case (r, l) => Literal(l)(r) }

      val tupOrPar =
        recurse
          .tupleOrParens
          .region
          .map {
            case (r, Left(p)) => Parens(p)(r)
            case (r, Right(tup)) => TupleCons(tup)(r)
          }

      /*
       * Note pattern bind needs to be before anything that looks like a pattern that can't handle
       * bind
       */
      val prefix = P(
        // these have keywords which need to be parsed before var (def, match, if)
        defP(indent) |
        lambdaP(indent) |
        matchP(recIndy)(indent) |
        ifElseP(recIndy)(indent) |
        dictP(recurse) |
        // vars are so common, try to parse them before the generic pattern
        decOrBind(varP | listP(recurse), indent) |
        patternBind(indent) |
        lits | // technically this can be a pattern: 3 = x, so it has to be after patternBind, but it is never total.
        constructorP |
        commentP(indent) |
        tupOrPar)

      def repFn[A](fn: P[A => A]): P[A => A] = {
        @annotation.tailrec
        def loop(a: A, fns: List[A => A]): A =
          fns match {
            case Nil => a
            case h :: tail => loop(h(a), tail)
          }

        fn.rep().map { opList =>

          { (a: A) => loop(a, opList.toList) }
        }
      }

      def apP[A](arg: P[A], fn: P[A => A]): P[A] =
        (arg ~ fn).map { case (a, f) => f(a) }

      def maybeAp[A](arg: P[A], fn: P[A => A]): P[A] =
        (arg ~ fn.?)
          .map {
            case (a, None) => a
            case (a, Some(f)) => f(a)
          }

      val applied: P[Declaration] = {
        val params = recurse.parensLines1
        // here we are using . syntax foo.bar(1, 2)
        val dotApply: P[Declaration => Declaration] =
          P("." ~ varP ~ params.?).region.map { case (r2, (fn, argsOpt)) =>
            val args = argsOpt.fold(List.empty[Declaration])(_.toList)

            { head: Declaration => Apply(fn, NonEmptyList(head, args), ApplyKind.Dot)(head.region + r2) }
          }.opaque(". apply operator")

        // here we directly call a function foo(1, 2)
        val applySuffix: P[Declaration => Declaration] = params.region.map { case (r, args) =>

          { fn: Declaration => Apply(fn, args, ApplyKind.Parens)(fn.region + r) }
        }.opaque("apply operator")

        apP(prefix, repFn(dotApply | applySuffix))
      }

      // Applying is higher precedence than any operators

      // now parse an operator apply
      val postOperators: P[Declaration] = {

        def convert(form: Operators.Formula[Declaration]): Declaration =
          form match {
            case Operators.Formula.Sym(r) => r
            case Operators.Formula.Op(left, op, right) =>
              val leftD = convert(left)
              val rightD = convert(right)
              // `op`(l, r)
              ApplyOp(leftD, Identifier.Operator(op), rightD)
          }

        // one or more operators
        val ops: P[Declaration => Operators.Formula[Declaration]] =
          Operators.Formula.infixOps1(applied)

        // This already parses as many as it can, so we don't need repFn
        val form = ops.map { fn =>

          { d: Declaration => convert(fn(d)) }
        }

        maybeAp(applied, form)
      }

      // here is if/ternary operator
      // it fully recurses on the else branch, which will parse any repeated ternaryies
      // so no need to repeat here for correct precedence
      val ternary: P[Declaration => Declaration] =
        P("if" ~ spaces ~ recurse ~ spaces ~ "else" ~ spaces ~ recurse)
          .region
          .map { case (region, (cond, falseCase)) =>
            { trueCase: Declaration =>
              val ifcase = NonEmptyList.of((cond, OptIndent.same(trueCase)))
              IfElse(ifcase, OptIndent.same(falseCase))(trueCase.region + region)
            }
          }.opaque("ternary operator")


      maybeAp(postOperators, (spaces ~ ternary))
        .opaque(s"Declaration.parser($indent)")
    }

  lazy val parser: Indy[Declaration] =
    Indy.suspend(Indy(parserCache))
}
