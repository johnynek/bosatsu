package org.bykn.bosatsu

import Parser.{ Combinators, Indy, maybeSpace, spaces, toEOL1, keySpace }
import cats.data.NonEmptyList
import org.bykn.bosatsu.graph.Memoize
import org.bykn.bosatsu.parser.{Parser => P, Parser1 => P1}
import org.typelevel.paiges.{ Doc, Document }
import scala.collection.immutable.SortedSet

import Indy.IndyMethods

import ListLang.{KVPair, SpliceOrItem}

import Identifier.{Bindable, Constructor}

import cats.implicits._
/**
 * Represents the syntactic version of Expr
 */
sealed abstract class Declaration {
  import Declaration._

  def region: Region

  def toDoc: Doc =
    this match {
      case Annotation(term, tpe) =>
        val tDoc = term match {
          case Var(_) | Parens(_) | Apply(_, _, _) => term.toDoc
          case _ => Doc.char('(') + term.toDoc + Doc.char(')')
        }
        tDoc + Doc.text(": ") + tpe.toDoc
      case Apply(fn, args, kind) =>
        val fnDoc = fn match {
          case Var(_) | Parens(_) | Apply(_, _, _) => fn.toDoc
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
        BindingStatement.document(Document[Pattern.Parsed], Document.instance[NonBinding](_.toDoc), withNewLine).document(b)
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
      case Ternary(trueCase, cond, falseCase) =>
        Doc.intercalate(Doc.space,
          trueCase.toDoc :: Doc.text("if") :: cond.toDoc :: Doc.text("else") :: falseCase.toDoc :: Nil)
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
      case m@Matches(arg, p) =>
        val da = arg match {
          // matches binds tighter than all these
          case Lambda(_, _) | IfElse(_, _) | ApplyOp(_, _, _) | Match(_, _, _) =>
            Parens(arg)(m.region).toDoc
          case _ =>
            arg.toDoc
        }
        da + Doc.text(" matches ") + Document[Pattern.Parsed].document(p)
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case TupleCons(h :: Nil) =>
        // we need a trailing comma here:
        Doc.char('(') + h.toDoc + Doc.char(',') + Doc.char(')')
      case TupleCons(items) =>
        Doc.char('(') + Doc.intercalate(Doc.text(", "),
          items.map(_.toDoc)) + Doc.char(')')
      case Var(name) =>
        Document[Identifier].document(name)

      case StringDecl(parts) =>
        val useDouble = parts.exists {
          case Right((_, str)) => str.contains('\'') && !str.contains('"')
          case Left(_) => false
        }
        val q = if (useDouble) '"' else '\''
        val inner = Doc.intercalate(Doc.empty,
          parts.toList.map {
            case Right((_, str)) => Doc.text(StringUtil.escape(q, str))
            case Left(decl) => Doc.text("${") + decl.toDoc + Doc.char('}')
          })
        Doc.char(q) + inner + Doc.char(q)

      case ListDecl(list) =>
        ListLang.document[NonBinding, Pattern.Parsed].document(list)

      case DictDecl(dict) =>
        ListLang.documentDict[NonBinding, Pattern.Parsed].document(dict)

      case RecordConstructor(name, args) =>
        val argDoc = Doc.char('{') +
          Doc.intercalate(Doc.char(',') + Doc.space,
          args.toList.map(_.toDoc)) + Doc.char('}')

        Declaration.identDoc.document(name) + Doc.space + argDoc
    }

  /**
   * Get the set of free variables in this declaration.
   * These are variables that must be defined at an outer
   * lexical scope in order to typecheck
   */
  def freeVars: SortedSet[Bindable] = {
    def loop(decl: Declaration, bound: Set[Bindable], acc: SortedSet[Bindable]): SortedSet[Bindable] =
      decl match {
        case Annotation(term, _) => loop(term, bound, acc)
        case Apply(fn, args, _) =>
          (fn :: args).foldLeft(acc) { (acc0, d) => loop(d, bound, acc0) }
        case ao@ApplyOp(left, _, right) =>
          val acc0 = loop(left, bound, acc)
          val acc1 = loop(ao.opVar, bound, acc0)
          loop(right, bound, acc1)
        case Binding(BindingStatement(n, v, in)) =>
          val acc0 = loop(v, bound, acc)
          val bound1 = bound ++ n.names
          loop(in.padded, bound1, acc0)
        case Comment(c) => loop(c.on.padded, bound, acc)
        case DefFn(d) =>
          val (body, rest) = d.result
          // def sets up a binding to itself, which
          // may or may not be recursive

          val boundRest = bound + d.name
          val boundBody = boundRest ++ d.args.flatMap(_.names)

          val acc1 = loop(body.get, boundBody, acc)
          loop(rest.padded, boundRest, acc1)
        case IfElse(ifCases, elseCase) =>
          val acc2 = ifCases.foldLeft(acc) { case (acc0, (cond, v)) =>
            val acc1 = loop(cond, bound, acc0)
            loop(v.get, bound, acc1)
          }
          loop(elseCase.get, bound, acc2)
        case Ternary(t, c, f) =>
          val acc1 = loop(t, bound, acc)
          val acc2 = loop(c, bound, acc1)
          loop(f, bound, acc2)
        case Lambda(args, body) =>
          val bound1 = bound ++ args.toList.flatMap(_.names)
          loop(body, bound1, acc)
        case Literal(lit) => acc
        case Match(_, typeName, args) =>
          val acc1 = loop(typeName, bound, acc)
          args.get.foldLeft(acc1) { case (acc0, (pat, res)) =>
            val bound1 = bound ++ pat.names
            loop(res.get, bound1, acc0)
          }
        case Matches(a, _) =>
          loop(a, bound, acc)
        case Parens(p) => loop(p, bound, acc)
        case TupleCons(items) =>
          items.foldLeft(acc) { (acc0, d) => loop(d, bound, acc0) }
        case Var(name: Bindable) if !bound(name) => acc + name
        case Var(_) => acc
        case StringDecl(items) =>
          items.foldLeft(acc) {
            case (acc, Left(nb)) => loop(nb, bound, acc)
            case (acc, _) => acc
          }
        case ListDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, sori) =>
            loop(sori.value, bound, acc0)
          }
        case ListDecl(ListLang.Comprehension(ex, b, in, filter)) =>
          val acc1 = loop(in, bound, acc)
          val bound1 = bound ++ b.names
          val acc2 = loop(ex.value, bound1, acc1)
          filter.fold(acc2)(loop(_, bound1, acc2))
        case DictDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, kv) =>
            val acc1 = loop(kv.key, bound, acc0)
            loop(kv.value, bound, acc1)
          }
        case DictDecl(ListLang.Comprehension(ex, b, in, filter)) =>
          val acc1 = loop(in, bound, acc)
          val bound1 = bound ++ b.names
          val acc2 = loop(ex.key, bound1, acc1)
          val acc3 = loop(ex.value, bound1, acc2)
          filter.fold(acc3)(loop(_, bound1, acc3))
        case RecordConstructor(_, args) =>
          // A constructor doesn't introduce new bindings
          args.foldLeft(acc) {
            case (acc, RecordArg.Pair(_, v)) =>
              loop(v, bound, acc)
            case (acc, RecordArg.Simple(v)) =>
              loop(Var(v)(decl.region), bound, acc)
          }
      }

    loop(this, Set.empty, SortedSet.empty)
  }

  final def isCheap: Boolean = {
    @annotation.tailrec
    def loop(decl: Declaration): Boolean =
      decl match {
        case Var(_) | Literal(_) => true
        case Annotation(term, _) => loop(term)
        case Parens(p) => loop(p)
        case _ => false
      }

    loop(this)
  }

  /**
   * Wrap in Parens is needed
   */
  def toNonBinding: NonBinding =
    this match {
      case nb: NonBinding => nb
      case decl => Parens(decl)(decl.region)
    }

  /**
   * This returns *all* names in the declaration, bound or not
   */
  def allNames: SortedSet[Bindable] = {
    def loop(decl: Declaration, acc: SortedSet[Bindable]): SortedSet[Bindable] =
      decl match {
        case Annotation(term, _) => loop(term, acc)
        case Apply(fn, args, _) =>
          (fn :: args).foldLeft(acc) { (acc0, d) => loop(d, acc0) }
        case ApplyOp(left, op, right) =>
          val acc0 = loop(left, acc)
          loop(right, acc0 + op)
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
        case Ternary(t, c, f) =>
          val acc1 = loop(t, acc)
          val acc2 = loop(c, acc1)
          loop(f, acc2)
        case Lambda(args, body) =>
          val acc1 = acc ++ args.toList.flatMap(_.names)
          loop(body, acc1)
        case Literal(lit) => acc
        case Match(_, typeName, args) =>
          val acc1 = loop(typeName, acc)
          args.get.foldLeft(acc1) { case (acc0, (pat, res)) =>
            loop(res.get, acc0 ++ pat.names)
          }
        case Matches(a, p) =>
          loop(a, acc ++ p.names)
        case Parens(p) => loop(p, acc)
        case TupleCons(items) =>
          items.foldLeft(acc) { (acc0, d) => loop(d, acc0) }
        case Var(name: Bindable) => acc + name
        case Var(_) => acc
        case StringDecl(nel) =>
          nel.foldLeft(acc) {
            case (acc0, Left(decl)) => loop(decl, acc0)
            case (acc0, Right(_)) => acc0
          }
        case ListDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, sori) =>
            loop(sori.value, acc0)
          }
        case ListDecl(ListLang.Comprehension(ex, b, in, filter)) =>
          val acc1 = loop(in, acc)
          val acc2 = loop(ex.value, acc1 ++ b.names)
          filter.fold(acc2)(loop(_, acc2))
        case DictDecl(ListLang.Cons(items)) =>
          items.foldLeft(acc) { (acc0, kv) =>
            val acc1 = loop(kv.key, acc0)
            loop(kv.value, acc1)
          }
        case DictDecl(ListLang.Comprehension(ex, b, in, filter)) =>
          val acc1 = loop(in, acc)
          val acc2 = loop(ex.key, acc1 ++ b.names)
          val acc3 = loop(ex.value, acc2)
          filter.fold(acc3)(loop(_, acc3))
        case RecordConstructor(_, args) =>
          args.foldLeft(acc) {
            case (acc, RecordArg.Pair(_, v)) => loop(v, acc)
            case (acc, RecordArg.Simple(n)) => acc + n
          }
      }
    loop(this, SortedSet.empty)
  }

  def replaceRegions(r: Region): Declaration =
    this match {
      case Binding(BindingStatement(n, v, in)) =>
        Binding(BindingStatement(n, v.replaceRegionsNB(r), in.map(_.replaceRegions(r))))(r)
      case Comment(CommentStatement(lines, c)) =>
        Comment(CommentStatement(lines, c.map(_.replaceRegions(r))))(r)
      case DefFn(d) =>
        DefFn(d.copy(result = (d.result._1.map(_.replaceRegions(r)), d.result._2.map(_.replaceRegions(r)))))(r)
      case nb: NonBinding => nb.replaceRegionsNB(r)
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

  /**
   * Try to substitute ex for ident in the expression: in
   *
   * This can fail if the free variables in ex are shadowed
   * above ident in in.
   *
   * this code is very similar to TypedExpr.substitute
   * if bugs are found in one, consult the other
   */
  def substitute[A](ident: Bindable, ex: NonBinding, in: Declaration): Option[Declaration] = {
    // if we hit a shadow, we don't need to substitute down
    // that branch
    @inline def shadows(i: Bindable): Boolean = i === ident

    // free variables in ex are being rebound,
    // this causes us to return None
    lazy val masks: Bindable => Boolean = ex.freeVars

    def loopLL[F[_]](ll: ListLang[F, NonBinding, Pattern.Parsed])(fn: F[NonBinding] => Option[F[NonBinding]]): Option[ListLang[F, NonBinding, Pattern.Parsed]] =
      ll match {
        case ListLang.Cons(items) =>
          items.traverse(fn)
            .map(ListLang.Cons(_))
        case ListLang.Comprehension(ex, b, in, filt) =>
          // b sets up bindings for filt and ex
          loop(in)
            .flatMap { in1 =>
              val pnames = b.names
              if (pnames.exists(masks)) None
              else if (pnames.exists(shadows)) Some(ListLang.Comprehension(ex, b, in1, filt))
              else {
                // no shadowing or masking
                (fn(ex), filt.traverse(loop))
                  .mapN(ListLang.Comprehension(_, b, in1, _))
              }
            }
      }

    def loopRA(ra: RecordArg): Option[RecordArg] =
      ra match {
        case RecordArg.Pair(fn, a) =>
          loop(a).map(RecordArg.Pair(fn, _))
        case RecordArg.Simple(fn) =>
          if (fn === ident) {
            ex match {
              case Var(newIdent) if newIdent === fn =>
                // this is identity
                Some(ra)
              case _ =>
                // This is no longer a simple RecordArg
                Some(RecordArg.Pair(fn, ex))
            }
          }
          else Some(ra)
      }

    def loop(decl: NonBinding): Option[NonBinding] =
      decl match {
        case Annotation(term, tpe) =>
          loop(term).map(Annotation(_, tpe)(decl.region))
        case Apply(fn, args, kind) =>
          (loop(fn), args.traverse(loop))
            .mapN(Apply(_, _, kind)(decl.region))
        case aop@ApplyOp(left, op, right) if (op: Bindable) === ident =>
          // we cannot make a general substition on ApplyOp
          ex match {
            case Var(op1: Identifier.Operator) =>
              (loop(left), loop(right))
                .mapN(ApplyOp(_, op1, _))
            case _ =>
              loop(aop.toApply)
          }
        case ApplyOp(left, op, right) =>
          (loop(left), loop(right))
            .mapN(ApplyOp(_, op, _))
        case IfElse(ifCases, elseCase) =>
          val ifs = ifCases.traverse { case (cond, br) =>
            (loop(cond), br.traverse(loopDec)).tupled
          }
          val elsec = elseCase.traverse(loopDec)

          (ifs, elsec).mapN(IfElse(_, _)(decl.region))
        case Ternary(t, c, f) =>
          (loop(t), loop(c), loop(f)).mapN(Ternary(_, _, _))
        case Lambda(args, body) =>
          // sets up a binding
          val pnames = args.toList.flatMap(_.names)
          if (pnames.exists(masks)) None
          else if (pnames.exists(shadows)) Some(decl)
          else loopDec(body).map(Lambda(args, _)(decl.region))
        case l@Literal(_) => Some(l)
        case Match(k, arg, cases) =>
          val caseRes =
            cases
              .traverse { nel =>
                nel.traverse { case (pat, res) =>
                  // like lambda:
                  val pnames = pat.names
                  if (pnames.exists(masks)) None
                  else if (pnames.exists(shadows)) Some((pat, res))
                  else res.traverse(loopDec).map((pat, _))
                }
              }

          (loop(arg), caseRes).mapN(Match(k, _, _)(decl.region))
        case Matches(a, p) =>
          // matches does not currently set up bindings
          loop(a).map(Matches(_, p)(decl.region))
        case Parens(p) =>
          loopDec(p).map(Parens(_)(decl.region))
        case TupleCons(items) =>
          items.traverse(loop).map(TupleCons(_)(decl.region))
        case Var(name: Bindable) if name === ident =>
          // here is the substition
          Some(ex.replaceRegionsNB(decl.region))
        case Var(_) => Some(decl)
        case StringDecl(nel) =>
          nel
            .traverse {
              case Left(nb) => loop(nb).map(Left(_))
              case right => Some(right)
            }
            .map(StringDecl(_)(decl.region))
        case ListDecl(ll) =>
          loopLL(ll)(_.traverse(loop))
            .map(ListDecl(_)(decl.region))
        case DictDecl(ll) =>
          loopLL(ll)(_.traverse(loop))
            .map(DictDecl(_)(decl.region))
        case RecordConstructor(c, args) =>
          args.traverse(loopRA)
            .map(RecordConstructor(c, _)(decl.region))
      }

    def loopDec(decl: Declaration): Option[Declaration] =
      decl match {
        case Binding(BindingStatement(n, v, in)) =>
          val thisNames = n.names
          if (thisNames.exists(masks)) None
          else if (thisNames.exists(shadows)) {
            // we only substitute on v
            loop(v)
              .map { v1 =>
                Binding(BindingStatement(n, v1, in))(decl.region)
              }
          }
          else {
            // we substitute on both
            (loop(v), in.traverse(loopDec))
              .mapN { (v1, in1) =>
                Binding(BindingStatement(n, v1, in1))(decl.region)
              }
          }
        case Comment(c) =>
          c.on
            .traverse(loopDec)
            .map { p1 =>
              Comment(CommentStatement(c.message, p1))(decl.region)
            }
        case DefFn(DefStatement(nm, args, rtype, (body, rest))) =>
          def go(scope: List[Bindable], d0: Declaration): Option[Declaration] =
            if (scope.exists(masks)) None
            else if (scope.exists(shadows)) Some(d0)
            else loopDec(d0)

          val bodyScope = nm :: args.flatMap(_.names)
          val restScope = nm :: Nil

          (body.traverse(go(bodyScope, _)), rest.traverse(go(restScope, _)))
            .mapN { (b, r) =>
              DefFn(DefStatement(nm, args, rtype, (b, r)))(decl.region)
            }
        case nb: NonBinding => loop(nb)
      }

    loopDec(in)
  }

  private val identDoc: Document[Identifier] =
    Identifier.document

  private val colonSpace = Doc.text(": ")

  sealed abstract class RecordArg {
    def toDoc: Doc =
      this match {
        case RecordArg.Pair(f, a) =>
          identDoc.document(f) + colonSpace + a.toDoc
        case RecordArg.Simple(f) =>
          identDoc.document(f)
      }
  }
  object RecordArg {
    final case class Pair(field: Bindable, arg: NonBinding) extends RecordArg
    // for cases like:
    // age = 47
    // Person { name: "Frank", age }
    final case class Simple(field: Bindable) extends RecordArg

    def parser(indent: String, declP: P1[NonBinding]): P1[RecordArg] = {
      val pairFn: P1[Bindable => Pair] = {
        val ws = Parser.maybeIndentedOrSpace(indent)
        ((ws.with1.soft *> P.char(':')) *> ws *> declP)
          .map { decl => Pair(_, decl) }
      }

      (Identifier.bindableParser ~ (pairFn.?))
        .map {
          case (b, None) => Simple(b)
          case (b, Some(fn)) => fn(b)
        }
    }
  }

  /**
   * These are all Declarations other than Binding, DefFn and Comment,
   * in other words, things that don't need to start with indentation
   */
  sealed abstract class NonBinding extends Declaration {
    def replaceRegionsNB(r: Region): NonBinding =
      this match {
        case Annotation(term, t) => Annotation(term.replaceRegionsNB(r), t)(r)
        case Apply(fn, args, s) =>
          Apply(fn.replaceRegionsNB(r), args.map(_.replaceRegionsNB(r)), s)(r)
        case ApplyOp(left, op, right) =>
          ApplyOp(left.replaceRegionsNB(r), op, right.replaceRegionsNB(r))
        case IfElse(ifCases, elseCase) =>
          IfElse(ifCases.map { case (bool, res) => (bool.replaceRegionsNB(r), res.map(_.replaceRegions(r))) },
            elseCase.map(_.replaceRegions(r)))(r)
        case Ternary(t, c, f) =>
          Ternary(t.replaceRegionsNB(r), c.replaceRegionsNB(r), f.replaceRegionsNB(r))
        case Lambda(args, body) =>
          Lambda(args, body.replaceRegions(r))(r)
        case Literal(lit) => Literal(lit)(r)
        case Match(rec, arg, branches) =>
          Match(rec,
            arg.replaceRegionsNB(r),
            branches.map(_.map { case (p, x) => (p, x.map(_.replaceRegions(r))) }))(r)
        case Matches(a, p) =>
          Matches(a.replaceRegionsNB(r), p)(r)
        case Parens(p) => Parens(p.replaceRegions(r))(r)
        case TupleCons(items) =>
          TupleCons(items.map(_.replaceRegionsNB(r)))(r)
        case Var(b) => Var(b)(r)
        case StringDecl(nel) =>
          val ne1 = nel.map {
            case Right((_, s)) => Right((r, s))
            case Left(e) => Left(e.replaceRegionsNB(r))
          }
          StringDecl(ne1)(r)
        case ListDecl(ListLang.Cons(items)) =>
          ListDecl(ListLang.Cons(items.map(_.map(_.replaceRegionsNB(r)))))(r)
        case ListDecl(ListLang.Comprehension(ex, b, in, filter)) =>
          ListDecl(ListLang.Comprehension(ex.map(_.replaceRegionsNB(r)), b, in.replaceRegionsNB(r), filter.map(_.replaceRegionsNB(r))))(r)
        case DictDecl(ListLang.Cons(items)) =>
          DictDecl(ListLang.Cons(items.map {
            case ListLang.KVPair(k, v) =>
              ListLang.KVPair(k.replaceRegionsNB(r), v.replaceRegionsNB(r))
          }))(r)
        case DictDecl(ListLang.Comprehension(ex, b, in, filter)) =>
          DictDecl(ListLang.Comprehension(ex.map(_.replaceRegionsNB(r)), b, in.replaceRegionsNB(r), filter.map(_.replaceRegionsNB(r))))(r)
        case RecordConstructor(c, args) =>
          val args1 = args.map {
            case RecordArg.Simple(b) => RecordArg.Simple(b)
            case RecordArg.Pair(k, v) => RecordArg.Pair(k, v.replaceRegionsNB(r))
          }
          RecordConstructor(c, args1)(r)
      }
  }

  object NonBinding {
    implicit val document: Document[NonBinding] =
      Document.instance(_.toDoc)
  }

  //
  // We use the pattern of an implicit region for two reasons:
  // 1. we don't want the region to play a role in pattern matching or equality, since it is about
  //    error reporting, and if we include it in equality it massively complicates tests.
  // 2. we want to be able to construct these for tests dummy values, so we can set up an implicit
  //    value in tests and construct them.
  // These reasons are a bit abusive, and we may revisit this in the future
  //
  case class Annotation(fn: NonBinding, tpe: TypeRef)(implicit val region: Region) extends NonBinding
  case class Apply(fn: NonBinding, args: NonEmptyList[NonBinding], kind: ApplyKind)(implicit val region: Region) extends NonBinding
  case class ApplyOp(left: NonBinding, op: Identifier.Operator, right: NonBinding) extends NonBinding {
    val region = left.region + right.region
    def opVar: Var = Var(op)(Region(left.region.end, right.region.start))

    def toApply: Apply =
      Apply(opVar, NonEmptyList(left, right :: Nil), ApplyKind.Parens)(region)
  }
  case class Binding(binding: BindingStatement[Pattern.Parsed, NonBinding, Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class Comment(comment: CommentStatement[Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class DefFn(deffn: DefStatement[Pattern.Parsed, (OptIndent[Declaration], Padding[Declaration])])(implicit val region: Region) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(NonBinding, OptIndent[Declaration])],
    elseCase: OptIndent[Declaration])(implicit val region: Region) extends NonBinding
  case class Ternary(trueCase: NonBinding, cond: NonBinding, falseCase: NonBinding) extends NonBinding {
    val region = trueCase.region + falseCase.region
  }
  case class Lambda(args: NonEmptyList[Pattern.Parsed], body: Declaration)(implicit val region: Region) extends NonBinding
  case class Literal(lit: Lit)(implicit val region: Region) extends NonBinding
  case class Match(
    kind: RecursionKind,
    arg: NonBinding,
    cases: OptIndent[NonEmptyList[(Pattern.Parsed, OptIndent[Declaration])]])(
    implicit val region: Region) extends NonBinding
  case class Matches(arg: NonBinding, pattern: Pattern.Parsed)(implicit val region: Region) extends NonBinding
  case class Parens(of: Declaration)(implicit val region: Region) extends NonBinding
  case class TupleCons(items: List[NonBinding])(implicit val region: Region) extends NonBinding
  case class Var(name: Identifier)(implicit val region: Region) extends NonBinding

  /**
   * This represents code like:
   * Foo { bar: 12 }
   */
  case class RecordConstructor(cons: Constructor, arg: NonEmptyList[RecordArg])(implicit val region: Region) extends NonBinding
  /**
   * This represents interpolated strings
   */
  case class StringDecl(items: NonEmptyList[Either[NonBinding, (Region, String)]])(implicit val region: Region) extends NonBinding
  /**
   * This represents the list construction language
   */
  case class ListDecl(list: ListLang[SpliceOrItem, NonBinding, Pattern.Parsed])(implicit val region: Region) extends NonBinding
  /**
   * Here are dict constructors and comprehensions
   */
  case class DictDecl(list: ListLang[KVPair, NonBinding, Pattern.Parsed])(implicit val region: Region) extends NonBinding

  val matchKindParser: P1[RecursionKind] =
    P.string1("match")
      .as(RecursionKind.NonRecursive)
      .orElse1(
        P.string1("recur")
        .as(RecursionKind.Recursive)).soft <* Parser.spaces.peek

  /**
   * A pattern can also be a declaration in some cases
   *
   * TODO, patterns don't parse with regions, so we lose track of precise position information
   * if we want to point to an inner portion of it
   */
  def toPattern(d: NonBinding): Option[Pattern.Parsed] =
    d match {
      case Annotation(term, tpe) =>
        toPattern(term).map(Pattern.Annotation(_, tpe))
      case Var(nm@Identifier.Constructor(_)) =>
        Some(Pattern.PositionalStruct(
          Pattern.StructKind.Named(nm, Pattern.StructKind.Style.TupleLike), Nil))
      case Var(v: Bindable) => Some(Pattern.Var(v))
      case Literal(lit) => Some(Pattern.Literal(lit))
      case StringDecl(NonEmptyList(Right((_, s)), Nil)) =>
        Some(Pattern.Literal(Lit.Str(s)))
      case StringDecl(items) =>
        def toStrPart(p: Either[NonBinding, (Region, String)]): Option[Pattern.StrPart] =
          p match {
            case Right((_, str)) => Some(Pattern.StrPart.LitStr(str))
            case Left(Var(v: Bindable)) => Some(Pattern.StrPart.NamedStr(v))
            case _ => None
          }
        items.traverse(toStrPart).map(Pattern.StrPat(_))
      case ListDecl(ListLang.Cons(elems)) =>
        val optParts: Option[List[Pattern.ListPart[Pattern.Parsed]]] =
          elems.traverse {
            case SpliceOrItem.Splice(Var(bn: Bindable)) =>
              Some(Pattern.ListPart.NamedList(bn))
            case SpliceOrItem.Item(p) =>
              toPattern(p).map(Pattern.ListPart.Item(_))
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
          Pattern.PositionalStruct(Pattern.StructKind.Named(nm,
            Pattern.StructKind.Style.TupleLike), argPats.toList)
        }
      case TupleCons(ps) =>
        ps.traverse(toPattern(_)).map { argPats =>
          Pattern.PositionalStruct(Pattern.StructKind.Tuple, argPats.toList)
        }
      case Parens(p: NonBinding) => toPattern(p)
      case RecordConstructor(cons, args) =>
        args.traverse {
          case RecordArg.Simple(b) => Some(Left(b))
          case RecordArg.Pair(k, v) =>
            toPattern(v).map { vpat =>
              Right((k, vpat))
            }
        }
        .map(Pattern.recordPat(cons, _)(Pattern.StructKind.Named(_, _)))
      case _ => None
    }

  val eqP: P[Unit] = P.char('=') <* (!Operators.multiToksP)
  /**
   * if cutPattern = false, we put Pattern.bindParser in NoCut
   * this is needed for parsing declarations currently because
   * patterns and some Declarations are ambiguous, so only the = signals them
   */
  def bindingParser[T](parser: Indy[NonBinding], cutPattern: Boolean): Indy[T => BindingStatement[Pattern.Parsed, NonBinding, T]] = {
    val patPart = Pattern.bindParser <* maybeSpace <* eqP
    val cutOrNot = if (cutPattern) patPart else patPart.backtrack
    val pat: Indy[Pattern.Parsed] = Indy.lift(cutOrNot)

    // allow = to be like a block, we can continue on the next line indented
    OptIndent.blockLike(pat, parser, P.unit)
      .map { case (pat, value) =>
        { rest => BindingStatement(pat, value.get, rest) }
      }
  }

  private def restP(parser: Indy[Declaration]): Indy[Padding[Declaration]] =
    parser.indentBefore.mapF(Padding.parser(_))

  def commentP(parser: Indy[Declaration]): Parser.Indy[Comment] =
    CommentStatement.parser(
        { indent => Padding.parser(P.string(indent).with1 *>  parser(indent)) }
      )
      .region
      .map { case (r, c) => Comment(c)(r) }

  def defP(parser: Indy[Declaration]): Indy[DefFn] = {
    val restParser: Indy[(OptIndent[Declaration], Padding[Declaration])] =
      OptIndent.indy(parser).product(Indy.lift(toEOL1) *> restP(parser))

    restParser.mapF { rp =>
      DefStatement.parser(Pattern.bindParser, maybeSpace.with1 *> rp)
        .region
        .map { case (r, d) => DefFn(d)(r) }
    }
  }

  def ifElseP(arg: Indy[NonBinding], expr: Indy[Declaration]): Indy[IfElse] = {

    def ifelif(str: String): Indy[(NonBinding, OptIndent[Declaration])] =
      OptIndent.block(Indy.lift(Parser.keySpace(str)).cutRight(arg), expr)

    /*
     * We don't need to parse the else term to the end,
     * EOL is a kind of a separator we only have in between
     */
    val elseTerm: Indy[OptIndent[Declaration]] =
      OptIndent
        .block(Indy.lift(P.string1("else") <* maybeSpace), expr)
        .map(_._2)
        .maybeMore // allow extra indentation

    val elifs1 = {
      val elifs = ifelif("elif").nonEmptyList(sepIndy = Indy.toEOLIndent)
      (elifs <* Indy.toEOLIndent).maybeMore // allow extra indentation
    }

    (ifelif("if") <* Indy.toEOLIndent)
      .cutThenOpt(elifs1)
      .cutThen(elseTerm)
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

  def stringDeclOrLit(inner: Indy[NonBinding]): Indy[NonBinding] = {
    val start = P.string1("${")
    val end = P.char('}')
    val q1 = '\''
    val q2 = '"'

    inner.mapF { p =>
      val plist = StringUtil.interpolatedString(q1, start, p, end)
        .orElse1(StringUtil.interpolatedString(q2, start, p, end))

      plist.region.map {
        case (r, Nil) =>
          // this is the empty string:
          Literal(Lit.EmptyStr)(r)
        case (r, Right((_, str)) :: Nil) =>
          Literal(Lit.Str(str))(r)
        case (r, h :: tail) =>
          StringDecl(NonEmptyList(h, tail))(r)
        }
    }
  }

  def lambdaP(parser: Indy[Declaration]): Indy[Lambda] = {
    val params = Indy.lift(P.char('\\') *> maybeSpace *> Pattern.bindParser.nonEmptyList)

    OptIndent.blockLike(params, parser, maybeSpace.with1 *> P.string1("->"))
      .region
      .map { case (r, (args, body)) => Lambda(args, body.get)(r) }
  }

  def matchP(arg: Indy[NonBinding], expr: Indy[Declaration]): Indy[Match] = {
    val withTrailingExpr = expr.cutLeftP(maybeSpace)
    val branch = OptIndent.block(Indy.lift(Pattern.matchParser), withTrailingExpr)

    val left = Indy.lift(matchKindParser <* spaces).cutThen(arg).cutLeftP(maybeSpace)
    OptIndent.block(left, branch.nonEmptyList(Indy.toEOLIndent))
      .region
      .map { case (r, ((kind, arg), branches)) =>
        Match(kind, arg, branches)(r)
      }
  }

  /**
   * These are keywords inside declarations (if, match, def)
   * that cannot be used by identifiers
   */
  val keywords: Set[String] =
    Set("from", "import", "if", "else", "elif", "match", "matches", "def", "recur", "struct", "enum")

  /**
   * A Parser that matches keywords
   */
  val keywordsP: P1[Unit] =
    P.oneOf1(keywords.toList.sorted.map(P.string1(_))) <* spaces

  val varP: P1[Var] =
    (!keywordsP).with1 *> Identifier.bindableParser.region.map { case (r, i) => Var(i)(r) }

  // this returns a Var with a Constructor or a RecordConstrutor
  def recordConstructorP(indent: String, declP: P1[NonBinding], noAnn: P1[NonBinding]): P1[NonBinding] = {
    val ws = Parser.maybeIndentedOrSpace(indent)
    val kv: P1[RecordArg] = RecordArg.parser(indent, noAnn)
    val kvs = kv.nonEmptyListOfWs(ws)

    // here is the record style: Foo {x: 1, ...
    val recArgs = kvs.bracketed(maybeSpace.with1.soft ~ P.char('{') ~ ws, ws ~ P.char('}'))

    // here is tuple style: Foo(a, b)
    val tupArgs = declP
      .parensLines1Cut
      .region
      .map { case (r, args) =>
        { nm: Var => Apply(nm, args, ApplyKind.Parens)(nm.region + r) }
      }

    (Identifier.consParser ~ Parser.either(recArgs, tupArgs).?)
      .region
      .map {
        case (region, (n, Some(Left(args)))) =>
          RecordConstructor(n, args)(region)
        case (region, (n, Some(Right(build)))) =>
          build(Var(n)(region))
        case (region, (n, None)) =>
          Var(n)(region)
      }
  }

  private def patternBind(nonBindingParser: Indy[NonBinding], decl: Indy[Declaration]): Indy[Declaration] =
    // we can't cut the pattern here because we have some ambiguity in declarations
    bindingParser[Padding[Declaration]](nonBindingParser <* Indy.lift(toEOL1), cutPattern = false)
      .cutThen(restP(decl))
      .region
      .map { case (region, (bindfn, decl)) =>
        Binding(bindfn(decl))(region)
      }

  private def listP(p: P1[NonBinding], src: P1[NonBinding]): P1[ListDecl] =
    ListLang.parser(p, src, Pattern.bindParser)
      .region
      .map { case (r, l) => ListDecl(l)(r) }

  private def dictP(p: P1[NonBinding], src: P1[NonBinding]): P1[DictDecl] =
    ListLang.dictParser(p, src, Pattern.bindParser)
      .region
      .map { case (r, l) => DictDecl(l)(r) }

  val lits: P1[Literal] = Lit.integerParser.region.map { case (r, l) => Literal(l)(r) }

  private sealed abstract class ParseMode
  private object ParseMode {
    case object Decl extends ParseMode
    case object NB extends ParseMode
    case object BranchArg extends ParseMode
    case object ComprehensionSource extends ParseMode
  }
  /*
   * This is not fully type-safe but we do it for efficiency:
   * we take a boolean to select if we allow Declaration
   * or not. If false, we only parse NonBinding, if true
   * we also parse Bind, Def, Comment
   */
  private[this] val parserCache: ((ParseMode, String)) => P1[Declaration] =
    Memoize.memoizeDagHashedConcurrent[(ParseMode, String), P1[Declaration]] { case ((pm, indent), rec) =>

      val recurse: P1[Declaration] = P.defer1(rec((ParseMode.Decl, indent))) // needs to be inside a P for laziness
      val recIndy: Indy[Declaration] = Indy { i => rec((ParseMode.Decl, i)) }

      // TODO: aren't NonBinding independent of indentation level>
      val recNonBind: P1[NonBinding] = P.defer1(rec((ParseMode.NB, indent))).asInstanceOf[P1[NonBinding]]
      val recNBIndy: Indy[NonBinding] = Indy { i => rec((ParseMode.NB, i)).asInstanceOf[P1[NonBinding]] }

      val recArg: P1[NonBinding] = P.defer1(rec((ParseMode.BranchArg, indent)).asInstanceOf[P1[NonBinding]])
      val recArgIndy: Indy[NonBinding] = Indy { i => rec((ParseMode.BranchArg, i)).asInstanceOf[P1[NonBinding]] }

      val recComp: P1[NonBinding] = P.defer1(rec((ParseMode.ComprehensionSource, indent))).asInstanceOf[P1[NonBinding]]

      val tupOrPar: P1[NonBinding] =
        Parser.parens(((recNonBind <* (!(maybeSpace ~ eqP)))
          .tupleOrParens0
          .map {
            case Left(p) => { r: Region =>  Parens(p)(r) }
            case Right(tup) => { r: Region => TupleCons(tup.toList)(r) }
          })
          .orElse(recurse.map { d => { r: Region => Parens(d)(r) } })
          // or it could be () which is just unit
          .orElse(P.pure({ r: Region => TupleCons(Nil)(r) }))
        )
        .region
        .map { case (r, fn) => fn(r) }

      // since \x -> y: t will parse like \x -> (y: t)
      // if we are in a branch arg, we can't parse annotations on the body of the lambda
      val lambBody = if (pm == ParseMode.BranchArg) recArgIndy.asInstanceOf[Indy[Declaration]] else recIndy
      val ternaryElseP = if (pm == ParseMode.BranchArg) recArg else recNonBind

      val allNonBind: P1[NonBinding] =
        P.defer1(
          P.oneOf1(
            lambdaP(lambBody)(indent) ::
            ifElseP(recArgIndy, recIndy)(indent) ::
            matchP(recArgIndy, recIndy)(indent) ::
            dictP(recArg, recComp) ::
            varP ::
            listP(recNonBind, recComp) ::
            lits ::
            stringDeclOrLit(recNBIndy)(indent) ::
            tupOrPar ::
            recordConstructorP(indent, recNonBind, recArg) ::
            Nil))

      /*
       * This is where we parse application, either direct, or dot-style
       */
      val applied: P1[NonBinding] = {
        val params = recNonBind.parensLines1Cut
        // here we are using . syntax foo.bar(1, 2)
        // we also allow foo.(anyExpression)(1, 2)
        val fn = varP.orElse1(recNonBind.parensCut)
        val slashcontinuation = ((maybeSpace ~ P.char('\\') ~ toEOL1).backtrack ~ Parser.maybeSpacesAndLines).?.void
        val dotApply: P1[NonBinding => NonBinding] =
          (slashcontinuation.with1 *> P.char('.') *> (fn ~ params.?))
            .region
            .map { case (r2, (fn, argsOpt)) =>
              val args = argsOpt.fold(List.empty[NonBinding])(_.toList)

              { head: NonBinding => Apply(fn, NonEmptyList(head, args), ApplyKind.Dot)(head.region + r2) }
            }

        // here we directly call a function foo(1, 2)
        val applySuffix: P1[NonBinding => NonBinding] =
          params
            .region
            .map { case (r, args) =>
              { fn: NonBinding => Apply(fn, args, ApplyKind.Parens)(fn.region + r) }
            }

        def repFn[A](fn: P1[A => A]): P[A => A] =
          fn.rep.map { opList =>
            { (a: A) => opList.foldLeft(a) { (arg, fn) => fn(arg) } }
          }

        (allNonBind ~ repFn(dotApply.orElse1(applySuffix)))
          .map { case (a, f) => f(a) }
      }
      // lower priority than calls is type annotation
      val annotated: P1[NonBinding] =
        if (pm == ParseMode.BranchArg) applied
        else {
          val an: P1[NonBinding => NonBinding] =
            (maybeSpace.with1.soft *> P.char(':') *> maybeSpace *> TypeRef.parser)
              // TODO remove this backtrack
              .backtrack
              .region
              .map { case (r, tpe) =>
                { nb: NonBinding => Annotation(nb, tpe)(nb.region + r) }
              }

          applied.maybeAp(an)
        }

      // matched
      val matched: P1[NonBinding] = {
        // x matches p
        val matchesOp =
          ((maybeSpace.with1 *> P.string1("matches") *> spaces).backtrack *> Pattern.matchParser)
            .region
            .map { case (region, pat) =>

              { nb: NonBinding => Matches(nb, pat)(nb.region + region) }
            }
            .rep1
            .map { fns => fns.toList.reduceLeft(_.andThen(_)) }

        annotated.maybeAp(matchesOp)
      }

      // Applying is higher precedence than any operators
      // now parse an operator apply
      def postOperators(nb: P1[NonBinding]): P1[NonBinding] = {

        def convert(form: Operators.Formula[NonBinding]): NonBinding =
          form match {
            case Operators.Formula.Sym(r) => r
            case Operators.Formula.Op(left, op, right) =>
              val leftD = convert(left)
              val rightD = convert(right)
              // `op`(l, r)
              ApplyOp(leftD, Identifier.Operator(op), rightD)
          }

        // one or more operators
        val ops: P1[NonBinding => Operators.Formula[NonBinding]] =
          Operators.Formula.infixOps1(nb)

        // This already parses as many as it can, so we don't need repFn
        val form = ops.map { fn =>

          { d: NonBinding => convert(fn(d)) }
        }

        nb.maybeAp(form)
      }

      // here is if/ternary operator
      // it fully recurses on the else branch, which will parse any repeated ternaryies
      // so no need to repeat here for correct precedence
      val ternary: P1[NonBinding => NonBinding] =
        (((spaces *> P.string1("if") *> spaces).backtrack *> recNonBind) ~ (spaces *> keySpace("else") *> ternaryElseP))
          .map { case (cond, falseCase) =>
            { trueCase: NonBinding => Ternary(trueCase, cond, falseCase) }
          }

      val finalNonBind: P1[NonBinding] =
        if (pm != ParseMode.ComprehensionSource) postOperators(matched).maybeAp(ternary)
        else postOperators(matched)

      if (pm != ParseMode.Decl) finalNonBind
      else {
        val finalBind: P1[Declaration] = P.defer1(
          P.oneOf1(
          // these have keywords which need to be parsed before var (def, match, if)
            defP(recIndy)(indent) ::
            // these are not ambiguous with patterns
            commentP(recIndy)(indent) ::
            /*
             * challenge is that not all Declarations are Patterns, and not
             * all Patterns are Declarations. So, bindings, which are: pattern = declaration
             * is a bit hard. This also makes cuts a bit dangerous, since this ambiguity
             * between pattern and declaration means if we use cuts too aggressively, we
             * will fail.
             *
             * If we parse a declaration first, if we see = we need to convert
             * to pattern. If we parse a pattern, but it was actually a declaration, we need
             * to convert there. This code tries to parse as a declaration first, then converts
             * it to pattern if we see an =
             */
            patternBind(recNBIndy, recIndy)(indent) ::
            Nil)
          )

        // we have to parse non-binds last
        finalBind.orElse1(finalNonBind)
      }
    }

  val parser: Indy[Declaration] =
    Indy { i => parserCache((ParseMode.Decl, i)) }
  val nonBindingParser: Indy[NonBinding] =
    Indy { i => parserCache((ParseMode.NB, i)) }.asInstanceOf[Indy[NonBinding]]
  val nonBindingParserNoTern: Indy[NonBinding] =
    Indy { i => parserCache((ParseMode.ComprehensionSource, i)) }.asInstanceOf[Indy[NonBinding]]
  val nonBindingParserNoAnn: Indy[NonBinding] =
    Indy { i => parserCache((ParseMode.BranchArg, i)) }.asInstanceOf[Indy[NonBinding]]
}
