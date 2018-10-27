package org.bykn.bosatsu

import Parser.{ Combinators, Indy, lowerIdent, upperIdent, maybeSpace, spaces, escapedString, toEOL }
import cats.data.NonEmptyList
import cats.implicits._
import com.stripe.dagon.Memoize
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Indy.IndyMethods
import org.bykn.fastparse_cats.StringInstances._

/**
 * Represents the syntax version of Expr
 */
sealed abstract class Declaration {
  import Declaration._

  def region: Region

  def toDoc: Doc = {
    this match {
      case Apply(fn, args, dotApply) =>
        val fnDoc = fn match {
          case Var(n) => Doc.text(n)
          case p@Parens(_) => p.toDoc
          case other => Doc.char('(') + other.toDoc + Doc.char(')')
        }

        val (prefix, body) =
          if (!dotApply) (fnDoc, args.toList)
          else (args.head.toDoc + Doc.char('.') + fnDoc, args.tail)

        body match {
          case Nil => prefix
          case notEmpty =>
            prefix + Doc.char('(') + Doc.intercalate(Doc.text(", "), notEmpty.map(_.toDoc)) + Doc.char(')')
        }
      case Binding(b) =>
        val d0 = Document[Padding[Declaration]]
        val withNewLine = Document.instance[Padding[Declaration]] { pd =>
           Doc.line + d0.document(pd)
        }
        BindingStatement.document(withNewLine).document(b)
      case Comment(c) =>
        CommentStatement.document[Padding[Declaration]].document(c)
      case Constructor(name) =>
        Doc.text(name)
      case DefFn(d) =>
        val pairDoc: Document[(OptIndent[Declaration], Padding[Declaration])] =
          Document.instance {
            case (fnBody, letBody) =>
                fnBody.sepDoc +
                Document[OptIndent[Declaration]].document(fnBody) +
                Doc.line +
                Document[Padding[Declaration]].document(letBody)
          }
        DefStatement.document(pairDoc).document(d)
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
        Doc.char('\\') + Doc.intercalate(Doc.text(", "), args.toList.map(Doc.text _)) + Doc.text(" -> ") + body.toDoc
      case LiteralInt(str) => Doc.text(str)
      case LiteralString(str, q) =>
          Doc.char(q) + Doc.text(Parser.escape(Set(q), str)) + Doc.char(q)
      case Match(typeName, args) =>
        val pid = Document[OptIndent[Declaration]]

        implicit val patDoc: Document[(Pattern[String, TypeRef], OptIndent[Declaration])] =
          Document.instance[(Pattern[String, TypeRef], OptIndent[Declaration])] {
            case (pat, decl) =>
              Document[Pattern[String, TypeRef]].document(pat) + Doc.text(":") + decl.sepDoc + pid.document(decl)
          }
        implicit def linesDoc[T: Document]: Document[NonEmptyList[T]] =
          Document.instance { ts => Doc.intercalate(Doc.line, ts.toList.map(Document[T].document _)) }

        val piPat = Document[OptIndent[NonEmptyList[(Pattern[String, TypeRef], OptIndent[Declaration])]]]
        // TODO this isn't quite right
        Doc.text("match ") + typeName.toDoc + Doc.char(':') + args.sepDoc +
          piPat.document(args)
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case Var(name) => Doc.text(name)

      case ListDecl(list) =>
        ListLang.document.document(list)
    }
  }

  def toExpr(
    nameToType: String => rankn.Type.Const,
    nameToCons: String => (PackageName, ConstructorName)): Expr[Declaration] = {

    def loop(decl: Declaration): Expr[Declaration] =
      decl match {
        case Apply(fn, args, _) =>
          @annotation.tailrec
          def loop0(fn: Expr[Declaration], args: List[Expr[Declaration]]): Expr[Declaration] =
            args match {
              case Nil => fn
              case h :: tail =>
                loop0(Expr.App(fn, h, this), tail)
            }
          loop0(loop(fn), args.toList.map(loop(_)))
        case Binding(BindingStatement(arg, value, Padding(_, dec))) =>
          Expr.Let(arg, loop(value), loop(dec), this)
        case Comment(CommentStatement(_, Padding(_, decl))) =>
          loop(decl).map(_ => this)
        case Constructor(name) =>
          Expr.Var(name, this)
        case DefFn(defstmt@DefStatement(_, _, _, _)) =>
          val (bodyExpr, inExpr) = defstmt.result match {
            case (oaBody, Padding(_, in)) =>
              (loop(oaBody.get), loop(in))
          }
          val lambda = defstmt.toLambdaExpr(bodyExpr, this)(_.toType(nameToType))
          Expr.Let(defstmt.name, lambda, inExpr, this)
        case IfElse(ifCases, elseCase) =>
          def ifExpr(cond: Expr[Declaration], ifTrue: Expr[Declaration], ifFalse: Expr[Declaration]): Expr[Declaration] =
            Expr.If(cond, ifTrue, ifFalse, this)

          def loop0(ifs: NonEmptyList[(Expr[Declaration], Expr[Declaration])], elseC: Expr[Declaration]): Expr[Declaration] =
            ifs match {
              case NonEmptyList((cond, ifTrue), Nil) =>
                ifExpr(cond, ifTrue, elseC)
              case NonEmptyList(ifTrue, h :: tail) =>
                val elseC1 = loop0(NonEmptyList(h, tail), elseC)
                loop0(NonEmptyList.of(ifTrue), elseC1)
            }
          loop0(ifCases.map { case (d0, d1) =>
            (loop(d0), loop(d1.get))
          }, loop(elseCase.get))
        case Lambda(args, body) =>
          Expr.buildLambda(args.map((_, None)), loop(body), this)
        case LiteralInt(str) =>
          Expr.Literal(Lit.Integer(new java.math.BigInteger(str)), this)
        case LiteralString(str, _) =>
          Expr.Literal(Lit.Str(str), this)
        case Parens(p) =>
          loop(p).map(_ => this)
        case Var(name) =>
          Expr.Var(name, this)
        case Match(arg, branches) =>
          val expBranches = branches.get.map { case (pat, oidecl) =>
            val decl = oidecl.get
            val newPattern = pat.mapName(nameToCons).mapType(_.toType(nameToType))
            (newPattern, loop(decl))
          }
          Expr.Match(loop(arg), expBranches, this)
        case l@ListDecl(list) =>
          list match {
            case ListLang.Cons(items) =>
              val revDecs: List[ListLang.SpliceOrItem[Expr[Declaration]]] = items.reverseMap {
                case ListLang.SpliceOrItem.Splice(s) =>
                  ListLang.SpliceOrItem.Splice(loop(s))
                case ListLang.SpliceOrItem.Item(item) =>
                  ListLang.SpliceOrItem.Item(loop(item))
              }

              // TODO we need to refer to Predef/EmptyList no matter what here
              // but we have no way to fully refer to an item
              val empty: Expr[Declaration] = Expr.Var("EmptyList", l)
              def cons(head: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
                Expr.App(Expr.App(Expr.Var("NonEmptyList", l), head, l), tail, l)

              def concat(headList: Expr[Declaration], tail: Expr[Declaration]): Expr[Declaration] =
                Expr.App(Expr.App(Expr.Var("concat", l), headList, l), tail, l)

              revDecs.foldLeft(empty) {
                case (tail, ListLang.SpliceOrItem.Item(i)) =>
                  cons(i, tail)
                case (tail, ListLang.SpliceOrItem.Splice(s)) =>
                  concat(s, tail)
              }
            case ListLang.Comprehension(_, _, _, _) => ???
          }
      }

    loop(this)
  }
}

object Declaration {
  implicit val document: Document[Declaration] = Document.instance[Declaration](_.toDoc)
  implicit val hasRegion: HasRegion[Declaration] =
    HasRegion.instance[Declaration](_.region)

  //
  // We use the pattern of an implicit region for two reasons:
  // 1. we don't want the region to play a role in pattern matching or equality, since it is about
  //    error reporting, and if we include it in equality it massively complicates tests.
  // 2. we want to be able to construct these for tests dummy values, so we can set up an implicit
  //    value in tests and construct them.
  // These reasons are a bit abusive, and we may revisit this in the future
  //

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration], useDotApply: Boolean)(implicit val region: Region) extends Declaration
  case class Binding(binding: BindingStatement[Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class Comment(comment: CommentStatement[Padding[Declaration]])(implicit val region: Region) extends Declaration
  case class Constructor(name: String)(implicit val region: Region) extends Declaration
  case class DefFn(deffn: DefStatement[(OptIndent[Declaration], Padding[Declaration])])(implicit val region: Region) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(Declaration, OptIndent[Declaration])],
    elseCase: OptIndent[Declaration])(implicit val region: Region) extends Declaration
  case class Lambda(args: NonEmptyList[String], body: Declaration)(implicit val region: Region) extends Declaration
  case class LiteralInt(asString: String)(implicit val region: Region) extends Declaration
  case class LiteralString(asString: String, quoteChar: Char)(implicit val region: Region) extends Declaration
  case class Match(arg: Declaration,
    cases: OptIndent[NonEmptyList[(Pattern[String, TypeRef], OptIndent[Declaration])]])(
    implicit val region: Region) extends Declaration
  case class Parens(of: Declaration)(implicit val region: Region) extends Declaration
  case class Var(name: String)(implicit val region: Region) extends Declaration

  /**
   * This represents the list construction language
   */
  case class ListDecl(list: ListLang[Declaration])(implicit val region: Region) extends Declaration

  private val restP: Indy[Padding[Declaration]] =
    (Indy.parseIndent *> parser).mapF(Padding.parser(_))

  // This is something we check after variables
  private val bindingOp: Indy[(String, Region) => Binding] = {
    val eqP = P("=" ~ !"=")

    (Indy.lift(P(maybeSpace ~ eqP ~/ maybeSpace)) *> parser <* Indy.lift(toEOL))
      .product(restP)
      .region
      .map { case (region, (value, rest)) =>

        { (str: String, r: Region) => Binding(BindingStatement(str, value, rest))(r + region) }
      }
  }

  val constructorP: P[Constructor] =
    upperIdent.region.map { case (r, c) => Constructor(c)(r) }

  val commentP: Parser.Indy[Comment] =
    CommentStatement.parser(Parser.Indy { indent => Padding.parser(P(indent ~ parser(indent)))})
      .region
      .map { case (r, c) => Comment(c)(r) }

  val defP: Indy[DefFn] = {
    val restParser: Indy[(OptIndent[Declaration], Padding[Declaration])] =
      OptIndent.indy(parser).product(Indy.lift(toEOL) *> restP)

    restParser.mapF { rp =>
      DefStatement.parser(maybeSpace ~ rp)
        .region
        .map { case (r, d) => DefFn(d)(r) }
    }
  }

  def ifElseP(expr: Indy[Declaration]): Indy[IfElse] = {

    def ifelif(str: String): Indy[(Declaration, OptIndent[Declaration])] =
      Indy.block(Indy.lift(P(str ~ spaces ~/ maybeSpace)) *> expr, expr)

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
    val params = Indy.lift(P("\\" ~/ maybeSpace ~ lowerIdent.nonEmptyList))

    Indy.blockLike(params, parser, "->")
      .region
      .map { case (r, (args, body)) => Lambda(args, body.get)(r) }
  }

  val literalIntP: P[LiteralInt] =
    Parser.integerString
      .region
      .map { case (r, i) => LiteralInt(i)(r) }

  val literalStringP: P[LiteralString] = {
    val q1 = '\''
    val q2 = '"'
    def str(q: Char): P[LiteralString] =
      escapedString(q).region.map { case (r, str) => LiteralString(str, q)(r) }

    str(q1) | str(q2)
  }

  def matchP(expr: Indy[Declaration]): Indy[Match] = {
    val withTrailing = expr <* Indy.lift(maybeSpace)
    val branch = Indy.block(Indy.lift(Pattern.parser), withTrailing)

    Indy.block(Indy.lift(P("match" ~ spaces)) *> withTrailing, branch.nonEmptyList(Indy.toEOLIndent))
      .region
      .map { case (r, (exp, branches)) =>
        Match(exp, branches)(r)
      }
  }

  val varP: P[Var] =
    lowerIdent.region.map { case (r, v) => Var(v)(r) }

  private val varOrBind: Indy[Declaration] =
    Indy.lift(varP).product(bindingOp.?)
      .map {
        case (v, None) => v
        case (varD@Var(v), Some(fn)) => fn(v, varD.region)
      }

  private def listP(p: P[Declaration]): P[ListDecl] =
    ListLang.parser(p)
      .region
      .map { case (r, l) => ListDecl(l)(r) }

  private[this] val parserCache: String => P[Declaration] =
    Memoize.function[String, P[Declaration]] { (indent, rec) =>

      val postOperators: List[P[Declaration => Declaration]] = {
        val params = P(rec(indent).nonEmptyList.parensCut)
        // here we are using . syntax foo.bar(1, 2)
        val dotApply =
          P("." ~/ varP ~ params.?).region.map { case (r2, (fn, argsOpt)) =>
            val args = argsOpt.fold(List.empty[Declaration])(_.toList)

            { head: Declaration => Apply(fn, NonEmptyList(head, args), true)(head.region + r2) }
          }

        // here we directly call a function foo(1, 2)
        val applySuffix = params.region.map { case (r, args) =>

          { fn: Declaration => Apply(fn, args, false)(fn.region + r) }
        }

        // here is if/ternary operator
        val ternary =
          P(spaces ~ P("if") ~ spaces ~/ rec(indent) ~ spaces ~ "else" ~ spaces ~ rec(indent))
            .region
            .map { case (region, (cond, falseCase)) =>
              { trueCase: Declaration =>
                val ifcase = NonEmptyList.of((cond, OptIndent.same(trueCase)))
                IfElse(ifcase, OptIndent.same(falseCase))(trueCase.region + region)
              }
            }

        dotApply :: applySuffix :: ternary :: Nil
      }

      val recIndy = Indy(rec)

      val prefix = P(listP(rec("")) | defP(indent) | literalIntP | literalStringP | lambdaP(indent) | matchP(recIndy)(indent) |
        ifElseP(recIndy)(indent) | varOrBind(indent) | constructorP | commentP(indent) |
        P(rec(indent).parens).region.map { case (r, p) => Parens(p)(r) })

      val opsList = postOperators.reduce(_ | _).rep().map(_.toList)

      @annotation.tailrec
      def loop[A](a: A, fns: List[A => A]): A =
        fns match {
          case Nil => a
          case h :: tail => loop(h(a), tail)
        }

      P(prefix ~ opsList).map { case (arg, fns) => loop(arg, fns) }
    }

  lazy val parser: Indy[Declaration] =
    Indy.suspend(Indy(parserCache))
}
