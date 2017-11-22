package org.bykn.edgemar

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace, spaces }
import cats.data.NonEmptyList
import cats.implicits._
import com.stripe.dagon.Memoize
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.toEOL

// TODO, in the future, we could recursively have patterns in the args
case class Pattern(typeName: String, bindings: List[String])
object Pattern {
  implicit val document: Document[Pattern] =
    Document.instance[Pattern] {
      case Pattern(n, Nil) => Doc.text(n)
      case Pattern(n, nonEmpty) =>
        Doc.text(n) +
          Doc.char('(') + Doc.intercalate(Doc.text(", "), nonEmpty.map(Doc.text)) + Doc.char(')')
    }
  val parser: P[Pattern] =
    P(upperIdent ~ (lowerIdent.listN(1).parens).?)
      .map {
        case (n, None) => Pattern(n, Nil)
        case (n, Some(ls)) => Pattern(n, ls)
      }
}

/**
 * Represents the syntax of declarations
 */
sealed abstract class Declaration {
  import Declaration._

  val standardIndentation: Int = 4

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
        BindingStatement.document[Padding[Declaration]].document(b)
      case Comment(c) =>
        CommentStatement.document[Padding[Declaration]].document(c)
      case Constructor(name) =>
        Doc.text(name)
      case DefFn(d) =>
        val pairDoc: Document[(Padding[Indented[Declaration]], Padding[Declaration])] =
          Document.instance {
            case (fnBody, letBody) =>
                Document[Padding[Indented[Declaration]]].document(fnBody) +
                Doc.line +
                Document[Padding[Declaration]].document(letBody)
          }
        DefStatement.document(pairDoc).document(d)
      case IfElse(ifCases, elseCase) =>
        def checkBody(cb: (Declaration, Padding[Indented[Declaration]])) = {
          val (check, body) = cb
          check.toDoc + Doc.char(':') + Doc.line + Document[Padding[Indented[Declaration]]].document(body)
        }

        val tail = Doc.text("else:") + Doc.line + Document[Padding[Indented[Declaration]]].document(elseCase) :: Nil
        val parts = (Doc.text("if ") + checkBody(ifCases.head)) :: (ifCases.tail.map(Doc.text("elif ") + checkBody(_))) ::: tail
        Doc.intercalate(Doc.line, parts)
      case Lambda(args, body) =>
        Doc.char('\\') + Doc.intercalate(Doc.text(", "), args.toList.map(Doc.text _)) + Doc.text(" -> ") + body.toDoc
      case LiteralBool(b) => if (b) trueDoc else falseDoc
      case LiteralInt(str) => Doc.text(str)
      case Match(typeName, args) =>
        val pid = Document[Padding[Indented[Declaration]]]
        implicit val patDoc: Document[(Pattern, Padding[Indented[Declaration]])] =
          Document.instance[(Pattern, Padding[Indented[Declaration]])] {
            case (pat, decl) =>
              Document[Pattern].document(pat) + Doc.text(":") + Doc.line + pid.document(decl)
          }
        val piPat = Document[Padding[Indented[(Pattern, Padding[Indented[Declaration]])]]]
        Doc.text("match ") + typeName.toDoc + Doc.char(':') + Doc.line +
          Doc.intercalate(Doc.line, args.toList.map(piPat.document _))
      case Op(left, op, right) =>
        left.toDoc + Doc.space + Doc.text(op.asString) + Doc.space + right.toDoc
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case Var(name) => Doc.text(name)
      case FfiLambda(lang, callsite, tpe) =>
        Doc.intercalate(Doc.space,
          List(Doc.text("ffi"), Doc.text(lang), Doc.text(callsite), Document[TypeRef].document(tpe)))
    }
  }

  def toExpr(pn: PackageName): Expr[Declaration] =
    this match {
      case Apply(fn, args, _) =>
        @annotation.tailrec
        def loop(fn: Expr[Declaration], args: List[Expr[Declaration]]): Expr[Declaration] =
          args match {
            case Nil => fn
            case h :: tail =>
              loop(Expr.App(fn, h, this), tail)
          }
        loop(fn.toExpr(pn), args.toList.map(_.toExpr(pn)))
      case Binding(BindingStatement(arg, value, Padding(_, dec))) =>
        Expr.Let(arg, value.toExpr(pn), dec.toExpr(pn), this)
      case Comment(CommentStatement(_, Padding(_, decl))) =>
        decl.toExpr(pn).map(_ => this)
      case Constructor(name) =>
        Expr.Var(name, this)
      case DefFn(DefStatement(nm, args, _, (Padding(_, Indented(_, body)), Padding(_, in)))) =>
        val lambda = buildLambda(args.map(_._1), body.toExpr(pn), this)
        val inExpr = in.toExpr(pn)
        Expr.Let(nm, lambda, inExpr, this)
      case IfElse(ifCases, Padding(_, Indented(_, elseCase))) =>
        def loop(ifs: NonEmptyList[(Expr[Declaration], Expr[Declaration])], elseC: Expr[Declaration]): Expr[Declaration] =
          ifs match {
            case NonEmptyList((cond, ifTrue), Nil) =>
              Expr.If(cond, ifTrue, elseC, this)
            case NonEmptyList(ifTrue, h :: tail) =>
              val elseC1 = loop(NonEmptyList(h, tail), elseC)
              loop(NonEmptyList.of(ifTrue), elseC1)
          }
        loop(ifCases.map { case (d0, Padding(_, Indented(_, d1))) => (d0.toExpr(pn), d1.toExpr(pn)) }, elseCase.toExpr(pn))
      case Lambda(args, body) =>
        buildLambda(args, body.toExpr(pn), this)
      case LiteralBool(b) =>
        Expr.Literal(Lit.Bool(b), this)
      case LiteralInt(str) =>
        Expr.Literal(Lit.Integer(str.toInt), this) // TODO use BigInt
      case Op(left, op, right) =>
        Expr.Op(left.toExpr(pn), op, right.toExpr(pn), this)
      case Parens(p) =>
        p.toExpr(pn).map(_ => this)
      case Var(name) =>
        Expr.Var(name, this)
      case FfiLambda(lang, callsite, tpe) =>
        Expr.Ffi(lang, callsite, Scheme.typeConstructor(tpe.toType(pn)), this)
      case Match(arg, branches) =>
        val expBranches = branches.map { case Padding(_, Indented(_, (Pattern(nm, bs), Padding(_, Indented(_, decl))))) =>
          (ConstructorName(nm), bs, decl.toExpr(pn))
        }
        Expr.Match(arg.toExpr(pn), expBranches, this)
    }
}

object Declaration {
  private val trueDoc = Doc.text("True")
  private val falseDoc = Doc.text("False")

  implicit val document: Document[Declaration] = Document.instance[Declaration](_.toDoc)

  def buildLambda(args: NonEmptyList[String], body: Expr[Declaration], outer: Declaration): Expr.Lambda[Declaration] =
    args match {
      case NonEmptyList(arg, Nil) =>
        Expr.Lambda(arg, body, outer)
      case NonEmptyList(arg, h :: tail) =>
        val body1 = buildLambda(NonEmptyList(h, tail), body, outer)
        buildLambda(NonEmptyList.of(arg), body1, outer)
    }

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration], useDotApply: Boolean) extends Declaration
  case class Binding(binding: BindingStatement[Padding[Declaration]]) extends Declaration
  case class Comment(comment: CommentStatement[Padding[Declaration]]) extends Declaration
  case class Constructor(name: String) extends Declaration
  case class DefFn(deffn: DefStatement[(Padding[Indented[Declaration]], Padding[Declaration])]) extends Declaration
  case class FfiLambda(lang: String, callsite: String, tpe: TypeRef) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(Declaration, Padding[Indented[Declaration]])], elseCase: Padding[Indented[Declaration]]) extends Declaration
  case class Lambda(args: NonEmptyList[String], body: Declaration) extends Declaration
  case class LiteralBool(toBoolean: Boolean) extends Declaration
  case class LiteralInt(asString: String) extends Declaration
  case class Match(arg: Declaration, cases: NonEmptyList[Padding[Indented[(Pattern, Padding[Indented[Declaration]])]]]) extends Declaration
  case class Op(left: Declaration, op: Operator, right: Declaration) extends Declaration
  case class Parens(of: Declaration) extends Declaration
  case class Var(name: String) extends Declaration

  // This is something we check after variables
  private def bindingOp(indent: String): P[String => Binding] = {
    val eqP = P("=" ~ !"=")

    val restParser = Padding.parser(P(indent ~ parser(indent)))
    P(maybeSpace ~ eqP ~/ maybeSpace ~ parser(indent) ~ maybeSpace ~ "\n" ~ restParser)
      .map { case (value, rest) =>

        { str: String => Binding(BindingStatement(str, value, rest)) }
      }
  }

  val constructorP: P[Constructor] =
    upperIdent.map(Constructor(_))

  def commentP(indent: String): P[Comment] =
    CommentStatement.parser(indent, Padding.parser(P(indent ~ parser(indent))))
      .map(Comment(_))

  private def padIn[T](indent: String)(fn: String => P[T]): P[Padding[Indented[T]]] =
    P(Padding.parser(P(indent ~ Indented.parser(fn))))

  private def padInP(indent: String): P[Padding[Indented[Declaration]]] =
    padIn(indent) { nextId => parser(nextId + indent) }

  private def exactlyIn[T](indent: String, nextId: String, p: P[T]): P[Padding[Indented[T]]] =
    Padding.parser(P(indent ~ Indented.exactly(nextId, p)))

  def defP(indent: String): P[DefFn] = {
    val restParser: P[(Padding[Indented[Declaration]], Padding[Declaration])] =
      P(padInP(indent) ~ toEOL ~ Padding.parser(indent ~ parser(indent)))

    DefStatement.parser(restParser).map(DefFn(_))
  }

  val ffiP: P[FfiLambda] =
    P("ffi" ~ spaces ~/ Parser.nonSpaces ~/ spaces ~/ Parser.nonSpaces ~/ spaces ~ TypeRef.parser)
      .map { case (l, c, t) => FfiLambda(l, c, t) }

  def ifElseP(indent: String): P[IfElse] = {
    // prefix should be strict identifier like "if " or "elif "
    def blockPart[T, U](prefix: String, condition: P[T], body: P[U]): P[(T, U)] =
      P(prefix ~/ maybeSpace ~ condition ~ maybeSpace ~ ":" ~/ toEOL ~ body)

    val ifDecl: P[Padding[Indented[(Declaration, String)]]] =
      padIn(indent) { nextId => parser(indent + nextId).map((_, nextId)) }

    def restDecl(str: String): P[Padding[Indented[Declaration]]] =
      exactlyIn(indent, str, parser(indent + str))

    val lazyCond = P(parser(indent))
    val ifP: P[(Declaration, Padding[Indented[(Declaration, String)]])] =
      blockPart("if ", lazyCond, ifDecl)

    ifP.flatMap {
      case (cond, Padding(p, Indented(c, (decl, i)))) =>
        val ifcase = (cond, Padding(p, Indented(c, decl)))
        val nextDecls = restDecl(i)

        val elifP = blockPart("elif ", lazyCond, nextDecls)
        val elseP = P("\n" ~ indent ~ "else" ~ maybeSpace ~ ":" ~/ toEOL ~ nextDecls)
        P(("\n" ~ indent ~ elifP).rep() ~ elseP)
          .map { case (tail, end) =>
            IfElse(NonEmptyList(ifcase, tail.toList), end)
          }
    }
  }

  def lambdaP(indent: String): P[Lambda] = {
    val body0 = P(maybeSpace ~ parser(indent))
    val body1 = P(toEOL ~/ padInP(indent).map(_.padded.value))
    P("\\" ~/ maybeSpace ~ lowerIdent.nonEmptyList ~ maybeSpace ~ "->" ~/ (body0 | body1))
      .map { case (args, body) => Lambda(args, body) }
  }

  val literalBoolP: P[LiteralBool] =
    Parser.tokenP("True", LiteralBool(true)) | Parser.tokenP("False", LiteralBool(false))

  val literalIntP: P[LiteralInt] =
    Parser.integerString.map(LiteralInt(_))

  def matchP(indent: String): P[Match] = {
    val firstCase: P[Padding[Indented[(Pattern, String, Padding[Indented[Declaration]])]]] = {
      def inner(str: String): P[(Pattern, String, Padding[Indented[Declaration]])] =
        P(Pattern.parser ~ ":" ~/ toEOL ~ padInP(indent + str))
          .map { case (p, pidoc) => (p, str, pidoc) }
      padIn(indent)(inner)
    }

    def restCases(i: String): P[Padding[Indented[(Pattern, Padding[Indented[Declaration]])]]] =
      exactlyIn(indent, i,
        P(Pattern.parser ~ ":" ~/ toEOL ~ padInP(indent + i)))

    P("match" ~ spaces ~/ parser(indent) ~ maybeSpace ~ ":" ~ toEOL ~ firstCase)
      .flatMap { case (exp, Padding(pad, Indented(i, (p1, ind, dec1)))) =>
        (toEOL ~ restCases(ind)).rep()
          .map { rest =>
            val head = Padding(pad, Indented(i, (p1, dec1)))
            Match(exp, NonEmptyList(head, rest.toList))
          }
      }
  }

  val varP: P[Var] =
    lowerIdent.map(Var(_))

  private def varOrBind(indent: String): P[Declaration] =
    P(varP ~ bindingOp(indent).?)
      .map {
        case (v, None) => v
        case (Var(v), Some(fn)) => fn(v)
      }

  private[this] val parserCache: String => P[Declaration] =
    Memoize.function[String, P[Declaration]] { (indent, rec) =>

      val postOperators: List[P[Declaration => Declaration]] = {
        val params = P(rec(indent).nonEmptyList.parens)
        val dotApply =
          P("." ~/ varP ~ params.?).map { case (fn, argsOpt) =>
            val args = argsOpt.fold(List.empty[Declaration])(_.toList)

            { head: Declaration => Apply(fn, NonEmptyList(head, args), true) }
          }

        val applySuffix = params.map { args =>
          Apply(_: Declaration, args, false)
        }

        def parseOp(o: Operator): P[Declaration => Declaration] =
          P(maybeSpace ~ o.asString ~/ maybeSpace ~ rec(indent)).map { right => Op(_, o, right) }

        dotApply :: applySuffix :: Operator.allOps.map(parseOp _)
      }
      val prefix = defP(indent) | literalIntP | literalBoolP | lambdaP(indent) | matchP(indent) |
        ifElseP(indent) | ffiP | varOrBind(indent) | constructorP | commentP(indent) | P(rec(indent).parens).map(Parens(_))

      val opsList = postOperators.reduce(_ | _).rep().map(_.toList)

      @annotation.tailrec
      def loop[A](a: A, fns: List[A => A]): A =
        fns match {
          case Nil => a
          case h :: tail => loop(h(a), tail)
        }

      P(prefix ~ opsList).map { case (arg, fns) => loop(arg, fns) }
    }

  def parser(indent: String): P[Declaration] =
    parserCache(indent)
}
