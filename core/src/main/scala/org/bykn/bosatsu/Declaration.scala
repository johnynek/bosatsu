package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace, spaces, escapedString, toEOL }
import cats.data.NonEmptyList
import cats.implicits._
import com.stripe.dagon.Memoize
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

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
      case LiteralInt(str) => Doc.text(str)
      case LiteralString(str, q) =>
          Doc.char(q) + Doc.text(Parser.escape(Set(q), str)) + Doc.char(q)
      case Match(typeName, args) =>
        val pid = Document[Padding[Indented[Declaration]]]
        implicit val patDoc: Document[(Pattern[String, TypeRef], Padding[Indented[Declaration]])] =
          Document.instance[(Pattern[String, TypeRef], Padding[Indented[Declaration]])] {
            case (pat, decl) =>
              Document[Pattern[String, TypeRef]].document(pat) + Doc.text(":") + Doc.line + pid.document(decl)
          }
        val piPat = Document[Padding[Indented[(Pattern[String, TypeRef], Padding[Indented[Declaration]])]]]
        Doc.text("match ") + typeName.toDoc + Doc.char(':') + Doc.line +
          Doc.intercalate(Doc.line, args.toList.map(piPat.document _))
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case Var(name) => Doc.text(name)
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
            case (Padding(_, Indented(_, body)), Padding(_, in)) =>
              (loop(body), loop(in))
          }
          val lambda = defstmt.toLambdaExpr(bodyExpr, this)(_.toType(nameToType))
          Expr.Let(defstmt.name, lambda, inExpr, this)
        case IfElse(ifCases, Padding(_, Indented(_, elseCase))) =>

          // TODO: we need a way to have an full name to the constructor in order for this "macro" to
          // be safe. So, we want to say Bosatsu/Predef#True or something.
          // we could just have ConstructorName require a PackageName
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
          loop0(ifCases.map { case (d0, Padding(_, Indented(_, d1))) =>
            (loop(d0), loop(d1))
          }, loop(elseCase))
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
          val expBranches = branches.map { case Padding(_, Indented(_, (pat, Padding(_, Indented(_, decl))))) =>
            val newPattern = pat.mapName(nameToCons).mapType(_.toType(nameToType))
            (newPattern, loop(decl))
          }
          Expr.Match(loop(arg), expBranches, this)
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
  case class DefFn(deffn: DefStatement[(Padding[Indented[Declaration]], Padding[Declaration])])(implicit val region: Region) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(Declaration, Padding[Indented[Declaration]])],
    elseCase: Padding[Indented[Declaration]])(implicit val region: Region) extends Declaration
  case class Lambda(args: NonEmptyList[String], body: Declaration)(implicit val region: Region) extends Declaration
  case class LiteralInt(asString: String)(implicit val region: Region) extends Declaration
  case class LiteralString(asString: String, quoteChar: Char)(implicit val region: Region) extends Declaration
  case class Match(arg: Declaration,
    cases: NonEmptyList[Padding[Indented[(Pattern[String, TypeRef], Padding[Indented[Declaration]])]]])(
    implicit val region: Region) extends Declaration
  case class Parens(of: Declaration)(implicit val region: Region) extends Declaration
  case class Var(name: String)(implicit val region: Region) extends Declaration

  // This is something we check after variables
  private def bindingOp(indent: String): P[(String, Region) => Binding] = {
    val eqP = P("=" ~ !"=")

    val restParser = Padding.parser(P(indent ~ parser(indent)))
    P(maybeSpace ~ eqP ~/ maybeSpace ~ parser(indent) ~ maybeSpace ~ "\n" ~ restParser)
      .region
      .map { case (region, (value, rest)) =>

        { (str: String, r: Region) => Binding(BindingStatement(str, value, rest))(r + region) }
      }
  }

  val constructorP: P[Constructor] =
    upperIdent.region.map { case (r, c) => Constructor(c)(r) }

  def commentP(indent: String): P[Comment] =
    CommentStatement.parser(indent, Padding.parser(P(indent ~ parser(indent))))
      .region
      .map { case (r, c) => Comment(c)(r) }

  private def padIn[T](indent: String)(fn: String => P[T]): P[Padding[Indented[T]]] =
    P(Padding.parser(P(indent ~ Indented.parser(fn))))

  private def padInP(indent: String): P[Padding[Indented[Declaration]]] =
    padIn(indent) { nextId => parser(nextId + indent) }

  private def exactlyIn[T](indent: String, nextId: String, p: P[T]): P[Padding[Indented[T]]] =
    Padding.parser(P(indent ~ Indented.exactly(nextId, p)))

  def defP(indent: String): P[DefFn] = {
    val restParser: P[(Padding[Indented[Declaration]], Padding[Declaration])] =
      P(padInP(indent) ~ toEOL ~ Padding.parser(indent ~ parser(indent)))

    DefStatement.parser(restParser)
      .region
      .map { case (r, d) => DefFn(d)(r) }
  }

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

    ifP.region.flatMap {
      case (r1, (cond, Padding(p, Indented(c, (decl, i))))) =>
        val ifcase = (cond, Padding(p, Indented(c, decl)))
        val nextDecls = restDecl(i)

        val elifP = blockPart("elif ", lazyCond, nextDecls)
        val elseP = P("\n" ~ indent ~ "else" ~ maybeSpace ~ ":" ~/ toEOL ~ nextDecls)
        P(("\n" ~ indent ~ elifP).rep() ~ elseP)
          .region
          .map { case (r2, (tail, end)) =>
            IfElse(NonEmptyList(ifcase, tail.toList), end)(r1 + r2)
          }
    }
  }

  def lambdaP(indent: String): P[Lambda] = {
    val body0 = P(maybeSpace ~ parser(indent))
    val body1 = P(toEOL ~/ padInP(indent).map(_.padded.value))
    P("\\" ~/ maybeSpace ~ lowerIdent.nonEmptyList ~ maybeSpace ~ "->" ~/ (body0 | body1))
      .region
      .map { case (r, (args, body)) => Lambda(args, body)(r) }
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

  def matchP(indent: String): P[Match] = {
    val firstCase: P[Padding[Indented[(Pattern[String, TypeRef], String, Padding[Indented[Declaration]])]]] = {
      def inner(str: String): P[(Pattern[String, TypeRef], String, Padding[Indented[Declaration]])] =
        P(Pattern.parser ~ ":" ~/ toEOL ~ padInP(indent + str))
          .map { case (p, pidoc) => (p, str, pidoc) }
      padIn(indent)(inner)
    }

    def restCases(i: String): P[Padding[Indented[(Pattern[String, TypeRef], Padding[Indented[Declaration]])]]] =
      exactlyIn(indent, i,
        P(Pattern.parser ~ ":" ~/ toEOL ~ padInP(indent + i)))

    P("match" ~ spaces ~/ parser(indent) ~ maybeSpace ~ ":" ~ toEOL ~ firstCase)
      .region
      .flatMap { case (r1, (exp, Padding(pad, Indented(i, (p1, ind, dec1))))) =>
        (toEOL ~ restCases(ind)).rep()
          .region
          .map { case (r2, rest) =>
            val head = Padding(pad, Indented(i, (p1, dec1)))
            Match(exp, NonEmptyList(head, rest.toList))(r1 + r2)
          }
      }
  }

  val varP: P[Var] =
    lowerIdent.region.map { case (r, v) => Var(v)(r) }

  private def varOrBind(indent: String): P[Declaration] =
    P(varP ~ bindingOp(indent).?)
      .map {
        case (v, None) => v
        case (varD@Var(v), Some(fn)) => fn(v, varD.region)
      }

  private[this] val parserCache: String => P[Declaration] =
    Memoize.function[String, P[Declaration]] { (indent, rec) =>

      val postOperators: List[P[Declaration => Declaration]] = {
        val params = P(rec(indent).nonEmptyList.parens)
        val dotApply =
          P("." ~/ varP ~ params.?).region.map { case (r2, (fn, argsOpt)) =>
            val args = argsOpt.fold(List.empty[Declaration])(_.toList)

            { head: Declaration => Apply(fn, NonEmptyList(head, args), true)(head.region + r2) }
          }

        val applySuffix = params.region.map { case (r, args) =>

          { fn: Declaration => Apply(fn, args, false)(fn.region + r) }
        }

        dotApply :: applySuffix :: Nil
      }
      val prefix = defP(indent) | literalIntP | literalStringP | lambdaP(indent) | matchP(indent) |
        ifElseP(indent) | varOrBind(indent) | constructorP | commentP(indent) |
        P(rec(indent).parens).region.map { case (r, p) => Parens(p)(r) }

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
