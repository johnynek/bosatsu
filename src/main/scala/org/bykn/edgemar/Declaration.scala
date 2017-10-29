package org.bykn.edgemar

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace, spaces }
import cats.data.NonEmptyList
import cats.implicits._
import com.stripe.dagon.Memoize
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

case class DefStatement[T](
    name: String,
    args: NonEmptyList[(String, Option[TypeRef])],
    retType: Option[TypeRef], result: T)

object DefStatement {
  private[this] val defDoc = Doc.text("def ")

  implicit def document[T: Document]: Document[DefStatement[T]] =
    Document.instance[DefStatement[T]] { defs =>
      import defs._
      val res = retType.fold(Doc.empty) { t => Doc.text(" -> ") + t.toDoc }
      val line0 = defDoc + Doc.text(name) + Doc.char('(') +
        Doc.intercalate(Doc.text(", "), args.toList.map(TypeRef.argDoc _)) +
        Doc.char(')') + res + Doc.text(":") + Doc.line
      line0 + Document[T].document(result)
    }

    /**
     * The resultTParser should parse some indentation
     */
    def parser[T](resultTParser: P[T]): P[DefStatement[T]] = {
      val args = argParser.nonEmptyList
      val result = P(maybeSpace ~ "->" ~/ maybeSpace ~ TypeRef.parser ~ maybeSpace).?
      P("def" ~ spaces ~/ Parser.lowerIdent ~ "(" ~ maybeSpace ~ args ~ maybeSpace ~ ")" ~
        result ~ ":" ~ "\n" ~ resultTParser)
        .map {
          case (name, args, resType, res) => DefStatement(name, args, resType, res)
        }
    }

    val argParser: P[(String, Option[TypeRef])] =
      P(lowerIdent ~ (":" ~/ maybeSpace ~ TypeRef.parser).?)
}

case class BindingStatement[T](name: String, value: Declaration, in: T)

object BindingStatement {
  private[this] val eqDoc = Doc.text(" = ")

  implicit def document[T: Document]: Document[BindingStatement[T]] =
    Document.instance[BindingStatement[T]] { let =>
      import let._
      Doc.text(name) + eqDoc + value.toDoc + Doc.line + Document[T].document(in)
    }
}

case class CommentStatement[T](message: NonEmptyList[String], on: T)

object CommentStatement {
  type Maybe[T] = Either[T, CommentStatement[T]]

  implicit def document[T: Document]: Document[CommentStatement[T]] =
    Document.instance[CommentStatement[T]] { comment =>
      import comment._
      val block = Doc.intercalate(Doc.line, message.toList.map { mes => Doc.char('#') + Doc.text(mes) })
      block + Doc.line + Document[T].document(on)
    }

  implicit def maybeDocument[T: Document]: Document[Maybe[T]] = {
    val docT = Document[T]
    Document.instance[Maybe[T]] {
      case Left(t) => docT.document(t)
      case Right(c) => document[T](docT).document(c)
    }
  }

  /** on should make sure indent is matching
   * this is to allow a P[Unit] that does nothing for testing or other applications
   */
  def parser[T](indent: String, onP: P[T]): P[CommentStatement[T]] = {
    val commentPart: P[String] = P("#" ~/ CharsWhile(_ != '\n').?.!)
    val commentBlock: P[NonEmptyList[String]] =
      P(commentPart ~ ("\n" ~ indent ~ commentPart).rep() ~ ("\n" | End))
        .map { case (c1, cs) => NonEmptyList(c1, cs.toList) }

    P(commentBlock ~ onP)
      .map { case (m, on) => CommentStatement(m, on) }
  }

  def maybeParser[T](indent: String, onP: P[T]): P[Maybe[T]] =
    parser(indent, onP).map(Right(_)) | (onP.map(Left(_)))
}

case class PackageStatement[T](name: String,
  exports: List[String],
  emptyLines: Int,
  contents: T)

// Represents vertical padding
case class Padding[T](lines: Int, padded: T)
object Padding {
  implicit def document[T: Document]: Document[Padding[T]] =
    Document.instance[Padding[T]] { padding =>
      Doc.line.repeat(padding.lines) + Document[T].document(padding.padded)
    }

  def parser[T](p: P[T]): P[Padding[T]] =
    P((maybeSpace ~ "\n").!.rep() ~ p).map { case (vec, t) => Padding(vec.size, t) }
}

case class Indented[T](spaces: Int, value: T) {
  require(spaces > 0, s"need non-empty indentation: $spaces")
}

object Indented {
  def spaceCount(str: String): Int =
    str.foldLeft(0) {
      case (s, ' ') => s + 1
      case (s, '\t') => s + 4
      case (_, c) => sys.error(s"unexpected space character($c) in $str")
    }

  implicit def document[T: Document]: Document[Indented[T]] =
    Document.instance[Indented[T]] { case Indented(i, t) =>
      Doc.spaces(i) + (Document[T].document(t).nested(i))
    }

  def parser[T](p: String => P[T]): P[Indented[T]] =
    Parser.indented { i =>
      p(i).map(Indented(spaceCount(i), _))
    }

  def exactly[T](str: String, p: P[T]): P[Indented[T]] = {
    val sc = spaceCount(str)
    P(str ~ p).map(Indented(sc, _))
  }
}

sealed abstract class Statement {

  final def toProgram: Program[Declaration, Statement] = {
    import Statement._

    def loop(s: Statement): Program[Declaration, Statement] =
      s match {
        case Bind(BindingStatement(nm, decl, Padding(_, rest))) =>
          val Program(binds, _) = loop(rest)
          Program((nm, decl.toExpr) :: binds, this)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(DefStatement(nm, args, _, (Padding(_, Indented(_, body)), Padding(_, in)))) =>
          // using body for the outer here is a bummer, but not really a good outer otherwise
          val eBody = body.toExpr
          val lam = Declaration.buildLambda(args.map(_._1), eBody, body)
          val Program(binds, _) = loop(in)
          Program((nm, lam) :: binds, this)
        case Struct(_, _, Padding(_, rest)) =>
          // TODO
          loop(rest)
        case Enum(_, _, Padding(_, rest)) =>
          // TODO
          loop(rest)
        case EndOfFile =>
          Program(Nil, EndOfFile)
      }

    loop(this)
  }
}

object Statement {
  case class Bind(bind: BindingStatement[Padding[Statement]]) extends Statement
  case class Comment(comment: CommentStatement[Padding[Statement]]) extends Statement
  case class Def(defstatement: DefStatement[(Padding[Indented[Declaration]], Padding[Statement])]) extends Statement
  case class Struct(name: String, args: List[(String, Option[TypeRef])], rest: Padding[Statement]) extends Statement
  case class Enum(name: String,
    items: NonEmptyList[Padding[Indented[(String, List[(String, Option[TypeRef])])]]],
    rest: Padding[Statement]) extends Statement
  case object EndOfFile extends Statement

  // this is a REPL test method we will remove
  // later.
  def infer(str: String): Option[(Any, Scheme)] = {
    parser.parse(str) match {
       case Parsed.Success(stmt, idx) if str.length == idx =>
         val prog = stmt.toProgram
         prog.getMainDecl.map { expr =>
           Expr.evaluate(expr).right.get
         }
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx with trace: ${extra.traced.trace}")
    }
  }

  val parser: P[Statement] = {
    val toEOL = P(maybeSpace ~ "\n")

    val bP = P(Parser.lowerIdent ~ maybeSpace ~ "=" ~/ maybeSpace ~ Declaration.parser("") ~ toEOL ~ Padding.parser(parser))
      .map { case (ident, value, rest) => Bind(BindingStatement(ident, value, rest)) }

    val cP = CommentStatement.parser("", P(Padding.parser(parser))).map(Comment(_))

    val defBody = Padding.parser(Indented.parser(Declaration.parser(_)))
    val dP: P[Def] = DefStatement.parser(P(defBody ~ "\n" ~ Padding.parser(parser))).map(Def(_))

    val end = P(End).map(_ => EndOfFile)

    val constructorP = P(Parser.upperIdent ~ (DefStatement.argParser).list.parens.? ~ toEOL)
      .map {
        case (n, None) => (n, Nil)
        case (n, Some(args)) => (n, args)
      }

    val struct = P("struct" ~ spaces ~/ constructorP ~ Padding.parser(parser))
      .map { case (name, args, rest) => Struct(name, args, rest) }

    val enum = P("enum" ~ spaces ~/ Parser.upperIdent ~/ ":" ~ toEOL ~ Padding.parser(Indented.parser { i => constructorP.map((_, i)) }))
      .flatMap { case (ename, Padding(p, Indented(i, (head, indent)))) =>
        P(Padding.parser(Indented.exactly(indent, constructorP)).rep() ~ Padding.parser(parser))
          .map { case (tail, rest) =>
            Enum(ename, NonEmptyList(Padding(p, Indented(i, head)), tail.toList), rest)
          }
      }

    // bP should come last so there is no ambiguity about identifiers
    cP | dP | struct | enum | bP | end
  }

  private def constructor(name: String, args: List[(String, Option[TypeRef])]): Doc =
    Doc.text(name) +
      (if (args.nonEmpty) { Doc.char('(') + Doc.intercalate(Doc.text(", "), args.toList.map(TypeRef.argDoc _)) + Doc.char(')') }
      else Doc.empty)

  implicit lazy val document: Document[Statement] =
    Document.instance[Statement] {
      case Bind(bs) =>
        Document[BindingStatement[Padding[Statement]]].document(bs)
      case Comment(cm) =>
        Document[CommentStatement[Padding[Statement]]].document(cm)
      case Def(d) =>
        val pair = Document.instance[(Padding[Indented[Declaration]], Padding[Statement])] {
          case (body, next) =>
            Document[Padding[Indented[Declaration]]].document(body) +
              Doc.line +
              Document[Padding[Statement]].document(next)
        };
        DefStatement.document(pair).document(d)
      case Struct(nm, args, rest) =>
        Doc.text("struct ") + constructor(nm, args) + Doc.line +
          Document[Padding[Statement]].document(rest)
      case Enum(nm, parts, rest) =>
        implicit val consDoc = Document.instance[(String, List[(String, Option[TypeRef])])] {
          case (nm, parts) => constructor(nm, parts)
        }

        val indentedCons =
          Doc.intercalate(Doc.line, parts.toList.map { cons =>
            Document[Padding[Indented[(String, List[(String, Option[TypeRef])])]]].document(cons)
          })

        Doc.text("enum ") + Doc.text(nm) + Doc.char(':') +
          Doc.line +
          indentedCons +
          Doc.line +
          Document[Padding[Statement]].document(rest)
      case EndOfFile => Doc.empty
    }
}

sealed abstract class TypeRef {
  import TypeRef._

  def toDoc: Doc =
    this match {
      case TypeVar(v) => Doc.text(v)
      case TypeName(n) => Doc.text(n)
      case TypeArrow(inner@TypeArrow(_, _), right) =>
        Doc.char('(') + inner.toDoc + Doc.char(')') + spaceArrow + right.toDoc
      case TypeArrow(left, right) =>
        left.toDoc + spaceArrow + right.toDoc
      case TypeApply(of, args) =>
        of.toDoc + Doc.char('[') + Doc.intercalate(commaSpace, args.toList.map(_.toDoc)) + Doc.char(']')
    }
}

object TypeRef {
  private val spaceArrow = Doc.text(" -> ")
  private val commaSpace = Doc.text(", ")
  private val colonSpace = Doc.text(": ")

  implicit val document: Document[TypeRef] = Document.instance[TypeRef](_.toDoc)

  def argDoc(st: (String, Option[TypeRef])): Doc =
    st match {
      case (s, None) => Doc.text(s)
      case (s, Some(tr)) => Doc.text(s) + colonSpace + (tr.toDoc)
    }

  case class TypeVar(asString: String) extends TypeRef {
    require(asString.charAt(0).isLower)
  }
  case class TypeName(asString: String) extends TypeRef {
    require(asString.charAt(0).isUpper)
  }
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef

  lazy val parser: P[TypeRef] = {
    val tvar = lowerIdent.map(TypeVar(_))
    val tname = upperIdent.map(TypeName(_))

    val maybeArrow: P[TypeRef => TypeRef] =
      P((maybeSpace ~ "->" ~/ maybeSpace ~ parser))
        .map { right => TypeArrow(_, right) }

    val maybeApp: P[TypeRef => TypeRef] =
      P(("[" ~/ maybeSpace ~ parser.nonEmptyList ~ maybeSpace ~ "]"))
        .map { args => TypeApply(_, args) }

    ((tvar | tname | P(parser.parens)) ~ maybeApp.? ~ maybeArrow.?)
      .map {
        case (t, optF1, optF2) =>
          val t1 = optF1.fold(t)(_(t))
          optF2.fold(t1)(_(t1))
      }
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
      case Apply(fn, args) =>
        val fnDoc = fn match {
          case Var(n) => Doc.text(n)
          case p@Parens(_) => p.toDoc
          case other => Doc.char('(') + other.toDoc + Doc.char(')')
        }
        fnDoc + Doc.char('(') + Doc.intercalate(Doc.text(", "), args.toList.map(_.toDoc)) + Doc.char(')')
      case Binding(b) =>
        BindingStatement.document[Padding[Declaration]].document(b)
      case Comment(c) =>
        CommentStatement.document[Padding[Declaration]].document(c)
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
      case Op(left, op, right) =>
        left.toDoc + Doc.space + Doc.text(op.asString) + Doc.space + right.toDoc
      case Parens(p) =>
        Doc.char('(') + p.toDoc + Doc.char(')')
      case Var(name) => Doc.text(name)
      case FfiLambda(_, _, _) => ???
    }
  }

  def toExpr: Expr[Declaration] =
    this match {
      case Apply(fn, args) =>
        @annotation.tailrec
        def loop(fn: Expr[Declaration], args: List[Expr[Declaration]]): Expr[Declaration] =
          args match {
            case Nil => fn
            case h :: tail =>
              loop(Expr.App(fn, h, this), tail)
          }
        loop(fn.toExpr, args.toList.map(_.toExpr))
      case Binding(BindingStatement(arg, value, Padding(_, dec))) =>
        Expr.Let(arg, value.toExpr, dec.toExpr, this)
      case Comment(CommentStatement(_, Padding(_, decl))) =>
        decl.toExpr.map(_ => this)
      case DefFn(DefStatement(nm, args, _, (Padding(_, Indented(_, body)), Padding(_, in)))) =>
        val lambda = buildLambda(args.map(_._1), body.toExpr, this)
        val inExpr = in.toExpr
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
        loop(ifCases.map { case (d0, Padding(_, Indented(_, d1))) => (d0.toExpr, d1.toExpr) }, elseCase.toExpr)
      case Lambda(args, body) =>
        buildLambda(args, body.toExpr, this)
      case LiteralBool(b) =>
        Expr.Literal(Lit.Bool(b), this)
      case LiteralInt(str) =>
        Expr.Literal(Lit.Integer(str.toInt), this) // TODO use BigInt
      case Op(left, op, right) =>
        Expr.Op(left.toExpr, op, right.toExpr, this)
      case Parens(p) =>
        p.toExpr.map(_ => this)
      case Var(name) =>
        Expr.Var(name, this)
      case FfiLambda(_, _, _) => ???
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

  case class Apply(fn: Declaration, args: NonEmptyList[Declaration]) extends Declaration
  case class Binding(binding: BindingStatement[Padding[Declaration]]) extends Declaration
  case class Comment(comment: CommentStatement[Padding[Declaration]]) extends Declaration
  case class DefFn(deffn: DefStatement[(Padding[Indented[Declaration]], Padding[Declaration])]) extends Declaration
  case class FfiLambda(lang: String, callsite: String, tpe: TypeRef) extends Declaration
  case class IfElse(ifCases: NonEmptyList[(Declaration, Padding[Indented[Declaration]])], elseCase: Padding[Indented[Declaration]]) extends Declaration
  case class Lambda(args: NonEmptyList[String], body: Declaration) extends Declaration
  case class LiteralBool(toBoolean: Boolean) extends Declaration
  case class LiteralInt(asString: String) extends Declaration
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

  def commentP(indent: String): P[Comment] =
    CommentStatement.parser(indent, Padding.parser(P(indent ~ parser(indent))))
      .map(Comment(_))

  def defP(indent: String): P[DefFn] = {
    val restParser: P[(Padding[Indented[Declaration]], Padding[Declaration])] =
      P(Padding.parser(P(indent ~ Indented.parser { nextId => parser (indent + nextId) })) ~ maybeSpace ~ "\n" ~
        Padding.parser(indent ~ parser(indent)))

    DefStatement.parser(restParser).map(DefFn(_))
  }

  def ifElseP(indent: String): P[IfElse] = {
    val toEOL = P(maybeSpace ~ "\n")
    // prefix should be strict identifier like "if " or "elif "
    def blockPart[T, U](prefix: String, condition: P[T], body: P[U]): P[(T, U)] =
      P(prefix ~/ maybeSpace ~ condition ~ maybeSpace ~ ":" ~/ toEOL ~ body)

    val ifDecl: P[Padding[Indented[(Declaration, String)]]] =
      Padding.parser(P(indent ~ Indented.parser { nextId => parser(indent + nextId).map((_, nextId)) }))

    def restDecl(str: String): P[Padding[Indented[Declaration]]] =
      Padding.parser(P(indent ~ Indented.exactly(str, parser(indent + str))))

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

  def lambdaP(indent: String): P[Lambda] =
    P("\\" ~/ maybeSpace ~ lowerIdent.nonEmptyList ~ maybeSpace ~ "->" ~/ maybeSpace ~ parser(indent))
      .map { case (args, body) => Lambda(args, body) }

  val literalBoolP: P[LiteralBool] =
    Parser.tokenP("True", LiteralBool(true)) | Parser.tokenP("False", LiteralBool(false))

  val literalIntP: P[LiteralInt] =
    Parser.integerString.map(LiteralInt(_))

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
        val applySuffix = P(rec(indent).nonEmptyList.parens).map { args => Apply(_: Declaration, args) }

        def parseOp(o: Operator): P[Declaration => Declaration] =
          P(maybeSpace ~ o.asString ~/ maybeSpace ~ rec(indent)).map { right => Op(_, o, right) }

        applySuffix :: Operator.allOps.map(parseOp _)
      }
      val prefix = defP(indent) | literalIntP | literalBoolP | lambdaP(indent) |
        ifElseP(indent) | varOrBind(indent) | commentP(indent) | P(rec(indent).parens).map(Parens(_))

      def checkOps(head: P[Declaration], ops: List[P[Declaration => Declaration]]): P[Declaration] =
        ops match {
          case Nil => head
          case h :: tail =>
            val h1 = P(head ~ h.?).map {
              case (h, None) => h
              case (h, Some(f)) => f(h)
            }
            checkOps(h1, tail)
        }

      checkOps(prefix, postOperators)
    }

  def parser(indent: String): P[Declaration] =
    parserCache(indent)
}
