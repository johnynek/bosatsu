package org.bykn.edgemar

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace, spaces }
import cats.data.{ NonEmptyList, State }
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.toEOL

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
      P("def" ~ spaces ~/ lowerIdent ~ "(" ~ maybeSpace ~ args ~ maybeSpace ~ ")" ~
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

  def toProgram(pn: PackageName): Program[Declaration, Statement] = {
    import Statement._

    def loop(s: Statement): Program[Declaration, Statement] =
      s match {
        case Bind(BindingStatement(nm, decl, Padding(_, rest))) =>
          val Program(te, binds, _) = loop(rest)
          Program(te, (nm, decl.toExpr(pn)) :: binds, this)
        case Comment(CommentStatement(_, Padding(_, on))) =>
          loop(on).copy(from = s)
        case Def(DefStatement(nm, args, _, (Padding(_, Indented(_, body)), Padding(_, in)))) =>
          // using body for the outer here is a bummer, but not really a good outer otherwise
          val eBody = body.toExpr(pn)
          val lam = Declaration.buildLambda(args.map(_._1), eBody, body)
          val Program(te, binds, _) = loop(in)
          Program(te, (nm, lam) :: binds, this)
        case s@Struct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = p.types.addDefinedType(s.toDefinition(pn)), from = s)
        case e@Enum(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = p.types.addDefinedType(e.toDefinition(pn)), from = e)
        case x@ExternalStruct(_, _, Padding(_, rest)) =>
          val p = loop(rest)
          p.copy(types = p.types.addDefinedType(x.toDefinition(pn)), from = x)
        case EndOfFile =>
          Program(TypeEnv.empty, Nil, EndOfFile)
      }

    loop(this)
  }
}

sealed abstract class TypeDefinitionStatement extends Statement {
  import Statement._

  def toDefinition(pname: PackageName): DefinedType = {
    def typeVar(i: Int): Type.Var = Type.Var(s"typeVar$i")

    type VarState[A] = State[Int, A]

    def nextVar: VarState[Type.Var] =
      for {
        id <- State.get[Int]
        _ <- State.modify[Int](_ + 1)
      } yield typeVar(id)

    def buildParam(p: (String, Option[TypeRef])): VarState[(ParamName, Type)] =
      p match {
        // TODO we need to be robust to var collisions with our gensym approach
        case (parname, Some(typeRef)) => State.pure((ParamName(parname), typeRef.toType(pname)))
        case (parname, None) => nextVar.map { v => (ParamName(parname), v) }
      }

    def buildParams(args: List[(String, Option[TypeRef])]): VarState[List[(ParamName, Type)]] =
      args.traverse(buildParam _)

    def explicits(ps: List[(ParamName, Type)]): List[Type.Var] =
      ps.flatMap { case (_, t) => t.varsIn }.distinct

    this match {
      case Struct(nm, args, _) =>
        val params = buildParams(args).runA(0).value
        val explicitTps = explicits(params)
        DefinedType(pname, TypeName(nm), explicitTps, (ConstructorName(nm), params) :: Nil)
      case Enum(nm, items, _) =>
        val constructorsS = items.traverse { case Padding(_, Indented(_, (nm, args))) =>
          buildParams(args).map((ConstructorName(nm), _))
        }

        val constructors = constructorsS.runA(0).value
        val allExplicits = explicits(constructors.map(_._2).toList.flatten)
        DefinedType(pname, TypeName(nm), allExplicits.distinct, constructors.toList)
      case ExternalStruct(nm, targs, _) =>
        DefinedType(pname, TypeName(nm), targs.map { case TypeRef.TypeVar(v) => Type.Var(v) }, Nil)
    }
  }
}

object Statement {
  case class Bind(bind: BindingStatement[Padding[Statement]]) extends Statement
  case class Comment(comment: CommentStatement[Padding[Statement]]) extends Statement
  case class Def(defstatement: DefStatement[(Padding[Indented[Declaration]], Padding[Statement])]) extends Statement
  case class Struct(name: String, args: List[(String, Option[TypeRef])], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class ExternalStruct(name: String, typeArgs: List[TypeRef.TypeVar], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class Enum(name: String,
    items: NonEmptyList[Padding[Indented[(String, List[(String, Option[TypeRef])])]]],
    rest: Padding[Statement]) extends TypeDefinitionStatement
  case object EndOfFile extends Statement

  val parser: P[Statement] = {
    val bindingP = P(lowerIdent ~ maybeSpace ~ "=" ~/ maybeSpace ~ Declaration.parser("") ~ toEOL ~ Padding.parser(parser))
      .map { case (ident, value, rest) => Bind(BindingStatement(ident, value, rest)) }

    val commentP = CommentStatement.parser("", P(Padding.parser(parser))).map(Comment(_))

    val defBody = Padding.parser(Indented.parser(Declaration.parser(_)))
    val defP: P[Def] = DefStatement.parser(P(defBody ~ "\n" ~ Padding.parser(parser))).map(Def(_))

    val end = P(End).map(_ => EndOfFile)

    val constructorP = P(upperIdent ~ (DefStatement.argParser).list.parens.? ~ toEOL)
      .map {
        case (n, None) => (n, Nil)
        case (n, Some(args)) => (n, args)
      }

    val externalStruct = P("external" ~ spaces ~/ "struct" ~/ spaces ~ upperIdent ~
      ("[" ~/ maybeSpace ~ lowerIdent.nonEmptyList ~ maybeSpace ~ "]").? ~ toEOL ~ Padding.parser(parser)).map {
        case (name, targs, rest) =>
          val tva = targs.fold(List.empty[TypeRef.TypeVar])(_.toList.map(TypeRef.TypeVar(_)))
          ExternalStruct(name, tva, rest)
      }

    val struct = P("struct" ~ spaces ~/ constructorP ~ Padding.parser(parser))
      .map { case (name, args, rest) => Struct(name, args, rest) }

    val enum = P("enum" ~ spaces ~/ upperIdent ~/ ":" ~ toEOL ~ Padding.parser(Indented.parser { i => constructorP.map((_, i)) }))
      .flatMap { case (ename, Padding(p, Indented(i, (head, indent)))) =>
        P(Padding.parser(Indented.exactly(indent, constructorP)).rep() ~ Padding.parser(parser))
          .map { case (tail, rest) =>
            Enum(ename, NonEmptyList(Padding(p, Indented(i, head)), tail.toList), rest)
          }
      }

    // bP should come last so there is no ambiguity about identifiers
    commentP | defP | struct | enum | externalStruct | bindingP | end
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
      case ExternalStruct(nm, targs, rest) =>
        val argsDoc = targs match {
          case Nil => Doc.empty
          case nonEmpty =>
            val params = nonEmpty.map { case TypeRef.TypeVar(v) => Doc.text(v) }
            Doc.char('[') + Doc.intercalate(Doc.text(", "), params) + Doc.char(']')
        }
        Doc.text("external struct ") + Doc.text(nm) + argsDoc + Doc.line +
          Document[Padding[Statement]].document(rest)
    }
}

