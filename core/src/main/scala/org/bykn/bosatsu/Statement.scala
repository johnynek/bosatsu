package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, upperIdent, maybeSpace, spaces }
import cats.Functor
import cats.data.{ NonEmptyList, State }
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.toEOL

case class DefStatement[T](
    name: String,
    args: NonEmptyList[(String, Option[TypeRef])],
    retType: Option[TypeRef], result: T) {


  /**
   * This ignores the name completely and just returns the lambda expression here
   *
   * This could be a traversal: TypeRef => F[rankn.Type] for an Applicative F[_]
   */
  def toLambdaExpr[A](resultExpr: Expr[A], tag: A)(trFn: TypeRef => rankn.Type): Expr[A] = {
    val unTypedBody = resultExpr
    val bodyExp =
      retType.fold(unTypedBody) { t =>
        Expr.Annotation(unTypedBody, trFn(t), tag)
      }
    val deepFunctor = Functor[NonEmptyList].compose[(String, ?)].compose[Option]
    Expr.buildLambda(deepFunctor.map(args)(trFn), bodyExp, tag)
  }
}

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
    val commentBlock: P[NonEmptyList[String]] =
      P(commentPart ~ ("\n" ~ indent ~ commentPart).rep() ~ ("\n" | End))
        .map { case (c1, cs) => NonEmptyList(c1, cs.toList) }

    P(commentBlock ~ onP)
      .map { case (m, on) => CommentStatement(m, on) }
  }

  val commentPart: P[String] = P("#" ~/ CharsWhile(_ != '\n').?.!)

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

  def toStream: Stream[Statement] = {
    import Statement._
    def loop(s: Statement): Stream[Statement] =
      s match {
        case Bind(BindingStatement(_, _, Padding(_, rest))) =>
          s #:: loop(rest)
        case Comment(CommentStatement(_, Padding(_, rest))) =>
          s #:: loop(rest)
        case Def(DefStatement(_, _, _, (Padding(_, Indented(_, _)), Padding(_, rest)))) =>
          s #:: loop(rest)
        case Struct(_, _, Padding(_, rest)) =>
          s #:: loop(rest)
        case Enum(_, _, Padding(_, rest)) =>
          s #:: loop(rest)
        case ExternalDef(_, _, _, Padding(_, rest)) =>
          s #:: loop(rest)
        case ExternalStruct(_, _, Padding(_, rest)) =>
          s #:: loop(rest)
        case EndOfFile =>
          s #:: Stream.empty
      }

    loop(this)
  }

}

sealed abstract class TypeDefinitionStatement extends Statement {
  import Statement._

  def toDefinition(pname: PackageName, nameToType: String => rankn.Type.Const): rankn.DefinedType = {
    import rankn.Type

    def typeVar(i: Long): Type.TyVar =
      Type.TyVar(Type.Var.Bound(s"anon$i"))

    type StT = ((Set[Type.TyVar], List[Type.TyVar]), Long)
    type VarState[A] = State[StT, A]

    def add(t: Type.TyVar): VarState[Type.TyVar] =
      State.modify[StT] { case ((ss, sl), i) => ((ss + t, t :: sl), i) }.as(t)

    lazy val nextVar: VarState[Type.TyVar] =
      for {
        vsid <- State.get[StT]
        ((existing, _), id) = vsid
        _ <- State.modify[StT] { case (s, id) => (s, id + 1L) }
        candidate = typeVar(id)
        tv <- if (existing(candidate)) nextVar else add(candidate)
      } yield tv

    def buildParam(p: (String, Option[Type])): VarState[(ParamName, Type)] =
      p match {
        case (parname, Some(tpe)) =>
          State.pure((ParamName(parname), tpe))
        case (parname, None) =>
          nextVar.map { v => (ParamName(parname), v) }
      }

    def existingVars[A](ps: List[(A, Option[Type])]): List[Type.TyVar] =
      Type.freeTyVars(ps.flatMap(_._2)).map(Type.TyVar(_))

    def buildParams(args: List[(String, Option[Type])]): VarState[List[(ParamName, Type)]] =
      args.traverse(buildParam _)

    this match {
      case Struct(nm, args, _) =>
        val deep = Functor[List].compose(Functor[(String, ?)]).compose(Functor[Option])
        val argsType = deep.map(args)(_.toType(nameToType))
        val initVars = existingVars(argsType)
        val initState = ((initVars.toSet, initVars.reverse), 0L)
        val (((_, typeVars), _), params) = buildParams(argsType).run(initState).value
        // we reverse to make sure we see in traversal order
        val typeParams = typeVars.reverse.map { tv =>
          tv.toVar match {
            case b@Type.Var.Bound(_) => b
            case unexpected => sys.error(s"unexpectedly parsed a non bound var: $unexpected")
          }
        }
        val tname = TypeName(nm)
        val consValueType =
          rankn.DefinedType
            .constructorValueType(
              pname,
              tname,
              typeParams,
              params.map(_._2))
        rankn.DefinedType(pname,
          tname,
          typeParams,
          (ConstructorName(nm), params, consValueType) :: Nil)
      case Enum(nm, items, _) =>
        val deep = Functor[List].compose(Functor[(String, ?)]).compose(Functor[Option])
        val conArgs = items.map { case Padding(_, Indented(_, (nm, args))) =>
          val argsType = deep.map(args)(_.toType(nameToType))
          (nm, argsType)
        }
        val constructorsS = conArgs.traverse { case (nm, argsType) =>
          buildParams(argsType).map { params =>
            (ConstructorName(nm), params)
          }
        }
        val initVars = existingVars(conArgs.toList.flatMap(_._2))
        val initState = ((initVars.toSet, initVars.reverse), 0L)
        val (((_, typeVars), _), constructors) = constructorsS.run(initState).value
        // we reverse to make sure we see in traversal order
        val typeParams = typeVars.reverse.map { tv =>
          tv.toVar match {
            case b@Type.Var.Bound(_) => b
            case unexpected => sys.error(s"unexpectedly parsed a non bound var: $unexpected")
          }
        }
        val tname = TypeName(nm)
        val finalCons = constructors.toList.map { case (c, params) =>
          val consValueType =
            rankn.DefinedType.constructorValueType(
              pname,
              tname,
              typeParams,
              params.map(_._2))
          (c, params, consValueType)
        }
        rankn.DefinedType(pname, TypeName(nm), typeParams, finalCons)
      case ExternalStruct(nm, targs, _) =>
        rankn.DefinedType(pname, TypeName(nm), targs.map { case TypeRef.TypeVar(v) => Type.Var.Bound(v) }, Nil)
    }
  }
}

object Statement {
  case class Bind(bind: BindingStatement[Padding[Statement]]) extends Statement
  case class Comment(comment: CommentStatement[Padding[Statement]]) extends Statement
  case class Def(defstatement: DefStatement[(Padding[Indented[Declaration]], Padding[Statement])]) extends Statement
  case class Struct(name: String, args: List[(String, Option[TypeRef])], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class ExternalDef(name: String, params: List[(String, TypeRef)], result: TypeRef, rest: Padding[Statement]) extends Statement
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

    val external = {
      val externalStruct = P("struct" ~/ spaces ~ upperIdent ~
        ("[" ~/ maybeSpace ~ lowerIdent.nonEmptyList ~ maybeSpace ~ "]").? ~ toEOL ~ Padding.parser(parser)).map {
          case (name, targs, rest) =>
            val tva = targs.fold(List.empty[TypeRef.TypeVar])(_.toList.map(TypeRef.TypeVar(_)))
            ExternalStruct(name, tva, rest)
        }

      val externalDef = {
        val argParser: P[(String, TypeRef)] = P(lowerIdent ~ ":" ~/ maybeSpace ~ TypeRef.parser)
        val args = P("(" ~ maybeSpace ~ argParser.nonEmptyList ~ maybeSpace ~ ")")
        val result = P(maybeSpace ~ "->" ~/ maybeSpace ~ TypeRef.parser ~ maybeSpace)
        P("def" ~ spaces ~/ lowerIdent ~ args.? ~ result ~ toEOL ~ Padding.parser(parser))
          .map {
            case (name, None, resType, res) => ExternalDef(name, Nil, resType, res)
            case (name, Some(args), resType, res) => ExternalDef(name, args.toList, resType, res)
          }
      }

      P("external" ~ spaces ~ (externalStruct|externalDef))
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
    commentP | defP | struct | enum | external | bindingP | end
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
      case ExternalDef(name, args, res, rest) =>
        val argDoc = args match {
          case Nil => Doc.empty
          case nonEmpty =>
            val da = Doc.intercalate(Doc.text(", "), nonEmpty.map { case (n, tr) =>
              Doc.text(n) + Doc.text(": ") + tr.toDoc
            })
            Doc.char('(') + da + Doc.char(')')
        }
        Doc.text("external def ") + Doc.text(name) + argDoc + Doc.text(" -> ") + res.toDoc + Doc.line +
          Document[Padding[Statement]].document(rest)
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

