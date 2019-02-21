package org.bykn.bosatsu

import Parser.{ Combinators, Indy, lowerIdent, upperIdent, maybeSpace, spaces }
import cats.Functor
import cats.data.{ NonEmptyList, State }
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import org.bykn.fastparse_cats.StringInstances._

import Indy.IndyMethods

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
  def parser[T](onP: Parser.Indy[T]): Parser.Indy[CommentStatement[T]] = Parser.Indy { indent =>
    val commentBlock: P[NonEmptyList[String]] =
      P(commentPart ~ ("\n" ~ indent ~ commentPart).rep() ~ ("\n" | End))
        .map { case (c1, cs) => NonEmptyList(c1, cs.toList) }

    P(commentBlock ~ onP(indent))
      .map { case (m, on) => CommentStatement(m, on) }
  }

  val commentPart: P[String] = P("#" ~/ CharsWhile(_ != '\n').?.!)
}

case class PackageStatement[T](name: String,
  exports: List[String],
  emptyLines: Int,
  contents: T)

sealed abstract class Statement {

  def toStream: Stream[Statement] = {
    import Statement._
    def loop(s: Statement): Stream[Statement] =
      s match {
        case Bind(BindingStatement(_, _, Padding(_, rest))) =>
          s #:: loop(rest)
        case Comment(CommentStatement(_, Padding(_, rest))) =>
          s #:: loop(rest)
        case Def(DefStatement(_, _, _, (_, Padding(_, rest)))) =>
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

  /**
   * This is the name of the type being defined
   */
  def name: String

  /**
   * here are the names of the constructors for this type
   */
  def constructors: List[String] =
    this match {
      case Struct(nm, _, _) => nm :: Nil
      case Enum(_, items, _) =>
        items.get.toList.map { case (nm, _) => nm }
      case ExternalStruct(_, _, _) => Nil
    }

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
        val typeParams = typeVars.reverseMap { tv =>
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
        val conArgs = items.get.map { case (nm, args) =>
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
        val typeParams = typeVars.reverseMap { tv =>
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

  def definitionsOf(s: Statement): Stream[TypeDefinitionStatement] =
    s.toStream.collect {
      case tds: TypeDefinitionStatement => tds
    }

  case class Bind(bind: BindingStatement[Pattern[Option[String], TypeRef], Padding[Statement]]) extends Statement
  case class Comment(comment: CommentStatement[Padding[Statement]]) extends Statement
  case class Def(defstatement: DefStatement[(OptIndent[Declaration], Padding[Statement])]) extends Statement
  case class Struct(name: String, args: List[(String, Option[TypeRef])], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class ExternalDef(name: String, params: List[(String, TypeRef)], result: TypeRef, rest: Padding[Statement]) extends Statement
  case class ExternalStruct(name: String, typeArgs: List[TypeRef.TypeVar], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class Enum(name: String,
    items: OptIndent[NonEmptyList[(String, List[(String, Option[TypeRef])])]],
    rest: Padding[Statement]) extends TypeDefinitionStatement
  case object EndOfFile extends Statement

  val parser: P[Statement] = {
    val recurse = P(parser)
    val padding = Padding.parser(recurse)

    // val bindingP = P(lowerIdent ~ maybeSpace ~ "=" ~/ maybeSpace ~ Declaration.parser("") ~ maybeSpace ~ padding)
    //   .map { case (ident, value, rest) => Bind(BindingStatement(ident, value, rest)) }
    val bindingP = {
      val patP = Indy.lift(Pattern.parser)
      val bop = BindingStatement
        .bindingParser[Pattern[Option[String], TypeRef], Padding[Statement]](
          Declaration.parser, Indy.lift(maybeSpace ~ padding))("")

      (Pattern.parser ~ bop).map { case (p, fn) =>
        Bind(fn(p))
      }
    }

    val commentP = CommentStatement.parser(Indy.lift(padding)).map(Comment(_)).run("")

    val defBody = maybeSpace ~ OptIndent.indy(Declaration.parser).run("")
    val defP: P[Def] = DefStatement.parser(P(defBody ~ maybeSpace ~ padding)).map(Def(_))

    val end = P(End).map(_ => EndOfFile)

    val constructorP = P(upperIdent ~ (DefStatement.argParser).list.parens.?)
      .map {
        case (n, None) => (n, Nil)
        case (n, Some(args)) => (n, args)
      }

    val external = {
      val typeParams = Parser.nonEmptyListToList(lowerIdent.nonEmptyListSyntax)
      val externalStruct =
        P("struct" ~/ spaces ~ upperIdent ~ typeParams ~ maybeSpace ~ Padding.parser(parser)).map {
          case (name, targs, rest) =>
            val tva = targs.map(TypeRef.TypeVar(_))
            ExternalStruct(name, tva, rest)
        }

      val externalDef = {
        val argParser: P[(String, TypeRef)] = P(lowerIdent ~ ":" ~/ maybeSpace ~ TypeRef.parser)
        val args = P("(" ~ maybeSpace ~ argParser.nonEmptyList ~ maybeSpace ~ ")")
        val result = P(maybeSpace ~ "->" ~/ maybeSpace ~ TypeRef.parser ~ maybeSpace)
        P("def" ~ spaces ~/ lowerIdent ~ args.? ~ result ~ maybeSpace ~ Padding.parser(parser))
          .map {
            case (name, None, resType, res) => ExternalDef(name, Nil, resType, res)
            case (name, Some(args), resType, res) => ExternalDef(name, args.toList, resType, res)
          }
      }

      P("external" ~ spaces ~/ (externalStruct|externalDef))
    }

    val struct = P("struct" ~ spaces ~/ constructorP ~ Padding.parser(parser))
      .map { case (name, args, rest) => Struct(name, args, rest) }

    val enum = {
      val sep = Indy.lift(P("," ~ maybeSpace)).combineK(Indy.toEOLIndent).map(_ => ())
      val variants = Indy.lift(constructorP ~ maybeSpace).nonEmptyList(sep)

      val nameVars = Indy.block(
        Indy.lift(P("enum" ~ spaces ~/ upperIdent)),
        variants).run("")

      (nameVars ~ padding)
        .map { case (ename, vars, rest) =>
          Enum(ename, vars, rest)
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
        Document[BindingStatement[Pattern[Option[String], TypeRef], Padding[Statement]]].document(bs)
      case Comment(cm) =>
        Document[CommentStatement[Padding[Statement]]].document(cm)
      case Def(d) =>
        val pair = Document.instance[(OptIndent[Declaration], Padding[Statement])] {
          case (body, next) =>
            body.sepDoc +
            Document[OptIndent[Declaration]].document(body) +
              Document[Padding[Statement]].document(next)
        };
        DefStatement.document(pair).document(d)
      case Struct(nm, args, rest) =>
        Doc.text("struct ") + constructor(nm, args) +
          Document[Padding[Statement]].document(rest)
      case Enum(nm, parts, rest) =>
        implicit val consDoc = Document.instance[(String, List[(String, Option[TypeRef])])] {
          case (nm, parts) => constructor(nm, parts)
        }

        val (colonSep, itemSep) = parts match {
          case OptIndent.SameLine(_) => (Doc.space, Doc.text(", "))
          case OptIndent.NotSameLine(_) => (Doc.empty, Doc.line)
        }

        implicit def neDoc[T: Document]: Document[NonEmptyList[T]] =
          Document.instance { ne =>
            Doc.intercalate(itemSep, ne.toList.map(Document[T].document _))
          }

        val indentedCons = OptIndent.document(neDoc(consDoc)).document(parts)

        Doc.text("enum ") + Doc.text(nm) + Doc.char(':') +
          colonSep +
          indentedCons +
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
        Doc.text("external def ") + Doc.text(name) + argDoc + Doc.text(" -> ") + res.toDoc +
          Document[Padding[Statement]].document(rest)
      case ExternalStruct(nm, targs, rest) =>
        val argsDoc = targs match {
          case Nil => Doc.empty
          case nonEmpty =>
            val params = nonEmpty.map { case TypeRef.TypeVar(v) => Doc.text(v) }
            Doc.char('[') + Doc.intercalate(Doc.text(", "), params) + Doc.char(']')
        }
        Doc.text("external struct ") + Doc.text(nm) + argsDoc +
          Document[Padding[Statement]].document(rest)
    }
}

