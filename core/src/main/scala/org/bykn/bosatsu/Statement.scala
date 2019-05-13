package org.bykn.bosatsu

import Parser.{ Combinators, Indy, lowerIdent, maybeSpace, spaces }
import cats.Functor
import cats.data.{ NonEmptyList, State }
import cats.implicits._
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import org.bykn.fastparse_cats.StringInstances._

import Indy.IndyMethods
import Identifier.{Bindable, Constructor}

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
  def name: Constructor

  /**
   * here are the names of the constructors for this type
   */
  def constructors: List[Constructor] =
    this match {
      case Struct(nm, _, _) => nm :: Nil
      case Enum(_, items, _) =>
        items.get.toList.map { case (nm, _) => nm }
      case ExternalStruct(_, _, _) => Nil
    }

  def toDefinition(pname: PackageName, nameToType: Constructor => rankn.Type.Const): rankn.DefinedType[Unit] = {
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

    def buildParam(p: (Bindable, Option[Type])): VarState[(Bindable, Type)] =
      p match {
        case (parname, Some(tpe)) =>
          State.pure((parname, tpe))
        case (parname, None) =>
          nextVar.map { v => (parname, v) }
      }

    def existingVars[A](ps: List[(A, Option[Type])]): List[Type.TyVar] = {
      val pt = ps.flatMap(_._2)
      Type.freeTyVars(pt).map(Type.TyVar(_))
    }

    def buildParams(args: List[(Bindable, Option[Type])]): VarState[List[(Bindable, Type)]] =
      args.traverse(buildParam _)

    this match {
      case Struct(nm, args, _) =>
        val deep = Functor[List].compose(Functor[(Bindable, ?)]).compose(Functor[Option])
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
          typeParams.map((_, ())),
          (nm, params, consValueType) :: Nil)
      case Enum(nm, items, _) =>
        val deep = Functor[List].compose(Functor[(Bindable, ?)]).compose(Functor[Option])
        val conArgs = items.get.map { case (nm, args) =>
          val argsType = deep.map(args)(_.toType(nameToType))
          (nm, argsType)
        }
        val constructorsS = conArgs.traverse { case (nm, argsType) =>
          buildParams(argsType).map { params =>
            (nm, params)
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
        rankn.DefinedType(pname, TypeName(nm), typeParams.map((_, ())), finalCons)
      case ExternalStruct(nm, targs, _) =>
        rankn.DefinedType(pname, TypeName(nm), targs.map { case TypeRef.TypeVar(v) => (Type.Var.Bound(v), ()) }, Nil)
    }
  }
}

object Statement {

  def definitionsOf(s: Statement): Stream[TypeDefinitionStatement] =
    s.toStream.collect {
      case tds: TypeDefinitionStatement => tds
    }

  case class Bind(bind: BindingStatement[Pattern.Parsed, Padding[Statement]]) extends Statement
  case class Comment(comment: CommentStatement[Padding[Statement]]) extends Statement
  case class Def(defstatement: DefStatement[Pattern.Parsed, (OptIndent[Declaration], Padding[Statement])]) extends Statement
  case class Struct(name: Constructor, args: List[(Bindable, Option[TypeRef])], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class ExternalDef(name: Bindable, params: List[(Bindable, TypeRef)], result: TypeRef, rest: Padding[Statement]) extends Statement
  case class ExternalStruct(name: Constructor, typeArgs: List[TypeRef.TypeVar], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class Enum(name: Constructor,
    items: OptIndent[NonEmptyList[(Constructor, List[(Bindable, Option[TypeRef])])]],
    rest: Padding[Statement]) extends TypeDefinitionStatement
  case object EndOfFile extends Statement

  // Parse a single item
  private val item: P[Statement => Statement] = {
     val padding = Padding.parseFn[Statement]

     val bindingP: P[Statement => Statement] = {
       val bop = BindingStatement
         .bindingParser[Pattern[Option[Identifier.Constructor], TypeRef], Statement => Padding[Statement]](
           Declaration.parser, Indy.lift(maybeSpace ~ padding))("")

       (Pattern.bindParser ~ bop).map { case (p, parseBs) =>
         val BindingStatement(n, v, fn) = parseBs(p)

         { next: Statement =>
           val bs = BindingStatement(n, v, fn(next))
           Bind(bs)
         }
       }
     }

     val commentP: P[Statement => Statement] =
       CommentStatement.parser(Indy.lift(padding))
         .map { case CommentStatement(text, on) =>

           { next: Statement =>
             Comment(CommentStatement(text, on(next)))
           }
         }.run("")

     val defBody = maybeSpace ~ OptIndent.indy(Declaration.parser).run("")
     val defP: P[Statement => Def] =
      DefStatement.parser(Pattern.bindParser, P(defBody ~ maybeSpace ~ padding))
        .map { case DefStatement(nm, args, ret, (body, resFn)) =>
          { next: Statement =>
            Def(DefStatement(nm, args, ret, (body, resFn(next))))
          }
        }

     val argParser: P[(Bindable, Option[TypeRef])] =
       P(Identifier.bindableParser ~ maybeSpace ~ (":" ~/ maybeSpace ~ TypeRef.parser).?)

     val constructorP = P(Identifier.consParser ~ argParser.parensLines1.?)
       .map {
         case (n, None) => (n, Nil)
         case (n, Some(args)) => (n, args.toList)
       }

     val external = {
       val typeParams = Parser.nonEmptyListToList(lowerIdent.nonEmptyListSyntax)
       val externalStruct =
         P("struct" ~/ spaces ~ Identifier.consParser ~ typeParams ~ maybeSpace ~ padding).map {
           case (name, targs, rest) =>
             val tva = targs.map(TypeRef.TypeVar(_))

             { next: Statement => ExternalStruct(name, tva, rest(next)) }
         }

       val externalDef = {
         val argParser: P[(Bindable, TypeRef)] = P(Identifier.bindableParser ~ ":" ~/ maybeSpace ~ TypeRef.parser)
         val args = P("(" ~ maybeSpace ~ argParser.nonEmptyList ~ maybeSpace ~ ")")
         val result = P(maybeSpace ~ "->" ~/ maybeSpace ~ TypeRef.parser ~ maybeSpace)
         P("def" ~ spaces ~/ Identifier.bindableParser ~ args.? ~ result ~ maybeSpace ~ padding)
           .map {
             case (name, optArgs, resType, res) =>
               val alist = optArgs match {
                 case None => Nil
                 case Some(ne) => ne.toList
               }

               { next: Statement => ExternalDef(name, alist, resType, res(next)) }
           }
       }

       P("external" ~ spaces ~/ (externalStruct|externalDef))
     }

     val struct = P("struct" ~ spaces ~/ constructorP ~ padding)
       .map { case (name, args, rest) =>
         { next: Statement => Struct(name, args, rest(next)) }
       }

     val enum = {
       val sep = Indy.lift(P("," ~ maybeSpace)).combineK(Indy.toEOLIndent).map(_ => ())
       val variants = Indy.lift(constructorP ~ maybeSpace).nonEmptyList(sep)

       val nameVars = Indy.block(
         Indy.lift(P("enum" ~ spaces ~/ Identifier.consParser)),
         variants).run("")

       (nameVars ~ padding)
         .map { case (ename, vars, rest) =>
           { next: Statement => Enum(ename, vars, rest(next)) }
         }
     }

     // bP should come last so there is no ambiguity about identifiers
     commentP | defP | struct | enum | external | bindingP
  }

  val parser: P[Statement] = {
    val end = P(End).map(_ => EndOfFile)
    Parser.chained(item, end)
  }

  private def constructor(name: Constructor, args: List[(Bindable, Option[TypeRef])]): Doc =
    Document[Identifier].document(name) +
      (if (args.nonEmpty) { Doc.char('(') + Doc.intercalate(Doc.text(", "), args.toList.map(TypeRef.argDoc[Bindable] _)) + Doc.char(')') }
      else Doc.empty)

  implicit lazy val document: Document[Statement] =
    Document.instance[Statement] {
      case Bind(bs) =>
        Document[BindingStatement[Pattern.Parsed, Padding[Statement]]].document(bs)
      case Comment(cm) =>
        Document[CommentStatement[Padding[Statement]]].document(cm)
      case Def(d) =>
        implicit val pair = Document.instance[(OptIndent[Declaration], Padding[Statement])] {
          case (body, next) =>
            body.sepDoc +
            Document[OptIndent[Declaration]].document(body) +
              Document[Padding[Statement]].document(next)
        };
        DefStatement.document[Pattern.Parsed, (OptIndent[Declaration], Padding[Statement])].document(d)
      case Struct(nm, args, rest) =>
        Doc.text("struct ") + constructor(nm, args) +
          Document[Padding[Statement]].document(rest)
      case Enum(nm, parts, rest) =>
        implicit val consDoc = Document.instance[(Constructor, List[(Bindable, Option[TypeRef])])] {
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

        Doc.text("enum ") + Document[Constructor].document(nm) + Doc.char(':') +
          colonSep +
          indentedCons +
          Document[Padding[Statement]].document(rest)
      case EndOfFile => Doc.empty
      case ExternalDef(name, args, res, rest) =>
        val argDoc = args match {
          case Nil => Doc.empty
          case nonEmpty =>
            val da = Doc.intercalate(Doc.text(", "), nonEmpty.map { case (n, tr) =>
              Document[Bindable].document(n) + Doc.text(": ") + tr.toDoc
            })
            Doc.char('(') + da + Doc.char(')')
        }
        Doc.text("external def ") + Document[Bindable].document(name) + argDoc + Doc.text(" -> ") + res.toDoc +
          Document[Padding[Statement]].document(rest)
      case ExternalStruct(nm, targs, rest) =>
        val argsDoc = targs match {
          case Nil => Doc.empty
          case nonEmpty =>
            val params = nonEmpty.map { case TypeRef.TypeVar(v) => Doc.text(v) }
            Doc.char('[') + Doc.intercalate(Doc.text(", "), params) + Doc.char(']')
        }
        Doc.text("external struct ") + Document[Constructor].document(nm) + argsDoc +
          Document[Padding[Statement]].document(rest)
    }
}

