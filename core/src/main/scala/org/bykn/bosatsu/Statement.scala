package org.bykn.bosatsu

import Parser.{ Combinators, Indy, lowerIdent, maybeSpace, spaces }
import cats.data.NonEmptyList
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
        case Struct(_, _, _, Padding(_, rest)) =>
          s #:: loop(rest)
        case Enum(_, _, _, Padding(_, rest)) =>
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
  import Statement.{Struct, Enum, ExternalStruct}

  /**
   * This is the name of the type being defined
   */
  def name: Constructor

  /**
   * here are the names of the constructors for this type
   */
  def constructors: List[Constructor] =
    this match {
      case Struct(nm, _, _, _) => nm :: Nil
      case Enum(_, _, items, _) =>
        items.get.toList.map { case (nm, _) => nm }
      case ExternalStruct(_, _, _) => Nil
    }
}

object Statement {

  /**
   * These introduce new values into scope
   */
  sealed abstract class ValueStatement extends Statement {
    /**
     * All the names that are bound by this statement
     */
    def names: List[Bindable] =
      this match {
        case Bind(BindingStatement(bound, _, _)) => bound.names // TODO Keep identifiers
        case Def(defstatement) => defstatement.name :: Nil
        case ExternalDef(name, _, _, _) => name :: Nil
      }
  }

  def definitionsOf(s: Statement): Stream[TypeDefinitionStatement] =
    s.toStream.collect {
      case tds: TypeDefinitionStatement => tds
    }

  def valuesOf(s: Statement): Stream[ValueStatement] =
    s.toStream.collect {
      case vs: ValueStatement => vs
    }

  //////
  // All the ValueStatements, which set up new bindings in the order they appear in the file
  /////.
  case class Bind(bind: BindingStatement[Pattern.Parsed, Padding[Statement]]) extends ValueStatement
  case class Def(defstatement: DefStatement[Pattern.Parsed, (OptIndent[Declaration], Padding[Statement])]) extends ValueStatement
  case class ExternalDef(name: Bindable, params: List[(Bindable, TypeRef)], result: TypeRef, rest: Padding[Statement]) extends ValueStatement

  //////
  // TypeDefinitionStatement types:
  //////
  case class Enum(name: Constructor,
    typeArgs: Option[NonEmptyList[TypeRef.TypeVar]],
    items: OptIndent[NonEmptyList[(Constructor, List[(Bindable, Option[TypeRef])])]],
    rest: Padding[Statement]) extends TypeDefinitionStatement
  case class ExternalStruct(name: Constructor, typeArgs: List[TypeRef.TypeVar], rest: Padding[Statement]) extends TypeDefinitionStatement
  case class Struct(name: Constructor,
    typeArgs: Option[NonEmptyList[TypeRef.TypeVar]],
    args: List[(Bindable, Option[TypeRef])],
    rest: Padding[Statement]) extends TypeDefinitionStatement

  ////
  // These have no effect on the semantics of the Statement linked list
  ////
  case class Comment(comment: CommentStatement[Padding[Statement]]) extends Statement
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

     val typeParams: P[NonEmptyList[TypeRef.TypeVar]] =
       lowerIdent.nonEmptyListSyntax.map { nel => nel.map { s => TypeRef.TypeVar(s.intern) } }

     val external = {
       val typeParamsList = Parser.nonEmptyListToList(typeParams)
       val externalStruct =
         P("struct" ~/ spaces ~ Identifier.consParser ~ typeParamsList ~ maybeSpace ~ padding).map {
           case (name, tva, rest) =>
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

     val struct = P("struct" ~ spaces ~/ Identifier.consParser ~ typeParams.? ~ argParser.parensLines1.? ~ padding)
       .map { case (name, typeArgs, argsOpt, rest) =>
         val argList = argsOpt match {
           case None => Nil
           case Some(ne) => ne.toList
         }

         { next: Statement => Struct(name, typeArgs, argList, rest(next)) }
       }

     val enum = {
       val constructorP = P(Identifier.consParser ~ argParser.parensLines1.?)
         .map {
           case (n, None) => (n, Nil)
           case (n, Some(args)) => (n, args.toList)
         }

       val sep = Indy.lift(P("," ~ maybeSpace)).combineK(Indy.toEOLIndent).map(_ => ())
       val variants = Indy.lift(constructorP ~ maybeSpace).nonEmptyList(sep)

       val nameVars = Indy.block(
         Indy.lift(P("enum" ~ spaces ~/ Identifier.consParser ~ (typeParams.?))),
         variants).run("")

       (nameVars ~ padding)
         .map { case ((ename, typeArgs), vars, rest) =>
           { next: Statement => Enum(ename, typeArgs, vars, rest(next)) }
         }
     }

     // bP should come last so there is no ambiguity about identifiers
     commentP | defP | struct | enum | external | bindingP
  }

  val parser: P[Statement] = {
    val end = P(End).map(_ => EndOfFile)
    Parser.chained(item, end)
  }

  private def constructor(name: Constructor, taDoc: Doc, args: List[(Bindable, Option[TypeRef])]): Doc =
    Document[Identifier].document(name) + taDoc +
      (if (args.nonEmpty) { Doc.char('(') + Doc.intercalate(Doc.text(", "), args.toList.map(TypeRef.argDoc[Bindable] _)) + Doc.char(')') }
      else Doc.empty)

  private def docTypeArgs(targs: List[TypeRef.TypeVar]): Doc =
    targs match {
      case Nil => Doc.empty
      case nonEmpty =>
        val params = nonEmpty.map { case TypeRef.TypeVar(v) => Doc.text(v) }
        Doc.char('[') + Doc.intercalate(Doc.text(", "), params) + Doc.char(']')
    }

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
      case Struct(nm, typeArgs, args, rest) =>
        val taDoc = typeArgs match {
          case None => Doc.empty
          case Some(ta) => docTypeArgs(ta.toList)
        }
        Doc.text("struct ") + constructor(nm, taDoc, args) +
          Document[Padding[Statement]].document(rest)
      case Enum(nm, typeArgs, parts, rest) =>
        implicit val consDoc = Document.instance[(Constructor, List[(Bindable, Option[TypeRef])])] {
          case (nm, parts) => constructor(nm, Doc.empty, parts)
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

        val taDoc = typeArgs match {
          case None => Doc.empty
          case Some(ta) => docTypeArgs(ta.toList)
        }

        Doc.text("enum ") + Document[Constructor].document(nm) + taDoc + Doc.char(':') +
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
        val argsDoc = docTypeArgs(targs)
        Doc.text("external struct ") + Document[Constructor].document(nm) + argsDoc +
          Document[Padding[Statement]].document(rest)
    }
}

