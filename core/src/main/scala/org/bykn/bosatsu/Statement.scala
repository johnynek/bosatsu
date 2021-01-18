package org.bykn.bosatsu

import Parser.{ Combinators, Indy, lowerIdent, maybeSpace, keySpace, toEOL }
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{ Doc, Document }
import scala.collection.immutable.SortedSet

import Indy.IndyMethods
import Identifier.{Bindable, Constructor}

sealed abstract class Statement {

  /**
   * This describes the region of the current statement, not the entire linked list
   * of statements
   */
  def region: Region

  def replaceRegions(r: Region): Statement = {
    import Statement._

    this match {
      case Bind(BindingStatement(p, v, in)) =>
        Bind(BindingStatement(p, v.replaceRegionsNB(r), in))(r)
      case Comment(c) =>
        Comment(c)(r)
      case Def(d) =>
        Def(d.copy(result = d.result.map(_.replaceRegions(r))))(r)
      case PaddingStatement(p) =>
        // this will just be some number of lines
        PaddingStatement(p)(r)
      case Struct(nm, typeArgs, args) =>
        Struct(nm, typeArgs, args)(r)
      case Enum(nm, typeArgs, parts) =>
        Enum(nm, typeArgs, parts)(r)
      case ExternalDef(name, args, res) =>
        ExternalDef(name, args, res)(r)
      case ExternalStruct(nm, targs) =>
        ExternalStruct(nm, targs)(r)
    }
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
      case Struct(nm, _, _) => nm :: Nil
      case Enum(_, _, items) =>
        items.get.toList.map { case (nm, _) => nm }
      case ExternalStruct(_, _) => Nil
    }
}

object Statement {

  def definitionsOf(stmts: Iterable[Statement]): Stream[TypeDefinitionStatement] =
    stmts.iterator.collect { case tds: TypeDefinitionStatement => tds }.toStream

  def valuesOf(stmts: Iterable[Statement]): Stream[ValueStatement] =
    stmts.iterator.collect { case vs: ValueStatement => vs }.toStream

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
        case ExternalDef(name, _, _) => name :: Nil
      }

    /**
     * These are all the free bindable names in the right hand side
     * of this binding
     */
    def freeVars: SortedSet[Bindable] =
      this match {
        case Bind(BindingStatement(_, decl, _)) => decl.freeVars
        case Def(defstatement) =>
          val innerFrees = defstatement.result.get.freeVars
          // but the def name and, args shadow
          (innerFrees - defstatement.name) -- defstatement.args.flatMap(_.names)
        case ExternalDef(name, _, _) => SortedSet.empty
      }

    /**
     * These are all the bindings, free or not, in this Statement
     */
    def allNames: SortedSet[Bindable] = {
      this match {
        case Bind(BindingStatement(pat, decl, _)) => decl.allNames ++ pat.names
        case Def(defstatement) =>
          (defstatement.result.get.allNames + defstatement.name) ++
            defstatement.args.flatMap(_.names)
        case ExternalDef(name, _, _) => SortedSet(name)
      }
    }
  }

  //////
  // All the ValueStatements, which set up new bindings in the order they appear in the file
  /////.
  case class Bind(bind: BindingStatement[Pattern.Parsed, Declaration.NonBinding, Unit])(val region: Region) extends ValueStatement
  case class Def(defstatement: DefStatement[Pattern.Parsed, OptIndent[Declaration]])(val region: Region) extends ValueStatement
  case class ExternalDef(name: Bindable, params: List[(Bindable, TypeRef)], result: TypeRef)(val region: Region) extends ValueStatement

  //////
  // TypeDefinitionStatement types:
  //////
  case class Enum(name: Constructor,
    typeArgs: Option[NonEmptyList[TypeRef.TypeVar]],
    items: OptIndent[NonEmptyList[(Constructor, List[(Bindable, Option[TypeRef])])]]
    )(val region: Region) extends TypeDefinitionStatement
  case class ExternalStruct(name: Constructor, typeArgs: List[TypeRef.TypeVar])(val region: Region) extends TypeDefinitionStatement
  case class Struct(name: Constructor,
    typeArgs: Option[NonEmptyList[TypeRef.TypeVar]],
    args: List[(Bindable, Option[TypeRef])])(val region: Region) extends TypeDefinitionStatement

  ////
  // These have no effect on the semantics of the Statement linked list
  ////
  case class PaddingStatement(padding: Padding[Unit])(val region: Region) extends Statement
  case class Comment(comment: CommentStatement[Unit])(val region: Region) extends Statement

  // Parse a single item
  final val parser1: P[Statement] = {

     val bindingP: P[Statement] =
       (Declaration
         .bindingParser[Unit](Declaration.nonBindingParser, cutPattern = true)("") <* toEOL)
         .region
         .map { case (region, bs) =>
            Bind(bs(()))(region)
         }

     val paddingSP: P[Statement] =
       Padding
         .nonEmptyParser
         .region
         .map { case (region, p) => PaddingStatement(p)(region) }

     val commentP: P[Statement] =
       CommentStatement.parser(_ => P.unit).region
         .map { case (region, cs) => Comment(cs)(region) }.run("")

     val defBody = maybeSpace.with1 *> OptIndent.indy(Declaration.parser).run("")
     val defP: P[Statement] =
      DefStatement.parser(Pattern.bindParser, defBody <* toEOL).region
        .map { case (region, DefStatement(nm, args, ret, body)) =>
          Def(DefStatement(nm, args, ret, body))(region)
        }

     val argParser: P[(Bindable, Option[TypeRef])] =
       Identifier.bindableParser ~ ((maybeSpace *> P.char(':')).backtrack *> maybeSpace *> TypeRef.parser).?

     val typeParams: P[NonEmptyList[TypeRef.TypeVar]] =
       lowerIdent.nonEmptyListSyntax.map { nel => nel.map { s => TypeRef.TypeVar(s.intern) } }

     val structKey = keySpace("struct")

     val external = {
       val typeParamsList = Parser.nonEmptyListToList(typeParams)

       val externalStruct =
         (structKey *> (Identifier.consParser ~ typeParamsList).region <* toEOL)
           .map {
             case (region, (name, tva)) => ExternalStruct(name, tva)(region)
           }

       val externalDef = {
         val argParser: P[(Bindable, TypeRef)] =
           Identifier.bindableParser ~ (maybeSpace *> P.char(':') *> maybeSpace *> TypeRef.parser)

         val args = P.char('(') *> maybeSpace *> argParser.nonEmptyList <* maybeSpace <* P.char(')')

         val result = maybeSpace.with1 *> P.string("->") *> maybeSpace *> TypeRef.parser

         (((keySpace("def") *> Identifier.bindableParser ~ args.? ~ result).region) <* toEOL)
           .map {
             case (region, ((name, optArgs), resType)) =>
               val alist = optArgs match {
                 case None => Nil
                 case Some(ne) => ne.toList
               }

               ExternalDef(name, alist, resType)(region)
           }
       }

       keySpace("external") *> externalStruct.orElse(externalDef)
     }

     val struct =
       ((structKey *> Identifier.consParser ~ typeParams.? ~ argParser.parensLines1Cut.?).region <* toEOL)
         .map { case (region, ((name, typeArgs), argsOpt)) =>
           val argList = argsOpt match {
             case None => Nil
             case Some(ne) => ne.toList
           }

           Struct(name, typeArgs, argList)(region)
         }

     val enum = {
       val constructorP =
         (Identifier.consParser ~ argParser.parensLines1Cut.?)
           .map {
             case (n, None) => (n, Nil)
             case (n, Some(args)) => (n, args.toList)
           }

       val sep = (Indy.lift(P.char(',') <* maybeSpace))
         .combineK(Indy.toEOLIndent)
         .void

       val variants = Indy.lift(constructorP <* maybeSpace).nonEmptyList(sep)

       val nameVars =
         OptIndent.block(
           Indy.lift(keySpace("enum") *> Identifier.consParser ~ (typeParams.?)),
           variants
         )
         .run("")
         .region

       (nameVars <* toEOL)
         .map { case (region, ((ename, typeArgs), vars)) =>
           Enum(ename, typeArgs, vars)(region)
         }
     }

     // bindingP should come last so there is no ambiguity about identifiers
     P.oneOf(commentP :: paddingSP :: defP :: struct :: enum :: external :: bindingP :: Nil)
  }

  /**
   * This parses the *rest* of the string (it must end with End)
   */
  val parser: P0[List[Statement]] =
    parser1.rep0 <* Parser.maybeSpacesAndLines <* P.end

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

  private implicit val dunit: Document[Unit] = Document.instance[Unit](_ => Doc.empty)

  implicit lazy val document: Document[Statement] =
    Document.instance[Statement] {
      case Bind(bs) =>
        Document[BindingStatement[Pattern.Parsed, Declaration.NonBinding, Unit]].document(bs) + Doc.line
      case Comment(cm) =>
        // Comments already end with newline
        Document[CommentStatement[Unit]].document(cm)
      case Def(d) =>
        implicit val pair = Document.instance[OptIndent[Declaration]] {
          body =>
            body.sepDoc +
            Document[OptIndent[Declaration]].document(body)
        }
        DefStatement.document[Pattern.Parsed, OptIndent[Declaration]].document(d) + Doc.line
      case PaddingStatement(p) =>
        // this will just be some number of lines
        Padding.document[Unit].document(p)
      case Struct(nm, typeArgs, args) =>
        val taDoc = typeArgs match {
          case None => Doc.empty
          case Some(ta) => docTypeArgs(ta.toList)
        }
        Doc.text("struct ") + constructor(nm, taDoc, args) + Doc.line
      case Enum(nm, typeArgs, parts) =>
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
          indentedCons + Doc.line
      case ExternalDef(name, args, res) =>
        val argDoc = args match {
          case Nil => Doc.empty
          case nonEmpty =>
            val da = Doc.intercalate(Doc.text(", "), nonEmpty.map { case (n, tr) =>
              Document[Bindable].document(n) + Doc.text(": ") + tr.toDoc
            })
            Doc.char('(') + da + Doc.char(')')
        }
        Doc.text("external def ") + Document[Bindable].document(name) + argDoc + Doc.text(" -> ") + res.toDoc + Doc.line
      case ExternalStruct(nm, targs) =>
        val argsDoc = docTypeArgs(targs)
        Doc.text("external struct ") + Document[Constructor].document(nm) + argsDoc + Doc.line
    }

  implicit lazy val documentList: Document[List[Statement]] =
    Document.instance[List[Statement]] { stmts =>
      Doc.intercalate(Doc.empty, stmts.toList.map(document.document(_)))
    }
}

