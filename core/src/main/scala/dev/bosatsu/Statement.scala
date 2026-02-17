package dev.bosatsu

import Parser.{
  Combinators,
  Indy,
  maybeSpace,
  maybeSpacesAndLines,
  keySpace,
  toEOL
}
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{Doc, Document}
import scala.collection.immutable.SortedSet

import Indy.IndyMethods
import Identifier.{Bindable, Constructor}

sealed abstract class Statement {

  /** This describes the region of the current statement, not the entire linked
    * list of statements
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
        val args1 = args.map { arg =>
          arg.copy(
            default = arg.default.map(_.replaceRegionsNB(r)),
            region = r
          )
        }
        Struct(nm, typeArgs, args1)(r)
      case Enum(nm, typeArgs, parts) =>
        val parts1 = parts.map(_.map { branch =>
          val args1 = branch.args.map { arg =>
            arg.copy(
              default = arg.default.map(_.replaceRegionsNB(r)),
              region = r
            )
          }
          branch.copy(args = args1, region = r)
        })
        Enum(
          nm,
          typeArgs,
          parts1
        )(r)
      case ExternalDef(name, ta, args, res) =>
        ExternalDef(name, ta, args, res)(r)
      case ExternalStruct(nm, targs) =>
        ExternalStruct(nm, targs)(r)
    }
  }
}

sealed abstract class TypeDefinitionStatement extends Statement {
  import Statement.{Struct, Enum, ExternalStruct}

  /** This is the name of the type being defined
    */
  def name: Constructor

  /** here are the names of the constructors for this type
    */
  def constructors: List[Constructor] =
    this match {
      case Struct(nm, _, _)  => nm :: Nil
      case Enum(_, _, items) =>
        items.get.toList.map(_.name)
      case ExternalStruct(_, _) => Nil
    }
}

object Statement {

  def definitionsOf(
      stmts: Iterable[Statement]
  ): LazyList[TypeDefinitionStatement] =
    stmts.iterator
      .collect { case tds: TypeDefinitionStatement => tds }
      .to(LazyList)

  def valuesOf(stmts: Iterable[Statement]): LazyList[ValueStatement] =
    stmts.iterator.collect { case vs: ValueStatement => vs }.to(LazyList)

  /** These introduce new values into scope
    */
  sealed abstract class ValueStatement extends Statement {

    /** All the names that are bound by this statement
      */
    def names: List[Bindable] =
      this match {
        case Bind(BindingStatement(bound, _, _)) =>
          bound.names // TODO Keep identifiers
        case Def(defstatement)          => defstatement.name :: Nil
        case ExternalDef(name, _, _, _) => name :: Nil
      }

    /** These are all the free bindable names in the right hand side of this
      * binding
      */
    def freeVars: SortedSet[Bindable] =
      this match {
        case Bind(BindingStatement(_, decl, _)) => decl.freeVars
        case Def(defstatement)                  =>
          val innerFrees = defstatement.result.get.freeVars
          // but the def name and, args shadow
          (innerFrees - defstatement.name) -- defstatement.args.toList.flatMap(
            _.patternNames
          )
        case ExternalDef(_, _, _, _) => SortedSet.empty
      }

    /** These are all the bindings, free or not, in this Statement
      */
    def allNames: SortedSet[Bindable] =
      this match {
        case Bind(BindingStatement(pat, decl, _)) => decl.allNames ++ pat.names
        case Def(defstatement)                    =>
          (defstatement.result.get.allNames + defstatement.name) ++ defstatement.args.toList
            .flatMap(_.patternNames)
        case ExternalDef(name, _, _, _) => SortedSet(name)
      }
  }

  //////
  // All the ValueStatements, which set up new bindings in the order they appear in the file
  ///// .
  case class Bind(
      bind: BindingStatement[Pattern.Parsed, Declaration.NonBinding, Unit]
  )(val region: Region)
      extends ValueStatement
  case class Def(
      defstatement: DefStatement[Pattern.Parsed, OptIndent[Declaration]]
  )(val region: Region)
      extends ValueStatement
  case class ExternalDef(
      name: Bindable,
      typeArgs: Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind])]],
      params: List[(Bindable, TypeRef)],
      result: TypeRef
  )(val region: Region)
      extends ValueStatement

  //////
  // TypeDefinitionStatement types:
  //////
  final case class ConstructorArg(
      name: Bindable,
      tpe: Option[TypeRef],
      default: Option[Declaration.NonBinding],
      region: Region
  ) {
    def toDoc: Doc = {
      val nameDoc = Document[Identifier].document(name)
      val typeDoc =
        tpe match {
          case None     => Doc.empty
          case Some(tr) => colonSpace + tr.toDoc
        }
      val defaultDoc =
        default match {
          case None       => Doc.empty
          case Some(expr) => Doc.text(" = ") + expr.toDoc
        }
      nameDoc + typeDoc + defaultDoc
    }
  }

  final case class EnumBranch(
      name: Constructor,
      typeArgs: Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])]],
      args: List[ConstructorArg],
      region: Region
  )

  case class Enum(
      name: Constructor,
      typeArgs: Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])]],
      items: OptIndent[NonEmptyList[EnumBranch]]
  )(val region: Region)
      extends TypeDefinitionStatement
  case class ExternalStruct(
      name: Constructor,
      typeArgs: List[(TypeRef.TypeVar, Option[Kind.Arg])]
  )(val region: Region)
      extends TypeDefinitionStatement
  case class Struct(
      name: Constructor,
      typeArgs: Option[NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])]],
      args: List[ConstructorArg]
  )(val region: Region)
      extends TypeDefinitionStatement

  ////
  // These have no effect on the semantics of the Statement linked list
  ////
  case class PaddingStatement(padding: Padding[Unit])(val region: Region)
      extends Statement
  case class Comment(comment: CommentStatement[Unit])(val region: Region)
      extends Statement

  // Parse a single item
  final val parser1: P[Statement] = {
    import Declaration.NonBinding

    val bindingLike: Indy[(Pattern.Parsed, OptIndent[NonBinding])] = {
      val pat = Pattern.bindParser
      val patPart = pat <* (maybeSpace *> Declaration.eqP *> maybeSpace)

      // allow = to be like a block, we can continue on the next line indented
      OptIndent.blockLike(
        Indy.lift(patPart),
        Declaration.nonBindingParser,
        P.unit
      )
    }
    val bindingP: P[Statement] =
      (bindingLike("") <* toEOL).region
        .map { case (region, (pat, value)) =>
          Bind(BindingStatement(pat, value.get, ()))(region)
        }

    val paddingSP: P[Statement] =
      Padding.nonEmptyParser.region
        .map { case (region, p) => PaddingStatement(p)(region) }

    val commentP: P[Statement] =
      CommentStatement
        .parser(_ => P.unit)
        .region
        .map { case (region, cs) => Comment(cs)(region) }
        .run("")

    val defBody = maybeSpace.with1 *> OptIndent.indy(Declaration.parser).run("")
    val defP: P[Statement] =
      DefStatement
        .parser(Pattern.bindParser, defBody <* toEOL)
        .region
        .map { case (region, DefStatement(nm, ta, args, ret, body)) =>
          Def(DefStatement(nm, ta, args, ret, body))(region)
        }

    val defaultArg: P[Declaration.NonBinding] =
      Declaration.eqP *> maybeSpace *> Declaration.nonBindingParser.run("")

    val argParser: P[ConstructorArg] =
      (Identifier.bindableParser ~ TypeRef.annotationParser.? ~
        (maybeSpace.soft.with1 *> defaultArg).?).region
        .map {
          case (region, ((name, tpe), None)) =>
            ConstructorArg(name, tpe, None, region)
          case (region, ((name, tpe), Some(dval))) =>
            ConstructorArg(name, tpe, Some(dval), region)
        }

    val structKey = keySpace("struct")

    val typeParams: P[NonEmptyList[(TypeRef.TypeVar, Option[Kind.Arg])]] = {
      val kindAnnot: P[Kind.Arg] =
        (maybeSpace.soft.with1 *> (P.char(
          ':'
        ) *> maybeSpace *> Kind.paramKindParser))

      TypeRef.typeParams(kindAnnot.?)
    }
    val external = {
      val externalStruct =
        (structKey *> (Identifier.consParser ~ Parser.nonEmptyListToList(
          typeParams
        )).region <* toEOL)
          .map { case (region, (name, tva)) =>
            ExternalStruct(name, tva)(region)
          }

      val argParser: P[(Bindable, TypeRef)] =
        Identifier.bindableParser ~ TypeRef.annotationParser

      val externalDef = {

        val kindAnnot: P[Kind] =
          (maybeSpace.soft.with1 *> (P.char(
            ':'
          ) *> maybeSpacesAndLines *> Kind.parser))
        val typeParams = TypeRef.typeParams(kindAnnot.?).?

        val args =
          P.char('(') *> maybeSpacesAndLines *> argParser.nonEmptyListOfWs(
            maybeSpacesAndLines
          ) <* maybeSpacesAndLines <* P
            .char(')')

        val result =
          maybeSpace.with1 *> P.string(
            "->"
          ) *> maybeSpacesAndLines *> TypeRef.parser

        (((keySpace(
          "def"
        ) *> Identifier.bindableParser ~ typeParams ~ args ~ result).region) <* toEOL)
          .map { case (region, (((name, tps), args), resType)) =>
            ExternalDef(name, tps, args.toList, resType)(region)
          }
      }

      val externalVal =
        (argParser <* toEOL).region
          .map { case (region, (name, resType)) =>
            ExternalDef(name, None, Nil, resType)(region)
          }

      keySpace("external") *> P.oneOf(
        externalStruct :: externalDef :: externalVal :: Nil
      )
    }

    val struct =
      ((structKey *> Identifier.consParser ~ typeParams.? ~ Parser
        .nonEmptyListToList(argParser.parensLines1Cut)).region <* toEOL)
        .map { case (region, ((name, typeArgs), argsList)) =>
          Struct(name, typeArgs, argsList)(region)
        }

    val enumP = {
      val constructorP =
        (Identifier.consParser ~ typeParams.? ~ argParser.parensLines1Cut.?).region
          .map {
            case (region, ((n, targs), None)) =>
              EnumBranch(n, targs, Nil, region)
            case (region, ((n, targs), Some(args))) =>
              EnumBranch(n, targs, args.toList, region)
          }

      val variants = Indy { indent =>
        val constructor = constructorP <* maybeSpace

        // commas can separate variants either inline or across lines
        val commaSep =
          (P.char(',') *> maybeSpace *> Indy.toEOLIndentWithComments(indent).?)
            .void
        val lineSep = Indy.toEOLIndentWithComments(indent).backtrack
        val sep = commaSep.orElse(lineSep)
        val rest = (sep.soft *> constructor).rep0
        val trailingComment = (maybeSpace *> Parser.lineComment).?.void

        (constructor ~ rest <* (P.char(',') *> maybeSpace).?.void <* trailingComment)
          .map {
          case (head, tail) => NonEmptyList(head, tail)
        }
      }

      val nameVars =
        OptIndent
          .block(
            Indy.lift(
              keySpace("enum") *> Identifier.consParser ~ (typeParams.?)
            ),
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
    P.oneOf(
      commentP :: paddingSP :: defP :: struct :: enumP :: external :: bindingP :: Nil
    )
  }

  /** This parses the *rest* of the string (it must end with End)
    */
  val parser: P0[List[Statement]] =
    parser1.rep0 <* maybeSpacesAndLines <* P.end

  private def constructor(
      name: Constructor,
      taDoc: Doc,
      args: List[ConstructorArg]
  ): Doc =
    Document[Identifier].document(name) + taDoc +
      (if (args.nonEmpty) {
         Doc.char('(') + Doc.intercalate(
           Doc.text(", "),
           args.toList.map(_.toDoc)
         ) + Doc.char(')')
       } else Doc.empty)

  private val colonSpace = Doc.text(": ")

  implicit private val dunit: Document[Unit] =
    Document.instance[Unit](_ => Doc.empty)

  private val optKindArgs: Document[Option[Kind.Arg]] =
    Document.instance {
      case None     => Doc.empty
      case Some(ka) => colonSpace + Kind.argDoc(ka)
    }

  implicit lazy val document: Document[Statement] = {
    val db =
      Document[BindingStatement[Pattern.Parsed, Declaration.NonBinding, Unit]]
    val dc = Document[CommentStatement[Unit]]
    implicit val pair: Document[OptIndent[Declaration]] =
      Document.instance[OptIndent[Declaration]] { body =>
        body.sepDoc +
          OptIndent.document[Declaration].document(body)
      }
    val dd = DefStatement.document[Pattern.Parsed, OptIndent[Declaration]]

    implicit val consDoc: Document[EnumBranch] = Document.instance[EnumBranch] {
      item =>
        val taDoc = item.typeArgs match {
          case None     => Doc.empty
          case Some(ta) => TypeRef.docTypeArgs(ta.toList)(optKindArgs.document)
        }
        constructor(item.name, taDoc, item.args)
    }

    Document.instance[Statement] {
      case Bind(bs) =>
        db.document(bs) + Doc.line
      case Comment(cm) =>
        // Comments already end with newline
        dc.document(cm)
      case Def(d) =>
        dd.document(d) + Doc.line
      case PaddingStatement(p) =>
        // this will just be some number of lines
        Padding.document[Unit].document(p)
      case Struct(nm, typeArgs, args) =>
        val taDoc = typeArgs match {
          case None     => Doc.empty
          case Some(ta) => TypeRef.docTypeArgs(ta.toList)(optKindArgs.document)
        }
        Doc.text("struct ") + constructor(nm, taDoc, args) + Doc.line
      case Enum(nm, typeArgs, parts) =>
        val (colonSep, itemSep) = parts match {
          case OptIndent.SameLine(_)    => (Doc.space, Doc.text(", "))
          case OptIndent.NotSameLine(_) => (Doc.empty, Doc.line)
        }

        given neDoc[T](using Document[T]): Document[NonEmptyList[T]] =
          Document.instance { ne =>
            Doc.intercalate(itemSep, ne.toList.map(Document[T].document))
          }

        val indentedCons =
          OptIndent.document[NonEmptyList[EnumBranch]].document(parts)

        val taDoc = typeArgs match {
          case None     => Doc.empty
          case Some(ta) => TypeRef.docTypeArgs(ta.toList)(optKindArgs.document)
        }

        Doc.text("enum ") + Document[Constructor].document(nm) + taDoc + Doc
          .char(':') +
          colonSep +
          indentedCons + Doc.line
      case ExternalDef(name, None, Nil, res) =>
        Doc.text("external ") + Document[Bindable].document(name) + Doc.text(
          ": "
        ) + res.toDoc + Doc.line
      case ExternalDef(name, tps, args, res) =>
        val taDoc = tps match {
          case None     => Doc.empty
          case Some(ta) =>
            TypeRef.docTypeArgs(ta.toList) {
              case None    => Doc.empty
              case Some(k) => colonSpace + Kind.toDoc(k)
            }
        }
        val argDoc = {
          val da = Doc.intercalate(
            Doc.text(", "),
            args.map { case (n, tr) =>
              Document[Bindable].document(n) + Doc.text(": ") + tr.toDoc
            }
          )
          Doc.char('(') + da + Doc.char(')')
        }
        Doc.text("external def ") + Document[Bindable].document(
          name
        ) + taDoc + argDoc + Doc.text(" -> ") + res.toDoc + Doc.line
      case ExternalStruct(nm, typeArgs) =>
        val taDoc =
          TypeRef.docTypeArgs(typeArgs.toList) {
            case None     => Doc.empty
            case Some(ka) => Doc.text(": ") + Kind.argDoc(ka)
          }
        Doc.text("external struct ") + Document[Constructor].document(
          nm
        ) + taDoc + Doc.line
    }
  }

  implicit lazy val documentList: Document[List[Statement]] =
    Document.instance[List[Statement]] { stmts =>
      Doc.intercalate(Doc.empty, stmts.toList.map(document.document(_)))
    }
}
