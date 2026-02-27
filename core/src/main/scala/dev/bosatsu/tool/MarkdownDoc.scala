package dev.bosatsu.tool

import _root_.cats.syntax.all._
import dev.bosatsu.{
  BindingStatement,
  CommentStatement,
  Declaration,
  ExportedName,
  Identifier,
  Kind,
  Package,
  PackageName,
  Pattern,
  PlatformIO,
  Referant,
  Statement,
  TypeRenderer,
  TypeDefinitionStatement,
  TypeName
}
import dev.bosatsu.rankn.{ConstructorParam, DefinedType, Type}
import org.typelevel.paiges.Doc
import java.util.Locale

object MarkdownDoc {
  private val fnTypeRegex = raw"^Fn\d+$$".r
  private val fnInputTypeParamRegex = raw"^i\d+$$".r

  private def trailingNumberParts(name: String): Option[(String, BigInt)] = {
    val lastNonDigit = name.lastIndexWhere(ch => !ch.isDigit)
    val digitStart = lastNonDigit + 1
    if (digitStart >= name.length) None
    else {
      val prefix = name.substring(0, digitStart)
      val digits = name.substring(digitStart)
      Some((prefix, BigInt(digits)))
    }
  }

  private def compareNames(left: String, right: String): Int =
    (trailingNumberParts(left), trailingNumberParts(right)) match {
      case (Some((leftPrefix, leftDigits)), Some((rightPrefix, rightDigits)))
          if leftPrefix == rightPrefix =>
        val cmp = leftDigits.compare(rightDigits)
        if (cmp == 0) left.compareTo(right) else cmp
      case _ =>
        left.compareTo(right)
    }

  private def sortByName[A](items: List[A])(nameFn: A => String): List[A] =
    items.sortWith { (left, right) =>
      compareNames(nameFn(left), nameFn(right)) < 0
    }

  private def orderedTypeParams[A](
      typeName: String,
      params: List[(String, A)]
  ): List[(String, A)] =
    typeName match {
      case fnTypeRegex() =>
        val inputParams = params.filter { case (name, _) =>
          fnInputTypeParamRegex.matches(name)
        }
        val zParams = params.filter { case (name, _) =>
          name == "z"
        }
        val recognizedCount = inputParams.size + zParams.size
        if (recognizedCount == params.size) {
          sortByName(inputParams)(_._1) ::: zParams
        } else params
      case _ =>
        params
    }

  final private case class ValueDocInfo(
      comment: List[String],
      params: List[Option[Identifier.Bindable]]
  ) {
    def merge(that: ValueDocInfo): ValueDocInfo =
      ValueDocInfo(
        comment = if (comment.nonEmpty) comment else that.comment,
        params = if (params.nonEmpty) params else that.params
      )
  }

  private object ValueDocInfo {
    val empty: ValueDocInfo = ValueDocInfo(Nil, Nil)
  }

  final private case class SourceDocs(
      values: Map[Identifier.Bindable, ValueDocInfo],
      types: Map[Identifier.Constructor, List[String]]
  ) {
    def merge(that: SourceDocs): SourceDocs = {
      val mergedValues = that.values.foldLeft(values) {
        case (acc, (name, info)) =>
          acc.updatedWith(name) {
            case Some(existing) => Some(existing.merge(info))
            case None           => Some(info)
          }
      }

      SourceDocs(
        values = mergedValues,
        types = types ++ that.types.filterNot { case (k, _) =>
          types.contains(k)
        }
      )
    }
  }

  private object SourceDocs {
    val empty: SourceDocs = SourceDocs(Map.empty, Map.empty)

    private def normalizeComment(lines: List[String]): List[String] = {
      val filtered = lines
        .map(_.replaceAll("\\s+$", ""))
        .filterNot { line =>
          val trimmed = line.trim
          trimmed.isEmpty || trimmed.forall(_ == '#')
        }

      val stripSingleLeadingSpace =
        filtered.exists(_.nonEmpty) &&
          filtered.filter(_.nonEmpty).forall(_.startsWith(" "))

      if (stripSingleLeadingSpace) {
        filtered.map { line =>
          if (line.nonEmpty) line.drop(1) else line
        }
      } else filtered
    }

    private def paramNameFromPattern(
        pat: Pattern.Parsed
    ): Option[Identifier.Bindable] =
      pat match {
        case Pattern.Var(name)        => Some(name)
        case Pattern.Annotation(p, _) => paramNameFromPattern(p)
        case Pattern.Named(name, _)   => Some(name)
        case _                        => pat.topNames.headOption
      }

    @annotation.tailrec
    private def topLambdaParamNames(
        decl: Declaration.NonBinding,
        acc: List[Option[Identifier.Bindable]]
    ): List[Option[Identifier.Bindable]] = {
      val unwrapped = decl match {
        case Declaration.Annotation(fn, _) =>
          Some(fn)
        case Declaration.Parens(nb: Declaration.NonBinding) =>
          Some(nb)
        case Declaration.CommentNB(comment) =>
          Some(comment.on.padded)
        case _ =>
          None
      }

      unwrapped match {
        case Some(next) =>
          topLambdaParamNames(next, acc)
        case None =>
          decl match {
            case Declaration.Lambda(args, body) =>
              val acc1 = acc ::: args.toList.map(paramNameFromPattern)
              body match {
                case nb: Declaration.NonBinding => topLambdaParamNames(nb, acc1)
                case _                          => acc1
              }
            case _ =>
              acc
          }
      }
    }

    private def sourceParamNames(
        value: Statement.ValueStatement
    ): List[Option[Identifier.Bindable]] =
      value match {
        case Statement.ExternalDef(_, _, params, _) =>
          params.map { case (name, _) => Some(name) }
        case Statement.Def(defstatement) =>
          defstatement.args.toList
            .flatMap(_.toList)
            .map(paramNameFromPattern)
        case Statement.Bind(BindingStatement(bound, decl, _))
            if bound.topNames.size == 1 =>
          topLambdaParamNames(decl, Nil)
        case _ =>
          Nil
      }

    def fromStatements(statements: List[Statement]): SourceDocs = {
      @annotation.tailrec
      def loop(
          rest: List[Statement],
          pending: List[String],
          valueDocs: Map[Identifier.Bindable, ValueDocInfo],
          typeDocs: Map[Identifier.Constructor, List[String]]
      ): SourceDocs =
        rest match {
          case Nil =>
            SourceDocs(valueDocs, typeDocs)

          case Statement.Comment(CommentStatement(message, _)) :: tail =>
            loop(
              tail,
              pending ::: normalizeComment(message.toList),
              valueDocs,
              typeDocs
            )

          case Statement.PaddingStatement(_) :: tail =>
            // Preserve pending comments over spacing so that doc blocks
            // separated by blank lines still attach to the next declaration.
            loop(tail, pending, valueDocs, typeDocs)

          case (value: Statement.ValueStatement) :: tail =>
            val nextInfo = ValueDocInfo(
              comment = pending,
              params = sourceParamNames(value)
            )
            val nextValues =
              if (nextInfo.comment.isEmpty && nextInfo.params.isEmpty) valueDocs
              else
                value.names.foldLeft(valueDocs) { case (m, name) =>
                  m.updatedWith(name) {
                    case Some(existing) => Some(existing.merge(nextInfo))
                    case None           => Some(nextInfo)
                  }
                }

            loop(tail, Nil, nextValues, typeDocs)

          case (tds: TypeDefinitionStatement) :: tail =>
            val nextTypes =
              if (pending.isEmpty) typeDocs
              else
                typeDocs.updatedWith(tds.name) {
                  case Some(existing) => Some(existing)
                  case None           => Some(pending)
                }

            loop(tail, Nil, valueDocs, nextTypes)
        }

      loop(statements, Nil, Map.empty, Map.empty)
    }
  }

  private type RenderCtx = TypeRenderer.Context

  private lazy val predefSourceDocs: SourceDocs =
    SourceDocs.fromStatements(Package.predefPackage.program)

  final private case class ConstructorDoc(
      name: Identifier.Constructor,
      fields: List[ConstructorParam],
      exists: List[Type.Var.Bound],
      tpe: Type
  )

  final private case class TypeDoc(
      dt: DefinedType[Kind.Arg],
      constructors: List[ConstructorDoc]
  )

  private val markdownRenderWidth: Int = 100

  private def groupedTypeParamList(items: List[Doc]): Doc =
    if (items.isEmpty) Doc.empty
    else {
      val body = Doc.intercalate(Doc.comma + Doc.line, items)
      (Doc
        .char('[') + (Doc.lineOrEmpty + body).nested(4) + Doc.lineOrEmpty + Doc
        .char(']')).grouped
    }

  private def groupedParamList(items: List[Doc]): Doc =
    if (items.isEmpty) Doc.text("()")
    else {
      val inline =
        Doc.char('(') + Doc.intercalate(Doc.text(", "), items) + Doc.char(')')
      val wrappedBody = Doc.intercalate(Doc.comma + Doc.line, items)
      val wrapped =
        (Doc.char('(') + (Doc.line + wrappedBody).nested(4) + Doc.line + Doc
          .char(')')).grouped

      val inlineLen = inline.render(markdownRenderWidth).length
      if (inlineLen <= markdownRenderWidth) inline else wrapped
    }

  private def inlineCode(str: String): Doc =
    Doc.char('`') + Doc.text(str.replace("`", "\\`")) + Doc.char('`')

  private def markdownCodeLink(label: String, id: String): Doc =
    Doc.text(show"[`${label.replace("`", "\\`")}`](#$id)")

  private def anchorSlug(raw: String): String = {
    val lowered = raw.toLowerCase(Locale.ROOT)
    val mapped = lowered.map { ch =>
      if (ch.isLetterOrDigit) ch else '-'
    }
    val collapsed = mapped.split("-+").toList.filter(_.nonEmpty).mkString("-")
    if (collapsed.isEmpty) "item" else collapsed
  }

  private def typeAnchorId(dt: DefinedType[Kind.Arg]): String =
    show"type-${anchorSlug(dt.name.asString)}"

  private def valueAnchorId(name: Identifier.Bindable): String =
    show"value-${anchorSlug(name.sourceCodeRepr)}"

  private def fenced(language: String, content: Doc): Doc =
    Doc.text(show"```$language") + Doc.hardLine + content + Doc.hardLine + Doc
      .text("```")

  private def maybeComment(lines: List[String]): Option[Doc] =
    if (lines.forall(_.isEmpty)) None
    else Some(Doc.intercalate(Doc.hardLine, lines.map(Doc.text(_))))

  private def renderType(tpe: Type, ctx: RenderCtx): String =
    TypeRenderer.render(tpe, ctx, markdownRenderWidth)

  private def typeNamePrefix(
      dt: DefinedType[Kind.Arg],
      ctx: RenderCtx
  ): String =
    TypeRenderer.typeNamePrefix(dt.packageName, dt.name, ctx)

  private def orderedTypeParamDocs(
      dt: DefinedType[Kind.Arg]
  ): List[Doc] =
    orderedTypeParams(
      dt.name.asString,
      dt.annotatedTypeParams.map { case (tv, kindArg) =>
        (tv.name, kindArg)
      }
    ).map { case (name, kindArg) =>
      Doc.text(name) + Doc.text(": ") + Kind.argDoc(kindArg)
    }

  private def quantifiedTypeParamDocs(
      params: List[(Type.Var.Bound, Kind)]
  ): List[Doc] =
    params.map { case (tv, kind) =>
      if (kind == Kind.Type) Doc.text(tv.name)
      else Doc.text(tv.name) + Doc.text(": ") + Kind.toDoc(kind)
    }

  private def typeSignatureDoc(
      dt: DefinedType[Kind.Arg],
      ctx: RenderCtx
  ): Doc = {
    val head =
      Doc.text("type ") + Doc.text(typeNamePrefix(dt, ctx) + dt.name.asString)
    val params = orderedTypeParamDocs(dt)
    head + groupedTypeParamList(params)
  }

  private def typeDisplayName(
      dt: DefinedType[Kind.Arg],
      ctx: RenderCtx
  ): String = {
    val localName = dt.name.asString
    val prefix = typeNamePrefix(dt, ctx)
    val params = orderedTypeParams(
      localName,
      dt.typeParams.map(tv => (tv.name, ()))
    ).map(_._1)
    val suffix =
      if (params.isEmpty) ""
      else params.mkString("[", ", ", "]")

    prefix + localName + suffix
  }

  private def typeIndexName(dt: DefinedType[Kind.Arg], ctx: RenderCtx): String =
    typeNamePrefix(dt, ctx) + dt.name.asString

  private def localTypeNames(pack: Package.Typed[Any]): Set[TypeName] =
    pack.types.definedTypes.iterator.collect {
      case ((pn, tn), _) if pn == pack.name => tn
    }.toSet

  private def defSignature(
      name: Identifier.Bindable,
      tpe: Type,
      sourceParams: List[Option[Identifier.Bindable]],
      ctx: RenderCtx
  ): Option[Doc] = {
    val (foralls, exists, rho) = Type.splitQuantifiers(tpe)
    if (exists.nonEmpty) None
    else
      rho match {
        case Type.Fun(args, out) =>
          val fparams = groupedTypeParamList(quantifiedTypeParamDocs(foralls))
          val params =
            args.toList.zipWithIndex
              .map { case (argT, idx) =>
                val paramName =
                  sourceParams
                    .lift(idx)
                    .flatten
                    .map(_.sourceCodeRepr)
                    .getOrElse(show"arg${idx + 1}")
                Doc.text(show"$paramName: ${renderType(argT, ctx)}")
              }
          Some(
            Doc.text(show"def ${name.sourceCodeRepr}") +
              fparams +
              groupedParamList(params) +
              Doc.text(show" -> ${renderType(out, ctx)}")
          )
        case _ =>
          None
      }
  }

  private def valueSignature(
      name: Identifier.Bindable,
      tpe: Type,
      sourceParams: List[Option[Identifier.Bindable]],
      ctx: RenderCtx
  ): Doc =
    defSignature(name, tpe, sourceParams, ctx)
      .getOrElse(
        Doc.text(show"${name.sourceCodeRepr}: ${renderType(tpe, ctx)}")
      )

  private def renderValueSection(
      values: List[(Identifier.Bindable, Type)],
      docs: SourceDocs,
      ctx: RenderCtx
  ): Option[Doc] =
    if (values.isEmpty) None
    else {
      val entries = values.map { case (name, tpe) =>
        val valueInfo = docs.values.getOrElse(name, ValueDocInfo.empty)
        val anchor = Doc.text(show"<a id=\"${valueAnchorId(name)}\"></a>")
        val title = Doc.text("### ") + inlineCode(name.sourceCodeRepr)
        val comment = maybeComment(valueInfo.comment)
        val signature =
          fenced("bosatsu", valueSignature(name, tpe, valueInfo.params, ctx))

        Doc.intercalate(
          Doc.hardLine + Doc.hardLine,
          List(Some(anchor), Some(title), comment, Some(signature)).flatten
        )
      }

      Some(
        Doc.text("## Values") + Doc.hardLine + Doc.hardLine + Doc.intercalate(
          Doc.hardLine + Doc.hardLine,
          entries
        )
      )
    }

  private def ctorFieldDoc(
      c: ConstructorDoc,
      typeParams: Set[Type.Var.Bound],
      ctx: RenderCtx
  ): Doc = {
    val existentialTypeParams = c.exists.filterNot(typeParams).map(_.name)
    val ctorName = Doc.text(c.name.sourceCodeRepr) + groupedTypeParamList(
      existentialTypeParams.map(Doc.text(_))
    )
    val nameWithFields =
      c.fields match {
        case Nil =>
          ctorName
        case nonEmpty =>
          val fields =
            nonEmpty
              .map { field =>
                Doc.text(
                  show"${field.name.sourceCodeRepr}: ${renderType(field.tpe, ctx)}"
                )
              }
          ctorName + groupedParamList(fields)
      }
    nameWithFields
  }

  private def renderTypeSection(
      types: List[TypeDoc],
      docs: SourceDocs,
      ctx: RenderCtx
  ): Option[Doc] =
    if (types.isEmpty) None
    else {
      val entries = types.map { td =>
        val dt = td.dt
        val anchor = Doc.text(show"<a id=\"${typeAnchorId(dt)}\"></a>")
        val title = Doc.text("### ") + inlineCode(typeDisplayName(dt, ctx))
        val comment = docs.types.get(dt.name.ident).flatMap(maybeComment)
        val typeSig = fenced("bosatsu", typeSignatureDoc(dt, ctx))
        val constructors =
          if (td.constructors.isEmpty) None
          else {
            val ctorItems =
              td.constructors.map { c =>
                val ctorDoc = ctorFieldDoc(c, td.dt.typeParams.toSet, ctx)
                val rendered = ctorDoc.render(markdownRenderWidth)
                if (rendered.contains('\n')) {
                  Doc.text("- ") + ctorDoc.nested(2)
                } else {
                  Doc.text("- ") + inlineCode(rendered)
                }
              }
            Some(
              Doc.text("#### Constructors") + Doc.hardLine + Doc.hardLine + Doc
                .intercalate(Doc.hardLine, ctorItems)
            )
          }

        Doc.intercalate(
          Doc.hardLine + Doc.hardLine,
          List(
            Some(anchor),
            Some(title),
            comment,
            Some(typeSig),
            constructors
          ).flatten
        )
      }

      Some(
        Doc.text("## Types") + Doc.hardLine + Doc.hardLine + Doc.intercalate(
          Doc.hardLine + Doc.hardLine,
          entries
        )
      )
    }

  private def typeDocs(pack: Package.Typed[Any]): List[TypeDoc] = {
    val allTypes =
      pack.exports.foldLeft(
        Map.empty[Identifier.Constructor, DefinedType[Kind.Arg]]
      ) {
        case (acc, ExportedName.TypeName(name, Referant.DefinedT(dt))) =>
          acc.updated(name, dt)
        case (acc, ExportedName.Constructor(_, Referant.Constructor(dt, _))) =>
          acc.updated(dt.name.ident, dt)
        case (acc, _) =>
          acc
      }

    val constructorTypes =
      pack.exports.foldLeft(
        Map.empty[Identifier.Constructor, Map[
          Identifier.Constructor,
          ConstructorDoc
        ]]
      ) { case (acc, exp) =>
        exp match {
          case ExportedName.Constructor(name, Referant.Constructor(dt, cfn)) =>
            val existing = acc.getOrElse(dt.name.ident, Map.empty)
            val ctorDoc =
              ConstructorDoc(
                name,
                cfn.args,
                cfn.exists.map(_._1),
                dt.fnTypeOf(cfn)
              )
            acc.updated(dt.name.ident, existing.updated(name, ctorDoc))
          case _ =>
            acc
        }
      }

    sortByName(allTypes.values.toList)(_.name.asString)
      .map { dt =>
        val ctors =
          sortByName(
            constructorTypes
              .getOrElse(dt.name.ident, Map.empty)
              .values
              .toList
          )(_.name.asString)
        TypeDoc(dt, ctors)
      }
  }

  private def valueDocs(
      pack: Package.Typed[Any]
  ): List[(Identifier.Bindable, Type)] =
    val pairs = pack.exports
      .foldLeft(Map.empty[Identifier.Bindable, Type]) { (acc, exp) =>
        exp match {
          case ExportedName.Binding(name, Referant.Value(tpe)) =>
            acc.updated(name, tpe)
          case _ =>
            acc
        }
      }
      .toList
    sortByName(pairs)(_._1.sourceCodeRepr)

  private def escapingDependencies(
      pack: Package.Typed[Any],
      values: List[(Identifier.Bindable, Type)],
      types: List[TypeDoc]
  ): List[PackageName] =
    (values.flatMap { case (_, tpe) =>
      Type.packageNamesIn(tpe)
    } ::: types.flatMap(
      _.constructors.flatMap(c => Type.packageNamesIn(c.tpe))
    )).distinct.sorted
      .filterNot(pn => (pn == pack.name) || (pn == PackageName.PredefName))

  private def dependenciesDoc(deps: List[PackageName]): Option[Doc] =
    if (deps.isEmpty) None
    else {
      val list =
        Doc.intercalate(
          Doc.text(", "),
          deps.map(pn => inlineCode(pn.asString))
        )
      Some(Doc.text("public dependencies: ") + list)
    }

  private def packageDoc(pack: Package.Typed[Any], docs: SourceDocs): Doc = {
    val ctx = TypeRenderer.Context(pack.name, localTypeNames(pack))
    val values = valueDocs(pack)
    val types = typeDocs(pack)

    val typeLinks =
      types.map { td =>
        val dt = td.dt
        (typeIndexName(dt, ctx), typeAnchorId(dt))
      }
    val valueLinks =
      values.map { case (name, _) =>
        (name.sourceCodeRepr, valueAnchorId(name))
      }
    val indexItems = List(
      if (typeLinks.isEmpty) None
      else
        Some(
          (Doc.text("- Types: ") + Doc
            .intercalate(
              Doc.comma + Doc.lineOrSpace,
              typeLinks.map { case (label, id) =>
                markdownCodeLink(label, id)
              }
            )).grouped
        ),
      if (valueLinks.isEmpty) None
      else
        Some(
          (Doc.text("- Values: ") + Doc
            .intercalate(
              Doc.comma + Doc.lineOrSpace,
              valueLinks.map { case (label, id) =>
                markdownCodeLink(label, id)
              }
            )).grouped
        )
    ).flatten
    val indexSection =
      if (indexItems.isEmpty) None
      else
        Some(
          Doc.text("## Index") + Doc.hardLine + Doc.hardLine + Doc.intercalate(
            Doc.hardLine,
            indexItems
          )
        )

    val sections = List(
      indexSection,
      renderTypeSection(types, docs, ctx),
      renderValueSection(values, docs, ctx)
    ).flatten

    val body =
      if (sections.isEmpty) Doc.text("No exported values or types.")
      else Doc.intercalate(Doc.hardLine + Doc.hardLine, sections)

    val deps = escapingDependencies(pack, values, types)
    val header = Doc.text("# ") + inlineCode(pack.name.asString)
    Doc.intercalate(
      Doc.hardLine + Doc.hardLine,
      header :: dependenciesDoc(deps).toList ::: body :: Nil
    )
  }

  private def outputPath[F[_], Path](
      platformIO: PlatformIO[F, Path],
      outdir: Path,
      packageName: PackageName
  ): Path = {
    val parts = packageName.parts.toList
    val dir = platformIO.resolve(outdir, parts.init)
    platformIO.resolve(dir, parts.last + ".md")
  }

  private def docsByPackage[F[_], Path](
      platformIO: PlatformIO[F, Path],
      sourcePaths: List[(Path, PackageName)],
      color: dev.bosatsu.LocationMap.Colorize
  ): F[Map[PackageName, SourceDocs]] = {
    import platformIO.moduleIOMonad

    sourcePaths
      .traverse { case (path, packageName) =>
        for {
          source <- platformIO.readUtf8(path)
          parsed <- CommandSupport.liftParseErrors(
            platformIO,
            PathParseError.parseString(
              Package.parser(Some(packageName)),
              path,
              source
            ),
            color
          )
          (_, parsedPackage) = parsed
          docs = SourceDocs.fromStatements(parsedPackage.program)
        } yield (packageName, docs)
      }
      .map { pairs =>
        val sourceDocs = pairs.foldLeft(Map.empty[PackageName, SourceDocs]) {
          case (acc, (name, docs)) =>
            acc.updatedWith(name) {
              case None       => Some(docs)
              case Some(prev) => Some(prev.merge(docs))
            }
        }
        sourceDocs.updatedWith(PackageName.PredefName) {
          case None       => Some(predefSourceDocs)
          case Some(prev) => Some(prev.merge(predefSourceDocs))
        }
      }
  }

  def generate[F[_], Path](
      platformIO: PlatformIO[F, Path],
      packages: List[Package.Typed[Any]],
      sourcePaths: List[(Path, PackageName)],
      outdir: Path,
      color: dev.bosatsu.LocationMap.Colorize
  ): F[List[(Path, Doc)]] = {
    import platformIO.moduleIOMonad

    docsByPackage(platformIO, sourcePaths, color)
      .map { packageDocs =>
        sortByName(packages)(_.name.asString)
          .map { pack =>
            val path = outputPath(platformIO, outdir, pack.name)
            val docs = packageDocs.getOrElse(pack.name, SourceDocs.empty)
            (path, packageDoc(pack, docs))
          }
      }
  }
}
