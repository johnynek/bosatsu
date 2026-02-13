package dev.bosatsu.tool

import _root_.cats.syntax.all._
import dev.bosatsu.{
  CommentStatement,
  ExportedName,
  Identifier,
  Kind,
  Package,
  PackageName,
  PlatformIO,
  Referant,
  Statement,
  TypeDefinitionStatement,
  TypeName
}
import dev.bosatsu.rankn.{DefinedType, Type}
import org.typelevel.paiges.Doc

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
      case _             =>
        params
    }

  private final case class SourceDocs(
      values: Map[Identifier.Bindable, List[String]],
      types: Map[Identifier.Constructor, List[String]]
  ) {
    def merge(that: SourceDocs): SourceDocs =
      SourceDocs(
        values = values ++ that.values.filterNot { case (k, _) =>
          values.contains(k)
        },
        types = types ++ that.types.filterNot { case (k, _) =>
          types.contains(k)
        }
      )
  }

  private object SourceDocs {
    val empty: SourceDocs = SourceDocs(Map.empty, Map.empty)

    private def normalizeComment(lines: List[String]): List[String] = {
      val stripSingleLeadingSpace =
        lines.exists(_.nonEmpty) &&
          lines.filter(_.nonEmpty).forall(_.startsWith(" "))

      val stripped =
        if (stripSingleLeadingSpace) {
          lines.map { line =>
            if (line.nonEmpty) line.drop(1) else line
          }
        } else lines

      stripped.map(_.replaceAll("\\s+$", ""))
    }

    def fromStatements(statements: List[Statement]): SourceDocs = {
      @annotation.tailrec
      def loop(
          rest: List[Statement],
          pending: List[String],
          valueDocs: Map[Identifier.Bindable, List[String]],
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
            val nextValues =
              if (pending.isEmpty) valueDocs
              else
                value.names.foldLeft(valueDocs) { case (m, name) =>
                  m.updatedWith(name) {
                    case Some(existing) => Some(existing)
                    case None           => Some(pending)
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

  private final case class RenderCtx(
      packageName: PackageName,
      localTypeNames: Set[TypeName]
  )

  private final case class ConstructorDoc(
      name: Identifier.Constructor,
      fields: List[(Identifier.Bindable, Type)],
      exists: List[Type.Var.Bound],
      tpe: Type
  )

  private final case class TypeDoc(
      dt: DefinedType[Kind.Arg],
      constructors: List[ConstructorDoc]
  )

  private val markdownRenderWidth: Int = 100

  private def groupedTypeParamList(items: List[Doc]): Doc =
    if (items.isEmpty) Doc.empty
    else {
      val body = Doc.intercalate(Doc.comma + Doc.line, items)
      (Doc.char('[') + (Doc.lineOrEmpty + body).nested(4) + Doc.lineOrEmpty + Doc
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

  private def fenced(language: String, content: Doc): Doc =
    Doc.text(show"```$language") + Doc.hardLine + content + Doc.hardLine + Doc
      .text("```")

  private def maybeComment(lines: List[String]): Option[Doc] =
    if (lines.forall(_.isEmpty)) None
    else Some(Doc.intercalate(Doc.hardLine, lines.map(Doc.text(_))))

  private def abbreviateTypeNames(raw: String, ctx: RenderCtx): String = {
    def nonNull(s: String | Null): String =
      Option(s).getOrElse("")

    val localPrefix = java.util.regex.Pattern.quote(ctx.packageName.asString + "::")
    val localRe = (localPrefix + "([A-Z][A-Za-z0-9_]*)").r
    val localShort = localRe.replaceAllIn(raw, m => nonNull(m.group(1)))

    val predefRe = "Bosatsu/Predef::([A-Z][A-Za-z0-9_]*)".r
    predefRe.replaceAllIn(localShort, { m =>
      val typeName = TypeName(nonNull(m.group(1)))
      if (ctx.localTypeNames(typeName)) nonNull(m.matched)
      else typeName.asString
    })
  }

  private def renderType(tpe: Type, ctx: RenderCtx): String =
    abbreviateTypeNames(
      Type.fullyResolvedDocument.document(tpe).render(markdownRenderWidth),
      ctx
    )

  private def typeNamePrefix(
      dt: DefinedType[Kind.Arg],
      ctx: RenderCtx
  ): String =
    if (dt.packageName == ctx.packageName) ""
    else if (
      (dt.packageName == PackageName.PredefName) &&
      !ctx.localTypeNames(dt.name)
    ) ""
    else dt.packageName.asString + "::"

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

  private def typeSignatureDoc(dt: DefinedType[Kind.Arg], ctx: RenderCtx): Doc = {
    val head = Doc.text("type ") + Doc.text(typeNamePrefix(dt, ctx) + dt.name.asString)
    val params = orderedTypeParamDocs(dt)
    head + groupedTypeParamList(params)
  }

  private def typeDisplayName(dt: DefinedType[Kind.Arg], ctx: RenderCtx): String = {
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

  private def localTypeNames(pack: Package.Typed[Any]): Set[TypeName] =
    pack.types.definedTypes.iterator.collect {
      case ((pn, tn), _) if pn == pack.name => tn
    }.toSet

  private def defSignature(
      name: Identifier.Bindable,
      tpe: Type,
      ctx: RenderCtx
  ): Option[Doc] = {
    val (foralls, exists, rho) = Type.splitQuantifiers(tpe)
    if (exists.nonEmpty) None
    else
      rho match {
        case Type.Fun(args, out) =>
          val fparams =
            if (foralls.isEmpty) ""
            else foralls.map(_._1.name).mkString("[", ", ", "]")
          val params =
            args.toList.zipWithIndex
              .map { case (argT, idx) =>
                Doc.text(show"arg${idx + 1}: ${renderType(argT, ctx)}")
              }
          Some(
            Doc.text(show"def ${name.sourceCodeRepr}$fparams") +
              groupedParamList(params) +
              Doc.text(show" -> ${renderType(out, ctx)}")
          )
        case _                   =>
          None
      }
  }

  private def valueSignature(
      name: Identifier.Bindable,
      tpe: Type,
      ctx: RenderCtx
  ): Doc =
    defSignature(name, tpe, ctx)
      .getOrElse(Doc.text(show"${name.sourceCodeRepr}: ${renderType(tpe, ctx)}"))

  private def renderValueSection(
      values: List[(Identifier.Bindable, Type)],
      docs: SourceDocs,
      ctx: RenderCtx
  ): Option[Doc] =
    if (values.isEmpty) None
    else {
      val entries = values.map { case (name, tpe) =>
        val title = Doc.text("### ") + inlineCode(name.sourceCodeRepr)
        val comment = docs.values.get(name).flatMap(maybeComment)
        val signature = fenced("bosatsu", valueSignature(name, tpe, ctx))

        Doc.intercalate(
          Doc.hardLine + Doc.hardLine,
          List(Some(title), comment, Some(signature)).flatten
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
              .map { case (fieldName, fieldTpe) =>
                Doc.text(show"${fieldName.sourceCodeRepr}: ${renderType(fieldTpe, ctx)}")
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
          List(Some(title), comment, Some(typeSig), constructors).flatten
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
      pack.exports.foldLeft(Map.empty[Identifier.Constructor, DefinedType[Kind.Arg]]) {
        case (acc, ExportedName.TypeName(name, Referant.DefinedT(dt))) =>
          acc.updated(name, dt)
        case (acc, ExportedName.Constructor(_, Referant.Constructor(dt, _))) =>
          acc.updated(dt.name.ident, dt)
        case (acc, _)                                                   =>
          acc
      }

    val constructorTypes =
      pack.exports.foldLeft(
        Map.empty[Identifier.Constructor, Map[Identifier.Constructor, ConstructorDoc]]
      ) { case (acc, exp) =>
        exp match {
          case ExportedName.Constructor(name, Referant.Constructor(dt, cfn)) =>
            val existing = acc.getOrElse(dt.name.ident, Map.empty)
            val ctorDoc =
              ConstructorDoc(name, cfn.args, cfn.exists.map(_._1), dt.fnTypeOf(cfn))
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

  private def valueDocs(pack: Package.Typed[Any]): List[(Identifier.Bindable, Type)] =
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
    sortByName(pairs)(_._1.asString)

  private def escapingDependencies(
      pack: Package.Typed[Any],
      values: List[(Identifier.Bindable, Type)],
      types: List[TypeDoc]
  ): List[PackageName] =
    (values.flatMap { case (_, tpe) =>
      Type.packageNamesIn(tpe)
    } ::: types.flatMap(_.constructors.flatMap(c =>
      Type.packageNamesIn(c.tpe)
    )))
      .distinct
      .sorted
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
    val ctx = RenderCtx(pack.name, localTypeNames(pack))
    val values = valueDocs(pack)
    val types = typeDocs(pack)

    val sections = List(
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
        pairs.foldLeft(Map.empty[PackageName, SourceDocs]) {
          case (acc, (name, docs)) =>
            acc.updatedWith(name) {
              case None       => Some(docs)
              case Some(prev) => Some(prev.merge(docs))
            }
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
