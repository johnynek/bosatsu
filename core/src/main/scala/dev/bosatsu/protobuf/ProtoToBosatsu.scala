package dev.bosatsu.protobuf

import cats.data.NonEmptyList
import dev.bosatsu.{
  BindingStatement,
  Declaration,
  DefStatement,
  ExportedName,
  Identifier,
  Import,
  ImportedName,
  Indented,
  ListLang,
  Lit,
  OptIndent,
  Package,
  PackageName,
  Padding,
  Pattern,
  Statement,
  TypeRef,
  TypeName => BosatsuTypeName,
  Region
}
import org.typelevel.paiges.Document

object ProtoToBosatsu {

  import ProtoDescriptorModel.*

  final case class GeneratedFile(
      packageName: PackageName,
      outputFilePath: String,
      content: String
  )

  private final case class EnumCodecInfo(
      packageName: PackageName,
      typeName: String
  ) {
    val encodeFnName: String = s"encode_enum_$typeName"
    val decodeFnName: String = s"decode_enum_$typeName"
  }

  private final case class MessageCodecInfo(
      packageName: PackageName,
      typeName: String
  ) {
    val encodeFnName: String = s"encode_$typeName"
    val decodeFnName: String = s"decode_$typeName"
  }

  private final case class RenderContext(
      enumCodecs: Map[(PackageName, String), EnumCodecInfo],
      messageCodecs: Map[(PackageName, String), MessageCodecInfo]
  )

  private final case class TypeDecl(
      name: String,
      statement: Statement,
      deps: Set[String]
  )

  private val generatedRegion: Region = Region(0, 1)

  def renderAll(files: Vector[ProtoFileModel]): Vector[GeneratedFile] = {
    val ctx = buildContext(files)
    files.sortBy(_.outputFilePath).map(renderFile(_, ctx))
  }

  private def buildContext(files: Vector[ProtoFileModel]): RenderContext = {
    val enumCodecs = files.iterator
      .flatMap(_.enums)
      .map { enumModel =>
        val key = (enumModel.packageName, enumModel.bosatsuName)
        key -> EnumCodecInfo(enumModel.packageName, enumModel.bosatsuName)
      }
      .toMap

    val messageCodecs = files.iterator
      .flatMap(_.messages)
      .map { messageModel =>
        val key = (messageModel.packageName, messageModel.bosatsuName)
        key -> MessageCodecInfo(messageModel.packageName, messageModel.bosatsuName)
      }
      .toMap

    RenderContext(enumCodecs, messageCodecs)
  }

  private def renderFile(
      fileModel: ProtoFileModel,
      ctx: RenderContext
  ): GeneratedFile = {
    val typeExports =
      fileModel.enums.map(_.bosatsuName) ++
        fileModel.messages.flatMap(messageModel =>
          messageModel.bosatsuName +: messageModel.oneofs.map(_.bosatsuTypeName)
        )

    val enumCodecExports = fileModel.enums.flatMap { enumModel =>
      val info = ctx.enumCodecs((enumModel.packageName, enumModel.bosatsuName))
      Vector(info.encodeFnName, info.decodeFnName)
    }

    val messageCodecExports = fileModel.messages.flatMap { messageModel =>
      val info =
        ctx.messageCodecs((messageModel.packageName, messageModel.bosatsuName))
      Vector(info.encodeFnName, info.decodeFnName)
    }

    val exports =
      (typeExports ++ enumCodecExports ++ messageCodecExports).distinct.sorted

    val typeStatements: Vector[Statement] = {
      val typeDecls =
        fileModel.enums.map(enumModel =>
          TypeDecl(
            name = enumModel.bosatsuName,
            statement = renderEnumStatement(enumModel),
            deps = Set.empty
          )
        ) ++
          fileModel.messages.flatMap { messageModel =>
            val oneofDecls = messageModel.oneofs.map { oneofModel =>
              TypeDecl(
                name = oneofModel.bosatsuTypeName,
                statement = renderOneofStatement(oneofModel),
                deps = oneofModel.fields.iterator.flatMap(field => fieldTypeDeps(field.fieldType)).toSet
              )
            }

            val messageDeps =
              messageModel.fields.iterator.flatMap(field => fieldTypeDeps(field.fieldType)).toSet ++
                messageModel.oneofs.iterator.map(_.bosatsuTypeName)

            oneofDecls :+ TypeDecl(
              name = messageModel.bosatsuName,
              statement = renderStructStatement(messageModel),
              deps = messageDeps
            )
          }

      orderTypeDecls(typeDecls)
    }

    val enumCodecStatements: Vector[Statement] =
      fileModel.enums.flatMap { enumModel =>
        val codecInfo = ctx.enumCodecs((enumModel.packageName, enumModel.bosatsuName))
        renderEnumCodecStatements(enumModel, codecInfo)
      }

    val orderedMessagesForCodecs = orderMessageModelsForCodecs(fileModel.messages)

    val messageCodecStatements: Vector[Statement] =
      orderedMessagesForCodecs.flatMap { messageModel =>
        val codecInfo =
          ctx.messageCodecs((messageModel.packageName, messageModel.bosatsuName))
        renderMessageCodecStatements(messageModel, codecInfo, ctx)
      }

    val statements =
      (typeStatements ++ enumCodecStatements ++ messageCodecStatements).toList

    val statementSources =
      statements.iterator.map(Document[Statement].document(_).render(120)).toVector

    val bytesImports = usedNamesInSources(statementSources, bytesImportCandidates)
    val usedWireImports = usedNamesInSources(statementSources, wireImports)
    val crossPackageImports = computeCrossPackageImports(fileModel, ctx)
    val imports =
      Vector(
        Option.when(bytesImports.nonEmpty) {
          importOf(PackageName.parts("Bosatsu", "IO", "Bytes"), bytesImports)
        },
        Option.when(usedWireImports.nonEmpty) {
          importOf(PackageName.parts("Bosatsu", "Proto", "Wire"), usedWireImports)
        }
      ).flatten ++
        crossPackageImports.map { case (pack, items) =>
          importOf(pack, items.toVector.sorted)
        }

    val parsedPack =
      Package(
        fileModel.bosatsuPackage,
        imports.toList,
        exports.toList.map(exportedNameOf),
        statements
      )
    val rendered = Document[Package.Parsed].document(parsedPack).render(80).trim + "\n"

    GeneratedFile(
      fileModel.bosatsuPackage,
      fileModel.outputFilePath,
      rendered
    )
  }

  private def importOf(
      packageName: PackageName,
      names: Vector[String]
  ): Import[PackageName, Unit] =
    Import(
      packageName,
      NonEmptyList.fromListUnsafe(
        names.toList.map(name => ImportedName.OriginalName(Identifier.unsafe(name), ()))
      )
    )

  private def exportedNameOf(name: String): ExportedName[Unit] =
    Identifier.unsafe(name) match {
      case bindable: Identifier.Bindable =>
        ExportedName.Binding(bindable, ())
      case constructor: Identifier.Constructor =>
        ExportedName.TypeName(constructor, ())
    }

  private def usedNamesInSources(
      statementSources: Vector[String],
      candidates: Set[String]
  ): Vector[String] = {
    val source = statementSources.mkString("\n")
    candidates.toVector.sorted.filter(name => containsIdentifier(source, name))
  }

  private def containsIdentifier(source: String, name: String): Boolean = {
    def isIdentChar(ch: Char): Boolean =
      ch.isLetterOrDigit || ch == '_'

    if (name.isEmpty) false
    else {
      var start = source.indexOf(name)
      while (start >= 0) {
        val end = start + name.length
        val leftOk = start == 0 || !isIdentChar(source.charAt(start - 1))
        val rightOk = end == source.length || !isIdentChar(source.charAt(end))
        if (leftOk && rightOk) return true
        start = source.indexOf(name, start + 1)
      }
      false
    }
  }

  private val bytesImportCandidates: Set[String] = Set(
    "Bytes",
    "concat_all_Bytes",
    "empty_Bytes",
    "utf8_bytes_from_String",
    "utf8_bytes_to_String"
  )

  private val wireImports: Set[String] = Set(
    "decode_bool",
    "decode_double",
    "decode_enum",
    "decode_fields",
    "decode_fixed32",
    "decode_fixed64",
    "decode_float",
    "decode_int32",
    "decode_int64",
    "decode_packed_fixed32",
    "decode_packed_fixed64",
    "decode_packed_varints",
    "decode_sfixed32",
    "decode_sfixed64",
    "decode_sint32",
    "decode_sint64",
    "decode_uint32",
    "decode_uint64",
    "encode_bool",
    "encode_double_bits",
    "encode_enum",
    "encode_fixed32",
    "encode_fixed32_bits",
    "encode_fixed64",
    "encode_fixed64_bits",
    "encode_float_bits",
    "encode_int32",
    "encode_int64",
    "encode_sfixed32_bits",
    "encode_sfixed64_bits",
    "encode_sint32",
    "encode_sint64",
    "encode_uint32",
    "encode_uint64",
    "encode_varint_u64",
    "field_fixed32",
    "field_fixed64",
    "field_length_delimited",
    "field_varint",
    "if_Some",
    "Fixed32",
    "Fixed64",
    "LengthDelimited",
    "Varint"
  )

  private def constructor(name: String): Identifier.Constructor =
    Identifier.unsafe(name) match {
      case c: Identifier.Constructor => c
      case other =>
        sys.error(s"expected constructor identifier: $other")
    }

  private def bindable(name: String): Identifier.Bindable =
    Identifier.unsafeBindable(name)

  private def typeNameRef(name: String): TypeRef =
    TypeRef.TypeName(BosatsuTypeName(constructor(name)))

  private def listType(inner: TypeRef): TypeRef =
    TypeRef.TypeApply(typeNameRef("List"), NonEmptyList.one(inner))

  private def optionType(inner: TypeRef): TypeRef =
    TypeRef.TypeApply(typeNameRef("Option"), NonEmptyList.one(inner))

  private def fieldTypeToTypeRef(fieldType: FieldType): TypeRef =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        typeNameRef(scalarType.bosatsuTypeString)
      case FieldType.EnumRef(_, typeName) =>
        typeNameRef(typeName)
      case FieldType.MessageRef(_, typeName) =>
        typeNameRef(typeName)
      case FieldType.MapEntry(keyType, valueType) =>
        listType(
          TypeRef.TypeTuple(
            List(
              typeNameRef(keyType.bosatsuTypeString),
              fieldTypeToTypeRef(valueType)
            )
          )
        )
    }

  private def fieldTypeRef(fieldModel: FieldModel): TypeRef =
    fieldModel.cardinality match {
      case Cardinality.Singular => fieldTypeToTypeRef(fieldModel.fieldType)
      case Cardinality.Optional => optionType(fieldTypeToTypeRef(fieldModel.fieldType))
      case Cardinality.Repeated =>
        fieldModel.fieldType match {
          case _: FieldType.MapEntry =>
            fieldTypeToTypeRef(fieldModel.fieldType)
          case _ =>
            listType(fieldTypeToTypeRef(fieldModel.fieldType))
        }
    }

  private def fieldTypeDeps(fieldType: FieldType): Set[String] =
    fieldType match {
      case FieldType.Scalar(_) => Set.empty
      case FieldType.EnumRef(_, typeName) =>
        Set(typeName)
      case FieldType.MessageRef(_, typeName) =>
        Set(typeName)
      case FieldType.MapEntry(_, valueType) =>
        fieldTypeDeps(valueType)
    }

  private def fieldTypeMessageDeps(fieldType: FieldType): Set[String] =
    fieldType match {
      case FieldType.MessageRef(_, typeName) =>
        Set(typeName)
      case FieldType.MapEntry(_, valueType) =>
        fieldTypeMessageDeps(valueType)
      case _ =>
        Set.empty
    }

  private def orderTypeDecls(
      declarations: Vector[TypeDecl]
  ): Vector[Statement] = {
    val definedNames = declarations.iterator.map(_.name).toSet
    val normalized = declarations.map(decl =>
      decl.copy(deps = decl.deps.intersect(definedNames))
    )

    @scala.annotation.tailrec
    def loop(
        remaining: Vector[TypeDecl],
        resolved: Set[String],
        acc: Vector[Statement]
    ): Vector[Statement] =
      if (remaining.isEmpty) acc
      else {
        val (ready, waiting) =
          remaining.partition(decl => (decl.deps -- resolved).isEmpty)

        if (ready.isEmpty) {
          // Keep output deterministic if a dependency cycle remains.
          acc ++ remaining.map(_.statement)
        } else {
          val nextResolved = resolved ++ ready.iterator.map(_.name)
          loop(waiting, nextResolved, acc ++ ready.map(_.statement))
        }
      }

    loop(normalized, Set.empty, Vector.empty)
  }

  private def orderMessageModelsForCodecs(
      messages: Vector[MessageModel]
  ): Vector[MessageModel] = {
    val knownNames = messages.iterator.map(_.bosatsuName).toSet
    val depsByName = messages.map { messageModel =>
      val deps =
        messageModel.fields.iterator
          .flatMap(field => fieldTypeMessageDeps(field.fieldType))
          .toSet ++
          messageModel.oneofs.iterator
            .flatMap(oneof => oneof.fields.iterator.flatMap(field => fieldTypeMessageDeps(field.fieldType)))
      messageModel.bosatsuName -> deps.intersect(knownNames)
    }.toMap

    @scala.annotation.tailrec
    def loop(
        remaining: Vector[MessageModel],
        resolved: Set[String],
        acc: Vector[MessageModel]
    ): Vector[MessageModel] =
      if (remaining.isEmpty) acc
      else {
        val (ready, waiting) = remaining.partition(messageModel =>
          (depsByName.getOrElse(messageModel.bosatsuName, Set.empty) -- resolved).isEmpty
        )

        if (ready.isEmpty) acc ++ remaining
        else {
          val nextResolved = resolved ++ ready.iterator.map(_.bosatsuName)
          loop(waiting, nextResolved, acc ++ ready)
        }
      }

    loop(messages, Set.empty, Vector.empty)
  }

  private def constructorArg(
      name: String,
      tpe: TypeRef
  ): Statement.ConstructorArg =
    Statement.ConstructorArg(bindable(name), Some(tpe), None, generatedRegion)

  private def notSameLine[A](value: A): OptIndent[A] =
    OptIndent.NotSameLine(Padding(1, Indented(2, value)))

  private def renderEnumStatement(enumModel: EnumModel): Statement =
    Statement.Enum(
      constructor(enumModel.bosatsuName),
      None,
      notSameLine(
        NonEmptyList.fromListUnsafe(
          (enumModel.values.map(valueModel =>
            Statement.EnumBranch(
              constructor(valueModel.bosatsuName),
              None,
              Nil,
              generatedRegion
            )
          ) :+
            Statement.EnumBranch(
              constructor(unknownEnumCtor(enumModel)),
              None,
              constructorArg("value", typeNameRef("Int")) :: Nil,
              generatedRegion
            )).toList
        )
      )
    )(generatedRegion)

  private def renderOneofStatement(oneofModel: OneofModel): Statement =
    Statement.Enum(
      constructor(oneofModel.bosatsuTypeName),
      None,
      notSameLine(
        NonEmptyList.fromListUnsafe(
          (Statement.EnumBranch(
            constructor(notSetCtor(oneofModel)),
            None,
            Nil,
            generatedRegion
          ) +:
            oneofModel.fields.map(oneofField =>
              Statement.EnumBranch(
                constructor(oneofField.bosatsuCaseName),
                None,
                constructorArg("value", fieldTypeToTypeRef(oneofField.fieldType)) :: Nil,
                generatedRegion
              )
            )).toList
        )
      )
    )(generatedRegion)

  private def renderStructStatement(messageModel: MessageModel): Statement = {
    val fields =
      messageModel.fields.map(fieldModel =>
        constructorArg(fieldModel.bosatsuName, fieldTypeRef(fieldModel))
      ) ++
        messageModel.oneofs.map(oneofModel =>
          constructorArg(oneofModel.bosatsuFieldName, typeNameRef(oneofModel.bosatsuTypeName))
        )

    Statement.Struct(
      constructor(messageModel.bosatsuName),
      None,
      fields.toList
    )(generatedRegion)
  }

  private def varRef(name: String): Declaration.NonBinding =
    Declaration.Var(Identifier.unsafe(name))(using generatedRegion)

  private def intLit(value: Int): Declaration.NonBinding =
    Declaration.Literal(Lit.Integer(value))(using generatedRegion)

  private def call(
      fn: String,
      args: Declaration.NonBinding*
  ): Declaration.NonBinding =
    Declaration.Apply(
      varRef(fn),
      NonEmptyList.fromListUnsafe(args.toList),
      Declaration.ApplyKind.Parens
    )(using generatedRegion)

  private def constructorValue(
      name: String,
      args: List[Declaration.NonBinding]
  ): Declaration.NonBinding =
    args match {
      case Nil      => varRef(name)
      case nonEmpty =>
        Declaration.Apply(
          varRef(name),
          NonEmptyList.fromListUnsafe(nonEmpty),
          Declaration.ApplyKind.Parens
        )(using generatedRegion)
    }

  private def constructorPattern(
      name: String,
      args: List[Pattern.Parsed]
  ): Pattern.Parsed =
    Pattern.PositionalStruct(
      Pattern.StructKind.Named(constructor(name), Pattern.StructKind.Style.TupleLike),
      args
    )

  private def argPattern(name: String, tpe: Option[TypeRef]): Pattern.Parsed =
    tpe match {
      case Some(tp) => Pattern.Annotation(Pattern.Var(bindable(name)), tp)
      case None     => Pattern.Var(bindable(name))
    }

  private def defStatement(
      name: String,
      args: List[(String, Option[TypeRef])],
      returnType: TypeRef,
      body: Declaration
  ): Statement =
    Statement.Def(
      DefStatement(
        bindable(name),
        None,
        NonEmptyList.one(args.map { case (argName, argType) =>
          argPattern(argName, argType)
        }),
        Some(returnType),
        notSameLine(body)
      )
    )(generatedRegion)

  private def enumCompareEq(
      valueName: String,
      compareTo: Int
  ): Declaration.NonBinding =
    Declaration.Matches(
      call("cmp_Int", varRef(valueName), intLit(compareTo)),
      constructorPattern("EQ", Nil)
    )(using generatedRegion)

  private def renderEnumCodecStatements(
      enumModel: EnumModel,
      codecInfo: EnumCodecInfo
  ): Vector[Statement] = {
    val encodeBranches =
      (enumModel.values.map(valueModel =>
        Declaration.MatchBranch(
          constructorPattern(valueModel.bosatsuName, Nil),
          None,
          OptIndent.same(intLit(valueModel.number))
        )(using generatedRegion)
      ) :+
        Declaration.MatchBranch(
          constructorPattern(unknownEnumCtor(enumModel), Pattern.Var(bindable("unknown")) :: Nil),
          None,
          OptIndent.same(varRef("unknown"))
        )(using generatedRegion)).toList

    val encodeBody =
      Declaration.Match(
        Declaration.MatchKind.Match,
        varRef("value"),
        notSameLine(NonEmptyList.fromListUnsafe(encodeBranches))
      )(using generatedRegion)

    val decodeCases =
      enumModel.values.map(valueModel =>
        (
          enumCompareEq("value", valueModel.number),
          notSameLine(constructorValue(valueModel.bosatsuName, Nil): Declaration)
        )
      )

    val decodeBody =
      Declaration.IfElse(
        NonEmptyList.fromListUnsafe(decodeCases.toList),
        notSameLine(constructorValue(unknownEnumCtor(enumModel), varRef("value") :: Nil): Declaration)
      )(using generatedRegion)

    Vector(
      defStatement(
        codecInfo.encodeFnName,
        List("value" -> Some(typeNameRef(enumModel.bosatsuName))),
        typeNameRef("Int"),
        encodeBody
      ),
      defStatement(
        codecInfo.decodeFnName,
        List("value" -> Some(typeNameRef("Int"))),
        typeNameRef(enumModel.bosatsuName),
        decodeBody
      )
    )
  }

  private def renderMessageCodecStatements(
      messageModel: MessageModel,
      codecInfo: MessageCodecInfo,
      ctx: RenderContext
  ): Vector[Statement] = {
    val mapHelpers = messageModel.fields.flatMap {
      case fieldModel @ FieldModel(_, _, _, _, mapType: FieldType.MapEntry, _) =>
        Vector(
          renderMapEncodeHelperStatement(fieldModel, mapType, ctx),
          renderMapDecodeHelperStatement(fieldModel, mapType, ctx)
        )
      case _ => Vector.empty
    }

    mapHelpers ++ Vector(
      renderEncodeMessageStatement(messageModel, codecInfo, ctx),
      renderDecodeMessageStatement(messageModel, codecInfo, ctx)
    )
  }

  private def renderEncodeMessageStatement(
      messageModel: MessageModel,
      codecInfo: MessageCodecInfo,
      ctx: RenderContext
  ): Statement = {
    val messageFieldNames =
      messageModel.fields.map(_.bosatsuName) ++
        messageModel.oneofs.map(_.bosatsuFieldName)

    val body: Declaration =
      if (messageFieldNames.isEmpty)
        matchExpr(
          Declaration.MatchKind.Match,
          varRef("input_value"),
          List(Pattern.WildCard -> varRef("empty_Bytes"))
        )
      else {
        val chunkListsExpr =
          listLiteral(
            messageModel.fields.map(fieldModel =>
              renderEncodeFieldExpr(fieldModel, fieldModel.bosatsuName, ctx)
            ) ++
              messageModel.oneofs.map(oneofModel =>
                renderEncodeOneofExpr(oneofModel, oneofModel.bosatsuFieldName, ctx)
              )
          )

        val chunkFold =
          dotCall(
            dotCall(
              varRef("chunk_lists"),
              "foldl_List",
              List(
                emptyListExpr,
                lambda(
                  List("acc", "next_chunk_list"),
                  dotCall(varRef("next_chunk_list"), "reverse_concat", List(varRef("acc")))
                )
              )
            ),
            "reverse"
          )

        val chunkBindings =
          bindVarIn(
            "chunk_lists",
            chunkListsExpr,
            bindVarIn(
              "chunks",
              chunkFold,
              call("concat_all_Bytes", varRef("chunks"))
            )
          )

        bindIn(
          messageRecordPattern(messageModel.bosatsuName, messageFieldNames),
          varRef("input_value"),
          chunkBindings
        )
      }

    defStatement(
      codecInfo.encodeFnName,
      List("input_value" -> Some(typeNameRef(messageModel.bosatsuName))),
      typeNameRef("Bytes"),
      body
    )
  }

  private def renderDecodeMessageStatement(
      messageModel: MessageModel,
      codecInfo: MessageCodecInfo,
      ctx: RenderContext
  ): Statement = {
    val loopVars = decodeLoopVars(messageModel)

    val loopBody =
      renderDecodeLoopBody(
        loopTarget = "fields",
        tailVar = "tail",
        bindFieldValueNames = messageModel.fields.nonEmpty || messageModel.oneofs.nonEmpty,
        emptyBody = someExpr(messageStructExpr(messageModel, decodeFinalValues(messageModel))),
        nonEmptyBody = renderDecodeLoopNonEmptyBody(messageModel, loopVars, ctx)
      )

    val decodeBody =
      optionMatch(
        call("decode_fields", varRef("bytes")),
        "parsed",
        localDefIn(
          name = "loop",
          args = "fields" :: loopVars.toList,
          returnType = optionType(typeNameRef(messageModel.bosatsuName)),
          body = loopBody,
          in = call("loop", (varRef("parsed") :: decodeInitialValues(messageModel, ctx).toList)*)
        )
      )

    defStatement(
      codecInfo.decodeFnName,
      List("bytes" -> Some(typeNameRef("Bytes"))),
      optionType(typeNameRef(messageModel.bosatsuName)),
      decodeBody
    )
  }

  private def renderDecodeLoopNonEmptyBody(
      messageModel: MessageModel,
      loopVars: Vector[String],
      ctx: RenderContext
  ): Declaration = {
    val defaultLoop =
      call("loop", (varRef("tail") :: loopVars.map(varRef).toList)*)

    if (messageModel.fields.isEmpty && messageModel.oneofs.isEmpty) defaultLoop
    else {
      val scalarCases =
        messageModel.fields.map(fieldModel =>
          (
            enumCompareEq("field_number", fieldModel.number),
            renderDecodeFieldBody(fieldModel, loopVars, ctx): Declaration
          )
        )

      val oneofCases =
        messageModel.oneofs.flatMap(oneofModel =>
          oneofModel.fields.map(oneofField =>
            (
              enumCompareEq("field_number", oneofField.number),
              renderDecodeOneofFieldBody(
                oneofModel,
                oneofField,
                loopVars,
                ctx
              ): Declaration
            )
          )
        )

      ifElseExpr(scalarCases ++ oneofCases, defaultLoop)
    }
  }

  private def renderDecodeFieldBody(
      fieldModel: FieldModel,
      loopVars: Vector[String],
      ctx: RenderContext
  ): Declaration = {
    val targetVar = decodeVarName(fieldModel)

    fieldModel.fieldType match {
      case _: FieldType.MapEntry =>
        val decodeExpr = call(mapDecodeHelperName(fieldModel), varRef("payload"))
        val updated =
          listLiteralWithSplices(
            List(
              ListLang.SpliceOrItem.Item(varRef("decoded_value")),
              ListLang.SpliceOrItem.Splice(varRef(targetVar))
            )
          )
        decodeFieldValueMatch(
          fieldModel.fieldType,
          decodeExpr,
          "decoded_value",
          call("loop", (varRef("tail") :: replaceLoopVar(loopVars, targetVar, updated).toList)*)
        )

      case _ =>
        fieldModel.cardinality match {
          case Cardinality.Repeated =>
            if (fieldModel.packed && fieldModel.fieldType.isPackable) {
              val packedDecodeFn =
                fieldModel.fieldType.wireKind match {
                  case WireKind.Varint  => "decode_packed_varints"
                  case WireKind.Fixed32 => "decode_packed_fixed32"
                  case WireKind.Fixed64 => "decode_packed_fixed64"
                  case WireKind.LengthDelimited => "decode_packed_varints"
                }

              val mappedExpr =
                dotCall(
                  varRef("packed"),
                  "foldl_List",
                  List(
                    emptyListExpr,
                    lambda(
                      List("acc", "packed_item"),
                      listLiteralWithSplices(
                        List(
                          ListLang.SpliceOrItem.Item(
                            decodePackedItemExpr(fieldModel.fieldType, varRef("packed_item"), ctx)
                          ),
                          ListLang.SpliceOrItem.Splice(varRef("acc"))
                        )
                      )
                    )
                  )
                )

              val updated =
                dotCall(varRef("mapped"), "reverse_concat", List(varRef(targetVar)))

              val packedBody =
                optionMatch(
                  call(packedDecodeFn, varRef("payload")),
                  "packed",
                  bindVarIn(
                    "mapped",
                    mappedExpr,
                    call(
                      "loop",
                      (varRef("tail") :: replaceLoopVar(loopVars, targetVar, updated).toList)*
                    )
                  )
                )

              matchExpr(
                Declaration.MatchKind.Match,
                varRef("field_value"),
                List(
                  wireCasePattern(WireKind.LengthDelimited) -> packedBody,
                  Pattern.WildCard -> noneExpr
                )
              )
            } else {
              val decodeExpr = decodeValue(fieldModel.fieldType, ctx)
              val updated =
                listLiteralWithSplices(
                  List(
                    ListLang.SpliceOrItem.Item(varRef("decoded_value")),
                    ListLang.SpliceOrItem.Splice(varRef(targetVar))
                  )
                )
              decodeFieldValueMatch(
                fieldModel.fieldType,
                decodeExpr,
                "decoded_value",
                call("loop", (varRef("tail") :: replaceLoopVar(loopVars, targetVar, updated).toList)*)
              )
            }

          case Cardinality.Optional =>
            val decodeExpr = decodeValue(fieldModel.fieldType, ctx)
            decodeFieldValueMatch(
              fieldModel.fieldType,
              decodeExpr,
              "decoded_value",
              call(
                "loop",
                (varRef("tail") ::
                  replaceLoopVar(loopVars, targetVar, someExpr(varRef("decoded_value"))).toList)*
              )
            )

          case Cardinality.Singular =>
            val decodeExpr = decodeValue(fieldModel.fieldType, ctx)
            decodeFieldValueMatch(
              fieldModel.fieldType,
              decodeExpr,
              "decoded_value",
              call(
                "loop",
                (varRef("tail") :: replaceLoopVar(loopVars, targetVar, varRef("decoded_value")).toList)*
              )
            )
        }
    }
  }

  private def renderDecodeOneofFieldBody(
      oneofModel: OneofModel,
      oneofField: OneofFieldModel,
      loopVars: Vector[String],
      ctx: RenderContext
  ): Declaration = {
    val oneofVar = oneofModel.bosatsuFieldName
    val decodedExpr = decodeValue(oneofField.fieldType, ctx)
    val updated = constructorValue(oneofField.bosatsuCaseName, varRef("decoded_variant") :: Nil)

    decodeFieldValueMatch(
      oneofField.fieldType,
      decodedExpr,
      "decoded_variant",
      call(
        "loop",
        (varRef("tail") :: replaceLoopVar(loopVars, oneofVar, updated).toList)*
      )
    )
  }

  private def renderMapEncodeHelperStatement(
      fieldModel: FieldModel,
      mapType: FieldType.MapEntry,
      ctx: RenderContext
  ): Statement = {
    val keyEncode =
      encodeFieldExpr(1, FieldType.Scalar(mapType.keyType), varRef("item_key"), ctx)
    val valueEncode = encodeFieldExpr(2, mapType.valueType, varRef("item_value"), ctx)
    val tupleType =
      TypeRef.TypeTuple(
        List(
          typeNameRef(mapType.keyType.bosatsuTypeString),
          fieldTypeToTypeRef(mapType.valueType)
        )
      )

    defStatement(
      mapEncodeHelperName(fieldModel),
      List("item" -> Some(tupleType)),
      typeNameRef("Bytes"),
      bindIn(
        tuplePattern(List(Pattern.Var(bindable("item_key")), Pattern.Var(bindable("item_value")))),
        varRef("item"),
        call(
          "field_length_delimited",
          intLit(fieldModel.number),
          call("concat_all_Bytes", listLiteral(List(keyEncode, valueEncode)))
        )
      )
    )
  }

  private def renderMapDecodeHelperStatement(
      fieldModel: FieldModel,
      mapType: FieldType.MapEntry,
      ctx: RenderContext
  ): Statement = {
    val resultType =
      optionType(
        TypeRef.TypeTuple(
          List(
            typeNameRef(mapType.keyType.bosatsuTypeString),
            fieldTypeToTypeRef(mapType.valueType)
          )
        )
      )

    val loopBody =
      renderDecodeLoopBody(
        loopTarget = "in_fields",
        tailVar = "tail",
        bindFieldValueNames = true,
        emptyBody = someExpr(tupleExpr(List(varRef("current_key"), varRef("current_value")))),
        nonEmptyBody = {
          val keyBody =
            decodeFieldValueMatch(
              FieldType.Scalar(mapType.keyType),
              decodeValue(FieldType.Scalar(mapType.keyType), ctx),
              "next_key",
              call("loop", varRef("tail"), varRef("next_key"), varRef("current_value"))
            )
          val valueBody =
            decodeFieldValueMatch(
              mapType.valueType,
              decodeValue(mapType.valueType, ctx),
              "next_value",
              call("loop", varRef("tail"), varRef("current_key"), varRef("next_value"))
            )

          ifElseExpr(
            List(
              enumCompareEq("field_number", 1) -> keyBody,
              enumCompareEq("field_number", 2) -> valueBody
            ),
            call("loop", varRef("tail"), varRef("current_key"), varRef("current_value"))
          )
        }
      )

    val helperBody =
      optionMatch(
        call("decode_fields", varRef("payload")),
        "fields",
        localDefIn(
          name = "loop",
          args = List("in_fields", "current_key", "current_value"),
          returnType = resultType,
          body = loopBody,
          in = call(
            "loop",
            varRef("fields"),
            defaultExprForType(FieldType.Scalar(mapType.keyType), ctx),
            defaultExprForType(mapType.valueType, ctx)
          )
        )
      )

    defStatement(
      mapDecodeHelperName(fieldModel),
      List("payload" -> Some(typeNameRef("Bytes"))),
      resultType,
      helperBody
    )
  }

  private def renderDecodeLoopBody(
      loopTarget: String,
      tailVar: String,
      bindFieldValueNames: Boolean,
      emptyBody: Declaration,
      nonEmptyBody: Declaration
  ): Declaration.NonBinding = {
    val pairPattern =
      if (bindFieldValueNames)
        tuplePattern(List(Pattern.Var(bindable("field_number")), Pattern.Var(bindable("field_value"))))
      else tuplePattern(List(Pattern.WildCard, Pattern.WildCard))

    val nonEmptyPattern =
      Pattern.ListPat(
        List(
          Pattern.ListPart.Item(pairPattern),
          Pattern.ListPart.NamedList(bindable(tailVar))
        )
      )

    matchExpr(
      Declaration.MatchKind.Recur,
      varRef(loopTarget),
      List(
        Pattern.ListPat(Nil) -> emptyBody,
        nonEmptyPattern -> nonEmptyBody
      )
    )
  }

  private def renderEncodeFieldExpr(
      fieldModel: FieldModel,
      valueName: String,
      ctx: RenderContext
  ): Declaration.NonBinding =
    fieldModel.cardinality match {
      case Cardinality.Repeated =>
        fieldModel.fieldType match {
          case _: FieldType.MapEntry =>
            listComprehension(
              call(mapEncodeHelperName(fieldModel), varRef("item")),
              "item",
              varRef(valueName)
            )

          case _ if fieldModel.packed && fieldModel.fieldType.isPackable =>
            val itemBitsExpr = encodeValue(fieldModel.fieldType, varRef("item"), ctx)
            val payloadEncodeFn =
              fieldModel.fieldType.wireKind match {
                case WireKind.Varint  => "encode_varint_u64"
                case WireKind.Fixed32 => "encode_fixed32"
                case WireKind.Fixed64 => "encode_fixed64"
                case WireKind.LengthDelimited => "encode_varint_u64"
              }

            val payload =
              call(
                "concat_all_Bytes",
                listComprehension(
                  call(payloadEncodeFn, itemBitsExpr),
                  "item",
                  varRef(valueName)
                )
              )

            ifElseExpr(
              List(Declaration.Matches(varRef(valueName), Pattern.ListPat(Nil))(using generatedRegion) -> emptyListExpr),
              listLiteral(
                List(
                  call("field_length_delimited", intLit(fieldModel.number), payload)
                )
              )
            )

          case _ =>
            listComprehension(
              encodeFieldExpr(fieldModel.number, fieldModel.fieldType, varRef("item"), ctx),
              "item",
              varRef(valueName)
            )
        }

      case Cardinality.Optional =>
        matchExpr(
          Declaration.MatchKind.Match,
          varRef(valueName),
          List(
            constructorPattern("Some", Pattern.Var(bindable("present_value")) :: Nil) ->
              listLiteral(
                List(
                  encodeFieldExpr(
                    fieldModel.number,
                    fieldModel.fieldType,
                    varRef("present_value"),
                    ctx
                  )
                )
              ),
            constructorPattern("None", Nil) -> emptyListExpr
          )
        )

      case Cardinality.Singular =>
        val encodedField =
          encodeFieldExpr(fieldModel.number, fieldModel.fieldType, varRef(valueName), ctx)

        fieldModel.fieldType match {
          case enumRef: FieldType.EnumRef =>
            val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
            ifElseExpr(
              List(
                cmpIntEq(call(enumCodec.encodeFnName, varRef(valueName)), 0) -> emptyListExpr
              ),
              listLiteral(List(encodedField))
            )
          case _ =>
            val defaultPat = defaultPatternForType(fieldModel.fieldType, ctx)
            ifElseExpr(
              List(
                Declaration.Matches(varRef(valueName), defaultPat)(using generatedRegion) -> emptyListExpr
              ),
              listLiteral(List(encodedField))
            )
        }
    }

  private def renderEncodeOneofExpr(
      oneofModel: OneofModel,
      oneofValue: String,
      ctx: RenderContext
  ): Declaration.NonBinding = {
    val branches =
      (constructorPattern(notSetCtor(oneofModel), Nil) -> emptyListExpr) +:
        oneofModel.fields.map(oneofField =>
          constructorPattern(
            oneofField.bosatsuCaseName,
            Pattern.Var(bindable("variant_value")) :: Nil
          ) ->
            listLiteral(
              List(
                encodeFieldExpr(
                  oneofField.number,
                  oneofField.fieldType,
                  varRef("variant_value"),
                  ctx
                )
              )
            )
        )

    matchExpr(Declaration.MatchKind.Match, varRef(oneofValue), branches.toList)
  }

  private def decodeFieldValueMatch(
      fieldType: FieldType,
      decodedExpr: Declaration.NonBinding,
      outVar: String,
      successBody: Declaration
  ): Declaration.NonBinding =
    matchExpr(
      Declaration.MatchKind.Match,
      varRef("field_value"),
      List(
        wireCasePattern(fieldType.wireKind) -> optionMatch(decodedExpr, outVar, successBody),
        Pattern.WildCard -> noneExpr
      )
    )

  private def wireCasePattern(wireKind: WireKind): Pattern.Parsed =
    wireKind match {
      case WireKind.Varint =>
        constructorPattern("Varint", Pattern.Var(bindable("bits")) :: Nil)
      case WireKind.Fixed64 =>
        constructorPattern("Fixed64", Pattern.Var(bindable("bits")) :: Nil)
      case WireKind.Fixed32 =>
        constructorPattern("Fixed32", Pattern.Var(bindable("bits")) :: Nil)
      case WireKind.LengthDelimited =>
        constructorPattern("LengthDelimited", Pattern.Var(bindable("payload")) :: Nil)
    }

  private def decodePackedItemExpr(
      fieldType: FieldType,
      sourceExpr: Declaration.NonBinding,
      ctx: RenderContext
  ): Declaration.NonBinding =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        decodeScalarPacked(scalarType, sourceExpr)
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        call(enumCodec.decodeFnName, call("decode_enum", sourceExpr))
      case _ => sourceExpr
    }

  private def decodeScalarPacked(
      scalarType: ScalarType,
      sourceExpr: Declaration.NonBinding
  ): Declaration.NonBinding =
    scalarType match {
      case ScalarType.DoubleType   => call("decode_double", sourceExpr)
      case ScalarType.FloatType    => call("decode_float", sourceExpr)
      case ScalarType.Int32Type    => call("decode_int32", sourceExpr)
      case ScalarType.Int64Type    => call("decode_int64", sourceExpr)
      case ScalarType.UInt32Type   => call("decode_uint32", sourceExpr)
      case ScalarType.UInt64Type   => call("decode_uint64", sourceExpr)
      case ScalarType.SInt32Type   => call("decode_sint32", sourceExpr)
      case ScalarType.SInt64Type   => call("decode_sint64", sourceExpr)
      case ScalarType.Fixed32Type  => call("decode_fixed32", sourceExpr)
      case ScalarType.Fixed64Type  => call("decode_fixed64", sourceExpr)
      case ScalarType.SFixed32Type => call("decode_sfixed32", sourceExpr)
      case ScalarType.SFixed64Type => call("decode_sfixed64", sourceExpr)
      case ScalarType.BoolType     => call("decode_bool", sourceExpr)
      case ScalarType.StringType   => sourceExpr
      case ScalarType.BytesType    => sourceExpr
    }

  private def encodeFieldExpr(
      fieldNumber: Int,
      fieldType: FieldType,
      valueExpr: Declaration.NonBinding,
      ctx: RenderContext
  ): Declaration.NonBinding = {
    val encoded = encodeValue(fieldType, valueExpr, ctx)
    fieldType.wireKind match {
      case WireKind.Varint  => call("field_varint", intLit(fieldNumber), encoded)
      case WireKind.Fixed64 => call("field_fixed64", intLit(fieldNumber), encoded)
      case WireKind.Fixed32 => call("field_fixed32", intLit(fieldNumber), encoded)
      case WireKind.LengthDelimited =>
        call("field_length_delimited", intLit(fieldNumber), encoded)
    }
  }

  private def encodeValue(
      fieldType: FieldType,
      valueExpr: Declaration.NonBinding,
      ctx: RenderContext
  ): Declaration.NonBinding =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        encodeScalarValue(scalarType, valueExpr)
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        call("encode_enum", call(enumCodec.encodeFnName, valueExpr))
      case messageRef: FieldType.MessageRef =>
        val messageCodec =
          ctx.messageCodecs((messageRef.packageName, messageRef.typeName))
        call(messageCodec.encodeFnName, valueExpr)
      case _: FieldType.MapEntry =>
        valueExpr
    }

  private def decodeValue(
      fieldType: FieldType,
      ctx: RenderContext
  ): Declaration.NonBinding =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        val source =
          scalarType.wireKind match {
            case WireKind.LengthDelimited => varRef("payload")
            case _                        => varRef("bits")
          }
        decodeScalarValue(scalarType, source)
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        someExpr(call(enumCodec.decodeFnName, call("decode_enum", varRef("bits"))))
      case messageRef: FieldType.MessageRef =>
        val messageCodec =
          ctx.messageCodecs((messageRef.packageName, messageRef.typeName))
        call(messageCodec.decodeFnName, varRef("payload"))
      case _: FieldType.MapEntry =>
        noneExpr
    }

  private def encodeScalarValue(
      scalarType: ScalarType,
      valueExpr: Declaration.NonBinding
  ): Declaration.NonBinding =
    scalarType match {
      case ScalarType.DoubleType   => call("encode_double_bits", valueExpr)
      case ScalarType.FloatType    => call("encode_float_bits", valueExpr)
      case ScalarType.Int32Type    => call("encode_int32", valueExpr)
      case ScalarType.Int64Type    => call("encode_int64", valueExpr)
      case ScalarType.UInt32Type   => call("encode_uint32", valueExpr)
      case ScalarType.UInt64Type   => call("encode_uint64", valueExpr)
      case ScalarType.SInt32Type   => call("encode_sint32", valueExpr)
      case ScalarType.SInt64Type   => call("encode_sint64", valueExpr)
      case ScalarType.Fixed32Type  => call("encode_fixed32_bits", valueExpr)
      case ScalarType.Fixed64Type  => call("encode_fixed64_bits", valueExpr)
      case ScalarType.SFixed32Type => call("encode_sfixed32_bits", valueExpr)
      case ScalarType.SFixed64Type => call("encode_sfixed64_bits", valueExpr)
      case ScalarType.BoolType     => call("encode_bool", valueExpr)
      case ScalarType.StringType   => call("utf8_bytes_from_String", valueExpr)
      case ScalarType.BytesType    => valueExpr
    }

  private def decodeScalarValue(
      scalarType: ScalarType,
      sourceExpr: Declaration.NonBinding
  ): Declaration.NonBinding =
    scalarType match {
      case ScalarType.DoubleType   => someExpr(call("decode_double", sourceExpr))
      case ScalarType.FloatType    => someExpr(call("decode_float", sourceExpr))
      case ScalarType.Int32Type    => someExpr(call("decode_int32", sourceExpr))
      case ScalarType.Int64Type    => someExpr(call("decode_int64", sourceExpr))
      case ScalarType.UInt32Type   => someExpr(call("decode_uint32", sourceExpr))
      case ScalarType.UInt64Type   => someExpr(call("decode_uint64", sourceExpr))
      case ScalarType.SInt32Type   => someExpr(call("decode_sint32", sourceExpr))
      case ScalarType.SInt64Type   => someExpr(call("decode_sint64", sourceExpr))
      case ScalarType.Fixed32Type  => someExpr(call("decode_fixed32", sourceExpr))
      case ScalarType.Fixed64Type  => someExpr(call("decode_fixed64", sourceExpr))
      case ScalarType.SFixed32Type => someExpr(call("decode_sfixed32", sourceExpr))
      case ScalarType.SFixed64Type => someExpr(call("decode_sfixed64", sourceExpr))
      case ScalarType.BoolType     => someExpr(call("decode_bool", sourceExpr))
      case ScalarType.StringType   => call("utf8_bytes_to_String", sourceExpr)
      case ScalarType.BytesType    => someExpr(sourceExpr)
    }

  private def defaultExprForType(
      fieldType: FieldType,
      ctx: RenderContext
  ): Declaration.NonBinding =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        scalarType match {
          case ScalarType.DoubleType | ScalarType.FloatType =>
            Declaration.Literal(Lit.Float64.fromDouble(0.0))(using generatedRegion)
          case ScalarType.BoolType =>
            varRef("False")
          case ScalarType.StringType =>
            Declaration.Literal(Lit.Str(""))(using generatedRegion)
          case ScalarType.BytesType =>
            varRef("empty_Bytes")
          case _ =>
            intLit(0)
        }
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        call(enumCodec.decodeFnName, intLit(0))
      case _: FieldType.MessageRef =>
        noneExpr
      case _: FieldType.MapEntry =>
        emptyListExpr
    }

  private def defaultPatternForType(
      fieldType: FieldType,
      ctx: RenderContext
  ): Pattern.Parsed =
    Declaration
      .toPattern(defaultExprForType(fieldType, ctx))
      .getOrElse(Pattern.WildCard)

  private def decodeVarName(fieldModel: FieldModel): String =
    fieldModel.cardinality match {
      case Cardinality.Repeated => s"${fieldModel.bosatsuName}_rev"
      case _                    => fieldModel.bosatsuName
    }

  private def decodeLoopVars(messageModel: MessageModel): Vector[String] =
    messageModel.fields.map(decodeVarName) ++
      messageModel.oneofs.map(_.bosatsuFieldName)

  private def decodeInitialValues(
      messageModel: MessageModel,
      ctx: RenderContext
  ): Vector[Declaration.NonBinding] =
    messageModel.fields.map(initialDecodeValue(_, ctx)) ++
      messageModel.oneofs.map(oneofModel => varRef(notSetCtor(oneofModel)))

  private def decodeFinalValues(
      messageModel: MessageModel
  ): Vector[Declaration.NonBinding] =
    messageModel.fields.map(finalDecodeValue) ++
      messageModel.oneofs.map(oneofModel => varRef(oneofModel.bosatsuFieldName))

  private def initialDecodeValue(
      fieldModel: FieldModel,
      ctx: RenderContext
  ): Declaration.NonBinding =
    fieldModel.cardinality match {
      case Cardinality.Repeated => emptyListExpr
      case Cardinality.Optional => noneExpr
      case Cardinality.Singular => defaultExprForType(fieldModel.fieldType, ctx)
    }

  private def finalDecodeValue(fieldModel: FieldModel): Declaration.NonBinding =
    fieldModel.cardinality match {
      case Cardinality.Repeated => dotCall(varRef(s"${fieldModel.bosatsuName}_rev"), "reverse")
      case _                    => varRef(decodeVarName(fieldModel))
    }

  private def messageStructExpr(
      messageModel: MessageModel,
      values: Vector[Declaration.NonBinding]
  ): Declaration.NonBinding = {
    val names = messageModel.fields.map(_.bosatsuName) ++
      messageModel.oneofs.map(_.bosatsuFieldName)
    val args =
      names.zip(values).map { case (name, value) =>
        Declaration.RecordArg.Pair(bindable(name), value)
      }.toList
    Declaration.RecordConstructor(constructor(messageModel.bosatsuName), args)(using generatedRegion)
  }

  private def replaceLoopVar(
      vars: Vector[String],
      target: String,
      replacement: Declaration.NonBinding
  ): Vector[Declaration.NonBinding] =
    vars.map(v => if (v == target) replacement else varRef(v))

  private def mapEncodeHelperName(fieldModel: FieldModel): String =
    s"encode_map_entry_${fieldModel.bosatsuName}"

  private def mapDecodeHelperName(fieldModel: FieldModel): String =
    s"decode_map_entry_${fieldModel.bosatsuName}"

  private def notSetCtor(oneofModel: OneofModel): String =
    s"${oneofModel.bosatsuTypeName}_NotSet"

  private def unknownEnumCtor(enumModel: EnumModel): String =
    s"${enumModel.bosatsuName}_Unknown"

  private def emptyListExpr: Declaration.NonBinding =
    Declaration.ListDecl(ListLang.Cons(Nil))(using generatedRegion)

  private def noneExpr: Declaration.NonBinding =
    varRef("None")

  private def someExpr(value: Declaration.NonBinding): Declaration.NonBinding =
    constructorValue("Some", value :: Nil)

  private def tupleExpr(
      values: List[Declaration.NonBinding]
  ): Declaration.NonBinding =
    Declaration.TupleCons(values)(using generatedRegion)

  private def listLiteral(
      items: Seq[Declaration.NonBinding]
  ): Declaration.NonBinding =
    listLiteralWithSplices(items.toList.map(ListLang.SpliceOrItem.Item(_)))

  private def listLiteralWithSplices(
      items: List[ListLang.SpliceOrItem[Declaration.NonBinding]]
  ): Declaration.NonBinding =
    Declaration.ListDecl(ListLang.Cons(items))(using generatedRegion)

  private def listComprehension(
      itemExpr: Declaration.NonBinding,
      itemName: String,
      source: Declaration.NonBinding
  ): Declaration.NonBinding =
    Declaration.ListDecl(
      ListLang.Comprehension(
        ListLang.SpliceOrItem.Item(itemExpr),
        Pattern.Var(bindable(itemName)),
        source,
        None
      )
    )(using generatedRegion)

  private def dotCall(
      receiver: Declaration.NonBinding,
      method: String,
      args: List[Declaration.NonBinding] = Nil
  ): Declaration.NonBinding =
    Declaration.Apply(
      varRef(method),
      NonEmptyList.fromListUnsafe(receiver :: args),
      Declaration.ApplyKind.Dot
    )(using generatedRegion)

  private def lambda(
      args: List[String],
      body: Declaration
  ): Declaration.NonBinding =
    Declaration.Lambda(
      NonEmptyList.fromListUnsafe(args.map(arg => Pattern.Var(bindable(arg)))),
      body
    )(using generatedRegion)

  private def ifElseExpr(
      ifCases: Seq[(Declaration.NonBinding, Declaration)],
      elseCase: Declaration
  ): Declaration.NonBinding =
    Declaration.IfElse(
      NonEmptyList.fromListUnsafe(ifCases.toList.map { case (cond, body) =>
        (cond, notSameLine(body))
      }),
      notSameLine(elseCase)
    )(using generatedRegion)

  private def matchExpr(
      kind: Declaration.MatchKind,
      target: Declaration.NonBinding,
      branches: List[(Pattern.Parsed, Declaration)]
  ): Declaration.NonBinding =
    Declaration.Match(
      kind,
      target,
      notSameLine(
        NonEmptyList.fromListUnsafe(
          branches.map { case (pattern, body) =>
            Declaration.MatchBranch(pattern, None, notSameLine(body))(using generatedRegion)
          }
        )
      )
    )(using generatedRegion)

  private def optionMatch(
      optionExpr: Declaration.NonBinding,
      someName: String,
      someBody: Declaration
  ): Declaration.NonBinding =
    matchExpr(
      Declaration.MatchKind.Match,
      optionExpr,
      List(
        constructorPattern("Some", Pattern.Var(bindable(someName)) :: Nil) -> someBody,
        Pattern.WildCard -> noneExpr
      )
    )

  private def cmpIntEq(
      left: Declaration.NonBinding,
      rightInt: Int
  ): Declaration.NonBinding =
    Declaration.Matches(
      call("cmp_Int", left, intLit(rightInt)),
      constructorPattern("EQ", Nil)
    )(using generatedRegion)

  private def tuplePattern(items: List[Pattern.Parsed]): Pattern.Parsed =
    Pattern.PositionalStruct(Pattern.StructKind.Tuple, items)

  private def messageRecordPattern(
      messageName: String,
      fieldNames: Vector[String]
  ): Pattern.Parsed =
    Pattern.recordPat(
      constructor(messageName),
      NonEmptyList.fromListUnsafe(fieldNames.toList.map(name => Left(bindable(name))))
    )((name, style) => Pattern.StructKind.Named(name, style))

  private def bindIn(
      pattern: Pattern.Parsed,
      value: Declaration.NonBinding,
      in: Declaration
  ): Declaration =
    Declaration.Binding(
      BindingStatement(pattern, value, Padding(1, in))
    )(using generatedRegion)

  private def bindVarIn(
      name: String,
      value: Declaration.NonBinding,
      in: Declaration
  ): Declaration =
    bindIn(Pattern.Var(bindable(name)), value, in)

  private def localDefIn(
      name: String,
      args: List[String],
      returnType: TypeRef,
      body: Declaration,
      in: Declaration
  ): Declaration =
    Declaration.DefFn(
      DefStatement(
        bindable(name),
        None,
        NonEmptyList.one(args.map(argName => Pattern.Var(bindable(argName)))),
        Some(returnType),
        (notSameLine(body), Padding(1, in))
      )
    )(using generatedRegion)

  private def computeCrossPackageImports(
      fileModel: ProtoFileModel,
      ctx: RenderContext
  ): Vector[(PackageName, Set[String])] = {
    val refs = referencedCodecs(fileModel, ctx)

    refs.toVector
      .collect { case (pack, names) if pack != fileModel.bosatsuPackage =>
        (pack, names)
      }
      .sortBy(_._1.asString)
  }

  private def referencedCodecs(
      fileModel: ProtoFileModel,
      ctx: RenderContext
  ): Map[PackageName, Set[String]] = {
    val referencedTypes =
      fileModel.messages.iterator.flatMap { messageModel =>
        messageModel.fields.iterator.map(_.fieldType) ++
          messageModel.oneofs.iterator.flatMap(_.fields.iterator.map(_.fieldType))
      }.toVector

    referencedTypes.foldLeft(Map.empty[PackageName, Set[String]]) {
      case (acc, FieldType.Scalar(_)) => acc
      case (acc, enumRef: FieldType.EnumRef) =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        val names =
          Set(enumRef.typeName, enumCodec.encodeFnName, enumCodec.decodeFnName)
        acc.updated(enumRef.packageName, acc.getOrElse(enumRef.packageName, Set.empty) ++ names)
      case (acc, messageRef: FieldType.MessageRef) =>
        val messageCodec =
          ctx.messageCodecs((messageRef.packageName, messageRef.typeName))
        val names =
          Set(messageRef.typeName, messageCodec.encodeFnName, messageCodec.decodeFnName)
        acc.updated(
          messageRef.packageName,
          acc.getOrElse(messageRef.packageName, Set.empty) ++ names
        )
      case (acc, mapEntry: FieldType.MapEntry) =>
        val next = addMapValueRefs(acc, mapEntry.valueType, ctx)
        next
    }
  }

  private def addMapValueRefs(
      input: Map[PackageName, Set[String]],
      valueType: FieldType,
      ctx: RenderContext
  ): Map[PackageName, Set[String]] =
    valueType match {
      case FieldType.Scalar(_) => input
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        val names =
          Set(enumRef.typeName, enumCodec.encodeFnName, enumCodec.decodeFnName)
        input.updated(
          enumRef.packageName,
          input.getOrElse(enumRef.packageName, Set.empty) ++ names
        )
      case messageRef: FieldType.MessageRef =>
        val messageCodec =
          ctx.messageCodecs((messageRef.packageName, messageRef.typeName))
        val names =
          Set(messageRef.typeName, messageCodec.encodeFnName, messageCodec.decodeFnName)
        input.updated(
          messageRef.packageName,
          input.getOrElse(messageRef.packageName, Set.empty) ++ names
        )
      case nestedMap: FieldType.MapEntry =>
        addMapValueRefs(input, nestedMap.valueType, ctx)
    }
}
