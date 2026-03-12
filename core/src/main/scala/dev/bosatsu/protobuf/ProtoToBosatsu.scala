package dev.bosatsu.protobuf

import dev.bosatsu.PackageName

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

    val crossPackageImports = computeCrossPackageImports(fileModel, ctx)

    val sb = new StringBuilder

    sb.append(s"package ${fileModel.bosatsuPackage.asString}\n\n")

    sb.append("from Bosatsu/IO/Bytes import (\n")
    sb.append("  Bytes,\n")
    sb.append("  concat_all_Bytes,\n")
    sb.append("  empty_Bytes,\n")
    sb.append("  utf8_bytes_from_String,\n")
    sb.append("  utf8_bytes_to_String,\n")
    sb.append(")\n\n")

    sb.append("from Bosatsu/Proto/Wire import (\n")
    wireImports.toVector.sorted.foreach { item =>
      sb.append(s"  $item,\n")
    }
    sb.append(")\n")

    if (crossPackageImports.nonEmpty) {
      sb.append("\n")
      crossPackageImports.foreach { case (pack, items) =>
        sb.append(s"from ${pack.asString} import (\n")
        items.toVector.sorted.foreach { item =>
          sb.append(s"  $item,\n")
        }
        sb.append(")\n")
      }
    }

    sb.append("\n")

    sb.append("export (\n")
    exports.foreach { item =>
      sb.append(s"  $item,\n")
    }
    sb.append(")\n\n")

    fileModel.enums.foreach { enumModel =>
      sb.append(renderEnum(enumModel))
      sb.append("\n\n")
    }

    fileModel.messages.foreach { messageModel =>
      messageModel.oneofs.foreach { oneofModel =>
        sb.append(renderOneof(oneofModel))
        sb.append("\n\n")
      }
      sb.append(renderStruct(messageModel))
      sb.append("\n\n")
    }

    fileModel.enums.foreach { enumModel =>
      val codecInfo = ctx.enumCodecs((enumModel.packageName, enumModel.bosatsuName))
      sb.append(renderEnumCodec(enumModel, codecInfo))
      sb.append("\n\n")
    }

    fileModel.messages.foreach { messageModel =>
      val codecInfo =
        ctx.messageCodecs((messageModel.packageName, messageModel.bosatsuName))
      sb.append(renderMessageCodec(messageModel, codecInfo, ctx))
      sb.append("\n\n")
    }

    GeneratedFile(
      fileModel.bosatsuPackage,
      fileModel.outputFilePath,
      sb.result().trim + "\n"
    )
  }

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

  private def renderEnum(enumModel: EnumModel): String = {
    val sb = new StringBuilder
    sb.append(s"enum ${enumModel.bosatsuName}:\n")
    enumModel.values.foreach { valueModel =>
      sb.append(s"  ${valueModel.bosatsuName}\n")
    }
    sb.append(s"  ${unknownEnumCtor(enumModel)}(value: Int)")
    sb.result()
  }

  private def renderOneof(oneofModel: OneofModel): String = {
    val sb = new StringBuilder
    sb.append(s"enum ${oneofModel.bosatsuTypeName}:\n")
    sb.append(s"  ${notSetCtor(oneofModel)}\n")
    oneofModel.fields.foreach { oneofField =>
      sb.append(
        s"  ${oneofField.bosatsuCaseName}(value: ${renderType(oneofField.fieldType)})\n"
      )
    }
    sb.result().trim
  }

  private def renderStruct(messageModel: MessageModel): String = {
    val fields =
      messageModel.fields.map(fieldModel =>
        s"${fieldModel.bosatsuName}: ${fieldModel.bosatsuTypeString}"
      ) ++
        messageModel.oneofs.map(oneofModel =>
          s"${oneofModel.bosatsuFieldName}: ${oneofModel.bosatsuTypeName}"
        )

    if (fields.isEmpty) s"struct ${messageModel.bosatsuName}"
    else s"struct ${messageModel.bosatsuName}(${fields.mkString(", ")})"
  }

  private def renderEnumCodec(
      enumModel: EnumModel,
      codecInfo: EnumCodecInfo
  ): String = {
    val sb = new StringBuilder

    sb.append(
      s"def ${codecInfo.encodeFnName}(value: ${enumModel.bosatsuName}) -> Int:\n"
    )
    sb.append("  match value:\n")
    enumModel.values.foreach { valueModel =>
      sb.append(s"    case ${valueModel.bosatsuName}: ${valueModel.number}\n")
    }
    sb.append(s"    case ${unknownEnumCtor(enumModel)}(unknown): unknown\n\n")

    sb.append(
      s"def ${codecInfo.decodeFnName}(value: Int) -> ${enumModel.bosatsuName}:\n"
    )

    enumModel.values.zipWithIndex.foreach { case (valueModel, idx) =>
      val kw = if (idx == 0) "if" else "elif"
      sb.append(s"  $kw cmp_Int(value, ${valueModel.number}) matches EQ:\n")
      sb.append(s"    ${valueModel.bosatsuName}\n")
    }
    sb.append("  else:\n")
    sb.append(s"    ${unknownEnumCtor(enumModel)}(value)")

    sb.result()
  }

  private def renderMessageCodec(
      messageModel: MessageModel,
      codecInfo: MessageCodecInfo,
      ctx: RenderContext
  ): String = {
    val sb = new StringBuilder

    sb.append(
      s"def ${codecInfo.encodeFnName}(value: ${messageModel.bosatsuName}) -> Bytes:\n"
    )

    val messageFieldNames =
      messageModel.fields.map(_.bosatsuName) ++
        messageModel.oneofs.map(_.bosatsuFieldName)

    if (messageFieldNames.nonEmpty) {
      sb.append(
        s"  ${messageModel.bosatsuName} { ${messageFieldNames.mkString(", ")} } = value\n"
      )
      sb.append("\n")
      sb.append("  chunk_lists = [\n")
      messageModel.fields.foreach { fieldModel =>
        sb.append(renderEncodeField(fieldModel, fieldModel.bosatsuName, ctx))
      }
      messageModel.oneofs.foreach { oneofModel =>
        sb.append(renderEncodeOneof(oneofModel, oneofModel.bosatsuFieldName, ctx))
      }
      sb.append("  ]\n")
      sb.append(
        "  chunks = chunk_lists.foldl_List([], (acc, next_chunk_list) -> next_chunk_list.reverse_concat(acc)).reverse()\n"
      )
      sb.append("  concat_all_Bytes(chunks)\n\n")
    } else {
      sb.append("  empty_Bytes\n\n")
    }

    messageModel.fields.foreach { fieldModel =>
      fieldModel.fieldType match {
        case mapType: FieldType.MapEntry =>
          sb.append(renderMapEncodeHelper(fieldModel, mapType, ctx))
          sb.append("\n\n")
          sb.append(renderMapDecodeHelper(fieldModel, mapType, ctx))
          sb.append("\n\n")
        case _ => ()
      }
    }

    sb.append(
      s"def ${codecInfo.decodeFnName}(bytes: Bytes) -> Option[${messageModel.bosatsuName}]:\n"
    )
    sb.append("  parsed <- decode_fields(bytes).if_Some()\n")

    val loopVars = decodeLoopVars(messageModel)
    sb.append(
      s"  def loop(fields, ${loopVars.mkString(", ")}) -> Option[${messageModel.bosatsuName}]:\n"
    )
    sb.append("    recur fields:\n")
    sb.append("      case []:\n")
    sb.append(
      s"        Some(${messageStructExpr(messageModel, decodeFinalValues(messageModel))})\n"
    )
    sb.append("      case [(field_number, field_value), *tail]:\n")

    if (messageModel.fields.isEmpty && messageModel.oneofs.isEmpty) {
      sb.append(s"        loop(tail, ${loopVars.mkString(", ")})\n")
    } else {
      val decodeBranches = renderDecodeBranches(messageModel, ctx)
      sb.append(decodeBranches)
      sb.append("        else:\n")
      sb.append(s"          loop(tail, ${loopVars.mkString(", ")})\n")
    }

    sb.append("\n")
    sb.append(
      s"  loop(parsed, ${decodeInitialValues(messageModel).mkString(", ")})"
    )

    sb.result()
  }

  private def renderEncodeField(
      fieldModel: FieldModel,
      valueName: String,
      ctx: RenderContext
  ): String = {
    val indent = "    "

    fieldModel.cardinality match {
      case Cardinality.Repeated =>
        fieldModel.fieldType match {
          case _: FieldType.MapEntry =>
            val helperName = mapEncodeHelperName(fieldModel)
            s"$indent[$helperName(item) for item in $valueName],\n"
          case _ if fieldModel.packed && fieldModel.fieldType.isPackable =>
            val itemBitsExpr = encodeValue(fieldModel.fieldType, "item", ctx)
            val payloadEncodeFn =
              fieldModel.fieldType.wireKind match {
                case WireKind.Varint  => "encode_varint_u64"
                case WireKind.Fixed32 => "encode_fixed32"
                case WireKind.Fixed64 => "encode_fixed64"
                case WireKind.LengthDelimited => "encode_varint_u64"
              }

            s"${indent}if $valueName matches []:\n" +
              s"$indent  []\n" +
              s"${indent}else:\n" +
              s"$indent  [field_length_delimited(${fieldModel.number}, concat_all_Bytes([$payloadEncodeFn($itemBitsExpr) for item in $valueName]))],\n"
          case _ =>
            val encodedField = encodeFieldExpr(fieldModel.number, fieldModel.fieldType, "item", ctx)
            s"$indent[$encodedField for item in $valueName],\n"
        }

      case Cardinality.Optional =>
        val encodedField =
          encodeFieldExpr(fieldModel.number, fieldModel.fieldType, "present_value", ctx)
        s"${indent}match $valueName:\n" +
          s"$indent  case Some(present_value): [$encodedField]\n" +
          s"$indent  case None: [],\n"

      case Cardinality.Singular =>
        val defaultExpr = defaultExprForType(fieldModel.fieldType, ctx)
        val encodedField =
          encodeFieldExpr(fieldModel.number, fieldModel.fieldType, valueName, ctx)
        s"${indent}if $valueName matches $defaultExpr:\n" +
          s"$indent  []\n" +
          s"${indent}else:\n" +
          s"$indent  [$encodedField],\n"
    }
  }

  private def renderEncodeOneof(
      oneofModel: OneofModel,
      oneofValue: String,
      ctx: RenderContext
  ): String = {
    val indent = "    "
    val sb = new StringBuilder

    sb.append(s"${indent}match $oneofValue:\n")
    sb.append(s"$indent  case ${notSetCtor(oneofModel)}: []\n")
    oneofModel.fields.foreach { oneofField =>
      val encoded =
        encodeFieldExpr(oneofField.number, oneofField.fieldType, "variant_value", ctx)
      sb.append(
        s"$indent  case ${oneofField.bosatsuCaseName}(variant_value): [$encoded]\n"
      )
    }
    sb.append(s"$indent,\n")

    sb.result()
  }

  private def renderMapEncodeHelper(
      fieldModel: FieldModel,
      mapType: FieldType.MapEntry,
      ctx: RenderContext
  ): String = {
    val helperName = mapEncodeHelperName(fieldModel)
    val keyEncode = encodeFieldExpr(1, FieldType.Scalar(mapType.keyType), "item_key", ctx)
    val valueEncode = encodeFieldExpr(2, mapType.valueType, "item_value", ctx)

    s"def $helperName(item: (${mapType.keyType.bosatsuTypeString}, ${renderType(mapType.valueType)})) -> Bytes:\n" +
      "  (item_key, item_value) = item\n" +
      s"  field_length_delimited(${fieldModel.number}, concat_all_Bytes([$keyEncode, $valueEncode]))"
  }

  private def renderMapDecodeHelper(
      fieldModel: FieldModel,
      mapType: FieldType.MapEntry,
      ctx: RenderContext
  ): String = {
    val helperName = mapDecodeHelperName(fieldModel)
    val keyDefault = defaultExprForType(FieldType.Scalar(mapType.keyType), ctx)
    val valueDefault = defaultExprForType(mapType.valueType, ctx)

    val keyDecode = decodeValue(FieldType.Scalar(mapType.keyType), ctx)
    val valueDecode = decodeValue(mapType.valueType, ctx)

    s"def $helperName(payload: Bytes) -> Option[(${mapType.keyType.bosatsuTypeString}, ${renderType(mapType.valueType)})]:\n" +
      "  fields <- decode_fields(payload).if_Some()\n" +
      s"  def loop(in_fields, current_key, current_value) -> Option[(${mapType.keyType.bosatsuTypeString}, ${renderType(mapType.valueType)})]:\n" +
      "    recur in_fields:\n" +
      "      case []:\n" +
      "        Some((current_key, current_value))\n" +
      "      case [(field_number, field_value), *tail]:\n" +
      "        if cmp_Int(field_number, 1) matches EQ:\n" +
      decodeFieldMatch(FieldType.Scalar(mapType.keyType), keyDecode, "next_key") +
      "              loop(tail, next_key, current_value)\n" +
      "            case _:\n" +
      "              None\n" +
      "        elif cmp_Int(field_number, 2) matches EQ:\n" +
      decodeFieldMatch(mapType.valueType, valueDecode, "next_value") +
      "              loop(tail, current_key, next_value)\n" +
      "            case _:\n" +
      "              None\n" +
      "        else:\n" +
      "          loop(tail, current_key, current_value)\n\n" +
      s"  loop(fields, $keyDefault, $valueDefault)"
  }

  private def renderDecodeBranches(
      messageModel: MessageModel,
      ctx: RenderContext
  ): String = {
    val sb = new StringBuilder

    val scalarFields = messageModel.fields.map(fieldModel =>
      (fieldModel.number, renderDecodeField(messageModel, fieldModel, ctx))
    )

    val oneofFields = messageModel.oneofs.flatMap(oneofModel =>
      oneofModel.fields.map(oneofField =>
        (oneofField.number, renderDecodeOneofField(messageModel, oneofModel, oneofField, ctx))
      )
    )

    val allFields = scalarFields ++ oneofFields

    allFields.zipWithIndex.foreach { case ((fieldNumber, body), idx) =>
      val kw = if (idx == 0) "if" else "elif"
      sb.append(s"        $kw cmp_Int(field_number, $fieldNumber) matches EQ:\n")
      sb.append(body)
    }

    sb.result()
  }

  private def renderDecodeField(
      messageModel: MessageModel,
      fieldModel: FieldModel,
      ctx: RenderContext
  ): String = {
    val loopValues = decodeLoopVars(messageModel)
    val targetVar = decodeVarName(fieldModel)

    fieldModel.fieldType match {
      case _: FieldType.MapEntry =>
        val helper = mapDecodeHelperName(fieldModel)
        decodeFieldMatch(
          fieldModel.fieldType,
          s"$helper(payload)",
          "decoded_value"
        ) +
          s"              loop(tail, ${replaceLoopVar(loopValues, targetVar, "[decoded_value, *" + targetVar + "]").mkString(", ")})\n" +
          "            case _:\n" +
          "              None\n"
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

              val unpackExpr =
                decodePackedItemExpr(fieldModel.fieldType, "packed_item", ctx)
              val updated =
                replaceLoopVar(
                  loopValues,
                  targetVar,
                  s"mapped.reverse_concat($targetVar)"
                )

              "          match field_value:\n" +
                "            case LengthDelimited(payload):\n" +
                s"              packed <- $packedDecodeFn(payload).if_Some()\n" +
                s"              mapped = packed.foldl_List([], (acc, packed_item) -> [$unpackExpr, *acc])\n" +
                s"              loop(tail, ${updated.mkString(", ")})\n" +
                "            case _:\n" +
                "              None\n"
            } else {
              val valueExpr = decodeValue(fieldModel.fieldType, ctx)
              val updated =
                replaceLoopVar(
                  loopValues,
                  targetVar,
                  "[decoded_value, *" + targetVar + "]"
                )

              decodeFieldMatch(fieldModel.fieldType, valueExpr, "decoded_value") +
                s"              loop(tail, ${updated.mkString(", ")})\n" +
                "            case _:\n" +
                "              None\n"
            }

          case Cardinality.Optional =>
            val valueExpr = decodeValue(fieldModel.fieldType, ctx)
            decodeFieldMatch(fieldModel.fieldType, valueExpr, "decoded_value") +
              s"              loop(tail, ${replaceLoopVar(loopValues, targetVar, "Some(decoded_value)").mkString(", ")})\n" +
              "            case _:\n" +
              "              None\n"

          case Cardinality.Singular =>
            val valueExpr = decodeValue(fieldModel.fieldType, ctx)
            decodeFieldMatch(fieldModel.fieldType, valueExpr, "decoded_value") +
              s"              loop(tail, ${replaceLoopVar(loopValues, targetVar, "decoded_value").mkString(", ")})\n" +
              "            case _:\n" +
              "              None\n"
        }
    }
  }

  private def renderDecodeOneofField(
      messageModel: MessageModel,
      oneofModel: OneofModel,
      oneofField: OneofFieldModel,
      ctx: RenderContext
  ): String = {
    val loopValues = decodeLoopVars(messageModel)
    val oneofVar = oneofModel.bosatsuFieldName
    val decodedExpr = decodeValue(oneofField.fieldType, ctx)

    decodeFieldMatch(oneofField.fieldType, decodedExpr, "decoded_variant") +
      s"              loop(tail, ${replaceLoopVar(loopValues, oneofVar, s"${oneofField.bosatsuCaseName}(decoded_variant)").mkString(", ")})\n" +
      "            case _:\n" +
      "              None\n"
  }

  private def decodeFieldMatch(
      fieldType: FieldType,
      decodedExpr: String,
      outVar: String
  ): String = {
    val pattern =
      fieldType.wireKind match {
        case WireKind.Varint          => "Varint(bits)"
        case WireKind.Fixed64         => "Fixed64(bits)"
        case WireKind.Fixed32         => "Fixed32(bits)"
        case WireKind.LengthDelimited => "LengthDelimited(payload)"
      }

    s"          match field_value:\n" +
      s"            case $pattern:\n" +
      s"              $outVar <- ($decodedExpr).if_Some()\n"
  }

  private def decodePackedItemExpr(
      fieldType: FieldType,
      itemName: String,
      ctx: RenderContext
  ): String =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        decodeScalarPacked(scalarType, itemName)
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        s"${enumCodec.decodeFnName}(decode_enum($itemName))"
      case _ => itemName
    }

  private def decodeScalarPacked(
      scalarType: ScalarType,
      sourceExpr: String
  ): String =
    scalarType match {
      case ScalarType.DoubleType   => s"decode_double($sourceExpr)"
      case ScalarType.FloatType    => s"decode_float($sourceExpr)"
      case ScalarType.Int32Type    => s"decode_int32($sourceExpr)"
      case ScalarType.Int64Type    => s"decode_int64($sourceExpr)"
      case ScalarType.UInt32Type   => s"decode_uint32($sourceExpr)"
      case ScalarType.UInt64Type   => s"decode_uint64($sourceExpr)"
      case ScalarType.SInt32Type   => s"decode_sint32($sourceExpr)"
      case ScalarType.SInt64Type   => s"decode_sint64($sourceExpr)"
      case ScalarType.Fixed32Type  => s"decode_fixed32($sourceExpr)"
      case ScalarType.Fixed64Type  => s"decode_fixed64($sourceExpr)"
      case ScalarType.SFixed32Type => s"decode_sfixed32($sourceExpr)"
      case ScalarType.SFixed64Type => s"decode_sfixed64($sourceExpr)"
      case ScalarType.BoolType     => s"decode_bool($sourceExpr)"
      case ScalarType.StringType   => sourceExpr
      case ScalarType.BytesType    => sourceExpr
    }

  private def encodeFieldExpr(
      fieldNumber: Int,
      fieldType: FieldType,
      valueExpr: String,
      ctx: RenderContext
  ): String = {
    val encoded = encodeValue(fieldType, valueExpr, ctx)
    fieldType.wireKind match {
      case WireKind.Varint  => s"field_varint($fieldNumber, $encoded)"
      case WireKind.Fixed64 => s"field_fixed64($fieldNumber, $encoded)"
      case WireKind.Fixed32 => s"field_fixed32($fieldNumber, $encoded)"
      case WireKind.LengthDelimited => s"field_length_delimited($fieldNumber, $encoded)"
    }
  }

  private def encodeValue(
      fieldType: FieldType,
      valueExpr: String,
      ctx: RenderContext
  ): String =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        encodeScalarValue(scalarType, valueExpr)
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        s"encode_enum(${enumCodec.encodeFnName}($valueExpr))"
      case messageRef: FieldType.MessageRef =>
        val messageCodec =
          ctx.messageCodecs((messageRef.packageName, messageRef.typeName))
        s"${messageCodec.encodeFnName}($valueExpr)"
      case _: FieldType.MapEntry =>
        valueExpr
    }

  private def decodeValue(
      fieldType: FieldType,
      ctx: RenderContext
  ): String =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        val source =
          scalarType.wireKind match {
            case WireKind.LengthDelimited => "payload"
            case _                        => "bits"
          }
        decodeScalarValue(scalarType, source)
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        s"Some(${enumCodec.decodeFnName}(decode_enum(bits)))"
      case messageRef: FieldType.MessageRef =>
        val messageCodec =
          ctx.messageCodecs((messageRef.packageName, messageRef.typeName))
        s"${messageCodec.decodeFnName}(payload)"
      case _: FieldType.MapEntry =>
        "None"
    }

  private def encodeScalarValue(scalarType: ScalarType, valueExpr: String): String =
    scalarType match {
      case ScalarType.DoubleType   => s"encode_double_bits($valueExpr)"
      case ScalarType.FloatType    => s"encode_float_bits($valueExpr)"
      case ScalarType.Int32Type    => s"encode_int32($valueExpr)"
      case ScalarType.Int64Type    => s"encode_int64($valueExpr)"
      case ScalarType.UInt32Type   => s"encode_uint32($valueExpr)"
      case ScalarType.UInt64Type   => s"encode_uint64($valueExpr)"
      case ScalarType.SInt32Type   => s"encode_sint32($valueExpr)"
      case ScalarType.SInt64Type   => s"encode_sint64($valueExpr)"
      case ScalarType.Fixed32Type  => s"encode_fixed32_bits($valueExpr)"
      case ScalarType.Fixed64Type  => s"encode_fixed64_bits($valueExpr)"
      case ScalarType.SFixed32Type => s"encode_sfixed32_bits($valueExpr)"
      case ScalarType.SFixed64Type => s"encode_sfixed64_bits($valueExpr)"
      case ScalarType.BoolType     => s"encode_bool($valueExpr)"
      case ScalarType.StringType   => s"utf8_bytes_from_String($valueExpr)"
      case ScalarType.BytesType    => valueExpr
    }

  private def decodeScalarValue(scalarType: ScalarType, sourceExpr: String): String =
    scalarType match {
      case ScalarType.DoubleType   => s"Some(decode_double($sourceExpr))"
      case ScalarType.FloatType    => s"Some(decode_float($sourceExpr))"
      case ScalarType.Int32Type    => s"Some(decode_int32($sourceExpr))"
      case ScalarType.Int64Type    => s"Some(decode_int64($sourceExpr))"
      case ScalarType.UInt32Type   => s"Some(decode_uint32($sourceExpr))"
      case ScalarType.UInt64Type   => s"Some(decode_uint64($sourceExpr))"
      case ScalarType.SInt32Type   => s"Some(decode_sint32($sourceExpr))"
      case ScalarType.SInt64Type   => s"Some(decode_sint64($sourceExpr))"
      case ScalarType.Fixed32Type  => s"Some(decode_fixed32($sourceExpr))"
      case ScalarType.Fixed64Type  => s"Some(decode_fixed64($sourceExpr))"
      case ScalarType.SFixed32Type => s"Some(decode_sfixed32($sourceExpr))"
      case ScalarType.SFixed64Type => s"Some(decode_sfixed64($sourceExpr))"
      case ScalarType.BoolType     => s"Some(decode_bool($sourceExpr))"
      case ScalarType.StringType   => "utf8_bytes_to_String(payload)"
      case ScalarType.BytesType    => "Some(payload)"
    }

  private def defaultExprForType(fieldType: FieldType, ctx: RenderContext): String =
    fieldType match {
      case FieldType.Scalar(scalarType) =>
        scalarType match {
          case ScalarType.DoubleType | ScalarType.FloatType => "0.0"
          case ScalarType.BoolType                          => "False"
          case ScalarType.StringType                        => "\"\""
          case ScalarType.BytesType                         => "empty_Bytes"
          case _                                            => "0"
        }
      case enumRef: FieldType.EnumRef =>
        val enumCodec = ctx.enumCodecs((enumRef.packageName, enumRef.typeName))
        s"${enumCodec.decodeFnName}(0)"
      case _: FieldType.MessageRef =>
        "None"
      case _: FieldType.MapEntry =>
        "[]"
    }

  private def decodeVarName(fieldModel: FieldModel): String =
    fieldModel.cardinality match {
      case Cardinality.Repeated => s"${fieldModel.bosatsuName}_rev"
      case _                    => fieldModel.bosatsuName
    }

  private def decodeLoopVars(messageModel: MessageModel): Vector[String] =
    messageModel.fields.map(decodeVarName) ++
      messageModel.oneofs.map(_.bosatsuFieldName)

  private def decodeInitialValues(messageModel: MessageModel): Vector[String] =
    messageModel.fields.map(initialDecodeValue) ++
      messageModel.oneofs.map(notSetCtor)

  private def decodeFinalValues(messageModel: MessageModel): Vector[String] =
    messageModel.fields.map(finalDecodeValue) ++
      messageModel.oneofs.map(_.bosatsuFieldName)

  private def initialDecodeValue(fieldModel: FieldModel): String =
    fieldModel.cardinality match {
      case Cardinality.Repeated => "[]"
      case Cardinality.Optional => "None"
      case Cardinality.Singular =>
        fieldModel.fieldType match {
          case FieldType.Scalar(scalarType) =>
            scalarType match {
              case ScalarType.DoubleType | ScalarType.FloatType => "0.0"
              case ScalarType.BoolType                          => "False"
              case ScalarType.StringType                        => "\"\""
              case ScalarType.BytesType                         => "empty_Bytes"
              case _                                            => "0"
            }
          case _: FieldType.EnumRef => "decode_enum(0)"
          case _: FieldType.MessageRef => "None"
          case _: FieldType.MapEntry => "[]"
        }
    }

  private def finalDecodeValue(fieldModel: FieldModel): String =
    fieldModel.cardinality match {
      case Cardinality.Repeated => s"${fieldModel.bosatsuName}_rev.reverse()"
      case _                    => decodeVarName(fieldModel)
    }

  private def messageStructExpr(
      messageModel: MessageModel,
      values: Vector[String]
  ): String = {
    val names = messageModel.fields.map(_.bosatsuName) ++
      messageModel.oneofs.map(_.bosatsuFieldName)

    val renderedArgs =
      names.zip(values).map { case (name, value) => s"$name: $value" }.mkString(", ")

    s"${messageModel.bosatsuName} { $renderedArgs }"
  }

  private def replaceLoopVar(
      vars: Vector[String],
      target: String,
      replacement: String
  ): Vector[String] =
    vars.map(v => if (v == target) replacement else v)

  private def mapEncodeHelperName(fieldModel: FieldModel): String =
    s"encode_map_entry_${fieldModel.bosatsuName}"

  private def mapDecodeHelperName(fieldModel: FieldModel): String =
    s"decode_map_entry_${fieldModel.bosatsuName}"

  private def notSetCtor(oneofModel: OneofModel): String =
    s"${oneofModel.bosatsuTypeName}_NotSet"

  private def unknownEnumCtor(enumModel: EnumModel): String =
    s"${enumModel.bosatsuName}_Unknown"

  private def renderType(fieldType: FieldType): String =
    fieldType match {
      case FieldType.Scalar(scalarType)         => scalarType.bosatsuTypeString
      case FieldType.EnumRef(_, typeName)       => typeName
      case FieldType.MessageRef(_, typeName)    => typeName
      case FieldType.MapEntry(keyType, valueType) =>
        s"List[(${keyType.bosatsuTypeString}, ${renderType(valueType)})]"
    }

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
