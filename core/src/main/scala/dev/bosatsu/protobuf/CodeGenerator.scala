package dev.bosatsu.protobuf

import com.google.protobuf.descriptor.{
  DescriptorProto,
  EnumDescriptorProto,
  FieldDescriptorProto,
  FileDescriptorProto
}
import com.google.protobuf.compiler.plugin.{
  CodeGeneratorRequest,
  CodeGeneratorResponse
}
import dev.bosatsu.PackageName
import dev.bosatsu.protobuf.ProtoDescriptorModel.{
  Cardinality,
  EnumModel,
  EnumValueModel,
  FieldModel,
  FieldType,
  MessageModel,
  OneofFieldModel,
  OneofModel,
  ProtoFileModel,
  ScalarType,
  Syntax
}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

object CodeGenerator {
  // Matches protoc's FEATURE_PROTO3_OPTIONAL bit (1 << 0).
  private val Proto3OptionalFeatureBit: Long = 1L

  private final case class EnumInfo(
      fileName: String,
      path: List[String],
      fullName: String,
      descriptor: EnumDescriptorProto
  )

  private final case class MessageInfo(
      fileName: String,
      path: List[String],
      fullName: String,
      descriptor: DescriptorProto
  ) {
    val isMapEntry: Boolean =
      descriptor.options.flatMap(_.mapEntry).contains(true)
  }

  private final case class TypeIndex(
      enumsByFullName: Map[String, EnumInfo],
      messagesByFullName: Map[String, MessageInfo],
      typeNameByFullName: Map[String, String],
      packageByFileName: Map[String, PackageName]
  )

  def generateResponse(
      request: CodeGeneratorRequest
  ): CodeGeneratorResponse = {
    val response = buildModels(request) match {
      case Left(errors) =>
        CodeGeneratorResponse.defaultInstance.withError(errors.mkString("\n"))

      case Right(models) =>
        val requestedFiles = request.fileToGenerate.toSet
        val selected = models.filter(model => requestedFiles(model.fileName))
        val generated = ProtoToBosatsu.renderAll(selected)
        val files = generated.map { output =>
          CodeGeneratorResponse.File.defaultInstance
            .withName(output.outputFilePath)
            .withContent(output.content)
        }
        CodeGeneratorResponse.defaultInstance.withFile(files)
    }
    response.withSupportedFeatures(Proto3OptionalFeatureBit)
  }

  /** Shared protobuf plugin protocol entrypoint for JVM and Scala.js wrappers.
    */
  def generateResponseBytes(requestBytes: Array[Byte]): Array[Byte] = {
    val response =
      try {
        generateResponse(CodeGeneratorRequest.parseFrom(requestBytes))
      } catch {
        case NonFatal(err) =>
          val msg = Option(err.getMessage).getOrElse(err.toString)
          CodeGeneratorResponse.defaultInstance
            .withError(msg)
            .withSupportedFeatures(Proto3OptionalFeatureBit)
      }
    response.toByteArray
  }

  private def buildModels(
      request: CodeGeneratorRequest
  ): Either[Vector[String], Vector[ProtoFileModel]] = {
    val descriptors = request.protoFile.toVector
    val validations = descriptors.flatMap(validateFile)
    if (validations.nonEmpty) Left(validations)
    else {
      val packageByFileName = descriptors.map { file =>
        val protoPackage = optionNonEmpty(file.getPackage)
        file.getName -> ProtoNaming.packageName(protoPackage, file.getName)
      }.toMap

      val allEnums = descriptors.flatMap(collectEnums)
      val allMessages = descriptors.flatMap(collectMessages)

      val rootTypeNames = {
        val typeCandidates =
          (allEnums.map(info => info.fullName -> ProtoNaming.flattenedTypeName(info.path)) ++
            allMessages
              .filterNot(_.isMapEntry)
              .map(info => info.fullName -> ProtoNaming.flattenedTypeName(info.path)))
            .toVector

        ProtoNaming.dedupeNames(typeCandidates)
      }

      val typeIndex = TypeIndex(
        enumsByFullName = allEnums.map(info => info.fullName -> info).toMap,
        messagesByFullName = allMessages.map(info => info.fullName -> info).toMap,
        typeNameByFullName = rootTypeNames,
        packageByFileName = packageByFileName
      )

      val cyclicMessageRefEdges = cyclicMessageReferenceEdges(allMessages, typeIndex)

      val sortedMessages =
        allMessages.filterNot(_.isMapEntry).sortBy(info => (info.fileName, info.fullName))

      val messageBaseNames = sortedMessages.map { info =>
        info.fullName -> rootTypeNames(info.fullName)
      }.toMap

      var usedTypeNames: Set[String] = rootTypeNames.values.toSet
      val oneofTypeNameByMessage: Map[String, Vector[(Int, String)]] =
        sortedMessages.map { info =>
          val messageType = messageBaseNames(info.fullName)
          val oneofDecls = info.descriptor.oneofDecl.toVector
          val activeOneofIndexes =
            info.descriptor.field.iterator
              .filter(field => field.oneofIndex.nonEmpty && !field.getProto3Optional)
              .map(_.getOneofIndex)
              .toSet

          val names = oneofDecls.zipWithIndex.collect {
            case (decl, idx) if activeOneofIndexes(idx) =>
              val base =
                s"${messageType}_${ProtoNaming.constructorBaseName(decl.getName)}"
              val unique = uniqueTypeName(base, usedTypeNames)
              usedTypeNames = usedTypeNames + unique
              idx -> unique
          }

          info.fullName -> names
        }.toMap

      val fileModels = descriptors.sortBy(_.getName).map { file =>
        val fileEnums =
          allEnums.filter(_.fileName == file.getName).sortBy(_.fullName)
        val fileMessages =
          sortedMessages.filter(_.fileName == file.getName)

        val enumModels = fileEnums.map(convertEnum(_, typeIndex))
        val messageModels = fileMessages.map { info =>
          convertMessage(
            info,
            typeIndex,
            messageBaseNames(info.fullName),
            oneofTypeNameByMessage.getOrElse(info.fullName, Vector.empty),
            cyclicMessageRefEdges
          )
        }

        val protoPackage = optionNonEmpty(file.getPackage)
        val bosatsuPackage = packageByFileName(file.getName)

        ProtoFileModel(
          fileName = file.getName,
          protoPackage = protoPackage,
          bosatsuPackage = bosatsuPackage,
          outputFilePath = ProtoNaming.outputFilePath(bosatsuPackage),
          syntax = Syntax.Proto3,
          enums = enumModels,
          messages = messageModels
        )
      }

      Right(fileModels)
    }
  }

  private def validateFile(file: FileDescriptorProto): Vector[String] = {
    val syntax = optionNonEmpty(file.getSyntax).getOrElse("proto2")
    val syntaxError =
      if (syntax != "proto3")
        Vector(s"${file.getName}: only proto3 is supported")
      else Vector.empty

    val extensionError =
      if (file.extension.nonEmpty)
        Vector(s"${file.getName}: extensions are not supported")
      else Vector.empty

    val groupErrors =
      collectMessages(file).flatMap { info =>
        info.descriptor.field.toVector.collect {
          case field if field.getType.equals(FieldDescriptorProto.Type.TYPE_GROUP) =>
            s"${file.getName}: groups are not supported (${field.getName})"
        }
      }

    syntaxError ++ extensionError ++ groupErrors
  }

  private def collectEnums(file: FileDescriptorProto): Vector[EnumInfo] = {
    val protoPackage = optionNonEmpty(file.getPackage)

    val topLevel = file.enumType.toVector.map { enumDescriptor =>
      val path = enumDescriptor.getName :: Nil
      EnumInfo(
        fileName = file.getName,
        path = path,
        fullName = fullNameOf(protoPackage, path),
        descriptor = enumDescriptor
      )
    }

    topLevel ++ collectNestedEnums(
      file.getName,
      protoPackage,
      Nil,
      file.messageType.toVector
    )
  }

  private def collectMessages(file: FileDescriptorProto): Vector[MessageInfo] = {
    val protoPackage = optionNonEmpty(file.getPackage)
    collectNestedMessages(
      file.getName,
      protoPackage,
      Nil,
      file.messageType.toVector
    )
  }

  private def collectNestedEnums(
      fileName: String,
      protoPackage: Option[String],
      parentPath: List[String],
      messages: Vector[DescriptorProto]
  ): Vector[EnumInfo] =
    messages.flatMap { messageDescriptor =>
      val messagePath = parentPath :+ messageDescriptor.getName
      val thisEnums = messageDescriptor.enumType.toVector.map { enumDescriptor =>
        val enumPath = messagePath :+ enumDescriptor.getName
        EnumInfo(
          fileName = fileName,
          path = enumPath,
          fullName = fullNameOf(protoPackage, enumPath),
          descriptor = enumDescriptor
        )
      }

      thisEnums ++ collectNestedEnums(
        fileName,
        protoPackage,
        messagePath,
        messageDescriptor.nestedType.toVector
      )
    }

  private def collectNestedMessages(
      fileName: String,
      protoPackage: Option[String],
      parentPath: List[String],
      messages: Vector[DescriptorProto]
  ): Vector[MessageInfo] =
    messages.flatMap { messageDescriptor =>
      val messagePath = parentPath :+ messageDescriptor.getName
      val current = MessageInfo(
        fileName = fileName,
        path = messagePath,
        fullName = fullNameOf(protoPackage, messagePath),
        descriptor = messageDescriptor
      )

      current +: collectNestedMessages(
        fileName,
        protoPackage,
        messagePath,
        messageDescriptor.nestedType.toVector
      )
    }

  private def convertEnum(
      enumInfo: EnumInfo,
      typeIndex: TypeIndex
  ): EnumModel = {
    val enumTypeName = typeIndex.typeNameByFullName(enumInfo.fullName)
    val packageName = typeIndex.packageByFileName(enumInfo.fileName)
    val values = enumInfo.descriptor.value.toVector

    val valueNames = ProtoNaming.dedupeNames(
      values.zipWithIndex.map { case (valueDescriptor, idx) =>
        idx -> s"${enumTypeName}_${ProtoNaming.constructorBaseName(valueDescriptor.getName)}"
      }
    )

    val valueModels = values.zipWithIndex.map { case (valueDescriptor, idx) =>
      EnumValueModel(
        protoName = valueDescriptor.getName,
        bosatsuName = valueNames(idx),
        number = valueDescriptor.getNumber
      )
    }

    EnumModel(
      protoName = enumInfo.path.last,
      bosatsuName = enumTypeName,
      packageName = packageName,
      values = valueModels
    )
  }

  private def convertMessage(
      messageInfo: MessageInfo,
      typeIndex: TypeIndex,
      messageTypeName: String,
      oneofTypeNames: Vector[(Int, String)],
      cyclicMessageRefEdges: Set[(String, String)]
  ): MessageModel = {
    val packageName = typeIndex.packageByFileName(messageInfo.fileName)
    val fields = messageInfo.descriptor.field.toVector

    val realOneofIndexes =
      fields.iterator
        .filter(field => field.oneofIndex.nonEmpty && !field.getProto3Optional)
        .map(_.getOneofIndex)
        .toSet

    val regularFields = fields.zipWithIndex.filterNot { case (field, _) =>
      field.oneofIndex.nonEmpty && realOneofIndexes(field.getOneofIndex)
    }

    val regularFieldNames = ProtoNaming.dedupeNames(
      regularFields.map { case (field, idx) =>
        idx -> ProtoNaming.bindableBaseName(field.getName)
      }
    )

    val regularModels = regularFields.map { case (field, idx) =>
      val resolvedType =
        resolveFieldType(
          field,
          typeIndex,
          messageInfo.fullName,
          cyclicMessageRefEdges
        )
      val isMap = resolvedType.isInstanceOf[FieldType.MapEntry]
      val isMessageField =
        field.getType.equals(FieldDescriptorProto.Type.TYPE_MESSAGE) && !isMap
      val inOneof = field.oneofIndex.nonEmpty && realOneofIndexes(field.getOneofIndex)
      val cardinality =
        fieldCardinality(field, resolvedType, isMap, inOneof, isMessageField)
      val packed = fieldPacked(field, resolvedType, cardinality)

      FieldModel(
        protoName = field.getName,
        bosatsuName = regularFieldNames(idx),
        number = field.getNumber,
        cardinality = cardinality,
        fieldType = resolvedType,
        packed = packed
      )
    }

    val oneofDecls = messageInfo.descriptor.oneofDecl.toVector
    var usedFieldNames = regularModels.map(_.bosatsuName).toSet

    val oneofTypeNameMap = oneofTypeNames.toMap

    val oneofModels = oneofDecls.zipWithIndex.collect {
      case (oneofDecl, oneofIdx) if realOneofIndexes(oneofIdx) =>
        val oneofFieldNameBase = ProtoNaming.bindableBaseName(oneofDecl.getName)
        val oneofFieldName = uniqueBindableName(oneofFieldNameBase, usedFieldNames)
        usedFieldNames = usedFieldNames + oneofFieldName

        val oneofTypeName = oneofTypeNameMap(oneofIdx)
        val oneofFields = fields.filter(field =>
          field.oneofIndex.contains(oneofIdx) && !field.getProto3Optional
        )

        val caseNames = ProtoNaming.dedupeNames(
          oneofFields.zipWithIndex.map { case (field, idx) =>
            idx -> s"${oneofTypeName}_${ProtoNaming.constructorBaseName(field.getName)}"
          }
        )

        val oneofFieldModels = oneofFields.zipWithIndex.map { case (field, idx) =>
          OneofFieldModel(
            protoName = field.getName,
            bosatsuCaseName = caseNames(idx),
            number = field.getNumber,
            fieldType = resolveFieldType(
              field,
              typeIndex,
              messageInfo.fullName,
              cyclicMessageRefEdges
            )
          )
        }

        OneofModel(
          protoName = oneofDecl.getName,
          bosatsuFieldName = oneofFieldName,
          bosatsuTypeName = oneofTypeName,
          fields = oneofFieldModels
        )
    }

    MessageModel(
      protoName = messageInfo.path.last,
      bosatsuName = messageTypeName,
      packageName = packageName,
      fields = regularModels,
      oneofs = oneofModels
    )
  }

  private def resolveFieldType(
      field: FieldDescriptorProto,
      typeIndex: TypeIndex,
      ownerMessageFullName: String,
      cyclicMessageRefEdges: Set[(String, String)]
  ): FieldType = {
    val fieldType = field.getType

    if (fieldType.equals(FieldDescriptorProto.Type.TYPE_DOUBLE))
      FieldType.Scalar(ScalarType.DoubleType)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_FLOAT))
      FieldType.Scalar(ScalarType.FloatType)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_INT32))
      FieldType.Scalar(ScalarType.Int32Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_INT64))
      FieldType.Scalar(ScalarType.Int64Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_UINT32))
      FieldType.Scalar(ScalarType.UInt32Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_UINT64))
      FieldType.Scalar(ScalarType.UInt64Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_SINT32))
      FieldType.Scalar(ScalarType.SInt32Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_SINT64))
      FieldType.Scalar(ScalarType.SInt64Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_FIXED32))
      FieldType.Scalar(ScalarType.Fixed32Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_FIXED64))
      FieldType.Scalar(ScalarType.Fixed64Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_SFIXED32))
      FieldType.Scalar(ScalarType.SFixed32Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_SFIXED64))
      FieldType.Scalar(ScalarType.SFixed64Type)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_BOOL))
      FieldType.Scalar(ScalarType.BoolType)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_STRING))
      FieldType.Scalar(ScalarType.StringType)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_BYTES))
      FieldType.Scalar(ScalarType.BytesType)
    else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_ENUM)) {
      val enumInfo =
        typeIndex.enumsByFullName.getOrElse(
          field.getTypeName,
          throw new IllegalArgumentException(
            s"unknown enum reference: ${field.getTypeName}"
          )
        )

      FieldType.EnumRef(
        packageName = typeIndex.packageByFileName(enumInfo.fileName),
        typeName = typeIndex.typeNameByFullName(enumInfo.fullName)
      )
    } else if (fieldType.equals(FieldDescriptorProto.Type.TYPE_MESSAGE)) {
      val messageInfo =
        typeIndex.messagesByFullName.getOrElse(
          field.getTypeName,
          throw new IllegalArgumentException(
            s"unknown message reference: ${field.getTypeName}"
          )
        )

      if (
        messageInfo.isMapEntry &&
        field.getLabel.equals(FieldDescriptorProto.Label.LABEL_REPEATED)
      ) {
        val mapEntryFields = messageInfo.descriptor.field.toVector
        val keyField = mapEntryFields.find(_.getName == "key").getOrElse {
          throw new IllegalArgumentException(
            s"invalid map entry without key: ${messageInfo.fullName}"
          )
        }
        val valueField = mapEntryFields.find(_.getName == "value").getOrElse {
          throw new IllegalArgumentException(
            s"invalid map entry without value: ${messageInfo.fullName}"
          )
        }

        val keyType =
          resolveFieldType(
            keyField,
            typeIndex,
            ownerMessageFullName,
            cyclicMessageRefEdges
          ) match {
          case FieldType.Scalar(scalarType) => scalarType
          case other                        =>
            throw new IllegalArgumentException(
              s"map keys must be scalar, found: $other"
            )
        }

        val valueType =
          resolveFieldType(
            valueField,
            typeIndex,
            ownerMessageFullName,
            cyclicMessageRefEdges
          )

        FieldType.MapEntry(keyType, valueType)
      } else {
        // Bosatsu type definitions cannot express multi-type mutual recursion.
        // For cyclic protobuf message references, use opaque Bytes payloads so
        // generated code remains type-checkable and wire-compatible.
        if (cyclicMessageRefEdges((ownerMessageFullName, messageInfo.fullName)))
          FieldType.Scalar(ScalarType.BytesType)
        else
          FieldType.MessageRef(
            packageName = typeIndex.packageByFileName(messageInfo.fileName),
            typeName = typeIndex.typeNameByFullName(messageInfo.fullName)
          )
      }
    } else
      throw new IllegalArgumentException(s"unsupported field type: $fieldType")
  }

  private def fieldCardinality(
      field: FieldDescriptorProto,
      fieldType: FieldType,
      isMap: Boolean,
      inOneof: Boolean,
      isMessageField: Boolean
  ): Cardinality =
    if (isMap || field.getLabel.equals(FieldDescriptorProto.Label.LABEL_REPEATED))
      Cardinality.Repeated
    else if (field.getProto3Optional) Cardinality.Optional
    else if (inOneof) Cardinality.Singular
    else {
      if (isMessageField) Cardinality.Optional
      else
        fieldType match {
          case _: FieldType.MessageRef => Cardinality.Optional
          case _                       => Cardinality.Singular
        }
    }

  private def fieldPacked(
      field: FieldDescriptorProto,
      fieldType: FieldType,
      cardinality: Cardinality
  ): Boolean =
    if (cardinality != Cardinality.Repeated || !fieldType.isPackable) false
    else field.options.flatMap(_.packed).getOrElse(true)

  private def fullNameOf(
      protoPackage: Option[String],
      path: List[String]
  ): String = {
    val packageParts =
      protoPackage.toList.flatMap(_.split("\\.").toList).filter(_.nonEmpty)
    ("." + (packageParts ++ path).mkString(".")).trim
  }

  private def optionNonEmpty(value: String): Option[String] = {
    val trimmed = value.trim
    if (trimmed.isEmpty) None else Some(trimmed)
  }

  private def uniqueBindableName(baseName: String, usedNames: Set[String]): String =
    if (!usedNames(baseName)) baseName
    else {
      @tailrec
      def loop(idx: Int): String = {
        val candidate = s"${baseName}_$idx"
        if (!usedNames(candidate)) candidate else loop(idx + 1)
      }

      loop(2)
    }

  private def uniqueTypeName(baseName: String, usedNames: Set[String]): String =
    uniqueBindableName(baseName, usedNames)

  private def cyclicMessageReferenceEdges(
      allMessages: Vector[MessageInfo],
      typeIndex: TypeIndex
  ): Set[(String, String)] = {
    val nonMapMessages = allMessages.filterNot(_.isMapEntry)
    val byFullName = nonMapMessages.map(msg => msg.fullName -> msg).toMap

    val edges: Map[String, Set[String]] = nonMapMessages.map { messageInfo =>
      val refs = messageInfo.descriptor.field.iterator
        .filter(_.getType.equals(FieldDescriptorProto.Type.TYPE_MESSAGE))
        .flatMap { field =>
          typeIndex.messagesByFullName
            .get(field.getTypeName)
            .filterNot(_.isMapEntry)
            .map(_.fullName)
        }
        .toSet
      messageInfo.fullName -> refs
    }.toMap

    val sccs = stronglyConnectedComponents(edges)

    sccs.iterator
      .flatMap { component =>
        val isSelfRecursive =
          component.exists(node => edges.getOrElse(node, Set.empty).contains(node))
        if (component.size <= 1 && !isSelfRecursive) Iterator.empty
        else {
          val componentSet = component.toSet
          component.iterator.flatMap { src =>
            edges
              .getOrElse(src, Set.empty)
              .iterator
              .filter(componentSet)
              .map(dst => (src, dst))
          }
        }
      }
      .filter { case (src, dst) =>
        byFullName.contains(src) && byFullName.contains(dst)
      }
      .toSet
  }

  private def stronglyConnectedComponents(
      graph: Map[String, Set[String]]
  ): Vector[Vector[String]] = {
    var index = 0
    val indices = mutable.Map.empty[String, Int]
    val lowLink = mutable.Map.empty[String, Int]
    val onStack = mutable.Set.empty[String]
    val stack = mutable.ArrayBuffer.empty[String]
    val components = mutable.ArrayBuffer.empty[Vector[String]]

    def strongConnect(node: String): Unit = {
      indices(node) = index
      lowLink(node) = index
      index += 1
      stack.append(node)
      onStack += node

      graph.getOrElse(node, Set.empty).toVector.sorted.foreach { next =>
        if (!indices.contains(next)) {
          strongConnect(next)
          lowLink(node) = lowLink(node).min(lowLink(next))
        } else if (onStack(next)) {
          lowLink(node) = lowLink(node).min(indices(next))
        }
      }

      if (lowLink(node) == indices(node)) {
        val component = mutable.ArrayBuffer.empty[String]
        var done = false
        while (!done && stack.nonEmpty) {
          val popped = stack.remove(stack.length - 1)
          onStack -= popped
          component.append(popped)
          done = popped == node
        }
        components.append(component.toVector)
      }
    }

    graph.keys.toVector.sorted.foreach { node =>
      if (!indices.contains(node)) strongConnect(node)
    }

    components.toVector
  }
}
