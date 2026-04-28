package dev.bosatsu.protobuf

import dev.bosatsu.PackageName

object ProtoDescriptorModel {

  enum Syntax derives CanEqual {
    case Proto3
    case Proto2
    case Unknown(value: String)
  }

  enum Cardinality derives CanEqual {
    case Singular
    case Optional
    case Repeated
  }

  enum ScalarType derives CanEqual {
    case DoubleType
    case FloatType
    case Int32Type
    case Int64Type
    case UInt32Type
    case UInt64Type
    case SInt32Type
    case SInt64Type
    case Fixed32Type
    case Fixed64Type
    case SFixed32Type
    case SFixed64Type
    case BoolType
    case StringType
    case BytesType

    def bosatsuTypeString: String =
      this match {
        case DoubleType | FloatType => "Float64"
        case Int32Type | Int64Type | UInt32Type | UInt64Type | SInt32Type |
            SInt64Type | Fixed32Type | Fixed64Type | SFixed32Type |
            SFixed64Type =>
          "Int"
        case BoolType => "Bool"
        case StringType => "String"
        case BytesType => "Bytes"
      }

    def wireKind: WireKind =
      this match {
        case DoubleType | Fixed64Type | SFixed64Type => WireKind.Fixed64
        case FloatType | Fixed32Type | SFixed32Type  => WireKind.Fixed32
        case StringType | BytesType                  => WireKind.LengthDelimited
        case _                                       => WireKind.Varint
      }

    def isPackable: Boolean =
      this match {
        case StringType | BytesType => false
        case _                      => true
      }
  }

  enum WireKind derives CanEqual {
    case Varint
    case Fixed64
    case LengthDelimited
    case Fixed32
  }

  sealed trait FieldType derives CanEqual {
    def bosatsuTypeString: String
    def wireKind: WireKind
    def isPackable: Boolean
  }

  object FieldType {
    final case class Scalar(scalarType: ScalarType) extends FieldType {
      def bosatsuTypeString: String = scalarType.bosatsuTypeString
      def wireKind: WireKind = scalarType.wireKind
      def isPackable: Boolean = scalarType.isPackable
    }

    final case class MessageRef(
        packageName: PackageName,
        typeName: String
    ) extends FieldType {
      def bosatsuTypeString: String = typeName
      val wireKind: WireKind = WireKind.LengthDelimited
      val isPackable: Boolean = false
    }

    final case class EnumRef(
        packageName: PackageName,
        typeName: String
    ) extends FieldType {
      def bosatsuTypeString: String = typeName
      val wireKind: WireKind = WireKind.Varint
      val isPackable: Boolean = true
    }

    final case class MapEntry(
        keyType: ScalarType,
        valueType: FieldType
    ) extends FieldType {
      def bosatsuTypeString: String =
        s"List[(${keyType.bosatsuTypeString}, ${valueType.bosatsuTypeString})]"

      val wireKind: WireKind = WireKind.LengthDelimited
      val isPackable: Boolean = false
    }
  }

  final case class EnumValueModel(
      protoName: String,
      bosatsuName: String,
      number: Int
  ) derives CanEqual

  final case class EnumModel(
      protoName: String,
      bosatsuName: String,
      packageName: PackageName,
      values: Vector[EnumValueModel]
  ) derives CanEqual

  final case class FieldModel(
      protoName: String,
      bosatsuName: String,
      number: Int,
      cardinality: Cardinality,
      fieldType: FieldType,
      packed: Boolean
  ) derives CanEqual {

    def isRepeated: Boolean = cardinality == Cardinality.Repeated

    def bosatsuTypeString: String = {
      val base =
        fieldType match {
          case mapEntry: FieldType.MapEntry => mapEntry.bosatsuTypeString
          case other                        => other.bosatsuTypeString
        }

      cardinality match {
        case Cardinality.Repeated =>
          fieldType match {
            case _: FieldType.MapEntry => base
            case _                     => s"List[$base]"
          }
        case Cardinality.Optional => s"Option[$base]"
        case Cardinality.Singular => base
      }
    }
  }

  final case class OneofFieldModel(
      protoName: String,
      bosatsuCaseName: String,
      number: Int,
      fieldType: FieldType
  ) derives CanEqual

  final case class OneofModel(
      protoName: String,
      bosatsuFieldName: String,
      bosatsuTypeName: String,
      fields: Vector[OneofFieldModel]
  ) derives CanEqual

  final case class MessageModel(
      protoName: String,
      bosatsuName: String,
      packageName: PackageName,
      fields: Vector[FieldModel],
      oneofs: Vector[OneofModel]
  ) derives CanEqual

  final case class ProtoFileModel(
      fileName: String,
      protoPackage: Option[String],
      bosatsuPackage: PackageName,
      outputFilePath: String,
      syntax: Syntax,
      enums: Vector[EnumModel],
      messages: Vector[MessageModel]
  ) derives CanEqual
}
