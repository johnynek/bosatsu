package dev.bosatsu.protobuf

import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.{
  CompileOptions,
  LocationMap,
  Package,
  PackageMap,
  PackageName,
  Par,
  Parser
}
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

class ProtoToBosatsuTest extends munit.FunSuite {

  private val bytesStub: String =
    """package Bosatsu/IO/Bytes
      |
      |export (
      |  Bytes,
      |  concat_all_Bytes,
      |  empty_Bytes,
      |  utf8_bytes_from_String,
      |  utf8_bytes_to_String,
      |)
      |
      |external struct Bytes
      |external empty_Bytes: Bytes
      |external def concat_all_Bytes(chunks: List[Bytes]) -> Bytes
      |external def utf8_bytes_from_String(str: String) -> Bytes
      |external def utf8_bytes_to_String(bytes: Bytes) -> Option[String]
      |""".stripMargin

  private val wireStub: String =
    """package Bosatsu/Proto/Wire
      |
      |from Bosatsu/IO/Bytes import Bytes
      |
      |export (
      |  FieldValue(),
      |  if_Some,
      |  encode_varint_u64,
      |  encode_fixed32,
      |  encode_fixed64,
      |  field_varint,
      |  field_fixed32,
      |  field_fixed64,
      |  field_length_delimited,
      |  decode_fields,
      |  decode_packed_varints,
      |  decode_packed_fixed32,
      |  decode_packed_fixed64,
      |  encode_int32,
      |  encode_int64,
      |  encode_uint32,
      |  encode_uint64,
      |  encode_sint32,
      |  encode_sint64,
      |  encode_fixed32_bits,
      |  encode_fixed64_bits,
      |  encode_sfixed32_bits,
      |  encode_sfixed64_bits,
      |  encode_bool,
      |  encode_enum,
      |  encode_double_bits,
      |  encode_float_bits,
      |  decode_int32,
      |  decode_int64,
      |  decode_uint32,
      |  decode_uint64,
      |  decode_sint32,
      |  decode_sint64,
      |  decode_fixed32,
      |  decode_fixed64,
      |  decode_sfixed32,
      |  decode_sfixed64,
      |  decode_bool,
      |  decode_enum,
      |  decode_double,
      |  decode_float,
      |)
      |
      |enum FieldValue:
      |  Varint(bits: Int)
      |  Fixed64(bits: Int)
      |  LengthDelimited(payload: Bytes)
      |  Fixed32(bits: Int)
      |
      |def if_Some(o: Option[a], fn: a -> Option[b]) -> Option[b]:
      |  match o:
      |    case Some(a): fn(a)
      |    case None: None
      |
      |external def encode_varint_u64(value: Int) -> Bytes
      |external def encode_fixed32(value: Int) -> Bytes
      |external def encode_fixed64(value: Int) -> Bytes
      |external def field_varint(field_number: Int, value: Int) -> Bytes
      |external def field_fixed32(field_number: Int, value: Int) -> Bytes
      |external def field_fixed64(field_number: Int, value: Int) -> Bytes
      |external def field_length_delimited(field_number: Int, payload: Bytes) -> Bytes
      |external def decode_fields(payload: Bytes) -> Option[List[(Int, FieldValue)]]
      |external def decode_packed_varints(payload: Bytes) -> Option[List[Int]]
      |external def decode_packed_fixed32(payload: Bytes) -> Option[List[Int]]
      |external def decode_packed_fixed64(payload: Bytes) -> Option[List[Int]]
      |external def encode_int32(value: Int) -> Int
      |external def encode_int64(value: Int) -> Int
      |external def encode_uint32(value: Int) -> Int
      |external def encode_uint64(value: Int) -> Int
      |external def encode_sint32(value: Int) -> Int
      |external def encode_sint64(value: Int) -> Int
      |external def encode_fixed32_bits(value: Int) -> Int
      |external def encode_fixed64_bits(value: Int) -> Int
      |external def encode_sfixed32_bits(value: Int) -> Int
      |external def encode_sfixed64_bits(value: Int) -> Int
      |external def encode_bool(value: Bool) -> Int
      |external def encode_enum(value: Int) -> Int
      |external def encode_double_bits(value: Float64) -> Int
      |external def encode_float_bits(value: Float64) -> Int
      |external def decode_int32(value: Int) -> Int
      |external def decode_int64(value: Int) -> Int
      |external def decode_uint32(value: Int) -> Int
      |external def decode_uint64(value: Int) -> Int
      |external def decode_sint32(value: Int) -> Int
      |external def decode_sint64(value: Int) -> Int
      |external def decode_fixed32(value: Int) -> Int
      |external def decode_fixed64(value: Int) -> Int
      |external def decode_sfixed32(value: Int) -> Int
      |external def decode_sfixed64(value: Int) -> Int
      |external def decode_bool(value: Int) -> Bool
      |external def decode_enum(value: Int) -> Int
      |external def decode_double(value: Int) -> Float64
      |external def decode_float(value: Int) -> Float64
      |""".stripMargin

  private def assertGeneratedCodeCompiles(
      generated: Vector[ProtoToBosatsu.GeneratedFile]
  ): Unit = {
    val parsedSources =
      (List(
        "Bosatsu/IO/Bytes.bosatsu" -> bytesStub,
        "Bosatsu/Proto/Wire.bosatsu" -> wireStub
      ) ++ generated.toList.map(g => g.outputFilePath -> g.content))
        .map { case (name, source) =>
          val pack = Parser.unsafeParse(Package.parser, source)
          ((name, LocationMap(source)), pack)
        }

    val nel =
      NonEmptyList
        .fromList(parsedSources)
        .getOrElse(fail("expected generated sources"))

    val checked =
      Par.noParallelism {
        PackageMap
          .typeCheckParsed(nel, Nil, "<generated>", CompileOptions.Default)
          .strictToValidated
      }

    checked.fold(errs => fail(errs.toList.mkString("\n")), _ => ())
  }

  test("render emits enums, oneofs, maps, and codecs") {
    val pkg = PackageName.parts("Proto", "Config", "V1")

    val modeEnum =
      EnumModel(
        protoName = "Mode",
        bosatsuName = "Mode",
        packageName = pkg,
        values = Vector(
          EnumValueModel("MODE_UNSPECIFIED", "Mode_ModeUnspecified", 0),
          EnumValueModel("MODE_ACTIVE", "Mode_ModeActive", 1)
        )
      )

    val endpointMessage =
      MessageModel(
        protoName = "Endpoint",
        bosatsuName = "Endpoint",
        packageName = pkg,
        fields = Vector(
          FieldModel(
            protoName = "host",
            bosatsuName = "host",
            number = 1,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.Scalar(ScalarType.StringType),
            packed = false
          ),
          FieldModel(
            protoName = "port",
            bosatsuName = "port",
            number = 2,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.Scalar(ScalarType.UInt32Type),
            packed = false
          )
        ),
        oneofs = Vector.empty
      )

    val appTransportOneof =
      OneofModel(
        protoName = "transport",
        bosatsuFieldName = "transport",
        bosatsuTypeName = "AppConfig_Transport",
        fields = Vector(
          OneofFieldModel(
            protoName = "unix_socket",
            bosatsuCaseName = "AppConfig_Transport_UnixSocket",
            number = 6,
            fieldType = FieldType.Scalar(ScalarType.StringType)
          ),
          OneofFieldModel(
            protoName = "endpoint",
            bosatsuCaseName = "AppConfig_Transport_Endpoint",
            number = 7,
            fieldType = FieldType.MessageRef(pkg, "Endpoint")
          )
        )
      )

    val appConfigMessage =
      MessageModel(
        protoName = "AppConfig",
        bosatsuName = "AppConfig",
        packageName = pkg,
        fields = Vector(
          FieldModel(
            protoName = "name",
            bosatsuName = "name",
            number = 1,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.Scalar(ScalarType.StringType),
            packed = false
          ),
          FieldModel(
            protoName = "weights",
            bosatsuName = "weights",
            number = 2,
            cardinality = Cardinality.Repeated,
            fieldType = FieldType.Scalar(ScalarType.SInt32Type),
            packed = true
          ),
          FieldModel(
            protoName = "labels",
            bosatsuName = "labels",
            number = 3,
            cardinality = Cardinality.Repeated,
            fieldType = FieldType.MapEntry(
              ScalarType.StringType,
              FieldType.Scalar(ScalarType.StringType)
            ),
            packed = false
          ),
          FieldModel(
            protoName = "mode",
            bosatsuName = "mode",
            number = 4,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.EnumRef(pkg, "Mode"),
            packed = false
          ),
          FieldModel(
            protoName = "ratio",
            bosatsuName = "ratio",
            number = 5,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.Scalar(ScalarType.FloatType),
            packed = false
          )
        ),
        oneofs = Vector(appTransportOneof)
      )

    val model =
      ProtoFileModel(
        fileName = "config_v1.proto",
        protoPackage = Some("config.v1"),
        bosatsuPackage = pkg,
        outputFilePath = "Proto/Config/V1.bosatsu",
        syntax = Syntax.Proto3,
        enums = Vector(modeEnum),
        messages = Vector(endpointMessage, appConfigMessage)
      )

    val rendered = ProtoToBosatsu.renderAll(Vector(model))
    assertEquals(rendered.length, 1)

    val content = rendered.head.content
    assert(content.contains("package Proto/Config/V1"), content)
    assert(content.contains("enum Mode:"), content)
    assert(content.contains("Mode_Unknown(value: Int)"), content)
    assert(content.contains("enum AppConfig_Transport:"), content)
    assert(content.contains("AppConfig_Transport_NotSet"), content)
    assert(content.contains("struct AppConfig("), content)
    assert(content.contains("labels: List[(String, String)]"), content)
    assert(content.contains("def encode_AppConfig"), content)
    assert(content.contains("def decode_AppConfig"), content)
    assert(content.contains("def encode_map_entry_labels"), content)
    assert(content.contains("def decode_map_entry_labels"), content)
    assertGeneratedCodeCompiles(rendered)
  }

  test("renderAll is deterministic and includes cross-package codec imports") {
    val sharedPkg = PackageName.parts("Proto", "Shared")
    val appPkg = PackageName.parts("Proto", "App")

    val sharedEnum =
      EnumModel(
        protoName = "Kind",
        bosatsuName = "Kind",
        packageName = sharedPkg,
        values = Vector(
          EnumValueModel("KIND_UNSPECIFIED", "Kind_KindUnspecified", 0),
          EnumValueModel("KIND_A", "Kind_KindA", 1)
        )
      )

    val childMessage =
      MessageModel(
        protoName = "Child",
        bosatsuName = "Child",
        packageName = sharedPkg,
        fields = Vector(
          FieldModel(
            protoName = "id",
            bosatsuName = "id",
            number = 1,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.Scalar(ScalarType.UInt64Type),
            packed = false
          )
        ),
        oneofs = Vector.empty
      )

    val usesSharedMessage =
      MessageModel(
        protoName = "UsesShared",
        bosatsuName = "UsesShared",
        packageName = appPkg,
        fields = Vector(
          FieldModel(
            protoName = "child",
            bosatsuName = "child",
            number = 1,
            cardinality = Cardinality.Optional,
            fieldType = FieldType.MessageRef(sharedPkg, "Child"),
            packed = false
          ),
          FieldModel(
            protoName = "kind",
            bosatsuName = "kind",
            number = 2,
            cardinality = Cardinality.Singular,
            fieldType = FieldType.EnumRef(sharedPkg, "Kind"),
            packed = false
          )
        ),
        oneofs = Vector.empty
      )

    val appFile =
      ProtoFileModel(
        fileName = "app.proto",
        protoPackage = Some("app"),
        bosatsuPackage = appPkg,
        outputFilePath = "Proto/App.bosatsu",
        syntax = Syntax.Proto3,
        enums = Vector.empty,
        messages = Vector(usesSharedMessage)
      )

    val sharedFile =
      ProtoFileModel(
        fileName = "shared.proto",
        protoPackage = Some("shared"),
        bosatsuPackage = sharedPkg,
        outputFilePath = "Proto/Shared.bosatsu",
        syntax = Syntax.Proto3,
        enums = Vector(sharedEnum),
        messages = Vector(childMessage)
      )

    val rendered = ProtoToBosatsu.renderAll(Vector(sharedFile, appFile))
    assertEquals(rendered.map(_.outputFilePath), Vector("Proto/App.bosatsu", "Proto/Shared.bosatsu"))

    val appContent = rendered.head.content
    assert(appContent.contains("from Proto/Shared import "), appContent)
    assert(appContent.contains("Child"), appContent)
    assert(appContent.contains("Kind"), appContent)
    assert(appContent.contains("encode_Child"), appContent)
    assert(appContent.contains("decode_Child"), appContent)
    assert(appContent.contains("encode_enum_Kind"), appContent)
    assert(appContent.contains("decode_enum_Kind"), appContent)
    assertGeneratedCodeCompiles(rendered)
  }
}
