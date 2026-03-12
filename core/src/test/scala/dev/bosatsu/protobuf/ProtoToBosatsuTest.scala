package dev.bosatsu.protobuf

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

class ProtoToBosatsuTest extends munit.FunSuite {

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
    assert(appContent.contains("from Proto/Shared import ("), appContent)
    assert(appContent.contains("Child"), appContent)
    assert(appContent.contains("Kind"), appContent)
    assert(appContent.contains("encode_Child"), appContent)
    assert(appContent.contains("decode_Child"), appContent)
    assert(appContent.contains("encode_enum_Kind"), appContent)
    assert(appContent.contains("decode_enum_Kind"), appContent)
  }
}
