package dev.bosatsu.protobuf

import com.google.protobuf.DescriptorProtos.{
  DescriptorProto,
  FieldDescriptorProto,
  FileDescriptorProto
}
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest

class ProtocPluginMainTest extends munit.FunSuite {

  private def scalarField(
      name: String,
      number: Int,
      fieldType: FieldDescriptorProto.Type,
      label: FieldDescriptorProto.Label = FieldDescriptorProto.Label.LABEL_OPTIONAL
  ): FieldDescriptorProto =
    FieldDescriptorProto.newBuilder()
      .setName(name)
      .setNumber(number)
      .setLabel(label)
      .setType(fieldType)
      .build()

  test("plugin generates bosatsu source for requested proto3 files") {
    val message =
      DescriptorProto.newBuilder()
        .setName("SimpleConfig")
        .addField(scalarField("name", 1, FieldDescriptorProto.Type.TYPE_STRING))
        .addField(scalarField("replicas", 2, FieldDescriptorProto.Type.TYPE_UINT32))
        .build()

    val file =
      FileDescriptorProto.newBuilder()
        .setName("config_v1.proto")
        .setPackage("config.v1")
        .setSyntax("proto3")
        .addMessageType(message)
        .build()

    val request =
      CodeGeneratorRequest.newBuilder()
        .addProtoFile(file)
        .addFileToGenerate(file.getName)
        .build()

    val response = ProtocPluginMain.generateResponse(request)
    assertEquals(response.getError, "")
    assertEquals(response.getFileCount, 1)

    val out = response.getFile(0)
    assertEquals(out.getName, "Proto/Config/V1.bosatsu")
    assert(out.getContent.contains("package Proto/Config/V1"), out.getContent)
    assert(out.getContent.contains("struct SimpleConfig("), out.getContent)
    assert(out.getContent.contains("def encode_SimpleConfig"), out.getContent)
    assert(out.getContent.contains("def decode_SimpleConfig"), out.getContent)
  }

  test("plugin output ordering is deterministic across requested files") {
    val aFile =
      FileDescriptorProto.newBuilder()
        .setName("a.proto")
        .setPackage("alpha")
        .setSyntax("proto3")
        .addMessageType(
          DescriptorProto.newBuilder()
            .setName("A")
            .addField(scalarField("value", 1, FieldDescriptorProto.Type.TYPE_INT32))
            .build()
        )
        .build()

    val zFile =
      FileDescriptorProto.newBuilder()
        .setName("z.proto")
        .setPackage("zeta")
        .setSyntax("proto3")
        .addMessageType(
          DescriptorProto.newBuilder()
            .setName("Z")
            .addField(scalarField("value", 1, FieldDescriptorProto.Type.TYPE_INT32))
            .build()
        )
        .build()

    val request =
      CodeGeneratorRequest.newBuilder()
        .addProtoFile(zFile)
        .addProtoFile(aFile)
        .addFileToGenerate(zFile.getName)
        .addFileToGenerate(aFile.getName)
        .build()

    val response = ProtocPluginMain.generateResponse(request)
    assertEquals(response.getError, "")
    assertEquals(response.getFileCount, 2)
    assertEquals(
      List(response.getFile(0).getName, response.getFile(1).getName),
      List("Proto/Alpha.bosatsu", "Proto/Zeta.bosatsu")
    )
  }

  test("plugin reports unsupported schema features") {
    val proto2File =
      FileDescriptorProto.newBuilder()
        .setName("legacy.proto")
        .setPackage("legacy")
        .setSyntax("proto2")
        .addMessageType(
          DescriptorProto.newBuilder()
            .setName("Legacy")
            .addField(scalarField("id", 1, FieldDescriptorProto.Type.TYPE_INT32))
            .build()
        )
        .build()

    val extensionFile =
      FileDescriptorProto.newBuilder()
        .setName("extension.proto")
        .setPackage("legacy")
        .setSyntax("proto3")
        .addMessageType(
          DescriptorProto.newBuilder()
            .setName("Base")
            .build()
        )
        .addExtension(
          FieldDescriptorProto.newBuilder()
            .setName("extra")
            .setNumber(100)
            .setLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL)
            .setType(FieldDescriptorProto.Type.TYPE_INT32)
            .setExtendee(".legacy.Base")
            .build()
        )
        .build()

    val groupFile =
      FileDescriptorProto.newBuilder()
        .setName("group.proto")
        .setPackage("legacy")
        .setSyntax("proto3")
        .addMessageType(
          DescriptorProto.newBuilder()
            .setName("Grouped")
            .addField(scalarField("old_group", 1, FieldDescriptorProto.Type.TYPE_GROUP))
            .build()
        )
        .build()

    val request =
      CodeGeneratorRequest.newBuilder()
        .addProtoFile(proto2File)
        .addProtoFile(extensionFile)
        .addProtoFile(groupFile)
        .addFileToGenerate(proto2File.getName)
        .addFileToGenerate(extensionFile.getName)
        .addFileToGenerate(groupFile.getName)
        .build()

    val response = ProtocPluginMain.generateResponse(request)
    assert(response.hasError)
    val error = response.getError
    assert(error.contains("legacy.proto: only proto3 is supported"), error)
    assert(error.contains("extension.proto: extensions are not supported"), error)
    assert(error.contains("group.proto: groups are not supported"), error)
  }
}
