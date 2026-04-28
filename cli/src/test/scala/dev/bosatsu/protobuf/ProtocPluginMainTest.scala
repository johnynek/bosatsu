package dev.bosatsu.protobuf

import com.google.protobuf.descriptor.{
  DescriptorProto,
  FieldDescriptorProto,
  FileDescriptorProto
}
import com.google.protobuf.compiler.plugin.CodeGeneratorRequest

class ProtocPluginMainTest extends munit.FunSuite {

  private def scalarField(
      name: String,
      number: Int,
      fieldType: FieldDescriptorProto.Type,
      label: FieldDescriptorProto.Label = FieldDescriptorProto.Label.LABEL_OPTIONAL
  ): FieldDescriptorProto =
    FieldDescriptorProto.defaultInstance
      .withName(name)
      .withNumber(number)
      .withLabel(label)
      .withType(fieldType)

  test("plugin generates bosatsu source for requested proto3 files") {
    val message =
      DescriptorProto.defaultInstance
        .withName("SimpleConfig")
        .withField(
          Vector(
            scalarField("name", 1, FieldDescriptorProto.Type.TYPE_STRING),
            scalarField("replicas", 2, FieldDescriptorProto.Type.TYPE_UINT32)
          )
        )

    val file =
      FileDescriptorProto.defaultInstance
        .withName("config_v1.proto")
        .withPackage("config.v1")
        .withSyntax("proto3")
        .withMessageType(Vector(message))

    val request =
      CodeGeneratorRequest.defaultInstance
        .withProtoFile(Vector(file))
        .withFileToGenerate(Vector(file.getName))

    val response = ProtocPluginMain.generateResponse(request)
    assertEquals(response.error, None)
    assertEquals(response.file.size, 1)

    val out = response.file.head
    assertEquals(out.getName, "Proto/Config/V1.bosatsu")
    assert(out.getContent.contains("package Proto/Config/V1"), out.getContent)
    assert(out.getContent.contains("struct SimpleConfig("), out.getContent)
    assert(out.getContent.contains("def encode_SimpleConfig"), out.getContent)
    assert(out.getContent.contains("def decode_SimpleConfig"), out.getContent)
  }

  test("plugin output ordering is deterministic across requested files") {
    val aFile =
      FileDescriptorProto.defaultInstance
        .withName("a.proto")
        .withPackage("alpha")
        .withSyntax("proto3")
        .withMessageType(
          Vector(
            DescriptorProto.defaultInstance
              .withName("A")
              .withField(
                Vector(
                  scalarField("value", 1, FieldDescriptorProto.Type.TYPE_INT32)
                )
              )
          )
        )

    val zFile =
      FileDescriptorProto.defaultInstance
        .withName("z.proto")
        .withPackage("zeta")
        .withSyntax("proto3")
        .withMessageType(
          Vector(
            DescriptorProto.defaultInstance
              .withName("Z")
              .withField(
                Vector(
                  scalarField("value", 1, FieldDescriptorProto.Type.TYPE_INT32)
                )
              )
          )
        )

    val request =
      CodeGeneratorRequest.defaultInstance
        .withProtoFile(Vector(zFile, aFile))
        .withFileToGenerate(Vector(zFile.getName, aFile.getName))

    val response = ProtocPluginMain.generateResponse(request)
    assertEquals(response.error, None)
    assertEquals(response.file.size, 2)
    assertEquals(
      List(response.file(0).getName, response.file(1).getName),
      List("Proto/Alpha.bosatsu", "Proto/Zeta.bosatsu")
    )
  }

  test("plugin reports unsupported schema features") {
    val proto2File =
      FileDescriptorProto.defaultInstance
        .withName("legacy.proto")
        .withPackage("legacy")
        .withSyntax("proto2")
        .withMessageType(
          Vector(
            DescriptorProto.defaultInstance
              .withName("Legacy")
              .withField(
                Vector(
                  scalarField("id", 1, FieldDescriptorProto.Type.TYPE_INT32)
                )
              )
          )
        )

    val extensionFile =
      FileDescriptorProto.defaultInstance
        .withName("extension.proto")
        .withPackage("legacy")
        .withSyntax("proto3")
        .withMessageType(
          Vector(
            DescriptorProto.defaultInstance
              .withName("Base")
          )
        )
        .withExtension(
          Vector(
            FieldDescriptorProto.defaultInstance
              .withName("extra")
              .withNumber(100)
              .withLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL)
              .withType(FieldDescriptorProto.Type.TYPE_INT32)
              .withExtendee(".legacy.Base")
          )
        )

    val groupFile =
      FileDescriptorProto.defaultInstance
        .withName("group.proto")
        .withPackage("legacy")
        .withSyntax("proto3")
        .withMessageType(
          Vector(
            DescriptorProto.defaultInstance
              .withName("Grouped")
              .withField(
                Vector(
                  scalarField("old_group", 1, FieldDescriptorProto.Type.TYPE_GROUP)
                )
              )
          )
        )

    val request =
      CodeGeneratorRequest.defaultInstance
        .withProtoFile(Vector(proto2File, extensionFile, groupFile))
        .withFileToGenerate(
          Vector(proto2File.getName, extensionFile.getName, groupFile.getName)
        )

    val response = ProtocPluginMain.generateResponse(request)
    assert(response.error.nonEmpty)
    val error = response.error.getOrElse("")
    assert(error.contains("legacy.proto: only proto3 is supported"), error)
    assert(error.contains("extension.proto: extensions are not supported"), error)
    assert(error.contains("group.proto: groups are not supported"), error)
  }
}
