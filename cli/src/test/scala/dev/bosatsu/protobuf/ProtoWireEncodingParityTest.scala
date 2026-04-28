package dev.bosatsu.protobuf

import com.google.protobuf.DescriptorProtos.{
  DescriptorProto,
  FieldDescriptorProto,
  FieldOptions,
  FileDescriptorProto
}
import com.google.protobuf.Descriptors
import com.google.protobuf.DynamicMessage
import dev.bosatsu.PackageName
import dev.bosatsu.protobuf.ProtoDescriptorModel.{
  Cardinality,
  FieldModel,
  FieldType,
  MessageModel,
  ProtoFileModel,
  ScalarType,
  Syntax
}
import java.nio.charset.StandardCharsets

class ProtoWireEncodingParityTest extends munit.FunSuite {

  private val modelPackage = PackageName.parts("Proto", "Config", "V1")

  private val generatedModel =
    ProtoFileModel(
      fileName = "config_v1.proto",
      protoPackage = Some("config.v1"),
      bosatsuPackage = modelPackage,
      outputFilePath = "Proto/Config/V1.bosatsu",
      syntax = Syntax.Proto3,
      enums = Vector.empty,
      messages = Vector(
        MessageModel(
          protoName = "SimpleConfig",
          bosatsuName = "SimpleConfig",
          packageName = modelPackage,
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
              protoName = "replicas",
              bosatsuName = "replicas",
              number = 2,
              cardinality = Cardinality.Singular,
              fieldType = FieldType.Scalar(ScalarType.UInt32Type),
              packed = false
            ),
            FieldModel(
              protoName = "weights",
              bosatsuName = "weights",
              number = 3,
              cardinality = Cardinality.Repeated,
              fieldType = FieldType.Scalar(ScalarType.SInt32Type),
              packed = true
            ),
            FieldModel(
              protoName = "ratio",
              bosatsuName = "ratio",
              number = 4,
              cardinality = Cardinality.Singular,
              fieldType = FieldType.Scalar(ScalarType.FloatType),
              packed = false
            ),
            FieldModel(
              protoName = "epsilon",
              bosatsuName = "epsilon",
              number = 5,
              cardinality = Cardinality.Singular,
              fieldType = FieldType.Scalar(ScalarType.DoubleType),
              packed = false
            )
          ),
          oneofs = Vector.empty
        )
      )
    )

  private lazy val generatedSrc: String =
    ProtoToBosatsu.renderAll(Vector(generatedModel)).head.content

  private def requireField(
      descriptor: Descriptors.Descriptor,
      fieldName: String
  ): Descriptors.FieldDescriptor =
    Option(descriptor.findFieldByName(fieldName))
      .getOrElse(fail(s"missing field: $fieldName"))

  private def encodeVarint(value: Long): List[Int] = {
    var rem = value
    val out = scala.collection.mutable.ListBuffer.empty[Int]
    while ((rem & ~0x7fL) != 0L) {
      out += (((rem & 0x7fL) | 0x80L).toInt & 0xff)
      rem = rem >>> 7
    }
    out += (rem.toInt & 0xff)
    out.toList
  }

  private def fieldKey(fieldNumber: Int, wireType: Int): List[Int] =
    encodeVarint(((fieldNumber << 3) | wireType).toLong)

  private def zigZag32(value: Int): Long =
    ((value << 1) ^ (value >> 31)).toLong & 0xffffffffL

  private def fixed32LE(value: Int): List[Int] =
    List(
      value & 0xff,
      (value >>> 8) & 0xff,
      (value >>> 16) & 0xff,
      (value >>> 24) & 0xff
    )

  private def fixed64LE(value: Long): List[Int] =
    List(
      (value & 0xffL).toInt,
      ((value >>> 8) & 0xffL).toInt,
      ((value >>> 16) & 0xffL).toInt,
      ((value >>> 24) & 0xffL).toInt,
      ((value >>> 32) & 0xffL).toInt,
      ((value >>> 40) & 0xffL).toInt,
      ((value >>> 48) & 0xffL).toInt,
      ((value >>> 56) & 0xffL).toInt
    )

  private lazy val manualSimpleConfigBytes: List[Int] = {
    val nameBytes = "api".getBytes(StandardCharsets.UTF_8).toList.map(_ & 0xff)
    val weightsPayload =
      List(-1, 0, 7).flatMap(n => encodeVarint(zigZag32(n)))
    val ratioBits = java.lang.Float.floatToRawIntBits(0.5f)
    val epsilonBits = java.lang.Double.doubleToRawLongBits(1.25d)

    fieldKey(1, 2) ++
      encodeVarint(nameBytes.length.toLong) ++
      nameBytes ++
      fieldKey(2, 0) ++
      encodeVarint(3L) ++
      fieldKey(3, 2) ++
      encodeVarint(weightsPayload.length.toLong) ++
      weightsPayload ++
      fieldKey(4, 5) ++
      fixed32LE(ratioBits) ++
      fieldKey(5, 1) ++
      fixed64LE(epsilonBits)
  }

  private lazy val javaSimpleConfigBytes: List[Int] = {
    val message =
      DescriptorProto.newBuilder()
        .setName("SimpleConfig")
        .addField(
          FieldDescriptorProto.newBuilder()
            .setName("name")
            .setNumber(1)
            .setLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL)
            .setType(FieldDescriptorProto.Type.TYPE_STRING)
            .build()
        )
        .addField(
          FieldDescriptorProto.newBuilder()
            .setName("replicas")
            .setNumber(2)
            .setLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL)
            .setType(FieldDescriptorProto.Type.TYPE_UINT32)
            .build()
        )
        .addField(
          FieldDescriptorProto.newBuilder()
            .setName("weights")
            .setNumber(3)
            .setLabel(FieldDescriptorProto.Label.LABEL_REPEATED)
            .setType(FieldDescriptorProto.Type.TYPE_SINT32)
            .setOptions(FieldOptions.newBuilder().setPacked(true).build())
            .build()
        )
        .addField(
          FieldDescriptorProto.newBuilder()
            .setName("ratio")
            .setNumber(4)
            .setLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL)
            .setType(FieldDescriptorProto.Type.TYPE_FLOAT)
            .build()
        )
        .addField(
          FieldDescriptorProto.newBuilder()
            .setName("epsilon")
            .setNumber(5)
            .setLabel(FieldDescriptorProto.Label.LABEL_OPTIONAL)
            .setType(FieldDescriptorProto.Type.TYPE_DOUBLE)
            .build()
        )
        .build()

    val file =
      FileDescriptorProto.newBuilder()
        .setName("config_v1.proto")
        .setPackage("config.v1")
        .setSyntax("proto3")
        .addMessageType(message)
        .build()

    val fileDescriptor = Descriptors.FileDescriptor.buildFrom(file, Array.empty)
    val descriptor =
      Option(fileDescriptor.findMessageTypeByName("SimpleConfig"))
        .getOrElse(fail("missing message descriptor: SimpleConfig"))

    val builder = DynamicMessage.newBuilder(descriptor)
    builder.setField(requireField(descriptor, "name"), "api")
    builder.setField(requireField(descriptor, "replicas"), Int.box(3))
    builder.addRepeatedField(requireField(descriptor, "weights"), Int.box(-1))
    builder.addRepeatedField(requireField(descriptor, "weights"), Int.box(0))
    builder.addRepeatedField(requireField(descriptor, "weights"), Int.box(7))
    builder.setField(requireField(descriptor, "ratio"), Float.box(0.5f))
    builder.setField(requireField(descriptor, "epsilon"), Double.box(1.25d))

    builder.build().toByteArray.toList.map(b => b & 0xff)
  }

  test("manual wire encoding parity matches protobuf-java bytes") {
    assertEquals(manualSimpleConfigBytes, javaSimpleConfigBytes)
  }

  test("generated source includes codec surface and wire hooks") {
    assert(generatedSrc.contains("def encode_SimpleConfig"), generatedSrc)
    assert(generatedSrc.contains("def decode_SimpleConfig"), generatedSrc)
    assert(generatedSrc.contains("decode_fields"), generatedSrc)
    assert(generatedSrc.contains("encode_float_bits"), generatedSrc)
    assert(generatedSrc.contains("decode_float"), generatedSrc)
    assert(generatedSrc.contains("encode_double_bits"), generatedSrc)
    assert(generatedSrc.contains("decode_double"), generatedSrc)
  }
}
