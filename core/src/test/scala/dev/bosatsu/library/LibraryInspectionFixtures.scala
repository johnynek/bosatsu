package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.{CompileOptions, LocationMap, Package, PackageMap, Par, Parser, ProtoConverter}
import dev.bosatsu.hashing.Algo
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** Typed, serialized inspection graphs shared by core and candidate-jar tests. */
private[bosatsu] object LibraryInspectionFixtures {
  final case class File(path: String, bytes: Array[Byte])

  private final case class Built(
      name: Name,
      version: Version,
      pack: Package.Compiled,
      protoLib: proto.Library,
      bytes: Array[Byte]
  ) {
    def path: String = s"test_workspace/inspect_library/${Library.defaultFileName(name, version)}"
    def blake3: String = Algo.hashBytes[Algo.Blake3](bytes).hex
    def sha256: String = Algo.hashBytes[Algo.Sha256](bytes).hex
  }

  private val version1 = Version(1, 0, 0)
  private val version2 = Version(2, 0, 0)

  private def compiled(
      source: String,
      dependencies: List[Built]
  ): Package.Compiled = {
    val parsed = Parser.unsafeParse(Package.parser, source)
    val interfaces = Package.interfaceOf(PackageMap.predefCompiled) ::
      dependencies.map(dep => Package.interfaceOf(dep.pack))
    val typed = Par.noParallelism {
      PackageMap.typeCheckParsed(
        NonEmptyList.one((("inspection fixture", LocationMap(source)), parsed)),
        interfaces,
        "<predef>",
        CompileOptions.Default
      ).strictToValidated.fold(
        errors => throw new AssertionError(errors.toList.mkString("\n")),
        identity
      )
    }
    typed.toMap(parsed.name)
  }

  private def dependency(library: Built): proto.LibDependency =
    proto.LibDependency(
      name = library.name.name,
      desc = Some(proto.LibDescriptor(
        version = Some(library.version.toProto),
        hashes = List(s"blake3:${library.blake3}"),
        uris = List(s"https://example.invalid/${Library.defaultFileName(library.name, library.version)}")
      ))
    )

  private def build(
      name: String,
      version: Version,
      source: String,
      public: List[Built] = Nil,
      privateDeps: List[Built] = Nil,
      unusedTransitivePublic: List[Built] = Nil,
      legacyInterfaces: Boolean = false
  ): Built = {
    val pack = compiled(source, public ::: privateDeps)
    val iface = ProtoConverter.interfaceToProto(Package.interfaceOf(pack)).get
    val raw = proto.Library(
      name = name,
      descriptor = Some(proto.LibDescriptor(version = Some(version.toProto))),
      exportedIfaces = List(
        if (legacyInterfaces) iface.copy(signatureDependencies = None) else iface
      ),
      internalPackages = List(ProtoConverter.packageToProto(pack).get),
      publicDependencies = public.map(dependency),
      privateDependencies = privateDeps.map(dependency),
      unusedTransitivePublicDependencies = unusedTransitivePublic.map(dependency)
    )
    Built(Name(name), version, pack, raw, raw.toByteArray)
  }

  private lazy val common = build(
    "common",
    version1,
    """package Common/Util
      |export common_value, Box()
      |struct Box(value: Int)
      |common_value = 40
      |""".stripMargin
  )

  private lazy val legacy = build(
    "legacy",
    version1,
    """package Legacy/Api
      |export legacy_value
      |legacy_value = 7
      |""".stripMargin
  )

  private lazy val publicChild = build(
    "public_child",
    version1,
    """package Public/Child
      |from Common/Util import common_value, Box
      |export public_value, make_box
      |exposes Common/Util
      |public_value = common_value
      |make_box = Box
      |""".stripMargin,
    public = List(common, legacy),
    legacyInterfaces = true
  )

  private lazy val privateChild = build(
    "private_child",
    version1,
    """package Private/Child
      |from Common/Util import common_value
      |export private_value
      |private_value = common_value
      |""".stripMargin,
    privateDeps = List(common)
  )

  private lazy val sharedV1 = build(
    "shared",
    version1,
    """package Shared/Api
      |export pick
      |def pick(value: Int) -> Int: value
      |""".stripMargin
  )

  private lazy val sharedV2 = build(
    "shared",
    version2,
    """package Shared/Api
      |export pick
      |def pick(flag: Bool) -> Int: 2 if flag else 0
      |""".stripMargin
  )

  private lazy val left = build(
    "left",
    version1,
    """package Left
      |from Shared/Api import pick
      |export answer
      |answer = pick(1)
      |""".stripMargin,
    privateDeps = List(sharedV1)
  )

  private lazy val right = build(
    "right",
    version1,
    """package Right
      |from Shared/Api import pick
      |export answer
      |answer = pick(True)
      |""".stripMargin,
    privateDeps = List(sharedV2)
  )

  private lazy val parser = build(
    "parser",
    version1,
    """package Parser
      |from Public/Child import public_value
      |from Private/Child import private_value
      |from Left import answer as left_answer
      |from Right import answer as right_answer
      |export parse_score
      |parse_score = (public_value, private_value, left_answer, right_answer)
      |private_helper = 1
      |""".stripMargin,
    public = List(publicChild),
    privateDeps = List(privateChild, left, right),
    unusedTransitivePublic = List(common, legacy)
  )

  private lazy val libraries: List[Built] = List(
    common,
    legacy,
    publicChild,
    privateChild,
    sharedV1,
    sharedV2,
    left,
    right,
    parser
  )

  private def pinJson(library: Built): String =
    s"""{"name":"${library.name.name}","version":"${library.version.render}","path":"${library.path}","hashes":["blake3:${library.blake3}","sha256:${library.sha256}"]}"""

  private def contextJson(declarations: List[Built]): String =
    s"""{"schema_version":1,"libraries":[${declarations.map(pinJson).mkString(",")}]}
      |""".stripMargin

  lazy val generatedFiles: List[File] = {
    val children = libraries.filterNot(_.name.name == parser.name.name)
    libraries.map(library => File(library.path, library.bytes)) ::: List(
      File(
        "test_workspace/inspect_library/parser-root.json",
        contextJson(List(parser)).getBytes(StandardCharsets.UTF_8)
      ),
      File(
        "test_workspace/inspect_library/parser-context.json",
        contextJson(children).getBytes(StandardCharsets.UTF_8)
      )
    )
  }

  lazy val parsedLibraries: List[(String, proto.Library, Array[Byte], List[String])] =
    libraries.map { library =>
      (
        library.path,
        library.protoLib,
        library.bytes,
        List(s"blake3:${library.blake3}", s"sha256:${library.sha256}")
      )
    }

  def write(): Unit =
    generatedFiles.foreach { file =>
      val path = Paths.get(file.path)
      Files.createDirectories(path.getParent)
      Files.write(path, file.bytes)
    }

  def main(args: Array[String]): Unit = {
    if (args.toList == List("--write")) write()
    else throw new IllegalArgumentException("use --write to regenerate test_workspace/inspect_library")
  }
}
