package dev.bosatsu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

/** The built-in names the compiler spells out, other than Predef's, checked
  * against the files that define them: the reserved packages' names against
  * their test_workspace sources, and the C runtime names against the
  * c_runtime headers. Predef's names are checked when the compiler compiles
  * (see [[PredefName]]). These are checked here, at test time, so that editing
  * those files doesn't recompile core. build.sbt hashes the files this test
  * reads into the test cache key, so editing them reruns it.
  *
  * The names are found by scanning the compiler's main sources for calls to
  * the [[ReservedPackage]] helpers and to `RuntimeIdent`, so each argument must
  * be a string literal, and the scan fails on one that isn't.
  */
class ReservedNamesTest extends munit.FunSuite {

  private val mainSourceRoots: List[String] = List(
    "core/src/main/scala",
    "core/.jvm/src/main/scala",
    "core/.js/src/main/scala"
  )

  // how sources name each reserved package, when they import it unrenamed
  private val reservedByObjectName: Map[String, ReservedPackage.Reserved] =
    Map(
      "Prog" -> ReservedPackage.Prog,
      "Int64" -> ReservedPackage.Int64,
      "Float64" -> ReservedPackage.Float64,
      "Lazy" -> ReservedPackage.Lazy,
      "Eval" -> ReservedPackage.Eval,
      "IoCore" -> ReservedPackage.IoCore,
      "IoBytes" -> ReservedPackage.IoBytes,
      "Array" -> ReservedPackage.Array,
      "Json" -> ReservedPackage.Json
    )

  private def read(path: Path): String =
    new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  private def sourceNames(source: String): BosatsuSourceNames =
    BosatsuSourceNames.scan(source, read(Paths.get(source))) match {
      case Right(names) => names
      case Left(err)    => fail(err)
    }

  private lazy val namesBySource: Map[String, BosatsuSourceNames] =
    (PredefName.source :: ReservedPackage.all.map(_.source)).iterator
      .map(source => (source, sourceNames(source)))
      .toMap

  private lazy val mainSources: List[(Path, String)] =
    mainSourceRoots.flatMap { root =>
      val stream = Files.walk(Paths.get(root))
      try
        stream
          .iterator()
          .asScala
          .filter(p => p.toString.endsWith(".scala"))
          .map(p => (p, read(p)))
          .toList
          .sortBy(_._1.toString)
      finally stream.close()
    }

  private val stringLiteral: Regex = "\"([^\"\\\\]*)\"".r

  private def lineOf(text: String, offset: Int): Int =
    text.iterator.take(offset).count(_ == '\n') + 1

  // The argument of a call found by a scan, which must be a string literal
  private def literalArg(path: Path, text: String, at: Int, arg: String): String =
    arg.trim match {
      case stringLiteral(name) => name.nn
      case other               =>
        fail(
          s"$path:${lineOf(text, at)}: expected a string literal, found `$other`"
        )
    }

  private def checkName(
      name: String,
      listDescription: String,
      known: List[String],
      where: String
  ): Option[String] =
    if (known.contains(name)) None
    else Some(s"$where: ${CheckedNames.notFound(name, listDescription, known)}")

  private def failIfAny(errors: List[String]): Unit =
    if (errors.nonEmpty) fail(errors.mkString("\n"))

  // The names a source uses for the reserved packages: the objects it imports
  // from ReservedPackage, under their imported names
  private val importLine: Regex =
    raw"import\s+(?:dev\.bosatsu\.)?ReservedPackage\.(\{[^}]*\}|\w+)".r
  private val renamed: Regex = raw"(\w+)\s+(?:as|=>)\s+(\w+)".r

  private def importedReserved(text: String): Map[String, ReservedPackage.Reserved] =
    importLine
      .findAllMatchIn(text)
      .flatMap { m =>
        val selectors = m.group(1).nn.stripPrefix("{").stripSuffix("}")
        selectors.split(",").iterator.map(_.trim).filter(_.nonEmpty).flatMap {
          case renamed(obj, local) =>
            reservedByObjectName.get(obj.nn).map((local.nn, _))
          case obj =>
            reservedByObjectName.get(obj).map((obj, _))
        }
      }
      .toMap

  private val helperCall: Regex =
    raw"(?<![\w.])((?:ReservedPackage\.)?\w+)\.(external|value|typeName)\(([^)]*)\)".r

  private final case class Use(
      reserved: ReservedPackage.Reserved,
      helper: String,
      name: String,
      where: String
  )

  private lazy val helperUses: List[Use] =
    mainSources.flatMap { case (path, text) =>
      val imported = importedReserved(text)
      helperCall.findAllMatchIn(text).flatMap { m =>
        val qualifier = m.group(1).nn
        val reservedOpt =
          if (qualifier.startsWith("ReservedPackage."))
            reservedByObjectName.get(qualifier.stripPrefix("ReservedPackage."))
          else imported.get(qualifier)
        reservedOpt.map { reserved =>
          val name = literalArg(path, text, m.start, m.group(3).nn)
          Use(reserved, m.group(2).nn, name, s"$path:${lineOf(text, m.start)}")
        }
      }
    }

  test("the scan of the test covers every reserved package") {
    assertEquals(
      reservedByObjectName.values.toSet,
      ReservedPackage.all.toSet,
      "add the new reserved package to reservedByObjectName"
    )
  }

  test("each reserved package name is its source's package") {
    failIfAny(ReservedPackage.all.flatMap { reserved =>
      val declared = namesBySource(reserved.source).packageName
      if (declared == reserved.name.asString) None
      else
        Some(
          s"`${reserved.name.asString}` is not the package of ${reserved.source}, which is `$declared`"
        )
    })
  }

  test("each name given to a reserved package helper is declared by its source") {
    val uses = helperUses
    // a scan that finds nothing must not quietly pass
    List("external", "value", "typeName").foreach { helper =>
      assert(uses.exists(_.helper == helper), s"found no uses of `$helper`")
    }
    // the JS platform's IO externals are scanned too
    assert(
      uses.exists(_.where.contains("core/.js/")),
      "found no uses in the JS sources"
    )

    failIfAny(uses.flatMap { use =>
      val names = namesBySource(use.reserved.source)
      val (listName, known) = use.helper match {
        case "external" => ("external declarations", names.externals)
        case "value"    => ("exported values", names.exportedValues)
        case _          => ("exported types", names.exportedTypes)
      }
      checkName(
        use.name,
        s"the $listName of ${names.packageName} in ${use.reserved.source}",
        known,
        use.where
      )
    })
  }

  test("every registered external is declared by its package's source") {
    val sources: Map[PackageName, String] =
      ((PackageName.PredefName, PredefName.source) ::
        ReservedPackage.all.map(r => (r.name, r.source))).toMap

    val registered = Predef.jvmExternals.toMap.keys.toList
    assert(registered.nonEmpty)
    failIfAny(registered.flatMap { case (pack, name) =>
      sources.get(pack) match {
        case None =>
          Some(
            s"external ${pack.asString}::${name.sourceCodeRepr} is registered for a package that ReservedPackage doesn't list"
          )
        case Some(source) =>
          val names = namesBySource(source)
          checkName(
            name.sourceCodeRepr,
            s"the external declarations of ${names.packageName} in $source",
            names.externals,
            "Predef.jvmExternals"
          )
      }
    })
  }

  private val runtimeIdentCall: Regex =
    raw"(?<![\w.])RuntimeIdent\(([^)]*)\)".r

  test("each C runtime name the C code generator uses is in a c_runtime header") {
    val headers = {
      val stream = Files.list(Paths.get("c_runtime"))
      try
        stream.iterator().asScala.filter(_.toString.endsWith(".h")).toList
      finally stream.close()
    }
    val idents = headers.iterator.flatMap(h => CRuntimeNames.identifiers(read(h))).toSet
    // a scan that finds nothing must not quietly reject every name
    assert(idents.exists(_.startsWith("bsts_")), s"no bsts_ identifiers in $headers")

    val uses = mainSources.flatMap { case (path, text) =>
      runtimeIdentCall.findAllMatchIn(text).map { m =>
        (literalArg(path, text, m.start, m.group(1).nn), s"$path:${lineOf(text, m.start)}")
      }
    }
    assert(uses.nonEmpty, "found no RuntimeIdent uses")

    val known = idents.toList.sorted
    failIfAny(uses.flatMap { case (name, where) =>
      checkName(name, "the identifiers in the c_runtime/*.h headers", known, where)
    })
  }
}
