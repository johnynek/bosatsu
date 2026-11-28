package dev.bosatsu

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

/** Finds the reserved-package and C runtime names a Scala source spells out,
  * in the shapes [[ReservedPackage]] and `RuntimeIdent` document. Anything
  * else that mentions them is an error, so a use the scan can't read is
  * reported rather than skipped.
  */
object ReservedNamesScan {

  /** `reserved.helper("name")` at `line`, where `reserved` is the name of a
    * [[ReservedPackage.Reserved]] case
    */
  final case class Use(reserved: String, helper: String, name: String, line: Int)

  /** `RuntimeIdent("name")` at `line` */
  final case class RuntimeUse(name: String, line: Int)

  final case class Result(
      uses: List[Use],
      runtimeUses: List[RuntimeUse],
      errors: List[String]
  )

  val reservedNames: Set[String] =
    ReservedPackage.Reserved.values.iterator.map(_.toString).toSet

  private val helpers = Set("external", "value", "typeName")
  private val members = Set("name", "source")

  // `import a.b.c`, or `import a.b.{x, y as z}` (selectors may span lines),
  // or a wildcard import
  private val importStatement: Regex =
    raw"\bimport\s+((?:\w+\.)*\w+)(?:\.(\{[^}]*\}|\*|_))?".r
  private val renamedSelector: Regex = raw"(\w+)\s+(?:as|=>)\s+(\w+)".r
  private val plainSelector: Regex = raw"(\w+)".r
  private val comments: Regex = raw"(?s)/\*.*?\*/|//[^\n]*".r
  private val packageClause: Regex = raw"(?m)^package\s+([\w.]+)".r
  private val stringLiteral: Regex = "\"([^\"\\\\]*)\"".r

  // after a package: `.helper("lit")`, or `.name` / `.source`
  private val helperCall: Regex =
    raw"\A\s*\.\s*(\w+)\s*\(([^)]*)\)".r
  private val member: Regex = raw"\A\s*\.\s*(\w+)\b(?!\s*\()".r
  private val qualified: Regex = raw"\A\s*\.\s*(\w+)".r
  private val runtimeCall: Regex = raw"\A\s*\(([^)]*)\)".r

  def scan(path: String, text: String): Result = {
    def lineOf(offset: Int): Int = text.iterator.take(offset).count(_ == '\n') + 1
    val errors = List.newBuilder[String]
    def error(offset: Int, msg: String): Unit =
      errors.addOne(s"$path:${lineOf(offset)}: $msg"): Unit

    val skipped: List[(Int, Int)] =
      comments.findAllMatchIn(text).map(m => (m.start, m.end)).toList
    def isSkipped(offset: Int, spans: List[(Int, Int)]): Boolean =
      spans.exists { case (s, e) => (s <= offset) && (offset < e) }

    // the names this source imports the reserved packages under
    val aliases = Map.newBuilder[String, String]
    val importSpans = List.newBuilder[(Int, Int)]
    var importsReservedPackage = false
    var importsRuntimeIdent = false
    importStatement.findAllMatchIn(text).foreach { m =>
      if (!isSkipped(m.start, skipped)) {
        importSpans.addOne((m.start, m.end)): Unit
        val path = m.group(1).nn.split('.').toList
        val selectors = Option(m.group(2))
        val mentions = m.matched.nn
        if (raw"\bReservedPackage\b".r.findFirstIn(mentions).nonEmpty)
          importsReservedPackage = true
        if (raw"\bRuntimeIdent\b".r.findFirstIn(mentions).nonEmpty)
          importsRuntimeIdent = true
        path.indexOf("ReservedPackage") match {
          case -1 =>
            // the selectors may name ReservedPackage or RuntimeIdent
            selectors.toList
              .flatMap(_.stripPrefix("{").stripSuffix("}").split(",").toList)
              .map(_.trim)
              .foreach {
                case renamedSelector(orig, _)
                    if (orig.nn == "ReservedPackage") || (orig.nn == "RuntimeIdent") =>
                  error(m.start, s"don't rename $orig in an import")
                case _ => ()
              }
            if ((path.lastOption.contains("RuntimeIdent")) && selectors.nonEmpty)
              error(m.start, "import RuntimeIdent itself, not its members")
          case idx =>
            path.drop(idx + 1) match {
              case Nil =>
                selectors match {
                  case None => ()
                  case Some("*") | Some("_") =>
                    error(m.start, "don't import ReservedPackage.*")
                  case Some(group) =>
                    group.stripPrefix("{").stripSuffix("}").split(",").iterator
                      .map(_.trim)
                      .filter(_.nonEmpty)
                      .foreach {
                        case renamedSelector(orig, local) if reservedNames(orig.nn) =>
                          aliases.addOne((local.nn, orig.nn)): Unit
                        case plainSelector(orig) if reservedNames(orig.nn) =>
                          aliases.addOne((orig.nn, orig.nn)): Unit
                        case other =>
                          error(m.start, s"`$other` is not a reserved package to import")
                      }
                }
              case obj :: Nil if reservedNames(obj) && selectors.isEmpty =>
                aliases.addOne((obj, obj)): Unit
              case _ =>
                error(m.start, s"can't read the import `${m.matched}`")
            }
        }
      }
    }
    val imports = importSpans.result()
    val aliasMap = aliases.result()

    // An unqualified ReservedPackage or RuntimeIdent is the one in
    // dev.bosatsu only where that is visible: in its package, or after an
    // import. Elsewhere the name may be something else, such as LibConfig's
    // Error.ReservedPackage.
    val filePackage = packageClause.findFirstMatchIn(text).map(_.group(1).nn)
    val seesReservedPackage =
      filePackage.contains("dev.bosatsu") || importsReservedPackage
    val seesRuntimeIdent =
      filePackage.contains("dev.bosatsu.codegen.clang") || importsRuntimeIdent
    def counts(at: Int, qualifier: String, sees: Boolean): Boolean = {
      val before = text.substring(0, at).nn
      if (before.endsWith(qualifier)) true
      else sees && !before.endsWith(".")
    }

    def literal(offset: Int, arg: String): Option[String] =
      arg.trim match {
        case stringLiteral(name) => Some(name.nn)
        case other               =>
          error(offset, s"expected a string literal, found `$other`")
          None
      }

    val uses = List.newBuilder[Use]
    // `reserved` at `offset`, with the rest of the use starting at `after`
    def readUse(reserved: String, offset: Int, after: Int): Unit = {
      val rest = text.substring(after).nn
      helperCall.findPrefixMatchOf(rest) match {
        case Some(call) if helpers(call.group(1).nn) =>
          literal(offset, call.group(2).nn).foreach { name =>
            uses.addOne(Use(reserved, call.group(1).nn, name, lineOf(offset))): Unit
          }
        case _ =>
          member.findPrefixMatchOf(rest) match {
            case Some(m) if members(m.group(1).nn) => ()
            case _                                 =>
              error(offset, s"can't read this use of $reserved")
          }
      }
    }

    def occurrences(word: String): Iterator[Int] =
      raw"(?<![\w.])${Regex.quote(word)}\b".r
        .findAllMatchIn(text)
        .map(_.start)
        .filter(o => !isSkipped(o, skipped) && !isSkipped(o, imports))

    // ReservedPackage.<Reserved>... wherever it means dev.bosatsu's
    raw"\bReservedPackage\b".r.findAllMatchIn(text).map(_.start).foreach { at =>
      if (
        !isSkipped(at, skipped) && !isSkipped(at, imports) &&
        counts(at, "dev.bosatsu.", seesReservedPackage)
      ) {
        val after = at + "ReservedPackage".length
        qualified.findPrefixMatchOf(text.substring(after).nn) match {
          case Some(q) if reservedNames(q.group(1).nn) =>
            readUse(q.group(1).nn, at, after + q.end)
          case _ =>
            error(at, "can't read this use of ReservedPackage")
        }
      }
    }
    aliasMap.foreach { case (local, reserved) =>
      occurrences(local).foreach(at => readUse(reserved, at, at + local.length))
    }

    val runtimeUses = List.newBuilder[RuntimeUse]
    raw"\bRuntimeIdent\b".r
      .findAllMatchIn(text)
      .map(_.start)
      .filter { at =>
        !isSkipped(at, skipped) && !isSkipped(at, imports) &&
        counts(at, "dev.bosatsu.codegen.clang.", seesRuntimeIdent)
      }
      .foreach { at =>
      runtimeCall.findPrefixMatchOf(text.substring(at + "RuntimeIdent".length).nn) match {
        case Some(call) =>
          literal(at, call.group(1).nn).foreach { name =>
            runtimeUses.addOne(RuntimeUse(name, lineOf(at))): Unit
          }
        case None =>
          error(at, "can't read this use of RuntimeIdent")
      }
    }

    Result(uses.result(), runtimeUses.result(), errors.result())
  }

  /** The message for each use whose name its package's source doesn't
    * declare
    */
  def checkUses(
      path: String,
      uses: List[Use],
      names: ReservedPackage.Reserved => BosatsuSourceNames
  ): List[String] =
    uses.flatMap { use =>
      val reserved = ReservedPackage.Reserved.valueOf(use.reserved)
      val sourceNames = names(reserved)
      val (listName, known) = use.helper match {
        case "external" => ("external declarations", sourceNames.externals)
        case "value"    => ("exported values", sourceNames.exportedValues)
        case _          => ("exported types", sourceNames.exportedTypes)
      }
      if (known.contains(use.name)) None
      else
        Some(
          s"$path:${use.line}: " + CheckedNames.notFound(
            use.name,
            s"the $listName of ${sourceNames.packageName} in ${reserved.source}",
            known
          )
        )
    }

  /** The message for each C runtime name not in `idents` */
  def checkRuntimeUses(
      path: String,
      uses: List[RuntimeUse],
      idents: Set[String]
  ): List[String] =
    uses.flatMap { use =>
      if (idents(use.name)) None
      else
        Some(
          s"$path:${use.line}: " + CheckedNames.notFound(
            use.name,
            "the identifiers in the c_runtime/*.h headers",
            idents
          )
        )
    }
}

/** Checks the built-in names the compiler spells out, other than Predef's,
  * against the files that define them: the reserved packages' names against
  * their test_workspace sources, and the C runtime names against the
  * c_runtime headers. Predef's names are checked when the compiler compiles
  * (see [[PredefName]]). These are checked here, at test time, so that editing
  * those files doesn't recompile core. build.sbt hashes the files this test
  * reads into coreJVM's test cache key, so editing one reruns it.
  */
class ReservedNamesTest extends munit.FunSuite {
  import ReservedNamesScan.{Use, RuntimeUse}

  private val mainSourceRoots: List[String] = List(
    "core/src/main/scala",
    "core/.jvm/src/main/scala",
    "core/.js/src/main/scala"
  )

  // the files that define the helpers, which the scan doesn't apply to
  private val definitions: Set[String] = Set(
    "core/src/main/scala/dev/bosatsu/ReservedPackage.scala",
    "core/src/main/scala/dev/bosatsu/codegen/clang/RuntimeIdent.scala"
  )

  private def read(path: Path): String =
    new String(Files.readAllBytes(path), StandardCharsets.UTF_8)

  private def sourceNames(source: String): BosatsuSourceNames =
    BosatsuSourceNames.scan(source, read(Paths.get(source))) match {
      case Right(names) => names
      case Left(err)    => fail(err)
    }

  private lazy val namesByReserved: Map[ReservedPackage.Reserved, BosatsuSourceNames] =
    ReservedPackage.all.iterator.map(r => (r, sourceNames(r.source))).toMap

  private lazy val headerIdents: Set[String] = {
    val stream = Files.list(Paths.get("c_runtime"))
    val headers =
      try stream.iterator().asScala.filter(_.toString.endsWith(".h")).toList
      finally stream.close()
    headers.iterator.flatMap(h => CRuntimeNames.identifiers(read(h))).toSet
  }

  private lazy val mainScans: List[(String, ReservedNamesScan.Result)] =
    mainSourceRoots.flatMap { root =>
      val stream = Files.walk(Paths.get(root))
      val files =
        try stream.iterator().asScala.filter(_.toString.endsWith(".scala")).toList
        finally stream.close()
      files.iterator
        .map(_.toString)
        .filterNot(definitions)
        .toList
        .sorted
        .map(p => (p, ReservedNamesScan.scan(p, read(Paths.get(p)))))
    }

  private def failIfAny(errors: List[String]): Unit =
    if (errors.nonEmpty) fail(errors.mkString("\n"))

  test("the compiler's sources use the reserved names only in shapes the scan reads") {
    failIfAny(mainScans.flatMap(_._2.errors))
  }

  test("each reserved package name is its source's package") {
    failIfAny(ReservedPackage.all.flatMap { reserved =>
      val declared = namesByReserved(reserved).packageName
      if (declared == reserved.name.asString) None
      else
        Some(
          s"`${reserved.name.asString}` is not the package of ${reserved.source}, which is `$declared`"
        )
    })
  }

  test("each name given to a reserved package helper is declared by its source") {
    val uses = mainScans.flatMap(_._2.uses)
    // a scan that finds nothing must not quietly pass
    List("external", "value", "typeName").foreach { helper =>
      assert(uses.exists(_.helper == helper), s"found no uses of `$helper`")
    }
    assert(
      mainScans.exists { case (p, r) => p.startsWith("core/.js/") && r.uses.nonEmpty },
      "found no uses in the JS sources"
    )
    failIfAny(mainScans.flatMap { case (path, result) =>
      ReservedNamesScan.checkUses(path, result.uses, namesByReserved)
    })
  }

  test("every registered external is declared by its package's source") {
    val externalsByPackage: Map[PackageName, (String, List[String])] =
      ((PackageName.PredefName, (PredefName.source, sourceNames(PredefName.source).externals)) ::
        ReservedPackage.all.map(r => (r.name, (r.source, namesByReserved(r).externals)))).toMap

    val registered = Predef.jvmExternals.toMap.keys.toList
    assert(registered.nonEmpty)
    failIfAny(registered.flatMap { case (pack, name) =>
      externalsByPackage.get(pack) match {
        case None =>
          Some(
            s"external ${pack.asString}::${name.sourceCodeRepr} is registered for a package that ReservedPackage doesn't list"
          )
        case Some((source, known)) =>
          if (known.contains(name.sourceCodeRepr)) None
          else
            Some(
              "Predef.jvmExternals: " + CheckedNames.notFound(
                name.sourceCodeRepr,
                s"the external declarations of ${pack.asString} in $source",
                known
              )
            )
      }
    })
  }

  test("each C runtime name the C code generator uses is in a c_runtime header") {
    // a scan that finds nothing must not quietly reject every name
    assert(headerIdents.exists(_.startsWith("bsts_")), "no bsts_ identifiers in c_runtime/*.h")
    assert(mainScans.exists(_._2.runtimeUses.nonEmpty), "found no RuntimeIdent uses")
    failIfAny(mainScans.flatMap { case (path, result) =>
      ReservedNamesScan.checkRuntimeUses(path, result.runtimeUses, headerIdents)
    })
  }

  // the scan itself, on small sources

  test("the scan reads qualified, imported, renamed and multi-line uses") {
    val src =
      """package dev.bosatsu.example
        |import dev.bosatsu.ReservedPackage
        |import dev.bosatsu.ReservedPackage.IoCore
        |import dev.bosatsu.ReservedPackage.{Array as ArrayPackage, Prog}
        |import dev.bosatsu.codegen.clang.RuntimeIdent
        |
        |val a = ReservedPackage.Json.typeName("Json")
        |val b = IoCore.external("path_sep")
        |val c = ArrayPackage
        |  .value("size_Array")
        |val d = Prog.name
        |val e = RuntimeIdent("bsts_integer_cmp")
        |// IoCore.external(notALiteral) in a comment is ignored
        |""".stripMargin
    val result = ReservedNamesScan.scan("x.scala", src)
    assertEquals(result.errors, Nil)
    assertEquals(
      result.uses.toSet,
      Set(
        Use("Json", "typeName", "Json", 7),
        Use("IoCore", "external", "path_sep", 8),
        Use("Array", "value", "size_Array", 9)
      )
    )
    assertEquals(result.runtimeUses, List(RuntimeUse("bsts_integer_cmp", 12)))
  }

  test("the scan ignores names it can't see as dev.bosatsu's") {
    // LibConfig has its own Error.ReservedPackage
    val src =
      """package dev.bosatsu.library
        |object Error {
        |  case class ReservedPackage(name: String)
        |  def check(e: Any) = e match { case ReservedPackage(n) => n }
        |  val e = Error.ReservedPackage("x")
        |}
        |""".stripMargin
    assertEquals(ReservedNamesScan.scan("x.scala", src), ReservedNamesScan.Result(Nil, Nil, Nil))
  }

  test("the scan reports uses it can't read") {
    // these sources are in package dev.bosatsu, where ReservedPackage is visible
    def errorsOf(src: String): List[String] =
      ReservedNamesScan.scan("x.scala", s"package dev.bosatsu\n$src").errors

    assertEquals(
      errorsOf("""val a = ReservedPackage.Prog.external(name)"""),
      List("x.scala:2: expected a string literal, found `name`")
    )
    assertEquals(
      errorsOf("import dev.bosatsu.ReservedPackage.*"),
      List("x.scala:2: don't import ReservedPackage.*")
    )
    assertEquals(
      errorsOf("import dev.bosatsu.{ReservedPackage as RP}"),
      List("x.scala:2: don't rename ReservedPackage in an import")
    )
    assertEquals(
      errorsOf("val p = ReservedPackage.Prog\nval q = p.value(\"pure\")"),
      List("x.scala:2: can't read this use of Prog")
    )
    assertEquals(
      errorsOf(
        "import dev.bosatsu.codegen.clang.RuntimeIdent\n" +
          """val i = RuntimeIdent.apply("bsts_integer_cmp")"""
      ),
      List("x.scala:3: can't read this use of RuntimeIdent")
    )
    assertEquals(
      errorsOf("val r = ReservedPackage.all"),
      List("x.scala:2: can't read this use of ReservedPackage")
    )
  }

  test("a misspelled name is reported with the closest declared names") {
    val prog = ReservedPackage.Prog
    val names = Map(prog -> sourceNames(prog.source))
    assertEquals(
      ReservedNamesScan.checkUses(
        "x.scala",
        List(Use("Prog", "external", "puer", 3)),
        names
      ),
      List(
        "x.scala:3: `puer` is not one of the external declarations of Bosatsu/Prog in test_workspace/Prog.bosatsu. Did you mean `pure`?"
      )
    )
    // map is defined in Bosatsu, not external
    assertEquals(
      ReservedNamesScan
        .checkUses("x.scala", List(Use("Prog", "external", "map", 4)), names)
        .length,
      1
    )
    assertEquals(
      ReservedNamesScan.checkRuntimeUses(
        "y.scala",
        List(RuntimeUse("bsts_integer_cpm", 7)),
        CRuntimeNames.identifiers("int bsts_integer_cmp(void); int bsts_other(void);")
      ),
      List(
        "y.scala:7: `bsts_integer_cpm` is not one of the identifiers in the c_runtime/*.h headers. Did you mean `bsts_integer_cmp`?"
      )
    )
    // equally close names are all suggested
    assertEquals(
      ReservedNamesScan.checkRuntimeUses(
        "y.scala",
        List(RuntimeUse("bsts_c", 8)),
        Set("bsts_a", "bsts_b")
      ),
      List(
        "y.scala:8: `bsts_c` is not one of the identifiers in the c_runtime/*.h headers. Did you mean one of `bsts_a`, `bsts_b`?"
      )
    )
  }
}
