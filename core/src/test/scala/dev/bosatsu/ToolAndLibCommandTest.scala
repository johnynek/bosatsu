package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eval
import cats.data.Chain
import cats.implicits._
import dev.bosatsu.edn.Edn
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.library.{LibConfig, Libraries, Name, Version}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.tool.{ExitCode, GraphOutput, Output, ShowEdn}
import munit.FunSuite
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

class ToolAndLibCommandTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]
  private val validHash1 = "blake3:" + ("1" * 64)
  private val validHash2 = "blake3:" + ("2" * 64)
  private val validHash3 = "blake3:" + ("3" * 64)

  private def renderJson[A: Json.Writer](value: A): String =
    Json.Writer.write(value).render

  private def runWithState(
      cmd: List[String],
      state: MemoryMain.State
  ): ErrorOr[(MemoryMain.State, Output[Chain[String]])] =
    module.run(cmd) match {
      case Left(help) =>
        Left(new Exception(s"got help: $help on command: $cmd"))
      case Right(io) =>
        for {
          stateOut <- io.run(state)
          (nextState, _) <- module.reportOutput(stateOut._2).run(stateOut._1)
        } yield (nextState, stateOut._2)
    }

  private def runWithStateAndExit(
      cmd: List[String],
      state: MemoryMain.State
  ): ErrorOr[(MemoryMain.State, Output[Chain[String]], ExitCode)] =
    module.run(cmd) match {
      case Left(help) =>
        Left(new Exception(s"got help: $help on command: $cmd"))
      case Right(io) =>
        for {
          stateOut <- io.run(state)
          (nextState, exitCode) <- module.reportOutput(stateOut._2).run(stateOut._1)
        } yield (nextState, stateOut._2, exitCode)
    }

  private def readStringFile(
      state: MemoryMain.State,
      path: Chain[String]
  ): String =
    state.get(path) match {
      case Some(Right(MemoryMain.FileContent.Str(s))) => s
      case other                                      =>
        fail(s"expected string file at ${path.mkString_("/")}, found: $other")
    }

  private def assertNoFile(state: MemoryMain.State, path: Chain[String]): Unit =
    state.get(path) match {
      case None  => ()
      case other =>
        fail(s"expected no file at ${path.mkString_("/")}, found: $other")
    }

  private def readLibraryFile(
      state: MemoryMain.State,
      path: Chain[String]
  ): Hashed[Algo.Blake3, proto.Library] =
    state.get(path) match {
      case Some(Right(MemoryMain.FileContent.Lib(lib))) => lib
      case other                                        =>
        fail(s"expected library file at ${path.mkString_("/")}, found: $other")
    }

  private def allFilePaths(
      state: MemoryMain.State,
      prefix: Chain[String] = Chain.empty
  ): List[Chain[String]] =
    state.children.toList.flatMap {
      case (name, Right(_)) =>
        List(prefix :+ name)
      case (name, Left(dir)) =>
        allFilePaths(dir, prefix :+ name)
    }

  private def filePathsUnder(
      state: MemoryMain.State,
      prefix: Chain[String]
  ): Set[Chain[String]] = {
    val prefixList = prefix.toList
    allFilePaths(state).filter { path =>
      path.toList.startsWith(prefixList)
    }.toSet
  }

  private def casPathFor(
      repoRoot: Chain[String],
      lib: Hashed[Algo.Blake3, proto.Library]
  ): Chain[String] = {
    val hex = lib.hash.hex
    repoRoot ++ Chain(".bosatsuc", "cas", "blake3", hex.take(2), hex.drop(2))
  }

  private def baseLibFiles(mainSrc: String): List[(Chain[String], String)] = {
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Foo.bosatsu") -> mainSrc
    )
  }

  private def baseLibFilesWithConf(
      mainSrc: String,
      conf: LibConfig
  ): List[(Chain[String], String)] = {
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Foo.bosatsu") -> mainSrc
    )
  }

  private val bosatsuJsonModuleSrc: String =
    """package Bosatsu/Json
|
|export Json(), Optional(), Nullable()
|
|enum Json:
|  JNull
|  JBool(value: Bool)
|  JString(value: String)
|  JInt(value: Int)
|  JFloat(value: Float64)
|  JArray(items: List[Json])
|  JObject(items: List[(String, Json)])
|
|enum Optional[a]:
|  Missing
|  Set(value: a)
|
|enum Nullable[a]:
|  Null
|  NonNull(value: a)
|""".stripMargin

  private def withBosatsuJsonModule(
      files: List[(Chain[String], String)]
  ): List[(Chain[String], String)] =
    files :+ (
      Chain("repo", "src", "Bosatsu", "Json.bosatsu") -> bosatsuJsonModuleSrc
    )

  private def packageKeywordFields(
      pack: Package.Typed[Any]
  ): Map[String, Edn] = {
    import Edn._

    ShowEdn.packageCodec.encode(pack.void) match {
      case EList(ESymbol("package") :: args) =>
        args.grouped(2).collect { case EKeyword(k) :: value :: Nil =>
          k -> value
        }.toMap
      case other =>
        fail(s"expected package edn, found: ${Edn.toDoc(other).render(120)}")
    }
  }

  private def vectorStrings(edn: Edn): List[String] = {
    import Edn._

    edn match {
      case EVector(items) =>
        items.map {
          case EString(s) => s
          case ESymbol(s) => s
          case other =>
            fail(
              s"expected string/symbol atom, found: ${Edn.toDoc(other).render(120)}"
            )
        }
      case other =>
        fail(s"expected vector, found: ${Edn.toDoc(other).render(120)}")
    }
  }

  private def atomString(edn: Edn): String = {
    import Edn._

    edn match {
      case EString(s) => s
      case ESymbol(s) => s
      case other =>
        fail(s"expected atom, found: ${Edn.toDoc(other).render(120)}")
    }
  }

  private def packageTypeNames(pack: Package.Typed[Any]): List[String] = {
    import Edn._

    packageKeywordFields(pack).get("types") match {
      case Some(EVector(items)) =>
        items.map {
          case EList(ESymbol("defined-type") :: _ :: nameEdn :: _) =>
            atomString(nameEdn)
          case other =>
            fail(s"unexpected type entry: ${Edn.toDoc(other).render(120)}")
        }
      case Some(other) =>
        fail(s"expected :types vector, found: ${Edn.toDoc(other).render(120)}")
      case None =>
        Nil
    }
  }

  private def packageDefNames(pack: Package.Typed[Any]): List[String] = {
    import Edn._

    packageKeywordFields(pack).get("defs") match {
      case Some(EVector(items)) =>
        items.map {
          case EList((ESymbol("def") | ESymbol("defrec")) :: nameEdn :: _ :: Nil) =>
            atomString(nameEdn)
          case other =>
            fail(s"unexpected def entry: ${Edn.toDoc(other).render(120)}")
        }
      case Some(other) =>
        fail(s"expected :defs vector, found: ${Edn.toDoc(other).render(120)}")
      case None =>
        Nil
    }
  }

  private def packageExternals(pack: Package.Typed[Any]): List[(String, Edn)] = {
    import Edn._

    packageKeywordFields(pack).get("externals") match {
      case Some(EVector(items)) =>
        items.map {
          case EVector(List(nameEdn, typeEdn)) =>
            (atomString(nameEdn), typeEdn)
          case other =>
            fail(s"unexpected external entry: ${Edn.toDoc(other).render(120)}")
        }
      case Some(other) =>
        fail(s"expected :externals vector, found: ${Edn.toDoc(other).render(120)}")
      case None =>
        Nil
    }
  }

  private def importItems(
      pack: Package.Typed[Any]
  ): List[(String, List[(String, String)])] = {
    import Edn._

    packageKeywordFields(pack).get("imports") match {
      case Some(EVector(imports)) =>
        imports.map {
          case EList(
                ESymbol("import") :: EString(pkgName) :: EVector(items) :: Nil
              ) =>
            val parsedItems = items.map {
              case EList(
                    ESymbol("item") :: EString(original) :: EString(local) :: _ :: _
                  ) =>
                (original, local)
              case other =>
                fail(
                  s"unexpected import item: ${Edn.toDoc(other).render(120)}"
                )
            }
            (pkgName, parsedItems)
          case other =>
            fail(s"unexpected import entry: ${Edn.toDoc(other).render(120)}")
        }
      case Some(other) =>
        fail(s"expected :imports vector, found: ${Edn.toDoc(other).render(120)}")
      case None =>
        fail("missing :imports field")
    }
  }

  private def showPackageFields(
      packs: List[Package.Typed[Any]]
  ): Map[String, Edn] = {
    import Edn._

    val rendered = ShowEdn.showDoc(packs, Nil).render(120)
    val parsed = Edn.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse show output: $err")
    }

    val packageEdn = parsed match {
      case EList(
            ESymbol("show") :: EKeyword("interfaces") :: _ :: EKeyword(
              "packages"
            ) :: EVector(packages) :: Nil
          ) =>
        packages.headOption.getOrElse(fail("expected one package"))
      case other =>
        fail(s"unexpected show output: ${Edn.toDoc(other).render(120)}")
    }

    packageEdn match {
      case EList(ESymbol("package") :: _ :: _ :: args) =>
        args.grouped(2).collect { case EKeyword(k) :: value :: Nil =>
          k -> value
        }.toMap
      case other =>
        fail(s"expected package form, found: ${Edn.toDoc(other).render(120)}")
    }
  }

  private def showJsonPackageNames(json: Json): List[String] =
    json match {
      case Json.JObject(fields) =>
        val byKey = fields.toMap
        assertEquals(byKey.get("$form"), Some(Json.JString("show")))
        byKey.get("packages") match {
          case Some(Json.JArray(packs)) =>
            packs.toList.map {
              case Json.JObject(packFields) =>
                val packMap = packFields.toMap
                assertEquals(packMap.get("$form"), Some(Json.JString("package")))
                packMap.get("name") match {
                  case Some(Json.JString(name)) => name
                  case other                    => fail(s"missing package name in $other")
                }
              case other =>
                fail(s"expected package object, found: $other")
            }
          case other =>
            fail(s"expected show packages array, found: $other")
        }
      case other =>
        fail(s"expected show json object, found: $other")
    }

  private def withInjectedPublicDep(
      state: MemoryMain.State,
      previousLibPath: Chain[String],
      dependencyLibPath: Chain[String],
      outPath: Chain[String]
  ): ErrorOr[MemoryMain.State] =
    (state.get(previousLibPath), state.get(dependencyLibPath)) match {
      case (
            Some(Right(MemoryMain.FileContent.Lib(previousLib))),
            Some(Right(MemoryMain.FileContent.Lib(depLib)))
          ) =>
        val depRef =
          proto.LibDependency(
            name = depLib.arg.name,
            desc = depLib.arg.descriptor
          )
        val rewritten = previousLib.arg.copy(
          publicDependencies =
            depRef :: previousLib.arg.publicDependencies.toList
        )
        val rehashed = Hashed(Algo.hashBytes(rewritten.toByteArray), rewritten)
        state.withFile(outPath, MemoryMain.FileContent.Lib(rehashed)) match {
          case Some(next) => Right(next)
          case None       =>
            Left(
              new Exception(show"failed to write rewritten library at $outPath")
            )
        }
      case (prev, dep) =>
        Left(
          new Exception(
            show"expected library files at $previousLibPath and $dependencyLibPath, found: ${prev.toString} and ${dep.toString}"
          )
        )
    }

  test("root eval command moved under tool") {
    module.run(List("eval", "--main", "MyLib/Foo")) match {
      case Left(_)  => ()
      case Right(_) => fail("expected parse failure without `tool` prefix")
    }
  }

  test("mainExceptionToString returns None for non-cli exceptions") {
    assertEquals(module.mainExceptionToString(new Exception("boom")), None)
  }

  test("tool assemble and tool extract-iface") {
    val src =
      """main = 1
"""
    val files = List(Chain("src", "Tool", "Foo.bosatsu") -> src)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "src",
          "--input",
          "src/Tool/Foo.bosatsu",
          "--output",
          "out/Tool.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "tooltest",
          "--version",
          "0.0.1",
          "--package",
          "out/Tool.Foo.bosatsu_package",
          "--output",
          "out/tooltest.bosatsu_lib"
        ),
        state1
      )
      (state2, out2) = s2
      _ = out2 match {
        case Output.Library(_, _) => ()
        case other                => fail(s"unexpected output: $other")
      }
      s3 <- runWithState(
        List(
          "tool",
          "extract-iface",
          "--input",
          "out/tooltest.bosatsu_lib",
          "--output",
          "out/tooltest.bosatsu_ifacelib"
        ),
        state2
      )
    } yield s3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out3)) =>
        out3 match {
          case Output.Library(lib, outPath) =>
            assertEquals(outPath, Chain("out", "tooltest.bosatsu_ifacelib"))
            assertEquals(lib.internalPackages.toList, Nil)
          case other =>
            fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval/json/show use library context") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List("lib", "eval", "--repo_root", "repo", "--main", "MyLib/Foo")
    ) match {
      case Right(Output.EvaluationResult(_, _, _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    module.runWith(files)(
      List("lib", "json", "write", "--repo_root", "repo", "--main", "MyLib/Foo")
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("42"), _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    module.runWith(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--json"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(showJsonPackageNames(json), List("MyLib/Foo"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show invalid package name parse error includes package hint") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "euler1")
    ) match {
      case Left(err) =>
        val msg = err.getMessage
        assert(msg.contains("could not parse euler1 as a package name"), msg)
        assert(msg.contains("parser error:"), msg)
        assert(msg.contains("use the package declaration name"), msg)
        assert(msg.contains("Euler/One"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("lib show includes original and local import names") {
    val src =
      """from Bosatsu/Predef import add as operator +, mul as operator *
|
|main = 1 + (2 * 3)
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        val imports = importItems(pack)
        val predefImport = imports.find(_._1 == "Bosatsu/Predef")
        assert(predefImport.nonEmpty, imports.toString)
        val names = predefImport.get._2
        assert(names.contains(("add", "operator +")), names.toString)
        assert(names.contains(("mul", "operator *")), names.toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show separates exported types and values to avoid name collisions") {
    val src =
      """export Opt(), Pair(), main
|
|enum Opt[a]:
|  N,
|  S(value: a)
|
|struct Pair[a](left: a, right: a)
|
|main = 1
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        val fields = packageKeywordFields(pack)

        val typeNames = fields.get("exported-types").toList.flatMap(vectorStrings)
        val valueNames = fields.get("exported-values").toList.flatMap(vectorStrings)

        assert(typeNames.contains("Opt"), typeNames.toString)
        assert(typeNames.contains("Pair"), typeNames.toString)
        assert(!typeNames.contains("main"), typeNames.toString)

        assert(valueNames.contains("N"), valueNames.toString)
        assert(valueNames.contains("S"), valueNames.toString)
        assert(valueNames.contains("Pair"), valueNames.toString)
        assert(valueNames.contains("main"), valueNames.toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show supports --type and --value selectors") {
    val src =
      """export Box(), helper, other, main
|
|struct Box(value: Int)
|
|helper = 42
|other = 99
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--type",
        "MyLib/Foo::Box",
        "--value",
        "MyLib/Foo::helper"
      )
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assertEquals(packageTypeNames(pack), List("Box"))
        assertEquals(packageDefNames(pack), List("helper"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show --value validates that the selected value exists") {
    val src =
      """main = 42
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::missing"
      )
    ) match {
      case Left(err) =>
        val msg = err.getMessage
        assert(msg.contains("value not found: MyLib/Foo::missing"), msg)
        assert(
          msg.contains(
            "fully inlined values may be absent from compiled code"
          ),
          msg
        )
        assert(msg.contains("compiled top-level values: [main]"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("lib show --no-opt retains values removed by optimization") {
    val src =
      """helper = 1
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::helper"
      )
    ) match {
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("value not found: MyLib/Foo::helper"), msg)
      case Right(other) =>
        fail(s"expected optimized show to omit helper, found: $other")
    }

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::helper",
        "--no-opt"
      )
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assert(packageDefNames(pack).contains("helper"), packageDefNames(pack).toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show --value includes local value dependencies and only needed imports") {
    val src =
      """from Bosatsu/Predef import add, mul
|
|def main(x):
|  add(x, 2)
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::main"
      )
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assert(packageDefNames(pack).contains("main"), packageDefNames(pack).toString)
        val imports = importItems(pack)
        val predefItems =
          imports.find(_._1 == "Bosatsu/Predef").map(_._2).getOrElse(Nil)
        assert(predefItems.contains(("add", "add")), predefItems.toString)
        assert(!predefItems.contains(("mul", "mul")), predefItems.toString)
        val showFields = showPackageFields(List(pack))
        assert(!showFields.contains("types"), showFields.toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show --value includes local types required by constructor usage") {
    val src =
      """struct Box(value: Int)
|
|helper = Box(1)
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::main"
      )
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assert(packageTypeNames(pack).contains("Box"), packageTypeNames(pack).toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib show --externals only includes external values with types") {
    val src =
      """external struct Box
|
|external def from_Int(i: Int) -> Box
|external def to_Int(box: Box) -> Int
|
|helper = 1
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--externals"
      )
    ) match {
      case Right(Output.ShowOutput(packs, interfaces, _)) =>
        assertEquals(interfaces, Nil)
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        val showFields = showPackageFields(packs)
        assertEquals(showFields.keySet, Set("externals"))
        assertEquals(packageTypeNames(pack), Nil)
        assertEquals(packageDefNames(pack), Nil)

        val externals = packageExternals(pack)
        assertEquals(externals.map(_._1), List("from_Int", "to_Int"))
        externals.foreach { case (_, tpeEdn) =>
          val rendered = Edn.toDoc(tpeEdn).render(120)
          assert(rendered.nonEmpty, "expected encoded external type to be present")
        }
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool show supports mixed package/type/value selectors") {
    val fooSrc =
      """package App/Foo
|
|struct Pair(left: Int, right: Int)
|
|keep = 1
|main = keep
|""".stripMargin
    val barSrc =
      """package App/Bar
|
|bar = 7
|""".stripMargin
    val files = List(
      Chain("src", "App", "Foo.bosatsu") -> fooSrc,
      Chain("src", "App", "Bar.bosatsu") -> barSrc
    )

    module.runWith(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Foo.bosatsu",
        "--input",
        "src/App/Bar.bosatsu",
        "--type",
        "App/Foo::Pair",
        "--value",
        "App/Foo::main",
        "--value",
        "App/Bar::bar"
      )
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        assertEquals(packs.map(_.name.asString), List("App/Foo", "App/Bar"))
        val byName = packs.map(pack => pack.name.asString -> pack).toMap
        val foo = byName.getOrElse("App/Foo", fail("missing App/Foo package"))
        val bar = byName.getOrElse("App/Bar", fail("missing App/Bar package"))
        assertEquals(packageTypeNames(foo), List("Pair"))
        assertEquals(packageDefNames(foo), List("main"))
        assertEquals(packageTypeNames(bar), Nil)
        assertEquals(packageDefNames(bar), List("bar"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool show --externals only includes packages with external values") {
    val extSrc =
      """package App/Ext
|
|external struct Token
|external def mk_Token(i: Int) -> Token
|external def read_Token(t: Token) -> Int
|
|main = 1
|""".stripMargin
    val regularSrc =
      """package App/Regular
|
|main = 2
|""".stripMargin
    val files = List(
      Chain("src", "App", "Ext.bosatsu") -> extSrc,
      Chain("src", "App", "Regular.bosatsu") -> regularSrc
    )

    module.runWith(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Ext.bosatsu",
        "--input",
        "src/App/Regular.bosatsu",
        "--externals"
      )
    ) match {
      case Right(Output.ShowOutput(packs, interfaces, _)) =>
        assertEquals(interfaces, Nil)
        assertEquals(packs.map(_.name.asString), List("App/Ext"))
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        val showFields = showPackageFields(packs)
        assertEquals(showFields.keySet, Set("externals"))
        assertEquals(packageTypeNames(pack), Nil)
        assertEquals(packageDefNames(pack), Nil)

        val externals = packageExternals(pack)
        assertEquals(externals.map(_._1), List("mk_Token", "read_Token"))
        externals.foreach { case (_, tpeEdn) =>
          val rendered = Edn.toDoc(tpeEdn).render(120)
          assert(rendered.nonEmpty, "expected encoded external type to be present")
        }
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool show --json emits machine-readable package selection output") {
    val fooSrc =
      """package App/Foo
|
|main = 1
|""".stripMargin
    val barSrc =
      """package App/Bar
|
|bar = 7
|""".stripMargin
    val files = List(
      Chain("src", "App", "Foo.bosatsu") -> fooSrc,
      Chain("src", "App", "Bar.bosatsu") -> barSrc
    )

    module.runWith(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Foo.bosatsu",
        "--input",
        "src/App/Bar.bosatsu",
        "--value",
        "App/Bar::bar",
        "--json"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(showJsonPackageNames(json), List("App/Bar"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool show --value missing includes inlining hint and candidates") {
    val src =
      """package App/Foo
|
|main = 1
|""".stripMargin
    val files = List(
      Chain("src", "App", "Foo.bosatsu") -> src
    )

    module.runWith(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Foo.bosatsu",
        "--value",
        "App/Foo::missing"
      )
    ) match {
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("value not found: App/Foo::missing"), msg)
        assert(
          msg.contains(
            "fully inlined values may be absent from compiled code"
          ),
          msg
        )
        assert(msg.contains("compiled top-level values: [main]"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("lib eval missing value reports CliException without stack trace") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "eval",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::missing"
      )
    ) match {
      case Left(err) =>
        val msg = module.mainExceptionToString(err).getOrElse(
          fail(s"expected CliException, found: $err")
        )
        assert(msg.contains("value MyLib/Foo::missing not found"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("tool eval missing value reports CliException without stack trace") {
    val src =
      """main = 42
"""
    val files = List(Chain("src", "Tool", "Foo.bosatsu") -> src)

    module.runWith(files)(
      List(
        "tool",
        "eval",
        "--main",
        "Tool/Foo::missing",
        "--package_root",
        "src",
        "--input",
        "src/Tool/Foo.bosatsu"
      )
    ) match {
      case Left(err) =>
        val msg = module.mainExceptionToString(err).getOrElse(
          fail(s"expected CliException, found: $err")
        )
        assert(msg.contains("value Tool/Foo::missing not found"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("tool eval --run executes Bosatsu/Prog::Main and forwards trailing args") {
    val progSrc =
      """package Bosatsu/Prog
|
|export Prog(), Main(), pure
|
|enum Prog[e, a]:
|  Pure(value: a)
|  Raise(err: e)
|
|struct Main(run: List[String] -> Prog[String, Int])
|
|def pure(a):
|  Pure(a)
|""".stripMargin
    val appSrc =
      """package Tool/Foo
|
|from Bosatsu/Prog import Main, Prog, pure
|
|main = Main(args -> pure(args.foldl_List(0, (n, _) -> add(n, 1))))
|""".stripMargin
    val files = List(
      Chain("src", "Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("src", "Tool", "Foo.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithStateAndExit(
        List(
          "tool",
          "eval",
          "--run",
          "--main",
          "Tool/Foo",
          "--package_root",
          "src",
          "--input",
          "src/Bosatsu/Prog.bosatsu",
          "--input",
          "src/Tool/Foo.bosatsu",
          "one",
          "two",
          "three"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.fromInt(3))
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval --run executes Bosatsu/Prog::Main and forwards trailing args") {
    val progSrc =
      """package Bosatsu/Prog
|
|export Prog(), Main(), pure
|
|enum Prog[e, a]:
|  Pure(value: a)
|  Raise(err: e)
|
|struct Main(run: List[String] -> Prog[String, Int])
|
|def pure(a):
|  Pure(a)
|""".stripMargin
    val appSrc =
      """from Bosatsu/Prog import Main, Prog, pure
|
|main = Main(args -> pure(args.foldl_List(0, (n, _) -> add(n, 1))))
|""".stripMargin
    val files =
      baseLibFiles(appSrc) :+ (
        Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> progSrc
      )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/Foo",
          "--run",
          "left",
          "right"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.fromInt(2))
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("tool eval --run rejects non Bosatsu/Prog::Main values") {
    val src =
      """main = 42
"""
    val files = List(Chain("src", "Tool", "Foo.bosatsu") -> src)

    module.runWith(files)(
      List(
        "tool",
        "eval",
        "--run",
        "--main",
        "Tool/Foo",
        "--package_root",
        "src",
        "--input",
        "src/Tool/Foo.bosatsu"
      )
    ) match {
      case Left(err) =>
        val msg = module.mainExceptionToString(err).getOrElse(
          fail(s"expected CliException, found: $err")
        )
        assert(msg.contains("--run requires a Bosatsu/Prog::Main value"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("lib eval constructor main reports actionable parse error") {
    val src =
      """enum Flag: True, False
main = 42
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "eval",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::True"
      )
    ) match {
      case Left(err) =>
        val msg = err.getMessage
        assert(msg.contains("got the help message for"), msg)
        assert(msg.contains("constructor or type name"), msg)
        assert(msg.contains("top-level value is required"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("lib json constructor main reports actionable parse error") {
    val src =
      """enum Flag: True, False
main = 42
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::True"
      )
    ) match {
      case Left(err) =>
        val msg = err.getMessage
        assert(msg.contains("got the help message for"), msg)
        assert(msg.contains("constructor or type name"), msg)
        assert(msg.contains("top-level value is required"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test(
    "tool doc writes markdown for source packages and excludes include packages"
  ) {
    val depSrc =
      """export DepBox(), depBox

struct DepBox(v: Int)

depBox = DepBox(7)
"""
    val appSrc =
      """from Dep/Util import depBox

export Box(), run, dep_main

# Box docs.
struct Box(v: Int)

# Run docs.
run = (b) -> match b:
  case Box(v): v

# Dependency docs.
dep_main = depBox
"""

    val files = List(
      Chain("dep", "Dep", "Util.bosatsu") -> depSrc,
      Chain("src", "App", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Util.bosatsu",
          "--output",
          "out/Dep.Util.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "doc",
          "--package_root",
          "src",
          "--input",
          "src/App/Main.bosatsu",
          "--include",
          "out/Dep.Util.bosatsu_package",
          "--outdir",
          "docs"
        ),
        state1
      )
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out)) =>
        out match {
          case Output.TranspileOut(outputs) =>
            assertEquals(
              outputs.map(_._1),
              List(Chain("docs", "App", "Main.md"))
            )
          case other =>
            fail(s"unexpected output: $other")
        }

        val markdown = readStringFile(state, Chain("docs", "App", "Main.md"))
        assert(markdown.contains("# `App/Main`"), markdown)
        assert(markdown.contains("public dependencies: `Dep/Util`"), markdown)
        assert(markdown.contains("## Values"), markdown)
        assert(markdown.contains("## Types"), markdown)
        assert(
          markdown.indexOf("## Types") < markdown.indexOf("## Values"),
          markdown
        )
        assert(markdown.contains("Box docs."), markdown)
        assert(markdown.contains("Run docs."), markdown)
        assert(markdown.contains("def run("), markdown)
        assert(markdown.contains("`Box(v: Int)`"), markdown)
        assert(!markdown.contains("Bosatsu/Predef::Int"), markdown)
        assert(markdown.contains("```bosatsu"), markdown)
        assertNoFile(state, Chain("docs", "Dep", "Util.md"))
    }
  }

  test("lib doc writes markdown in package directory layout") {
    val src =
      """export Thing(), mk

# Thing docs.
struct Thing(v: Int)

# Mk docs.
mk = (x) -> Thing(x)
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "doc",
        "--repo_root",
        "repo",
        "--name",
        "mylib",
        "--outdir",
        "outdocs"
      )
    ) match {
      case Right(Output.TranspileOut(outputs)) =>
        assertEquals(
          outputs.map(_._1),
          List(Chain("outdocs", "MyLib", "Foo.md"))
        )
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "outdocs"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown =
          readStringFile(state, Chain("outdocs", "MyLib", "Foo.md"))
        assert(markdown.contains("# `MyLib/Foo`"), markdown)
        assert(markdown.contains("Thing docs."), markdown)
        assert(markdown.contains("Mk docs."), markdown)
        assert(!markdown.contains("public dependencies:"), markdown)
        assert(
          markdown.indexOf("## Types") < markdown.indexOf("## Values"),
          markdown
        )
        assert(markdown.contains("def mk("), markdown)
        assert(markdown.contains("`Thing(v: Int)`"), markdown)
        assert(!markdown.contains("Bosatsu/Predef::Int"), markdown)
        assert(markdown.contains("## Values"), markdown)
        assert(markdown.contains("## Types"), markdown)
    }
  }

  test("tool doc includes comments that appear before enum declarations") {
    val src =
      """export Mode(), mode
# Mode docs.
enum Mode:
  Auto, Manual

mode = Auto
"""
    val files = List(Chain("src", "EnumDocs", "Main.bosatsu") -> src)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
          "--package_root",
          "src",
          "--input",
          "src/EnumDocs/Main.bosatsu",
          "--outdir",
          "docs"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown =
          readStringFile(state, Chain("docs", "EnumDocs", "Main.md"))
        assert(markdown.contains("Mode docs."), markdown)
        assert(markdown.contains("### `Mode`"), markdown)
    }
  }

  test("tool doc wraps long constructor and def parameter lists") {
    val src =
      """export Massive(), build

struct Massive(alpha: Int, beta: Int, gamma: Int, delta: Int, epsilon: Int, zeta: Int, eta: Int, theta: Int, iota: Int, kappa: Int, lambda: Int)

build = (a, b, c, d, e, f, g, h, i, j, k) -> Massive(a, b, c, d, e, f, g, h, i, j, k)
"""
    val files = List(Chain("src", "Wrap", "Main.bosatsu") -> src)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
          "--package_root",
          "src",
          "--input",
          "src/Wrap/Main.bosatsu",
          "--outdir",
          "docs"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown = readStringFile(state, Chain("docs", "Wrap", "Main.md"))
        assert(markdown.contains("def build(a: Int, b: Int, c: Int"), markdown)
        assert(!markdown.contains("arg1:"), markdown)
        assert(markdown.contains("Massive(\n"), markdown)
        assert(markdown.contains("alpha: Int,\n"), markdown)
    }
  }

  test("tool doc --include_predef includes Bosatsu/Predef markdown") {
    val src =
      """export main,
main = 1
"""
    val files = List(Chain("src", "Simple", "Main.bosatsu") -> src)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
          "--package_root",
          "src",
          "--input",
          "src/Simple/Main.bosatsu",
          "--outdir",
          "docs",
          "--include_predef"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out)) =>
        out match {
          case Output.TranspileOut(outputs) =>
            val outPaths = outputs.map(_._1.toList.mkString("/")).toSet
            assert(outPaths("docs/Simple/Main.md"), outputs.toString)
            assert(
              outPaths("docs/Bosatsu/Predef.md"),
              outputs.toString
            )
          case other =>
            fail(s"unexpected output: $other")
        }

        val predefDoc =
          readStringFile(state, Chain("docs", "Bosatsu", "Predef.md"))
        assert(predefDoc.contains("# `Bosatsu/Predef`"), predefDoc)
        assert(!predefDoc.contains("public dependencies:"), predefDoc)
        assert(predefDoc.contains("## Index"), predefDoc)
        val indexSection =
          predefDoc.substring(
            predefDoc.indexOf("## Index"),
            predefDoc.indexOf("## Types")
          )
        assert(predefDoc.contains("[`Bool`](#type-bool)"), predefDoc)
        assert(indexSection.contains("[`Fn2`](#type-fn2)"), predefDoc)
        assert(
          !indexSection.contains("[`Fn2[i0, i1, z]`](#type-fn2)"),
          predefDoc
        )
        assert(predefDoc.contains("[`int_loop`](#value-int-loop)"), predefDoc)
        assert(predefDoc.contains("<a id=\"type-bool\"></a>"), predefDoc)
        assert(predefDoc.contains("<a id=\"value-int-loop\"></a>"), predefDoc)
        assert(predefDoc.contains("type Dict[k: *, v: +*]"), predefDoc)
        assert(predefDoc.contains("type Int"), predefDoc)
        assert(!predefDoc.contains("type Int: *"), predefDoc)
        assert(!predefDoc.contains("type Bool: *"), predefDoc)
        assert(predefDoc.contains("Standard dictionaries"), predefDoc)
        assert(!predefDoc.contains("############"), predefDoc)
        assert(predefDoc.contains("- `EmptyList`"), predefDoc)
        assert(
          predefDoc.contains("- `NonEmptyList(head: a, tail: List[a])`"),
          predefDoc
        )
        assert(!predefDoc.contains("EmptyList: forall"), predefDoc)
        assert(predefDoc.contains("### `Fn1[i0, z]`"), predefDoc)
        assert(predefDoc.contains("### `Fn2[i0, i1, z]`"), predefDoc)
        assert(
          predefDoc.contains(
            "### `Fn10[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, z]`"
          ),
          predefDoc
        )
        assert(
          predefDoc.indexOf("### `Fn1[i0, z]`") < predefDoc.indexOf(
            "### `Fn2[i0, i1, z]`"
          ),
          predefDoc
        )
        assert(
          predefDoc.indexOf("### `Fn2[i0, i1, z]`") < predefDoc.indexOf(
            "### `Fn10[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, z]`"
          ),
          predefDoc
        )
        assert(
          predefDoc.indexOf("### `Tuple2[a, b]`") < predefDoc.indexOf(
            "### `Tuple10[a, b, c, d, e, f, g, h, i, j]`"
          ),
          predefDoc
        )
        assert(
          predefDoc.contains("returned Int is <= 0"),
          predefDoc
        )
        assert(
          predefDoc.contains("intValue"),
          predefDoc
        )
        assert(predefDoc.contains("def int_loop[a]("), predefDoc)
        assert(predefDoc.contains("intValue: Int"), predefDoc)
        assert(predefDoc.contains("state: a"), predefDoc)
        assert(predefDoc.contains("fn: (Int, a) -> (Int, a)"), predefDoc)
    }
  }

  test("lib doc --include_predef includes Bosatsu/Predef markdown") {
    val src =
      """export main,
main = 1
"""
    val files = baseLibFiles(src)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "docs",
          "--include_predef"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out)) =>
        out match {
          case Output.TranspileOut(outputs) =>
            val outPaths = outputs.map(_._1.toList.mkString("/")).toSet
            assert(outPaths("docs/MyLib/Foo.md"), outputs.toString)
            assert(
              outPaths("docs/Bosatsu/Predef.md"),
              outputs.toString
            )
          case other =>
            fail(s"unexpected output: $other")
        }

        val predefDoc =
          readStringFile(state, Chain("docs", "Bosatsu", "Predef.md"))
        assert(predefDoc.contains("# `Bosatsu/Predef`"), predefDoc)
    }
  }

  test(
    "tool commands can evaluate and show packages from --pub_dep libraries"
  ) {
    val depSrc =
      """main = 9
"""
    val files = List(Chain("dep", "Dep", "Foo.bosatsu") -> depSrc)

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Foo.bosatsu",
          "--output",
          "out/Dep.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Foo.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "json",
          "write",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--main",
          "Dep/Foo"
        ),
        state2
      )
      (state3, outJson) = s3
      s4 <- runWithState(
        List("tool", "show", "--pub_dep", "out/dep.bosatsu_lib"),
        state3
      )
      (_, outShow) = s4
    } yield (outJson, outShow)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((jsonOut, showOut)) =>
        jsonOut match {
          case Output.JsonOutput(Json.JNumberStr("9"), _) => ()
          case other => fail(s"unexpected json output: $other")
        }
        showOut match {
          case Output.ShowOutput(packs, _, _) =>
            assertEquals(packs.map(_.name.asString), List("Dep/Foo"))
          case other =>
            fail(s"unexpected show output: $other")
        }
    }
  }

  test("tool assemble rejects interface-only dependency libraries") {
    val depSrc =
      """depValue = 9
main = depValue
"""
    val appSrc =
      """main = 1
"""
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("app", "App", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Foo.bosatsu",
          "--output",
          "out/Dep.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Foo.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "extract-iface",
          "--input",
          "out/dep.bosatsu_lib",
          "--output",
          "out/dep.bosatsu_ifacelib"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "app",
          "--input",
          "app/App/Main.bosatsu",
          "--output",
          "out/App.Main.bosatsu_package"
        ),
        state3
      )
      (state4, _) = s4
    } yield state4

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state4) =>
        val privateDepResult = runWithState(
          List(
            "tool",
            "assemble",
            "--name",
            "app",
            "--version",
            "0.0.1",
            "--package",
            "out/App.Main.bosatsu_package",
            "--priv_dep",
            "out/dep.bosatsu_ifacelib",
            "--output",
            "out/app_priv.bosatsu_lib"
          ),
          state4
        )
        privateDepResult match {
          case Right((_, out)) =>
            fail(
              s"expected assemble failure when using .bosatsu_ifacelib private dependency, got: $out"
            )
          case Left(err) =>
            val msg = Option(err.getMessage).getOrElse(err.toString)
            assert(
              msg.contains("invalid private dependency libraries"),
              s"unexpected private-dep error: $msg"
            )
        }

        val publicDepResult = runWithState(
          List(
            "tool",
            "assemble",
            "--name",
            "app",
            "--version",
            "0.0.1",
            "--package",
            "out/App.Main.bosatsu_package",
            "--pub_dep",
            "out/dep.bosatsu_ifacelib",
            "--output",
            "out/app_pub.bosatsu_lib"
          ),
          state4
        )
        publicDepResult match {
          case Right((_, out)) =>
            fail(
              s"expected assemble failure when using .bosatsu_ifacelib public dependency, got: $out"
            )
          case Left(err) =>
            val msg = Option(err.getMessage).getOrElse(err.toString)
            assert(
              msg.contains("invalid public dependency libraries"),
              s"unexpected public-dep error: $msg"
            )
        }
    }
  }

  test("tool deps emits sorted json and dot output") {
    val alphaSrc =
      """from Missing/Dep import x
"""
    val zedSrc =
      ""
    val files = List(
      Chain("src", "Zed", "Main.bosatsu") -> zedSrc,
      Chain("src", "Alpha", "Main.bosatsu") -> alphaSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "deps",
          "--package_root",
          "src",
          "--input",
          "src/Zed/Main.bosatsu",
          "--input",
          "src/Alpha/Main.bosatsu",
          "--graph_format",
          "json",
          "--output",
          "out/deps.json"
        ),
        s0
      )
      (state1, jsonOut) = s1
      _ = jsonOut match {
        case Output.DepsOutput(_, Some(path), GraphOutput.Json) =>
          assertEquals(path, Chain("out", "deps.json"))
        case other =>
          fail(s"unexpected json deps output: $other")
      }
      s2 <- runWithState(
        List(
          "tool",
          "deps",
          "--package_root",
          "src",
          "--input",
          "src/Zed/Main.bosatsu",
          "--input",
          "src/Alpha/Main.bosatsu",
          "--graph_format",
          "dot",
          "--output",
          "out/deps.dot"
        ),
        state1
      )
      (state2, dotOut) = s2
      _ = dotOut match {
        case Output.DepsOutput(_, Some(path), GraphOutput.Dot) =>
          assertEquals(path, Chain("out", "deps.dot"))
        case other =>
          fail(s"unexpected dot deps output: $other")
      }
    } yield state2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        val jsonStr = readStringFile(state, Chain("out", "deps.json"))
        Json.parserFile.parseAll(jsonStr) match {
          case Right(Json.JArray(items)) =>
            val itemList = items.toList
            assertEquals(itemList.size, 2)

            val paths = itemList.map {
              case Json.JObject(fields) =>
                fields.collectFirst { case ("path", Json.JString(p)) =>
                  p
                } match {
                  case Some(p) => p
                  case None    => fail(s"missing path field in: ${fields}")
                }
              case other =>
                fail(s"expected json object item, found: $other")
            }
            assertEquals(
              paths,
              List("src/Alpha/Main.bosatsu", "src/Zed/Main.bosatsu")
            )

            val alphaDependsOn: Option[List[String]] = itemList.collectFirst {
              case Json.JObject(fields) if fields.exists {
                    case ("package", Json.JString("Alpha/Main")) => true
                    case _                                       => false
                  } =>
                fields
                  .collectFirst { case ("dependsOn", Json.JArray(values)) =>
                    values.toList.collect { case Json.JString(s) => s }
                  }
                  .getOrElse(Nil)
            }
            assertEquals(alphaDependsOn, Some(List("Missing/Dep")))

          case Right(other) =>
            fail(s"expected json array output, found: $other")
          case Left(err) =>
            fail(show"failed to parse deps json output: $err")
        }

        val dotStr = readStringFile(state, Chain("out", "deps.dot"))
        assert(dotStr.contains("digraph G {"), dotStr)
        assert(dotStr.contains("Alpha/Main"), dotStr)
        assert(dotStr.contains("Zed/Main"), dotStr)
        assert(dotStr.contains("Missing/Dep"), dotStr)
    }
  }

  test(
    "tool assemble fails when previous public dependencies are not provided"
  ) {
    val depSrc =
      """export depValue,
depValue = 9
main = depValue
"""
    val appV1Src =
      """main = 1
"""
    val appV2Src =
      """main = 2
"""
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("app_v1", "App", "Main.bosatsu") -> appV1Src,
      Chain("app_v2", "App", "Main.bosatsu") -> appV2Src
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Foo.bosatsu",
          "--output",
          "out/Dep.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Foo.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "app_v1",
          "--input",
          "app_v1/App/Main.bosatsu",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--output",
          "out/App.Main.v1.bosatsu_package"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "app",
          "--version",
          "0.0.1",
          "--package",
          "out/App.Main.v1.bosatsu_package",
          "--output",
          "out/app_prev_base.bosatsu_lib"
        ),
        state3
      )
      (state4, _) = s4
      state5 <- withInjectedPublicDep(
        state4,
        Chain("out", "app_prev_base.bosatsu_lib"),
        Chain("out", "dep.bosatsu_lib"),
        Chain("out", "app_prev.bosatsu_lib")
      )
      s5 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "app_v2",
          "--input",
          "app_v2/App/Main.bosatsu",
          "--output",
          "out/App.Main.v2.bosatsu_package"
        ),
        state5
      )
      (state6, _) = s5
      s6 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "app",
          "--version",
          "0.0.2",
          "--package",
          "out/App.Main.v2.bosatsu_package",
          "--previous_lib",
          "out/app_prev.bosatsu_lib",
          "--output",
          "out/app_new.bosatsu_lib"
        ),
        state6
      )
    } yield s6

    result match {
      case Right((_, out)) =>
        fail(
          s"expected missing previous public dependency failure, got: $out"
        )
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains("missing previous public dependency"),
          s"unexpected error: $msg"
        )
    }
  }

  test(
    "tool deps includes interface/package info from includes and dependency libraries"
  ) {
    val depSrc =
      """export DepBox(), depBox,

struct DepBox(v: Int)

depBox = DepBox(1)
"""
    val appSrc =
      """from Dep/Foo import depBox

export main,

main = depBox
"""
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("app", "App", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Foo.bosatsu",
          "--output",
          "out/Dep.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Foo.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "app",
          "--input",
          "app/App/Main.bosatsu",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--output",
          "out/App.Main.bosatsu_package",
          "--interface_out",
          "out/App.Main.bosatsig"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "deps",
          "--include",
          "out/App.Main.bosatsu_package",
          "--interface",
          "out/App.Main.bosatsig",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--graph_format",
          "json",
          "--output",
          "out/deps_mixed.json"
        ),
        state3
      )
      (state4, out4) = s4
      _ = out4 match {
        case Output.DepsOutput(_, Some(path), GraphOutput.Json) =>
          assertEquals(path, Chain("out", "deps_mixed.json"))
        case other =>
          fail(s"unexpected deps output: $other")
      }
    } yield state4

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        val jsonStr = readStringFile(state, Chain("out", "deps_mixed.json"))
        Json.parserFile.parseAll(jsonStr) match {
          case Right(Json.JArray(items)) =>
            val rows = items.toList.collect { case Json.JObject(fields) =>
              fields
            }
            val kinds = rows.flatMap(_.collectFirst {
              case ("kind", Json.JString(k)) => k
            })
            assert(kinds.contains("interface"), jsonStr)
            assert(kinds.contains("package"), jsonStr)

            val packages =
              rows.flatMap(_.collectFirst { case ("package", Json.JString(p)) =>
                p
              })
            assert(packages.contains("Dep/Foo"), jsonStr)
            assert(packages.contains("App/Main"), jsonStr)

            val appPackDeps: Option[List[String]] = rows.collectFirst {
              case fields
                  if fields.contains(("package", Json.JString("App/Main"))) &&
                    fields.contains(("kind", Json.JString("package"))) =>
                fields
                  .collectFirst { case ("dependsOn", Json.JArray(values)) =>
                    values.toList.collect { case Json.JString(s) => s }
                  }
                  .getOrElse(Nil)
            }
            assertEquals(appPackDeps, Some(List("Dep/Foo")))
          case Right(other) =>
            fail(show"expected json array output, found: $other")
          case Left(err) =>
            fail(show"failed to parse deps json output: $err")
        }
    }
  }

  test("tool json apply reads --json_input and json errors are CliExceptions") {
    val applySrc =
      """main = (x) -> x.add(1)
"""
    val applyFiles = List(
      Chain("src", "Json", "Foo.bosatsu") -> applySrc,
      Chain("in.json") -> "[123]"
    )

    val applyFromPath = module.runWith(applyFiles)(
      List(
        "tool",
        "json",
        "apply",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_input",
        "in.json"
      )
    )
    applyFromPath match {
      case Right(Output.JsonOutput(Json.JNumberStr("124"), _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    val wrongAritySrc =
      """main = (x, y) -> x.add(y)
"""
    val wrongArity = module.runWith(
      List(Chain("src", "Json", "Foo.bosatsu") -> wrongAritySrc)
    )(
      List(
        "tool",
        "json",
        "apply",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_string",
        "[1]"
      )
    )
    wrongArity match {
      case Right(out) =>
        fail(s"expected apply error, got output: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("required a json array of size 2"), msg)
        assert(
          module.mainExceptionToString(err).nonEmpty,
          show"expected CliException: $msg"
        )
    }
  }

  test("tool json traverse rejects non-array inputs") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = List(Chain("src", "Json", "Foo.bosatsu") -> src)

    module.runWith(files)(
      List(
        "tool",
        "json",
        "traverse",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_string",
        "{}"
      )
    ) match {
      case Right(out) =>
        fail(s"expected traverse error, got output: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("require an array or arrays for traverse"), msg)
        assert(
          module.mainExceptionToString(err).nonEmpty,
          show"expected CliException: $msg"
        )
    }
  }

  test("tool json write/apply/traverse support --yaml output") {
    val writeSrc =
      """main = [1, 2]
"""
    val fnSrc =
      """main = (x) -> x.add(1)
"""
    val writeFiles = List(Chain("src", "Json", "Foo.bosatsu") -> writeSrc)
    val fnFiles = List(Chain("src", "Json", "Foo.bosatsu") -> fnSrc)

    module.runWith(writeFiles)(
      List(
        "tool",
        "json",
        "write",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--yaml"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        assertEquals(doc.render(120), "- 1\n- 2")
      case Right(other) =>
        fail(s"expected yaml write output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(fnFiles)(
      List(
        "tool",
        "json",
        "apply",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--yaml",
        "--json_string",
        "[41]"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        assertEquals(doc.render(120), "42")
      case Right(other) =>
        fail(s"expected yaml apply output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(fnFiles)(
      List(
        "tool",
        "json",
        "traverse",
        "--package_root",
        "src",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--yaml",
        "--json_string",
        "[[1], [4]]"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        assertEquals(doc.render(120), "- 2\n- 5")
      case Right(other) =>
        fail(s"expected yaml traverse output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool check without inputs reports a cli error") {
    module.runWith(Nil)(List("tool", "check")) match {
      case Right(out) =>
        fail(s"expected no-input check failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("no inputs given to check"), msg)
        assert(
          module.mainExceptionToString(err).nonEmpty,
          show"expected CliException: $msg"
        )
    }
  }

  test("tool check accepts todo but tool show rejects it") {
    val src =
      """package Todo/Foo
|
|main = todo(1)
|""".stripMargin
    val files = List(
      Chain("src", "Todo", "Foo.bosatsu") -> src
    )

    module.runWith(files)(
      List(
        "tool",
        "check",
        "--package_root",
        "src",
        "--input",
        "src/Todo/Foo.bosatsu"
      )
    ) match {
      case Right(Output.CompileOut(_, _, _)) => ()
      case Right(other)                      => fail(s"unexpected output: $other")
      case Left(err)                         => fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "tool",
        "show",
        "--package_root",
        "src",
        "--input",
        "src/Todo/Foo.bosatsu",
        "--value",
        "Todo/Foo::main"
      )
    ) match {
      case Right(out) =>
        fail(s"expected emit-mode rejection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("todo"), msg)
        assert(msg.contains("only available in type-check mode"), msg)
    }
  }

  test("tool check --cache_dir writes cache artifacts and reuses them") {
    val depSrc =
      """package Cache/Dep
        |export dep
        |dep = 1
        |""".stripMargin
    val appSrc =
      """package Cache/App
        |from Cache/Dep import dep
        |main = dep.add(1)
        |""".stripMargin
    val files = List(
      Chain("src", "Cache", "Dep.bosatsu") -> depSrc,
      Chain("src", "Cache", "App.bosatsu") -> appSrc
    )
    val cmd = List(
      "tool",
      "check",
      "--package_root",
      "src",
      "--input",
      "src/Cache/Dep.bosatsu",
      "--input",
      "src/Cache/App.bosatsu",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, state1)
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        val keyPrefix = Chain("cache", "keys", "blake3")
        val casPrefix = Chain("cache", "cas", "blake3")
        val keyFiles1 = filePathsUnder(state1, keyPrefix)
        val casFiles1 = filePathsUnder(state1, casPrefix)

        assert(keyFiles1.nonEmpty, "expected key cache files")
        assert(casFiles1.nonEmpty, "expected cas cache files")
        assertEquals(filePathsUnder(state2, keyPrefix), keyFiles1)
        assertEquals(filePathsUnder(state2, casPrefix), casFiles1)
        (out1, out2) match {
          case (
                Output.CompileOut(packs1, _, _),
                Output.CompileOut(packs2, _, _)
              ) =>
            assertEquals(packs2.map(_.name), packs1.map(_.name))
          case other =>
            fail(s"unexpected outputs: $other")
        }
    }
  }

  test("tool check --cache_dir keeps cache keys stable on comment-only edits") {
    val depSrc =
      """package Cache/Dep
        |export dep
        |dep = 1
        |""".stripMargin
    val appSrc =
      """package Cache/App
        |from Cache/Dep import dep
        |main = dep.add(1)
        |""".stripMargin
    val appSrcWithComments =
      """package Cache/App
        |
        |# comment should not change the source hash
        |from Cache/Dep import dep
        |
        |main = dep.add(1)
        |""".stripMargin
    val files = List(
      Chain("src", "Cache", "Dep.bosatsu") -> depSrc,
      Chain("src", "Cache", "App.bosatsu") -> appSrc
    )
    val appPath = Chain("src", "Cache", "App.bosatsu")
    val cmd = List(
      "tool",
      "check",
      "--package_root",
      "src",
      "--input",
      "src/Cache/Dep.bosatsu",
      "--input",
      "src/Cache/App.bosatsu",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(cmd, s0)
      (state1, _) = s1
      stateWithComments <- state1
        .withFile(appPath, MemoryMain.FileContent.Str(appSrcWithComments))
        .toRight(new Exception("failed to update source file"))
      s2 <- runWithState(cmd, stateWithComments)
      (state2, _) = s2
    } yield (state1, state2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2)) =>
        val keyPrefix = Chain("cache", "keys", "blake3")
        val casPrefix = Chain("cache", "cas", "blake3")
        assertEquals(filePathsUnder(state2, keyPrefix), filePathsUnder(state1, keyPrefix))
        assertEquals(filePathsUnder(state2, casPrefix), filePathsUnder(state1, casPrefix))
    }
  }

  test("dependency interface changes invalidate dependent cache entries") {
    val depV1 =
      """package Cache/Dep
        |export dep
        |dep = 1
        |""".stripMargin
    val depV2 =
      """package Cache/Dep
        |export dep, flag
        |dep = 1
        |flag = True
        |""".stripMargin
    val appSrc =
      """package Cache/App
        |from Cache/Dep import dep
        |main = dep.add(1)
        |""".stripMargin
    val depPath = Chain("src", "Cache", "Dep.bosatsu")
    val files = List(
      depPath -> depV1,
      Chain("src", "Cache", "App.bosatsu") -> appSrc
    )
    val cmd = List(
      "tool",
      "check",
      "--package_root",
      "src",
      "--input",
      "src/Cache/Dep.bosatsu",
      "--input",
      "src/Cache/App.bosatsu",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(cmd, s0)
      (state1, _) = s1
      stateWithDepChange <- state1
        .withFile(depPath, MemoryMain.FileContent.Str(depV2))
        .toRight(new Exception("failed to update dependency source"))
      s2 <- runWithState(cmd, stateWithDepChange)
      (state2, _) = s2
    } yield (state1, state2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2)) =>
        val keyPrefix = Chain("cache", "keys", "blake3")
        val casPrefix = Chain("cache", "cas", "blake3")
        val keyCount1 = filePathsUnder(state1, keyPrefix).size
        val keyCount2 = filePathsUnder(state2, keyPrefix).size
        val casCount1 = filePathsUnder(state1, casPrefix).size
        val casCount2 = filePathsUnder(state2, casPrefix).size

        assert(
          keyCount2 >= keyCount1 + 2,
          s"expected at least two new key entries, before=$keyCount1 after=$keyCount2"
        )
        assert(
          casCount2 >= casCount1 + 1,
          s"expected at least one new cas entry, before=$casCount1 after=$casCount2"
        )
    }
  }

  test("tool check without --cache_dir does not write cache artifacts") {
    val src =
      """package Cache/Foo
        |main = 1
        |""".stripMargin
    val files = List(
      Chain("src", "Cache", "Foo.bosatsu") -> src
    )
    val cmd = List(
      "tool",
      "check",
      "--package_root",
      "src",
      "--input",
      "src/Cache/Foo.bosatsu"
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(cmd, s0)
      (state1, _) = s1
    } yield state1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        assertEquals(filePathsUnder(state, Chain("cache")), Set.empty)
    }
  }

  test("lib deps list text output includes public and private sections") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "zed",
          "--version",
          "0.0.1",
          "--hash",
          validHash1,
          "--uri",
          "https://example.com/zed.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "alpha",
          "--version",
          "0.0.2",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/alpha.bosatsu_lib",
          "--private",
          "--no-fetch"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List("lib", "deps", "list", "--repo_root", "repo"),
        state2
      )
      (_, out3) = s3
    } yield out3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(Output.Basic(doc, None)) =>
        val rendered = doc.render(120)
        assert(rendered.contains("public deps:"), rendered)
        assert(rendered.contains("private deps:"), rendered)
        assert(rendered.contains("zed"), rendered)
        assert(rendered.contains("alpha"), rendered)
      case Right(other) =>
        fail(s"unexpected output: $other")
    }
  }

  test("lib json apply and traverse use CliException paths") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = baseLibFiles(src) :+ (Chain("repo", "in.json") -> "[41]")

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_input",
        "repo/in.json"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("42"), _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    val applyWrongArity = module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[1,2]"
      )
    )
    applyWrongArity match {
      case Right(out) =>
        fail(s"expected apply arity error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("required a json array of size 1"), msg)
    }

    val applyInvalidData = module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[\"x\"]"
      )
    )
    applyInvalidData match {
      case Right(out) =>
        fail(s"expected apply json conversion error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid input json"), msg)
    }

    val applyParseError = module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "}123456789012345678901234567890"
      )
    )
    applyParseError match {
      case Right(out) =>
        fail(s"expected apply parse error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("could not parse a JSON record at 1"), msg)
    }

    val traverseInvalidData = module.runWith(files)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[\"x\"]]"
      )
    )
    traverseInvalidData match {
      case Right(out) =>
        fail(s"expected traverse invalid-data error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid input json"), msg)
    }

    val traverseWrongArity = module.runWith(files)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[1,2]]"
      )
    )
    traverseWrongArity match {
      case Right(out) =>
        fail(s"expected traverse arity error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("required a json array of size 1"), msg)
    }

    val traverseNonArray = module.runWith(files)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "{}"
      )
    )
    traverseNonArray match {
      case Right(out) =>
        fail(s"expected traverse non-array error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("require an array for traverse"), msg)
    }
  }

  test("lib json write/apply/traverse support --yaml output") {
    val writeSrc =
      """main = [1, 2]
"""
    val fnSrc =
      """main = (x) -> x.add(1)
"""
    val writeFiles = baseLibFiles(writeSrc)
    val fnFiles = baseLibFiles(fnSrc)

    module.runWith(writeFiles)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--yaml"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        assertEquals(doc.render(120), "- 1\n- 2")
      case Right(other) =>
        fail(s"expected yaml write output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(fnFiles)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--yaml",
        "--json_string",
        "[41]"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        assertEquals(doc.render(120), "42")
      case Right(other) =>
        fail(s"expected yaml apply output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(fnFiles)(
      List(
        "lib",
        "json",
        "traverse",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--yaml",
        "--json_string",
        "[[1], [4]]"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        assertEquals(doc.render(120), "- 2\n- 5")
      case Right(other) =>
        fail(s"expected yaml traverse output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib json write rejects unsupported function output") {
    val src =
      """main = (x: Int) -> x
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::main"
      )
    ) match {
      case Right(out) =>
        fail(s"expected unsupported-type error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("cannot convert type to Json"), msg)
        assert(msg.contains("function types are not serializable to Json"), msg)
    }
  }

  test("lib json write supports Char, Array, and Bytes external values") {
    val charSrc =
      """main = match int_to_Char(127):
  case Some(c): c
  case None: .'?'
"""
    val charFiles = baseLibFiles(charSrc)

    module.runWith(charFiles)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JString(v), _)) =>
        assertEquals(v, "\u007f")
      case Right(other) =>
        fail(s"expected char json output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    val arraySrc =
      """from Bosatsu/Collection/Array import from_List_Array

main = from_List_Array([1, 2, 3])
"""
    val arrayPkgSrc =
      """package Bosatsu/Collection/Array

export Array, from_List_Array, size_Array

external struct Array[a: +*]
external def from_List_Array[a](xs: List[a]) -> Array[a]
external def size_Array[a](ary: Array[a]) -> Int
"""
    val arrayFiles = baseLibFiles(arraySrc) :+ (
      Chain("repo", "src", "Bosatsu", "Collection", "Array.bosatsu") -> arrayPkgSrc
    )

    module.runWith(arrayFiles)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JArray(
            Vector(
              Json.JNumberStr("1"),
              Json.JNumberStr("2"),
              Json.JNumberStr("3")
            )
          )
        )
      case Right(other) =>
        fail(s"expected array json output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    val bytesSrc =
      """from Bosatsu/IO/Bytes import from_List_Int
|
|main = from_List_Int([-1, 0, 1, 255, 256])
|""".stripMargin
    val bytesPkgSrc =
      """package Bosatsu/IO/Bytes
|
|export Bytes, from_List_Int, size_Bytes
|
|external struct Bytes
|external def from_List_Int(ints: List[Int]) -> Bytes
|external def size_Bytes(bytes: Bytes) -> Int
|""".stripMargin
    val bytesFiles = baseLibFiles(bytesSrc) :+ (
      Chain("repo", "src", "Bosatsu", "IO", "Bytes.bosatsu") -> bytesPkgSrc
    )

    module.runWith(bytesFiles)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JArray(
            Vector(
              Json.JNumberStr("255"),
              Json.JNumberStr("0"),
              Json.JNumberStr("1"),
              Json.JNumberStr("255"),
              Json.JNumberStr("0")
            )
          )
        )
      case Right(other) =>
        fail(s"expected bytes json output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib json write/apply supports Bosatsu/Json::Json directly") {
    val src =
      """from Bosatsu/Json import (
|  Json,
|  JNull,
|  JBool,
|  JInt,
|  JFloat,
|  JArray,
|  JObject,
|)
|
|export main, id_json, nan_json
|
|id_json = (j: Json) -> j
|nan_json = JFloat(.NaN)
|
|main = JObject([
|  ("a", JInt(1)),
|  ("b", JArray([JBool(True), JNull])),
|])
|""".stripMargin
    val files = withBosatsuJsonModule(baseLibFiles(src))

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JObject(
            List(
              "a" -> Json.JNumberStr("1"),
              "b" -> Json.JArray(Vector(Json.JBool(true), Json.JNull))
            )
          )
        )
      case Right(other) =>
        fail(s"expected Json AST passthrough output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::id_json",
        "--json_string",
        "[{\"k\":[1,false]}]"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JObject(
            List(
              "k" -> Json.JArray(Vector(Json.JNumberStr("1"), Json.JBool(false)))
            )
          )
        )
      case Right(other) =>
        fail(s"expected Json AST identity output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::nan_json"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JString(value), _)) =>
        assertEquals(value, "NaN")
      case Right(other) =>
        fail(s"expected non-finite float string encoding, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib json apply accepts Float64 encoded as strings") {
    val src =
      """main = (x: Float64) -> x
|""".stripMargin
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[\"NaN\"]"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JString(v), _)) =>
        assertEquals(v, "NaN")
      case Right(other) =>
        fail(s"expected Float64 NaN string output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[\"Infinity\"]"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JString(v), _)) =>
        assertEquals(v, "Infinity")
      case Right(other) =>
        fail(s"expected Float64 Infinity string output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib json Optional fields omit absent keys and decode missing keys") {
    val src =
      """from Bosatsu/Json import Optional, Missing, Set
|
|export Payload(), absent_payload, present_payload, echo
|
|struct Payload(name: String, note: Optional[String])
|
|absent_payload = Payload("a", Missing)
|present_payload = Payload("a", Set("x"))
|echo = (p: Payload) -> p
|""".stripMargin
    val files = withBosatsuJsonModule(baseLibFiles(src))

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::absent_payload"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(json, Json.JObject(List("name" -> Json.JString("a"))))
      case Right(other) =>
        fail(s"expected Optional absent field omission, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::present_payload"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JObject(
            List("name" -> Json.JString("a"), "note" -> Json.JString("x"))
          )
        )
      case Right(other) =>
        fail(s"expected Optional present field encoding, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::echo",
        "--json_string",
        "[{\"name\":\"a\"}]"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(json, Json.JObject(List("name" -> Json.JString("a"))))
      case Right(other) =>
        fail(s"expected Optional missing key decode, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib json Nullable flattens nested nullability") {
    val src =
      """from Bosatsu/Json import Nullable, Null, NonNull
|
|export flat_null, echo
|
|flat_null: Nullable[Nullable[Int]] = NonNull(Null)
|echo = (n: Nullable[Nullable[Int]]) -> n
|""".stripMargin
    val files = withBosatsuJsonModule(baseLibFiles(src))

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::flat_null"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JNull, _)) => ()
      case Right(other) =>
        fail(s"expected Nullable flatten-to-null output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::echo",
        "--json_string",
        "[null]"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JNull, _)) => ()
      case Right(other) =>
        fail(s"expected Nullable null decode, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    module.runWith(files)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::echo",
        "--json_string",
        "[1]"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("1"), _)) => ()
      case Right(other) =>
        fail(s"expected Nullable non-null decode, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib json apply supports Char, Array, and Bytes arguments") {
    val charFnSrc =
      """main = (c: Char) -> (c, c)
"""
    val charFnFiles = baseLibFiles(charFnSrc)

    module.runWith(charFnFiles)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[\"👋\"]"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JArray(Vector(Json.JString("👋"), Json.JString("👋")))
        )
      case Right(other) =>
        fail(s"expected char apply output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    val charInvalid = module.runWith(charFnFiles)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[\"ab\"]"
      )
    )
    charInvalid match {
      case Right(out) =>
        fail(s"expected invalid char input error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid input json"), msg)
    }

    val arrayFnSrc =
      """from Bosatsu/Collection/Array import Array, size_Array

main = (xs: Array[Int]) -> size_Array(xs)
"""
    val arrayPkgSrc =
      """package Bosatsu/Collection/Array

export Array, from_List_Array, size_Array

external struct Array[a: +*]
external def from_List_Array[a](xs: List[a]) -> Array[a]
external def size_Array[a](ary: Array[a]) -> Int
"""
    val arrayFnFiles = baseLibFiles(arrayFnSrc) :+ (
      Chain("repo", "src", "Bosatsu", "Collection", "Array.bosatsu") -> arrayPkgSrc
    )

    module.runWith(arrayFnFiles)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[1,2,3]]"
      )
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("3"), _)) => ()
      case Right(other) =>
        fail(s"expected array apply output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    val bytesFnSrc =
      """from Bosatsu/IO/Bytes import Bytes, to_List_Int
|
|main = (bs: Bytes) -> to_List_Int(bs)
|""".stripMargin
    val bytesPkgSrc =
      """package Bosatsu/IO/Bytes
|
|export Bytes, from_List_Int, to_List_Int, size_Bytes
|
|external struct Bytes
|external def from_List_Int(ints: List[Int]) -> Bytes
|external def to_List_Int(bytes: Bytes) -> List[Int]
|external def size_Bytes(bytes: Bytes) -> Int
|""".stripMargin
    val bytesFnFiles = baseLibFiles(bytesFnSrc) :+ (
      Chain("repo", "src", "Bosatsu", "IO", "Bytes.bosatsu") -> bytesPkgSrc
    )

    module.runWith(bytesFnFiles)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[0,255,42]]"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(
          json,
          Json.JArray(
            Vector(
              Json.JNumberStr("0"),
              Json.JNumberStr("255"),
              Json.JNumberStr("42")
            )
          )
        )
      case Right(other) =>
        fail(s"expected bytes apply output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    val bytesInvalid = module.runWith(bytesFnFiles)(
      List(
        "lib",
        "json",
        "apply",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--json_string",
        "[[256]]"
      )
    )
    bytesInvalid match {
      case Right(out) =>
        fail(s"expected invalid bytes json input error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid input json"), msg)
    }
  }

  test("lib json write reports unknown package clearly") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "Nope/Foo"
      )
    ) match {
      case Right(out) =>
        fail(s"expected package lookup error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("package not found: Nope/Foo"), msg)
        assert(!msg.contains("unknown error"), msg)
    }
  }

  test("lib show reports unknown package clearly") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--package",
        "Nope/Foo"
      )
    ) match {
      case Right(out) =>
        fail(s"expected package lookup error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("package not found: Nope/Foo"), msg)
        assert(!msg.contains("unknown error"), msg)
        assert(!msg.contains("java.lang.Exception"), msg)
    }
  }

  test("lib json write reports missing value in a known package") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::does_not_exist"
      )
    ) match {
      case Right(out) =>
        fail(s"expected missing-value error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("value not found: MyLib/Foo::does_not_exist"), msg)
        assert(msg.contains("valid json values: [main]"), msg)
        assert(!msg.contains("unknown error"), msg)
    }
  }

  test("lib json write missing-value suggestions are filtered and sorted") {
    val src =
      """export alpha, zeta, fn, main

alpha = 1
zeta = "z"
fn = (x: Int) -> x
main = 0
"""
    val files = baseLibFiles(src)

    module.runWith(files)(
      List(
        "lib",
        "json",
        "write",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::nope"
      )
    ) match {
      case Right(out) =>
        fail(s"expected missing-value error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("value not found: MyLib/Foo::nope"), msg)
        assert(msg.contains("valid json values: [alpha, main, zeta]"), msg)
        assert(!msg.contains("fn"), msg)
    }
  }

  test(
    "lib check renders previous descriptor details when previous is missing"
  ) {
    val previousDesc = proto.LibDescriptor(
      version = Some(Version(0, 0, 0).toProto),
      hashes = validHash1 :: Nil,
      uris = "https://example.com/mylib-0.0.0.bosatsu_lib" :: Nil
    )
    val conf = LibConfig
      .init(Name("mylib"), "https://example.com", Version(0, 0, 1))
      .copy(previous = Some(previousDesc))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    module.runWith(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected missing-previous failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("previous not in cas"), msg)
    }
  }

  test("lib check accepts todo but lib show rejects it") {
    val files = baseLibFiles("main = todo(1)\n")

    module.runWith(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(Output.Basic(_, _)) => ()
      case Right(other)              => fail(s"unexpected output: $other")
      case Left(err)                 => fail(err.getMessage)
    }

    module.runWith(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(out) =>
        fail(s"expected emit-mode rejection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("todo"), msg)
        assert(msg.contains("only available in type-check mode"), msg)
    }
  }

  test("lib fetch reports total fetched objects by default") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(List("lib", "fetch", "--repo_root", "repo")) match {
      case Right(Output.Basic(doc, None)) =>
        val rendered = doc.render(120)
        assert(rendered.contains("fetched 0 objects."), rendered)
      case Right(other) =>
        fail(s"expected basic output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib fetch --quiet suppresses successful output") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(
      List("lib", "fetch", "--repo_root", "repo", "--quiet")
    ) match {
      case Right(Output.Basic(doc, None)) =>
        assertEquals(doc.render(120), "")
      case Right(other) =>
        fail(s"expected basic output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib fetch fails on dependency download issues and reports details") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_with_uri",
          "--version",
          "0.0.1",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/dep_with_uri.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(List("lib", "fetch", "--repo_root", "repo"), state1)
    } yield s2

    result match {
      case Right((_, out)) =>
        fail(s"expected lib fetch failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to fetch"), msg)
    }
  }

  test("lib fetch reports previous fetch failures") {
    val previousDesc = proto.LibDescriptor(
      version = Some(Version(0, 0, 0).toProto),
      hashes = validHash3 :: Nil,
      uris = Nil
    )
    val conf = LibConfig
      .init(Name("mylib"), "https://example.com", Version(0, 0, 1))
      .copy(previous = Some(previousDesc))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    module.runWith(files)(List("lib", "fetch", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected previous fetch failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to fetch previous"), msg)
    }
  }

  test("lib build without --main_pack reports missing main") {
    val files = baseLibFiles("main = 1\n")

    module.runWith(files)(
      List("lib", "build", "--repo_root", "repo", "--outdir", "out")
    ) match {
      case Right(out) =>
        fail(s"expected no-main build failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("no main defined"), msg)
    }
  }

  test(
    "lib build succeeds when importing package from private dependency in CAS"
  ) {
    val depSrc =
      """package Dep/Foo
|
|export dep,
|
|dep = 1
|""".stripMargin
    val progSrc =
      """package Bosatsu/Prog
|
|export Main()
|
|struct Main(x: Int)
|""".stripMargin
    val appSrc =
      """package MyLib/Main
|
|from Dep/Foo import dep
|from Bosatsu/Prog import Main
|
|main = Main(dep)
|""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("repo", "src", "MyLib", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--package_root",
          "dep",
          "--input",
          "dep/Dep/Foo.bosatsu",
          "--output",
          "out/Dep.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Foo.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      depLib = readLibraryFile(state2, Chain("out", "dep.bosatsu_lib"))
      s3 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep",
          "--version",
          "0.0.1",
          "--hash",
          depLib.hash.toIdent,
          "--uri",
          "https://example.com/dep.bosatsu_lib",
          "--private",
          "--no-fetch"
        ),
        state2
      )
      (state3, _) = s3
      state4 <- state3.withFile(
        casPathFor(Chain("repo"), depLib),
        MemoryMain.FileContent.Lib(depLib)
      ) match {
        case Some(next) => Right(next)
        case None       =>
          Left(
            new Exception("failed to inject dependency library into repo CAS")
          )
      }
      s4 <- runWithState(List("lib", "check", "--repo_root", "repo"), state4)
      (state5, _) = s4
      s5 <- runWithState(
        List(
          "lib",
          "build",
          "--repo_root",
          "repo",
          "--outdir",
          "out",
          "-m",
          "MyLib/Main"
        ),
        state5
      )
    } yield s5

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out)) =>
        out match {
          case Output.Basic(_, _) => ()
          case other              => fail(s"unexpected output: $other")
        }
    }
  }

  test(
    "lib test --filter scopes local typechecking to matching package roots"
  ) {
    val targetSrc =
      """test_one = Assertion(True, "ok")
"""
    val unrelatedBrokenSrc =
      """bad = does_not_exist
"""
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Euler", "One.bosatsu") -> targetSrc,
      Chain("repo", "src", "MyLib", "ReproMin8.bosatsu") -> unrelatedBrokenSrc,
      Chain("repo", "cc_conf.json") -> ccConfJson
    )

    module.runWith(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--filter",
        "MyLib/Euler/.*",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected failure in memory mode, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("system not supported in memory mode"), msg)
        assert(!msg.contains("ReproMin8"), msg)
    }
  }

  test(
    "lib check --filter scopes local typechecking to matching package roots"
  ) {
    val targetSrc =
      """one = 1
"""
    val unrelatedBrokenSrc =
      """bad = does_not_exist
"""

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Euler", "One.bosatsu") -> targetSrc,
      Chain("repo", "src", "MyLib", "ReproMin8.bosatsu") -> unrelatedBrokenSrc
    )

    module.runWith(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected unfiltered check failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("does_not_exist"), msg)
    }

    module.runWith(files)(
      List(
        "lib",
        "check",
        "--repo_root",
        "repo",
        "--filter",
        "MyLib/Euler/.*"
      )
    ) match {
      case Right(Output.Basic(_, _)) => ()
      case Right(other)              => fail(s"unexpected output: $other")
      case Left(err)                 =>
        fail(Option(err.getMessage).getOrElse(err.toString))
    }
  }

  test("Output.Many stops at first non-success exit code") {
    val first = Output.TestOutput(
      List(
        (
          PackageName.parts("Pkg"),
          Some(Eval.now(Test.Assertion(false, "boom")))
        )
      ),
      Colorize.None
    )
    val second =
      Output.Basic(
        Doc.text("should not be written"),
        Some(Chain("out", "later.txt"))
      )
    val out = Output.Many(Chain(first, second))

    val result = for {
      s0 <- MemoryMain.State.from[ErrorOr](Nil)
      s1 <- module.reportOutput(out).run(s0)
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        assertEquals(state.get(Chain("out", "later.txt")), None)
    }
  }
}
