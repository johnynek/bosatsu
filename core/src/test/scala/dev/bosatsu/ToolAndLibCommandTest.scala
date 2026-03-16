package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eval
import cats.data.Chain
import cats.implicits._
import dev.bosatsu.edn.Edn
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.library.{LibConfig, Libraries, Name, Version}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.tool.{CliException, ExitCode, GraphOutput, Output, ShowEdn}
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
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

  private def withSystemStdin[A](stdin: String)(fn: => A): A = {
    val previous: InputStream = System.in
    val next =
      new ByteArrayInputStream(stdin.getBytes(StandardCharsets.UTF_8))
    System.setIn(next)
    try fn
    finally {
      System.setIn(previous)
      next.close()
    }
  }

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

  private def runAndReportWithState(
      cmd: List[String],
      state: MemoryMain.State
  ): ErrorOr[(MemoryMain.State, ExitCode)] =
    module.run(cmd) match {
      case Left(help) =>
        Left(new Exception(s"got help: $help on command: $cmd"))
      case Right(io) =>
        module.report(io).run(state)
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

  private def readJsonFile[A: Json.Reader](
      state: MemoryMain.State,
      path: Chain[String]
  ): A = {
    val jsonStr = readStringFile(state, path)
    val json = Json.parserFile.parseAll(jsonStr) match {
      case Right(value) => value
      case Left(err)    =>
        fail(s"expected valid json at ${path.mkString_("/")}, found error: $err")
    }

    Json.Reader[A].read(Json.Path.Root, json) match {
      case Right(value)         => value
      case Left((msg, got, jp)) =>
        fail(show"failed to decode json at ${path.mkString_("/")}: $msg, json=$got, path=$jp")
    }
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

  private def isSourcePath(path: Chain[String]): Boolean =
    path.lastOption match {
      case Some(name) =>
        !name.contains(".") || name.endsWith(".bosatsu")
      case None       =>
        false
    }

  private def sourceParts(path: Chain[String]): List[String] =
    path.toList match {
      case Nil => Nil
      case init :+ last if last.endsWith(".bosatsu") =>
        init :+ last.stripSuffix(".bosatsu")
      case parts => parts
    }

  private def hasExplicitPackage(src: String): Boolean =
    src.linesIterator.exists(_.trim.startsWith("package "))

  private def normalizePackagePart(part: String): String =
    part.headOption match {
      case Some(head) if head.isLower => s"${head.toUpper}${part.substring(1)}"
      case _                          => part
    }

  private def inferredPackage(path: Chain[String]): Option[PackageName] = {
    val parts = sourceParts(path)
    if (parts.isEmpty) None
    else {
      def startsUpper(part: String): Boolean =
        part.headOption.exists(_.isUpper)

      val tails = parts.tails.collect { case h :: t => h :: t }.toList
      val chosen = tails.find(_.forall(startsUpper)).getOrElse(parts)
      val raw = chosen.mkString("/")

      PackageName.parse(raw).orElse {
        val normalized = chosen.map(normalizePackagePart).mkString("/")
        PackageName.parse(normalized)
      }
    }
  }

  private def withExplicitPackage(path: Chain[String], src: String): String =
    if (!isSourcePath(path) || hasExplicitPackage(src)) src
    else {
      inferredPackage(path) match {
        case Some(pkg) => s"package ${pkg.asString}\n\n$src"
        case None      => src
      }
    }

  private def normalizeFiles(
      files: List[(Chain[String], String)]
  ): List[(Chain[String], String)] =
    files.map { case (path, src) =>
      (path, withExplicitPackage(path, src))
    }

  private def runWithFiles(
      files: List[(Chain[String], String)]
  )(cmd: List[String]): ErrorOr[Output[Chain[String]]] =
    module.runWith(normalizeFiles(files))(cmd)

  private def stateFromFiles(
      files: List[(Chain[String], String)]
  ): ErrorOr[MemoryMain.State] =
    MemoryMain.State.from[ErrorOr](normalizeFiles(files))

  private def casPathFor(
      repoRoot: Chain[String],
      lib: Hashed[Algo.Blake3, proto.Library]
  ): Chain[String] = {
    val hex = lib.hash.hex
    repoRoot ++ Chain(".bosatsuc", "cas", "blake3", hex.take(2), hex.drop(2))
  }

  private def stateWithConfiguredCachedDep(
      appSrc: String,
      depNameInConfig: String,
      depVersionInConfig: String
  ): ErrorOr[(MemoryMain.State, Hashed[Algo.Blake3, proto.Library])] = {
    val depSrc =
      """export dep,
|
|dep = 1
|""".stripMargin
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("dep", "Dep", "Foo.bosatsu") -> depSrc,
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Foo.bosatsu") -> appSrc
    )

    for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
          depNameInConfig,
          "--version",
          depVersionInConfig,
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
    } yield (state4, depLib)
  }

  private def stateWithTwoVersionMismatchedCachedDeps(
      appSrc: String
  ): ErrorOr[MemoryMain.State] = {
    val depASrc =
      """export depA,
|
|depA = 1
|""".stripMargin
    val depBSrc =
      """export depB,
|
|depB = 2
|""".stripMargin
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("dep_a", "DepA", "Foo.bosatsu") -> depASrc,
      Chain("dep_b", "DepB", "Foo.bosatsu") -> depBSrc,
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Foo.bosatsu") -> appSrc
    )

    for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--input",
          "dep_a/DepA/Foo.bosatsu",
          "--output",
          "out/DepA.Foo.bosatsu_package"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep_a",
          "--version",
          "0.0.1",
          "--package",
          "out/DepA.Foo.bosatsu_package",
          "--output",
          "out/dep_a.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "check",
          "--input",
          "dep_b/DepB/Foo.bosatsu",
          "--output",
          "out/DepB.Foo.bosatsu_package"
        ),
        state2
      )
      (state3, _) = s3
      s4 <- runWithState(
        List(
          "tool",
          "assemble",
          "--name",
          "dep_b",
          "--version",
          "0.0.2",
          "--package",
          "out/DepB.Foo.bosatsu_package",
          "--output",
          "out/dep_b.bosatsu_lib"
        ),
        state3
      )
      (state4, _) = s4
      depALib = readLibraryFile(state4, Chain("out", "dep_a.bosatsu_lib"))
      depBLib = readLibraryFile(state4, Chain("out", "dep_b.bosatsu_lib"))
      s5 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_a",
          "--version",
          "5.0.0",
          "--hash",
          depALib.hash.toIdent,
          "--uri",
          "https://example.com/dep_a.bosatsu_lib",
          "--private",
          "--no-fetch"
        ),
        state4
      )
      (state5, _) = s5
      s6 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_b",
          "--version",
          "6.0.0",
          "--hash",
          depBLib.hash.toIdent,
          "--uri",
          "https://example.com/dep_b.bosatsu_lib",
          "--private",
          "--no-fetch"
        ),
        state5
      )
      (state6, _) = s6
      state7 <- state6.withFile(
        casPathFor(Chain("repo"), depALib),
        MemoryMain.FileContent.Lib(depALib)
      ) match {
        case Some(next) => Right(next)
        case None       =>
          Left(
            new Exception(
              "failed to inject dep_a library into repo CAS"
            )
          )
      }
      state8 <- state7.withFile(
        casPathFor(Chain("repo"), depBLib),
        MemoryMain.FileContent.Lib(depBLib)
      ) match {
        case Some(next) => Right(next)
        case None       =>
          Left(
            new Exception(
              "failed to inject dep_b library into repo CAS"
            )
          )
      }
    } yield state8
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

  private val minimalProgModuleSrc: String =
    """package Bosatsu/Prog
|
|export (unit, pure, raise_error, recover, ignore_err, await, recursive, map, map_err, observe, Prog, Main(), ProgTest())
|
|external struct Prog[err: +*, res: +*]
|
|external def pure[err, res](a: res) -> Prog[err, res]
|external def raise_error[err, res](e: err) -> Prog[err, res]
|external def flat_map(prog: Prog[err, res], fn: res -> Prog[err, res1]) -> Prog[err, res1]
|
|def map(prog: Prog[err, res], fn: res -> res1) -> Prog[err, res1]:
|  prog.flat_map(res -> pure(fn(res)))
|
|external def recover(prog: Prog[err, res], fn: err -> Prog[err1, res]) -> Prog[err1, res]
|
|def map_err(prog: Prog[err, res], fn: err -> err1) -> Prog[err1, res]:
|  prog.recover(res -> raise_error(fn(res)))
|
|def ignore_err[err, res](prog: Prog[err, res], default: res) -> forall e. Prog[e, res]:
|  prog.recover(_ -> pure(default))
|
|external def apply_fix(a: a,
|  fn: (a -> Prog[err, b]) -> (a -> Prog[err, b])) -> Prog[err, b]
|external def observe[a](a: a) -> forall err. Prog[err, Unit]
|
|def await(p, fn): p.flat_map(fn)
|
|def recursive(fn: (a -> Prog[err, b]) -> (a -> Prog[err, b])) -> (a -> Prog[err, b]):
|  a -> apply_fix(a, fn)
|
|unit: forall err. Prog[err, ()] = pure(())
|
|struct Main(run: List[String] -> forall err. Prog[err, Int])
|struct ProgTest(test_fn: List[String] -> forall err. Prog[err, Test])
|""".stripMargin

  private val minimalIoErrorModuleSrc: String =
    """package Bosatsu/IO/Error
|
|export IOError()
|
|enum IOError:
|  InvalidArgument(context: String)
|""".stripMargin

  private def libFilesWithMinimalProgAndIoCore(
      appPath: Chain[String],
      appSrc: String,
      ioCoreSrc: String
  ): List[(Chain[String], String)] = {
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))

    List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      appPath -> appSrc,
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("repo", "src", "Bosatsu", "IO", "Error.bosatsu") -> minimalIoErrorModuleSrc,
      Chain("repo", "src", "Bosatsu", "IO", "Core.bosatsu") -> ioCoreSrc
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
    def jsonNameAtom(value: Json): Option[String] =
      value match {
        case Json.JString(name) => Some(name)
        case Json.JObject(("$sym", Json.JString(name)) :: Nil) =>
          Some(name)
        case _ => None
      }

    def jsonListLike(value: Json): Option[Vector[Json]] =
      value match {
        case Json.JArray(items) => Some(items)
        case Json.JObject(("$vec", Json.JArray(items)) :: Nil) =>
          Some(items)
        case _ => None
      }

    json match {
      case Json.JObject(fields) =>
        val byKey = fields.toMap
        assertEquals(byKey.get("$form"), Some(Json.JString("show")))
        byKey.get("packages") match {
          case Some(packsJson) =>
            val packs = jsonListLike(packsJson).getOrElse {
              fail(s"expected show packages array, found: ${Some(packsJson)}")
            }
            packs.toList.map {
              case Json.JObject(packFields) =>
                val packMap = packFields.toMap
                assertEquals(packMap.get("$form"), Some(Json.JString("package")))
                packMap.get("name") match {
                  case Some(nameJson) =>
                    jsonNameAtom(nameJson).getOrElse {
                      fail(s"missing package name in ${Some(nameJson)}")
                    }
                  case other                    => fail(s"missing package name in $other")
                }
              case other =>
                fail(s"expected package object, found: $other")
            }
          case None =>
            fail("expected show packages array, found: None")
        }
      case other =>
        fail(s"expected show json object, found: $other")
    }

  private def showJsonPackageFieldKeys(json: Json): List[Set[String]] =
    def jsonListLike(value: Json): Option[Vector[Json]] =
      value match {
        case Json.JArray(items) => Some(items)
        case Json.JObject(("$vec", Json.JArray(items)) :: Nil) =>
          Some(items)
        case _ => None
      }

    json match {
      case Json.JObject(fields) =>
        val byKey = fields.toMap
        assertEquals(byKey.get("$form"), Some(Json.JString("show")))
        byKey.get("packages") match {
          case Some(packsJson) =>
            val packs = jsonListLike(packsJson).getOrElse {
              fail(s"expected show packages array, found: ${Some(packsJson)}")
            }
            packs.toList.map {
              case Json.JObject(packFields) =>
                val packMap = packFields.toMap
                assertEquals(packMap.get("$form"), Some(Json.JString("package")))
                packMap.keySet
              case other =>
                fail(s"expected package object, found: $other")
            }
          case None =>
            fail("expected show packages array, found: None")
        }
      case other =>
        fail(s"expected show json object, found: $other")
    }

  private def libShowJsonSize(
      files: List[(Chain[String], String)],
      value: String,
      noOpt: Boolean
  ): Int = {
    val cmd =
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--value",
        value,
        "--json"
      ) ::: (if (noOpt) List("--no-opt") else Nil)

    runWithFiles(files)(cmd) match {
      case Right(Output.JsonOutput(json, _)) =>
        json.render.length
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
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

  private def withExportedPredefInterface(
      lib: Hashed[Algo.Blake3, proto.Library]
  ): ErrorOr[Hashed[Algo.Blake3, proto.Library]] =
    ProtoConverter
      .interfaceToProto(Package.interfaceOf(PackageMap.predefCompiled))
      .toEither
      .leftMap(err =>
        new Exception(
          s"failed to encode Bosatsu/Predef interface: ${Option(err.getMessage).getOrElse(err.toString)}"
        )
      )
      .map { predefIface =>
        val exportedIfaces = lib.arg.exportedIfaces.toList
        val updatedIfaces =
          if (
            exportedIfaces.exists(iface =>
              ProtoConverter.iname(iface) == PackageName.PredefName.asString
            )
          ) {
            exportedIfaces
          } else {
            predefIface :: exportedIfaces
          }
        val rewritten = lib.arg.copy(exportedIfaces = updatedIfaces)
        Hashed(Algo.hashBytes(rewritten.toByteArray), rewritten)
      }

  private def addConfiguredCachedDep(
      state: MemoryMain.State,
      depLib: Hashed[Algo.Blake3, proto.Library],
      depName: String,
      depVersion: String,
      isPublic: Boolean
  ): ErrorOr[MemoryMain.State] = {
    val visibilityFlag = if (isPublic) "--public" else "--private"
    for {
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          depName,
          "--version",
          depVersion,
          "--hash",
          depLib.hash.toIdent,
          "--uri",
          show"https://example.com/$depName.bosatsu_lib",
          visibilityFlag,
          "--no-fetch"
        ),
        state
      )
      (state1, _) = s1
      state2 <- state1.withFile(
        casPathFor(Chain("repo"), depLib),
        MemoryMain.FileContent.Lib(depLib)
      ) match {
        case Some(next) => Right(next)
        case None       =>
          Left(
            new Exception("failed to inject dependency library into repo CAS")
          )
      }
    } yield state2
  }

  private def stateWithLibDocDependency(
      appSrc: String,
      depSrc: String,
      depDocBaseUrl: String,
      includePredefInterface: Boolean
  ): ErrorOr[MemoryMain.State] = {
    val files =
      baseLibFiles(appSrc) :+ (Chain("dep", "Dep", "Util.bosatsu") -> depSrc)

    for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Util.bosatsu_package",
          "--doc_base_url",
          depDocBaseUrl,
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      depLib = readLibraryFile(state2, Chain("out", "dep.bosatsu_lib"))
      depLibWithPredef <-
        if (includePredefInterface) withExportedPredefInterface(depLib)
        else Right(depLib)
      state3 <-
        if (includePredefInterface)
          state2.withFile(
            Chain("out", "dep.bosatsu_lib"),
            MemoryMain.FileContent.Lib(depLibWithPredef)
          ) match {
            case Some(next) => Right(next)
            case None       =>
              Left(
                new Exception(
                  "failed to rewrite dependency library after adding Predef interface"
                )
              )
          }
        else Right(state2)
      state4 <- addConfiguredCachedDep(
        state3,
        depLibWithPredef,
        depName = "dep",
        depVersion = "0.0.1",
        isPublic = false
      )
    } yield state4
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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

    runWithFiles(files)(
      List("lib", "eval", "--repo_root", "repo", "--main", "MyLib/Foo")
    ) match {
      case Right(Output.EvaluationResult(_, _, _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    runWithFiles(files)(
      List("lib", "json", "write", "--repo_root", "repo", "--main", "MyLib/Foo")
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("42"), _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    runWithFiles(files)(
      List("lib", "show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    runWithFiles(files)(
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

  test("lib show --package-names only emits package names") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--package-names"
      )
    ) match {
      case Right(Output.Basic(doc, _)) =>
        val rendered = doc.render(120)
        assert(rendered.contains("(package :name MyLib/Foo)"), rendered)
        assert(!rendered.contains(":imports"), rendered)
        assert(!rendered.contains(":exports"), rendered)
        assert(!rendered.contains(":types"), rendered)
        assert(!rendered.contains(":defs"), rendered)
        assert(!rendered.contains(":externals"), rendered)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--package-names",
        "--json"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(showJsonPackageNames(json), List("MyLib/Foo"))
        assertEquals(showJsonPackageFieldKeys(json), List(Set("$form", "name")))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test(
    "lib eval --main scopes local typechecking to selected package roots and transitive deps"
  ) {
    val mainSrc =
      """from MyLib/Dep import dep
|
|main = add(dep, 2)
|""".stripMargin
    val depSrc =
      """export dep
|
|dep = 40
|""".stripMargin
    val unrelatedBrokenSrc =
      """bad = does_not_exist
|""".stripMargin

    val files = baseLibFiles(mainSrc) ++ List(
      Chain("repo", "src", "MyLib", "Dep.bosatsu") -> depSrc,
      Chain("repo", "src", "MyLib", "ReproMinEval.bosatsu") -> unrelatedBrokenSrc
    )

    runWithFiles(files)(
      List(
        "lib",
        "eval",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo::main"
      )
    ) match {
      case Right(Output.EvaluationResult(_, _, doc)) =>
        assertEquals(doc.value.render(80), "42")
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(Option(err.getMessage).getOrElse(err.toString))
    }
  }

  test(
    "lib show --package scopes local typechecking to selected package roots and transitive deps"
  ) {
    val mainSrc =
      """from MyLib/Dep import dep
|
|main = add(dep, 2)
|""".stripMargin
    val depSrc =
      """export dep
|
|dep = 40
|""".stripMargin
    val unrelatedBrokenSrc =
      """bad = does_not_exist
|""".stripMargin

    val files = baseLibFiles(mainSrc) ++ List(
      Chain("repo", "src", "MyLib", "Dep.bosatsu") -> depSrc,
      Chain("repo", "src", "MyLib", "ReproMinShow.bosatsu") -> unrelatedBrokenSrc
    )

    runWithFiles(files)(
      List(
        "lib",
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo"
      )
    ) match {
      case Right(Output.ShowOutput(packs, _, _)) =>
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
        val imports = importItems(packs.headOption.getOrElse(fail("expected one package")))
        assert(imports.exists(_._1 == "MyLib/Dep"), imports.toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(Option(err.getMessage).getOrElse(err.toString))
    }
  }

  test("lib show invalid package name parse error includes package hint") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

  test("lib show avoids exploding repeated non-lambda global calls") {
    val ranges = List(
      (0, 10),
      (20, 30),
      (40, 50),
      (60, 70),
      (80, 90),
      (100, 110),
      (120, 130),
      (140, 150)
    )
    val condExpr = ranges.reverse.foldLeft("True") { case (acc, (lo, hi)) =>
      s"andb(in_range_Int(i, $lo, $hi), $acc)"
    }

    val src =
      s"""def andb(left: Bool, right: Bool) -> Bool:
|  if left:
|    right
|  else:
|    False
|
|def lte_Int(left: Int, right: Int) -> Bool:
|  cmp_Int(left, right) matches LT | EQ
|
|def gte_Int(left: Int, right: Int) -> Bool:
|  cmp_Int(left, right) matches GT | EQ
|
|def in_range_Int(item: Int, low: Int, high: Int) -> Bool:
|  andb(gte_Int(item, low), lte_Int(item, high))
|
|def main(i: Int) -> Bool:
|  $condExpr
|""".stripMargin

    val files = baseLibFiles(src)
    val optSize = libShowJsonSize(files, "MyLib/Foo::main", noOpt = false)
    val noOptSize = libShowJsonSize(files, "MyLib/Foo::main", noOpt = true)

    assert(noOptSize > 0)
    assert(
      optSize <= ((noOptSize * 14) / 10),
      s"expected optimized main size to stay close to no-opt: opt=$optSize noOpt=$noOptSize"
    )
  }

  test("lib show --value includes local value dependencies and only needed imports") {
    val src =
      """from Bosatsu/Predef import add, mul
|
|def main(x):
|  add(x, 2)
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

  test("lib eval handles parse/sort repro without StackOverflowError") {
    val src =
      """package MyLib/Repro22EvalStack2
|
|def operator <=(a: String, b: String) -> Bool:
|  cmp_String(a, b) matches LT | EQ
|
|def parse_names(s: String) -> List[String]:
|  recur s:
|    case "":
|      []
|    case "\"${name}\",${rest}":
|      [name, *parse_names(rest)]
|    case "\"${name}\"":
|      [name]
|    case ",${rest}":
|      parse_names(rest)
|    case "$.{_}${rest}":
|      parse_names(rest)
|
|def insert_sorted(name: String, sorted: List[String]) -> List[String]:
|  recur sorted:
|    case []:
|      [name]
|    case [h, *t]:
|      if name <= h:
|        [name, h, *t]
|      else:
|        [h, *insert_sorted(name, t)]
|
|def sort_names(names: List[String]) -> List[String]:
|  names.foldl_List([], (acc, name) -> insert_sorted(name, acc))
|
|def list_len(xs: List[String], acc: Int) -> Int:
|  recur xs:
|    case []:
|      acc
|    case [_, *t]:
|      list_len(t, acc.add(1))
|
|def pad3(i: Int) -> String:
|  a = i.div(100)
|  b = i.mod_Int(100).div(10)
|  c = i.mod_Int(10)
|  "${int_to_String(a)}${int_to_String(b)}${int_to_String(c)}"
|
|def quoted(i: Int) -> String:
|  "\"${pad3(i)}\""
|
|def make_csv(n: Int) -> String:
|  int_loop(n, "", (i, acc) ->
|    part = quoted(i)
|    next =
|      if acc matches "":
|        part
|      else:
|        "${part},${acc}"
|    (i.sub(1), next))
|
|names_csv = make_csv(178)
|computed = list_len(sort_names(parse_names(names_csv)), 0)
|
|test = Assertion(computed.eq_Int(178), "repro22 eval stack 2")
|""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Repro22EvalStack2.bosatsu") -> src
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/Repro22EvalStack2::computed"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.EvaluationResult(_, _, _) => ()
          case other                            => fail(s"unexpected output: $other")
        }
    }
  }

  test("tool eval missing value reports CliException without stack trace") {
    val src =
      """main = 42
"""
    val files = List(Chain("src", "Tool", "Foo.bosatsu") -> src)

    runWithFiles(files)(
      List(
        "tool",
        "eval",
        "--main",
        "Tool/Foo::missing",
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

  test(
    "tool eval --run executes Bosatsu/Prog::Main and includes synthetic argv[0]"
  ) {
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
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "tool",
          "eval",
          "--run",
          "--main",
          "Tool/Foo",
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
        assertEquals(exitCode, ExitCode.fromInt(4))
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("tool eval --run forwards -- passthrough args") {
    val appSrc =
      """package Tool/Foo
|
|from Bosatsu/Prog import Main, pure
|
|main = Main(args -> match args:
|  case [_, "--compact"]:
|    pure(0)
|  case _:
|    pure(1)
|)
|""".stripMargin
    val files = List(
      Chain("src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("src", "Tool", "Foo.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "tool",
          "eval",
          "--run",
          "--main",
          "Tool/Foo",
          "--input",
          "src/Bosatsu/Prog.bosatsu",
          "--input",
          "src/Tool/Foo.bosatsu",
          "--",
          "--compact"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("tool eval without --run rejects -- passthrough args") {
    val src =
      """main = 42
"""
    val files = List(Chain("src", "Tool", "Foo.bosatsu") -> src)

    runWithFiles(files)(
      List(
        "tool",
        "eval",
        "--main",
        "Tool/Foo",
        "--input",
        "src/Tool/Foo.bosatsu",
        "--",
        "--compact"
      )
    ) match {
      case Left(err) =>
        val msg = module.mainExceptionToString(err).getOrElse(
          fail(s"expected CliException, found: $err")
        )
        assert(msg.contains("trailing args require --run"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("tool eval --run supports Bosatsu/Prog observe in Main") {
    val appSrc =
      """package Tool/ObserveMain
|
|from Bosatsu/Prog import Main, await, observe, pure
|
|main = Main(_ -> (
|  _ <- observe((1, 2, 3)).await()
|  pure(0)
|))
|""".stripMargin
    val files = List(
      Chain("src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("src", "Tool", "ObserveMain.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "tool",
          "eval",
          "--run",
          "--main",
          "Tool/ObserveMain",
          "--input",
          "src/Bosatsu/Prog.bosatsu",
          "--input",
          "src/Tool/ObserveMain.bosatsu"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("tool eval --run reads stdin for IO/Std read_line and read_all_stdin") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export (Handle, stdin, stdout, read_utf8, write_utf8, flush)
|
|external struct Handle
|external stdin: Handle
|external stdout: Handle
|external def read_utf8(h: Handle, max_chars: Int) -> Prog[IOError, Option[String]]
|external def write_utf8(h: Handle, s: String) -> Prog[IOError, Unit]
|external def flush(h: Handle) -> Prog[IOError, Unit]
|""".stripMargin
    val ioStdSrc =
      """package Bosatsu/IO/Std
|
|from Bosatsu/Prog import Prog, pure, await, recursive
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Core import stdin, stdout, read_utf8, write_utf8, flush
|
|export (println, read_line, read_all_stdin)
|
|def println(str: String) -> Prog[IOError, Unit]:
|  (
|    _ <- write_utf8(stdout, "${str}\n").await()
|    flush(stdout)
|  )
|
|def trim_trailing_cr(s: String) -> String:
|  match s.rpartition_String("\r"):
|    case Some((prefix, suffix)):
|      if suffix matches "":
|        prefix
|      else:
|        s
|    case None:
|      s
|
|def concat_rev_chunks(rev_chunks: List[String]) -> String:
|  concat_String(rev_chunks.reverse())
|
|read_line: Prog[IOError, Option[String]] =
|  recursive(rec -> rev_chunks ->
|    read_utf8(stdin, 1).await(chunk ->
|      match chunk:
|        case None:
|          if rev_chunks matches []:
|            pure(None)
|          else:
|            pure(Some(trim_trailing_cr(concat_rev_chunks(rev_chunks))))
|        case Some(piece):
|          match piece.partition_String("\n"):
|            case Some((before, _)):
|              pure(Some(trim_trailing_cr(concat_rev_chunks([before, *rev_chunks]))))
|            case None:
|              rec([piece, *rev_chunks])
|    )
|  )([])
|
|read_all_stdin: Prog[IOError, String] =
|  recursive(rec -> rev_chunks ->
|    read_utf8(stdin, 4096).await(chunk ->
|      match chunk:
|        case None: pure(concat_rev_chunks(rev_chunks))
|        case Some(piece): rec([piece, *rev_chunks])
|    )
|  )([])
|""".stripMargin
    val appSrc =
      """package Tool/StdinEcho
|
|from Bosatsu/Prog import Prog, Main, pure, recover, await
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Std import read_line, read_all_stdin, println
|
|def render_opt_String(opt: Option[String]) -> String:
|  match opt:
|    case Some(s): s
|    case None: "<none>"
|
|run_prog: Prog[IOError, Int] = (
|  first <- read_line.await()
|  _ <- println("line=${render_opt_String(first)}").await()
|  rest <- read_all_stdin.await()
|  _ <- println("rest=${rest}").await()
|  pure(0)
|)
|
|main = Main(_ -> recover(run_prog, _ -> pure(1)))
|""".stripMargin

    val files = List(
      Chain("src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("src", "Bosatsu", "IO", "Error.bosatsu") -> minimalIoErrorModuleSrc,
      Chain("src", "Bosatsu", "IO", "Core.bosatsu") -> ioCoreSrc,
      Chain("src", "Bosatsu", "IO", "Std.bosatsu") -> ioStdSrc,
      Chain("src", "Tool", "StdinEcho.bosatsu") -> appSrc
    )

    val result = withSystemStdin("alpha\nbeta\ngamma") {
      for {
        s0 <- stateFromFiles(files)
        s1 <- runWithStateAndExit(
          List(
            "tool",
            "eval",
            "--run",
            "--main",
            "Tool/StdinEcho",
            "--input",
            "src/Bosatsu/Prog.bosatsu",
            "--input",
            "src/Bosatsu/IO/Error.bosatsu",
            "--input",
            "src/Bosatsu/IO/Core.bosatsu",
            "--input",
            "src/Bosatsu/IO/Std.bosatsu",
            "--input",
            "src/Tool/StdinEcho.bosatsu"
          ),
          s0
        )
      } yield s1
    }

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        assertEquals(state.stdOut.render(200), "line=alpha\nrest=beta\ngamma\n\n")
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("tool eval --run truncates Main exit code to low 32 bits") {
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
|main = Main(_ -> pure(2147483648))
|""".stripMargin
    val files = List(
      Chain("src", "Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("src", "Tool", "Foo.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "tool",
          "eval",
          "--run",
          "--main",
          "Tool/Foo",
          "--input",
          "src/Bosatsu/Prog.bosatsu",
          "--input",
          "src/Tool/Foo.bosatsu"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.fromInt(Int.MinValue))
        val errOutput = state.stdErr.render(200)
        assert(
          !errOutput.contains("expected Main to return an Int exit code"),
          errOutput
        )
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test(
    "lib eval --run executes Bosatsu/Prog::Main and includes synthetic argv[0]"
  ) {
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
      s0 <- stateFromFiles(files)
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
        assertEquals(exitCode, ExitCode.fromInt(3))
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval --run merges positional and -- passthrough args in order") {
    val appSrc =
      """from Bosatsu/Prog import Main, pure
|
|main = Main(args -> match args:
|  case [_, "foo", "--bar"]:
|    pure(0)
|  case _:
|    pure(1)
|)
|""".stripMargin
    val files =
      baseLibFiles(appSrc) :+ (
        Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc
      )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/Foo",
          "--run",
          "foo",
          "--",
          "--bar"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval without --run rejects -- passthrough args") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "lib",
        "eval",
        "--repo_root",
        "repo",
        "--main",
        "MyLib/Foo",
        "--",
        "--compact"
      )
    ) match {
      case Left(err) =>
        val msg = module.mainExceptionToString(err).getOrElse(
          fail(s"expected CliException, found: $err")
        )
        assert(msg.contains("trailing args require --run"), msg)
      case Right(other) =>
        fail(s"expected error, found output: $other")
    }
  }

  test("lib eval --run supports Bosatsu/Prog observe in Main") {
    val appSrc =
      """from Bosatsu/Prog import Main, await, observe, pure
|
|main = Main(_ -> (
|  _ <- observe("payload").await()
|  pure(0)
|))
|""".stripMargin
    val files =
      baseLibFiles(appSrc) :+ (
        Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc
      )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/Foo",
          "--run"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval --run reads stdin for IO/Std read_line and read_all_stdin") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export (Handle, stdin, stdout, read_utf8, write_utf8, flush)
|
|external struct Handle
|external stdin: Handle
|external stdout: Handle
|external def read_utf8(h: Handle, max_chars: Int) -> Prog[IOError, Option[String]]
|external def write_utf8(h: Handle, s: String) -> Prog[IOError, Unit]
|external def flush(h: Handle) -> Prog[IOError, Unit]
|""".stripMargin
    val ioStdSrc =
      """package Bosatsu/IO/Std
|
|from Bosatsu/Prog import Prog, pure, await, recursive
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Core import stdin, stdout, read_utf8, write_utf8, flush
|
|export (println, read_line, read_all_stdin)
|
|def println(str: String) -> Prog[IOError, Unit]:
|  (
|    _ <- write_utf8(stdout, "${str}\n").await()
|    flush(stdout)
|  )
|
|def trim_trailing_cr(s: String) -> String:
|  match s.rpartition_String("\r"):
|    case Some((prefix, suffix)):
|      if suffix matches "":
|        prefix
|      else:
|        s
|    case None:
|      s
|
|def concat_rev_chunks(rev_chunks: List[String]) -> String:
|  concat_String(rev_chunks.reverse())
|
|read_line: Prog[IOError, Option[String]] =
|  recursive(rec -> rev_chunks ->
|    read_utf8(stdin, 1).await(chunk ->
|      match chunk:
|        case None:
|          if rev_chunks matches []:
|            pure(None)
|          else:
|            pure(Some(trim_trailing_cr(concat_rev_chunks(rev_chunks))))
|        case Some(piece):
|          match piece.partition_String("\n"):
|            case Some((before, _)):
|              pure(Some(trim_trailing_cr(concat_rev_chunks([before, *rev_chunks]))))
|            case None:
|              rec([piece, *rev_chunks])
|    )
|  )([])
|
|read_all_stdin: Prog[IOError, String] =
|  recursive(rec -> rev_chunks ->
|    read_utf8(stdin, 4096).await(chunk ->
|      match chunk:
|        case None: pure(concat_rev_chunks(rev_chunks))
|        case Some(piece): rec([piece, *rev_chunks])
|    )
|  )([])
|""".stripMargin
    val appSrc =
      """from Bosatsu/Prog import Prog, Main, pure, recover, await
|
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Std import read_line, read_all_stdin, println
|
|def render_opt_String(opt: Option[String]) -> String:
|  match opt:
|    case Some(s): s
|    case None: "<none>"
|
|run_prog: Prog[IOError, Int] = (
|  first <- read_line.await()
|  _ <- println("line=${render_opt_String(first)}").await()
|  rest <- read_all_stdin.await()
|  _ <- println("rest=${rest}").await()
|  pure(0)
|)
|
|main = Main(_ -> recover(run_prog, _ -> pure(1)))
|""".stripMargin

    val files =
      baseLibFiles(appSrc) ++ List(
        Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
        Chain("repo", "src", "Bosatsu", "IO", "Error.bosatsu") -> minimalIoErrorModuleSrc,
        Chain("repo", "src", "Bosatsu", "IO", "Core.bosatsu") -> ioCoreSrc,
        Chain("repo", "src", "Bosatsu", "IO", "Std.bosatsu") -> ioStdSrc
      )

    val result = withSystemStdin("alpha\nbeta\ngamma") {
      for {
        s0 <- stateFromFiles(files)
        s1 <- runWithStateAndExit(
          List(
            "lib",
            "eval",
            "--repo_root",
            "repo",
            "--main",
            "MyLib/Foo",
            "--run"
          ),
          s0
        )
      } yield s1
    }

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        assertEquals(state.stdOut.render(200), "line=alpha\nrest=beta\ngamma\n\n")
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval --run handles now_mono duration_to_nanos newtype representation") {
    val progSrc =
      """package Bosatsu/Prog
|
|export (unit, pure, raise_error, recover, ignore_err, await, recursive, map, map_err, Prog, Main(), ProgTest())
|
|external struct Prog[err: +*, res: +*]
|
|external def pure[err, res](a: res) -> Prog[err, res]
|external def raise_error[err, res](e: err) -> Prog[err, res]
|external def flat_map(prog: Prog[err, res], fn: res -> Prog[err, res1]) -> Prog[err, res1]
|
|def map(prog: Prog[err, res], fn: res -> res1) -> Prog[err, res1]:
|  prog.flat_map(res -> pure(fn(res)))
|
|external def recover(prog: Prog[err, res], fn: err -> Prog[err1, res]) -> Prog[err1, res]
|
|def map_err(prog: Prog[err, res], fn: err -> err1) -> Prog[err1, res]:
|  prog.recover(res -> raise_error(fn(res)))
|
|def ignore_err[err, res](prog: Prog[err, res], default: res) -> forall e. Prog[e, res]:
|  prog.recover(_ -> pure(default))
|
|external def apply_fix(a: a,
|  fn: (a -> Prog[err, b]) -> (a -> Prog[err, b])) -> Prog[err, b]
|
|def await(p, fn): p.flat_map(fn)
|
|def recursive(fn: (a -> Prog[err, b]) -> (a -> Prog[err, b])) -> (a -> Prog[err, b]):
|  a -> apply_fix(a, fn)
|
|unit: forall err. Prog[err, ()] = pure(())
|
|struct Main(run: List[String] -> forall err. Prog[err, Int])
|struct ProgTest(test_fn: List[String] -> forall err. Prog[err, Test])
|""".stripMargin

    val ioErrorSrc =
      """package Bosatsu/IO/Error
|
|export IOError()
|
|enum IOError:
|  InvalidArgument(context: String)
|""".stripMargin

    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export Duration, now_mono, duration_to_nanos
|
|struct Duration(to_nanos: Int)
|
|external now_mono: Prog[IOError, Duration]
|
|def duration_to_nanos(d: Duration) -> Int:
|  Duration { to_nanos } = d
|  to_nanos
|""".stripMargin

    val appSrc =
      """package MyLib/MinParensProbs
|
|from Bosatsu/Prog import Prog, Main, await, pure, recover
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Core import Duration, now_mono, duration_to_nanos
|
|main = Main(_ -> (
|  d <- now_mono.await()
|  pure(mod_Int(duration_to_nanos(d), 1))
|).recover(_ -> pure(1)))
|""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "MinParensProbs.bosatsu") -> appSrc,
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("repo", "src", "Bosatsu", "IO", "Error.bosatsu") -> ioErrorSrc,
      Chain("repo", "src", "Bosatsu", "IO", "Core.bosatsu") -> ioCoreSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/MinParensProbs",
          "--run",
          "p"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval --run handles now_wall instant newtype representation") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export Instant, now_wall, instant_to_nanos
|
|struct Instant(epoch_nanos: Int)
|
|external now_wall: Prog[IOError, Instant]
|
|def instant_to_nanos(i: Instant) -> Int:
|  Instant { epoch_nanos } = i
|  epoch_nanos
|""".stripMargin

    val appSrc =
      """package MyLib/NowWallRepr
|
|from Bosatsu/Prog import Prog, Main, await, pure, recover
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Core import Instant, now_wall, instant_to_nanos
|
|main = Main(_ -> (
|  i <- now_wall.await()
|  pure(mod_Int(instant_to_nanos(i), 1))
|).recover(_ -> pure(1)))
|""".stripMargin

    val files = libFilesWithMinimalProgAndIoCore(
      Chain("repo", "src", "MyLib", "NowWallRepr.bosatsu"),
      appSrc,
      ioCoreSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/NowWallRepr",
          "--run",
          "p"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("lib eval --run handles list_dir path newtype representation") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export Path, string_to_Path, path_to_String, list_dir
|
|struct Path(to_String: String)
|
|def string_to_Path(raw: String) -> Path:
|  Path(raw)
|
|def path_to_String(path: Path) -> String:
|  Path { to_String } = path
|  to_String
|
|external def list_dir(path: Path) -> Prog[IOError, List[Path]]
|""".stripMargin

    val appSrc =
      """package MyLib/ListDirPathRepr
|
|from Bosatsu/Prog import Prog, Main, await, pure, recover
|from Bosatsu/IO/Error import IOError
|from Bosatsu/IO/Core import Path, string_to_Path, path_to_String, list_dir
|
|main = Main(_ -> (
|  paths <- list_dir(string_to_Path(".")).await()
|  match paths:
|    case [h, *_]:
|      _ = cmp_String(path_to_String(h), path_to_String(h))
|      pure(0)
|    case []:
|      pure(1)
|).recover(_ -> pure(1)))
|""".stripMargin

    val files = libFilesWithMinimalProgAndIoCore(
      Chain("repo", "src", "MyLib", "ListDirPathRepr.bosatsu"),
      appSrc,
      ioCoreSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "lib",
          "eval",
          "--repo_root",
          "repo",
          "--main",
          "MyLib/ListDirPathRepr",
          "--run",
          "p"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
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

    runWithFiles(files)(
      List(
        "tool",
        "eval",
        "--run",
        "--main",
        "Tool/Foo",
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
        assert(
          markdown.contains("public dependencies: [`Dep/Util`](../Dep/Util.md)"),
          markdown
        )
        assert(markdown.contains("## Values"), markdown)
        assert(markdown.contains("## Types"), markdown)
        assert(
          markdown.indexOf("## Types") < markdown.indexOf("## Values"),
          markdown
        )
        assert(markdown.contains("Box docs."), markdown)
        assert(markdown.contains("Run docs."), markdown)
        assert(markdown.contains("def run("), markdown)
        assert(markdown.contains("[`Box`](#type-box)"), markdown)
        assert(
          markdown.contains(
            "[`Dep/Util::DepBox`](../Dep/Util.md#type-depbox)"
          ),
          markdown
        )
        assert(markdown.contains("`Box(v: Int)`"), markdown)
        assert(!markdown.contains("Bosatsu/Predef::Int"), markdown)
        assert(markdown.contains("```bosatsu"), markdown)
        assertNoFile(state, Chain("docs", "Dep", "Util.md"))
    }
  }

  test("tool doc routes dependency links to external doc_base_url") {
    val depSrc =
      """export DepBox(), depBox
|
|struct DepBox(v: Int)
|
|depBox = DepBox(7)
|""".stripMargin
    val appSrc =
      """from Dep/Util import depBox
|
|export dep_main
|
|dep_main = depBox
|""".stripMargin

    val files = List(
      Chain("dep", "Dep", "Util.bosatsu") -> depSrc,
      Chain("src", "App", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Util.bosatsu_package",
          "--doc_base_url",
          "https://docs.example.com/deps",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "doc",
          "--input",
          "src/App/Main.bosatsu",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--outdir",
          "docs"
        ),
        state2
      )
    } yield s3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown = readStringFile(state, Chain("docs", "App", "Main.md"))
        assert(
          markdown.contains(
            "public dependencies: [`Dep/Util`](https://docs.example.com/deps/Dep/Util.md)"
          ),
          markdown
        )
        assert(
          markdown.contains(
            "[`Dep/Util::DepBox`](https://docs.example.com/deps/Dep/Util.md#type-depbox)"
          ),
          markdown
        )
    }
  }

  test("tool doc falls back to relative links when dependency has no doc_base_url") {
    val depSrc =
      """export DepBox(), depBox
|
|struct DepBox(v: Int)
|
|depBox = DepBox(7)
|""".stripMargin
    val appSrc =
      """from Dep/Util import depBox
|
|export dep_main
|
|dep_main = depBox
|""".stripMargin

    val files = List(
      Chain("dep", "Dep", "Util.bosatsu") -> depSrc,
      Chain("src", "App", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
          "assemble",
          "--name",
          "dep",
          "--version",
          "0.0.1",
          "--package",
          "out/Dep.Util.bosatsu_package",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runWithState(
        List(
          "tool",
          "doc",
          "--input",
          "src/App/Main.bosatsu",
          "--pub_dep",
          "out/dep.bosatsu_lib",
          "--outdir",
          "docs"
        ),
        state2
      )
    } yield s3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown = readStringFile(state, Chain("docs", "App", "Main.md"))
        assert(
          markdown.contains("public dependencies: [`Dep/Util`](../Dep/Util.md)"),
          markdown
        )
        assert(
          markdown.contains("[`Dep/Util::DepBox`](../Dep/Util.md#type-depbox)"),
          markdown
        )
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

    runWithFiles(files)(
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
      s0 <- stateFromFiles(files)
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
        assert(!markdown.contains("source code:"), markdown)
        assert(!markdown.contains("public dependencies:"), markdown)
        assert(
          markdown.indexOf("## Types") < markdown.indexOf("## Values"),
          markdown
        )
        assert(markdown.contains("def mk("), markdown)
        assert(markdown.contains("[`Thing`](#type-thing)"), markdown)
        assert(markdown.contains("`Thing(v: Int)`"), markdown)
        assert(!markdown.contains("Bosatsu/Predef::Int"), markdown)
        assert(markdown.contains("## Values"), markdown)
        assert(markdown.contains("## Types"), markdown)
    }
  }

  test("lib doc --source_repo_url includes source code links") {
    val src =
      """export Thing(), mk

# Thing docs.
struct Thing(v: Int)

# Mk docs.
mk = (x) -> Thing(x)
"""
    val files = baseLibFiles(src)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "outdocs",
          "--source_repo_url",
          "https://example.com/repo/blob/main"
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
        assert(markdown.contains("source code:"), markdown)
        assert(
          markdown.contains(
            "[`src/MyLib/Foo.bosatsu`](https://example.com/repo/blob/main/src/MyLib/Foo.bosatsu)"
          ),
          markdown
        )
    }
  }

  test("lib doc --remote_doc_links_html rewrites dependency links to .html") {
    val depSrc =
      """export DepBox(), depBox
|
|struct DepBox(v: Int)
|
|depBox = DepBox(7)
|""".stripMargin
    val appSrc =
      """from Dep/Util import depBox
|
|export dep_main
|
|dep_main = depBox
|""".stripMargin

    val result = for {
      state <- stateWithLibDocDependency(
        appSrc,
        depSrc,
        depDocBaseUrl = "https://docs.example.com/deps",
        includePredefInterface = false
      )
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
          "--remote_doc_links_html"
        ),
        state
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown =
          readStringFile(state, Chain("docs", "MyLib", "Foo.md"))
        assert(
          markdown.contains(
            "public dependencies: [`Dep/Util`](https://docs.example.com/deps/Dep/Util.html)"
          ),
          markdown
        )
        assert(
          markdown.contains(
            "[`Dep/Util::DepBox`](https://docs.example.com/deps/Dep/Util.html#type-depbox)"
          ),
          markdown
        )
    }
  }

  test("lib doc keeps dependency links as .md when html rewrite flag is off") {
    val depSrc =
      """export DepBox(), depBox
|
|struct DepBox(v: Int)
|
|depBox = DepBox(7)
|""".stripMargin
    val appSrc =
      """from Dep/Util import depBox
|
|export dep_main
|
|dep_main = depBox
|""".stripMargin

    val result = for {
      state <- stateWithLibDocDependency(
        appSrc,
        depSrc,
        depDocBaseUrl = "https://docs.example.com/deps",
        includePredefInterface = false
      )
      s1 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "docs"
        ),
        state
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val markdown =
          readStringFile(state, Chain("docs", "MyLib", "Foo.md"))
        assert(
          markdown.contains(
            "public dependencies: [`Dep/Util`](https://docs.example.com/deps/Dep/Util.md)"
          ),
          markdown
        )
        assert(
          markdown.contains(
            "[`Dep/Util::DepBox`](https://docs.example.com/deps/Dep/Util.md#type-depbox)"
          ),
          markdown
        )
    }
  }

  test("lib doc prefers local Predef links when included and remote when not") {
    val depSrc =
      """export dep_value
|
|dep_value = 1
|""".stripMargin
    val appSrc =
      """export use_int
|
|external def use_int(a: Int) -> Int
|""".stripMargin

    val result = for {
      state0 <- stateWithLibDocDependency(
        appSrc,
        depSrc,
        depDocBaseUrl = "https://docs.example.com/deps",
        includePredefInterface = true
      )
      s1 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "docs_remote"
        ),
        state0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "docs_local",
          "--include_predef"
        ),
        state1
      )
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val remoteMarkdown =
          readStringFile(state, Chain("docs_remote", "MyLib", "Foo.md"))
        assert(
          remoteMarkdown.contains(
            "https://docs.example.com/deps/Bosatsu/Predef.md#type-int"
          ),
          remoteMarkdown
        )

        val localMarkdown =
          readStringFile(state, Chain("docs_local", "MyLib", "Foo.md"))
        assert(
          localMarkdown.contains("../Bosatsu/Predef.md#type-int"),
          localMarkdown
        )
        assert(
          !localMarkdown.contains(
            "https://docs.example.com/deps/Bosatsu/Predef.md#type-int"
          ),
          localMarkdown
        )
        val predefDoc =
          readStringFile(state, Chain("docs_local", "Bosatsu", "Predef.md"))
        assert(predefDoc.contains("# `Bosatsu/Predef`"), predefDoc)
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
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

  test("tool doc renders Unit -> a as () -> a in external signatures") {
    val src =
      """export apply_default
external def apply_default[a](default: Unit -> a) -> a
"""
    val files = List(Chain("src", "UnitFn", "Main.bosatsu") -> src)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
          "--input",
          "src/UnitFn/Main.bosatsu",
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
        val markdown = readStringFile(state, Chain("docs", "UnitFn", "Main.md"))
        assert(
          markdown.contains("def apply_default[a](default: () -> a) -> a"),
          markdown
        )
        assert(!markdown.contains("default: (()) -> a"), markdown)
    }
  }

  test("tool doc renders deduplicated sorted value references") {
    val src =
      """export concat_all_Array
external def concat_all_Array(arrays: List[Dict[String, Int]]) -> List[Dict[String, Int]]
"""
    val files = List(Chain("src", "Refs", "Main.bosatsu") -> src)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
          "--input",
          "src/Refs/Main.bosatsu",
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
        val markdown = readStringFile(state, Chain("docs", "Refs", "Main.md"))
        assert(
          markdown.contains(
            "references: [`Dict`](../Bosatsu/Predef.md#type-dict), [`Int`](../Bosatsu/Predef.md#type-int), [`List`](../Bosatsu/Predef.md#type-list), [`String`](../Bosatsu/Predef.md#type-string)"
          ),
          markdown
        )
        assert(!markdown.contains("type signature:"), markdown)
    }
  }

  test("tool doc --include_predef includes Bosatsu/Predef markdown") {
    val src =
      """export main,
main = 1
"""
    val files = List(Chain("src", "Simple", "Main.bosatsu") -> src)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "doc",
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
        def containsAny(strs: List[String]): Boolean =
          strs.exists(predefDoc.contains)
        assert(
          containsAny("div(a, 0) == 0" :: "Integer division." :: Nil),
          predefDoc
        )
        assert(
          containsAny("mod_Int(a, 0) == a" :: "Integer modulus." :: Nil),
          predefDoc
        )
        assert(
          containsAny(
            "all `.NaN` values are equal" ::
              "Total Float64 comparison." :: Nil
          ),
          predefDoc
        )
        assert(
          containsAny(
            "dividing by `0.0` yields `∞`, `-∞`, or `.NaN`" ::
              "Floating-point division." :: Nil
          ),
          predefDoc
        )
    }
  }

  test("lib doc --include_predef includes Bosatsu/Predef markdown") {
    val src =
      """export main,
main = 1
"""
    val files = baseLibFiles(src)

    val result = for {
      s0 <- stateFromFiles(files)
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

  test("lib doc labels private packages and can exclude them") {
    val publicPack = PackageName.parts("MyLib", "Public")
    val privatePack = PackageName.parts("MyLib", "Private")
    val conf = LibConfig(
      name = Name("mylib"),
      repoUri = "https://example.com",
      nextVersion = Version(0, 0, 1),
      previous = None,
      exportedPackages = LibConfig.PackageFilter.Name(publicPack) :: Nil,
      allPackages =
        LibConfig.PackageFilter.Name(publicPack) ::
          LibConfig.PackageFilter.Name(privatePack) ::
          Nil,
      publicDeps = Nil,
      privateDeps = Nil,
      defaultMain = None
    )

    val publicSrc =
      """package MyLib/Public
|
|from MyLib/Private import helper
|
|export run
|
|run = helper
|""".stripMargin
    val privateSrc =
      """package MyLib/Private
|
|export helper
|
|helper = 42
|""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Public.bosatsu") -> publicSrc,
      Chain("repo", "src", "MyLib", "Private.bosatsu") -> privateSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "docs_all"
        ),
        s0
      )
      (stateAll, outAll) = s1
      s2 <- runWithState(
        List(
          "lib",
          "doc",
          "--repo_root",
          "repo",
          "--name",
          "mylib",
          "--outdir",
          "docs_pub",
          "--exclude_private_packages"
        ),
        s0
      )
      (statePub, outPub) = s2
    } yield (stateAll, outAll, statePub, outPub)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((stateAll, outAll, statePub, outPub)) =>
        outAll match {
          case Output.TranspileOut(outputs) =>
            val outPaths = outputs.map(_._1.toList.mkString("/")).toSet
            assert(outPaths("docs_all/MyLib/Public.md"), outputs.toString)
            assert(outPaths("docs_all/MyLib/Private.md"), outputs.toString)
          case other =>
            fail(s"unexpected output: $other")
        }

        val publicMarkdown =
          readStringFile(stateAll, Chain("docs_all", "MyLib", "Public.md"))
        val privateMarkdown =
          readStringFile(stateAll, Chain("docs_all", "MyLib", "Private.md"))
        assert(!publicMarkdown.contains("private package"), publicMarkdown)
        assert(privateMarkdown.contains("private package"), privateMarkdown)

        outPub match {
          case Output.TranspileOut(outputs) =>
            val outPaths = outputs.map(_._1.toList.mkString("/")).toSet
            assert(outPaths("docs_pub/MyLib/Public.md"), outputs.toString)
            assert(!outPaths("docs_pub/MyLib/Private.md"), outputs.toString)
          case other =>
            fail(s"unexpected output: $other")
        }
        assertNoFile(statePub, Chain("docs_pub", "MyLib", "Private.md"))
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "deps",
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

  test("tool deps parses explicit package declarations with body statements") {
    val aSrc =
      """package QA/A
x = 1
"""
    val bSrc =
      """package QA/B
from QA/A import x
y = x
"""
    val files = List(
      Chain("src", "QA", "A.bosatsu") -> aSrc,
      Chain("src", "QA", "B.bosatsu") -> bSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "deps",
          "--input",
          "src/QA/A.bosatsu",
          "--input",
          "src/QA/B.bosatsu",
          "--graph_format",
          "json",
          "--output",
          "out/deps.json"
        ),
        s0
      )
    } yield s1._1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        val jsonStr = readStringFile(state, Chain("out", "deps.json"))
        Json.parserFile.parseAll(jsonStr) match {
          case Right(Json.JArray(items)) =>
            val packageDeps = items.toList.collect {
              case Json.JObject(fields) =>
                val pkg = fields.collectFirst {
                  case ("package", Json.JString(p)) => p
                } match {
                  case Some(p) => p
                  case None    => fail(show"missing package in output row: $fields")
                }
                val deps = fields
                  .collectFirst { case ("dependsOn", Json.JArray(values)) =>
                    values.toList.collect { case Json.JString(s) => s }
                  }
                  .getOrElse(Nil)
                (pkg, deps)
            }.toMap

            assertEquals(packageDeps.get("QA/A"), Some(Nil))
            assertEquals(packageDeps.get("QA/B"), Some(List("QA/A")))
          case Right(other) =>
            fail(show"expected deps json array output, found: $other")
          case Left(err) =>
            fail(show"failed to parse deps json output: $err")
        }
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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

    val applyFromPath = runWithFiles(applyFiles)(
      List(
        "tool",
        "json",
        "apply",
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
    val wrongArity = runWithFiles(
      List(Chain("src", "Json", "Foo.bosatsu") -> wrongAritySrc)
    )(
      List(
        "tool",
        "json",
        "apply",
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

  test("tool json traverse supports object inputs") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = List(Chain("src", "Json", "Foo.bosatsu") -> src)

    runWithFiles(files)(
      List(
        "tool",
        "json",
        "traverse",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_string",
        """{"a":[1],"b":[4]}"""
      )
    ) match {
      case Right(Output.JsonOutput(Json.JObject(items), _)) =>
        assertEquals(items, List("a" -> Json.JNumberStr("2"), "b" -> Json.JNumberStr("5")))
      case Right(out) =>
        fail(s"expected traverse object output, got output: $out")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool json traverse rejects non-array/object inputs") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = List(Chain("src", "Json", "Foo.bosatsu") -> src)

    runWithFiles(files)(
      List(
        "tool",
        "json",
        "traverse",
        "--input",
        "src/Json/Foo.bosatsu",
        "--main",
        "Json/Foo",
        "--json_string",
        "1"
      )
    ) match {
      case Right(out) =>
        fail(s"expected traverse error, got output: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains(
            "require an array of argument arrays or an object of argument arrays for traverse"
          ),
          msg
        )
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

    runWithFiles(writeFiles)(
      List(
        "tool",
        "json",
        "write",
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

    runWithFiles(fnFiles)(
      List(
        "tool",
        "json",
        "apply",
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

    runWithFiles(fnFiles)(
      List(
        "tool",
        "json",
        "traverse",
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
    runWithFiles(Nil)(List("tool", "check")) match {
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

  test("tool check unknown import package includes span context and hint") {
    val src =
      """package QA/UnknownImport
        |
        |from Foo/Bar import baz
        |x = 1
        |""".stripMargin
    val files = List(
      Chain("tmp", "UnknownImport.bosatsu") -> src
    )

    runWithFiles(files)(
      List(
        "tool",
        "check",
        "--color",
        "none",
        "--input",
        "tmp/UnknownImport.bosatsu",
        "--output",
        "tmp/out",
        "--interface_out",
        "tmp/iface"
      )
    ) match {
      case Right(out) =>
        fail(s"expected unknown-import package failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains(":3:6, package QA/UnknownImport"),
          msg
        )
        assert(msg.contains("Unknown package `Foo/Bar` in import."), msg)
        assert(msg.contains("from Foo/Bar import baz"), msg)
        assert(msg.linesIterator.exists(_.contains("^^^^^^^")), msg)
        assert(
          msg.contains("Hint: add source containing package `Foo/Bar`"),
          msg
        )
        assert(msg.contains("--input/--input_dir"), msg)
        assert(!msg.contains("--package_root"), msg)
        assert(msg.contains("--pub_dep/--priv_dep"), msg)
    }
  }

  test("tool check unknown import package suggests nearest package typo") {
    val depSrc =
      """package Foo/Baz
        |export baz
        |
        |baz = 1
        |""".stripMargin
    val appSrc =
      """package QA/UnknownImport
        |
        |from Foo/Bar import baz
        |x = 1
        |""".stripMargin
    val files = List(
      Chain("src", "Foo", "Baz.bosatsu") -> depSrc,
      Chain("src", "QA", "UnknownImport.bosatsu") -> appSrc
    )

    runWithFiles(files)(
      List(
        "tool",
        "check",
        "--color",
        "none",
        "--input",
        "src/Foo/Baz.bosatsu",
        "--input",
        "src/QA/UnknownImport.bosatsu",
        "--output",
        "tmp/out",
        "--interface_out",
        "tmp/iface"
      )
    ) match {
      case Right(out) =>
        fail(s"expected unknown-import package failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("Unknown package `Foo/Bar` in import."), msg)
        assert(msg.contains("Did you mean package `Foo/Baz`?"), msg)
    }
  }

  test("tool check enumerates and summarizes mixed diagnostics") {
    val src =
      """package QA/Bleed
        |
        |def parse_count(input: Int) -> Int:
        |  left = input.add(1)
        |  right = input.add(2)
        |  "oops"
        |
        |main = parse_count(0)
        |""".stripMargin
    val files = List(
      Chain("src", "QA", "Bleed.bosatsu") -> src
    )

    runWithFiles(files)(
      List(
        "tool",
        "check",
        "--color",
        "none",
        "--input",
        "src/QA/Bleed.bosatsu"
      )
    ) match {
      case Right(out) =>
        fail(s"expected mixed-diagnostic failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("1. 2 unused values"), rendered)
        assert(rendered.contains("2. type error"), rendered)
        assert(rendered.contains("unused value 'left'"), rendered)
        assert(rendered.contains("unused value 'right'"), rendered)
        assert(rendered.contains("2 unused values"), rendered)
        assert(rendered.contains("1 type error"), rendered)
        assert(rendered.contains("errors: 2 unused values, 1 type error"), rendered)
        assert(
          rendered.linesIterator.exists(_.startsWith("----------------")),
          rendered
        )
      case Left(err) =>
        fail(err.getMessage)
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

    runWithFiles(files)(
      List(
        "tool",
        "check",
        "--input",
        "src/Todo/Foo.bosatsu"
      )
    ) match {
      case Right(Output.CompileOut(_, _, _)) => ()
      case Right(other)                      => fail(s"unexpected output: $other")
      case Left(err)                         => fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
        "tool",
        "show",
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
      "--input",
      "src/Cache/Dep.bosatsu",
      "--input",
      "src/Cache/App.bosatsu",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(files)
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

  test("tool check --cache_dir reuses cas blobs on comment-only edits") {
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
        |# comment-only edits should still reuse the same compiled package bytes
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
      "--input",
      "src/Cache/Dep.bosatsu",
      "--input",
      "src/Cache/App.bosatsu",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(files)
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
        assertNotEquals(filePathsUnder(state2, keyPrefix), filePathsUnder(state1, keyPrefix))
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
      "--input",
      "src/Cache/Dep.bosatsu",
      "--input",
      "src/Cache/App.bosatsu",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(files)
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
      "--input",
      "src/Cache/Foo.bosatsu"
    )

    val result = for {
      s0 <- stateFromFiles(files)
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

  test("lib check --cache_dir writes cache artifacts and reuses them") {
    val files = baseLibFiles("main = 1\n")
    val cmd = List(
      "lib",
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(files)
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
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
    }
  }

  test(
    "lib check without cache flags writes default cache artifacts and reuses them"
  ) {
    val files = baseLibFiles("main = 1\n")
    val cmd = List(
      "lib",
      "check",
      "--repo_root",
      "repo"
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, state1)
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        val keyPrefix =
          Chain("repo", ".bosatsuc", "infer-cache", "keys", "blake3")
        val casPrefix = Chain("repo", ".bosatsuc", "infer-cache", "cas", "blake3")
        val keyFiles1 = filePathsUnder(state1, keyPrefix)
        val casFiles1 = filePathsUnder(state1, casPrefix)

        assert(keyFiles1.nonEmpty, "expected key cache files in default infer cache")
        assert(casFiles1.nonEmpty, "expected cas cache files in default infer cache")
        assertEquals(filePathsUnder(state2, keyPrefix), keyFiles1)
        assertEquals(filePathsUnder(state2, casPrefix), casFiles1)
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
    }
  }

  test("lib check --no_cache does not write infer cache artifacts") {
    val files = baseLibFiles("main = 1\n")
    val cmd = List(
      "lib",
      "check",
      "--repo_root",
      "repo",
      "--no_cache"
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(cmd, s0)
      (state1, _) = s1
    } yield state1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(state) =>
        assertEquals(
          filePathsUnder(state, Chain("repo", ".bosatsuc", "infer-cache")),
          Set.empty
        )
    }
  }

  test("lib check rejects --cache_dir with --no_cache") {
    val cmd = List(
      "lib",
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--no_cache"
    )

    module.run(cmd) match {
      case Left(_)  => ()
      case Right(_) =>
        fail("expected parser error when both --cache_dir and --no_cache are supplied")
    }
  }

  test("lib deps list text output includes public and private sections") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
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

    runWithFiles(files)(
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

    val applyWrongArity = runWithFiles(files)(
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

    val applyInvalidData = runWithFiles(files)(
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

    val applyParseError = runWithFiles(files)(
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

    val traverseInvalidData = runWithFiles(files)(
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

    val traverseWrongArity = runWithFiles(files)(
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

    val traverseNonArray = runWithFiles(files)(
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

    runWithFiles(writeFiles)(
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

    runWithFiles(fnFiles)(
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

    runWithFiles(fnFiles)(
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

    runWithFiles(files)(
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

    runWithFiles(charFiles)(
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

    runWithFiles(arrayFiles)(
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

    runWithFiles(bytesFiles)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(charFnFiles)(
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

    val charInvalid = runWithFiles(charFnFiles)(
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

    runWithFiles(arrayFnFiles)(
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

    runWithFiles(bytesFnFiles)(
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

    val bytesInvalid = runWithFiles(bytesFnFiles)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(
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

    runWithFiles(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected missing-previous failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("previous not in cas"), msg)
    }
  }

  test("lib check accepts todo but lib show rejects it") {
    val files = baseLibFiles("main = todo(1)\n")

    runWithFiles(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(Output.Basic(_, _)) => ()
      case Right(other)              => fail(s"unexpected output: $other")
      case Left(err)                 => fail(err.getMessage)
    }

    runWithFiles(files)(
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

  test("lib check malformed bosatsu_libs.json reports parse error without stack trace") {
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> "{ bad json"
    )

    val result = for {
      state0 <- stateFromFiles(files)
      stateAndExit <- runAndReportWithState(
        List("lib", "check", "--color", "none", "--repo_root", "repo"),
        state0
      )
    } yield stateAndExit

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val errOutput = state.stdErr.render(200)
        assert(
          errOutput.contains("config parse failed: repo/bosatsu_libs.json:1:3"),
          errOutput
        )
        assert(errOutput.contains("expected"), errOutput)
        assert(!errOutput.contains("unknown error"), errOutput)
        assert(!errOutput.contains("java.lang.Exception"), errOutput)
        assert(!errOutput.contains("\tat dev.bosatsu"), errOutput)
    }
  }

  test("lib check malformed <lib>_conf.json reports parse error without stack trace") {
    val libs = Libraries(SortedMap(Name("qa") -> "src"))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "qa_conf.json") -> "{ bad json",
      Chain("repo", "src", "QA", "Foo.bosatsu") ->
        """package QA/Foo
          |
          |x = 1
          |""".stripMargin
    )

    val result = for {
      state0 <- stateFromFiles(files)
      stateAndExit <- runAndReportWithState(
        List("lib", "check", "--color", "none", "--repo_root", "repo"),
        state0
      )
    } yield stateAndExit

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val errOutput = state.stdErr.render(200)
        assert(
          errOutput.contains("config parse failed: repo/src/qa_conf.json:1:3"),
          errOutput
        )
        assert(errOutput.contains("expected"), errOutput)
        assert(!errOutput.contains("unknown error"), errOutput)
        assert(!errOutput.contains("java.lang.Exception"), errOutput)
        assert(!errOutput.contains("\tat dev.bosatsu"), errOutput)
    }
  }

  test("lib fetch reports total fetched objects by default") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(List("lib", "fetch", "--repo_root", "repo")) match {
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

    runWithFiles(files)(
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

  test("lib fetch accepts matching descriptor on cached dependency hit") {
    val result = for {
      setup <- stateWithConfiguredCachedDep("main = 1\n", "dep", "0.0.1")
      (state, _) = setup
      fetched <- runWithState(List("lib", "fetch", "--repo_root", "repo"), state)
    } yield fetched

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, Output.Basic(doc, None))) =>
        val rendered = doc.render(120)
        assert(rendered.contains("fetched 0 objects."), rendered)
      case Right((_, other)) =>
        fail(s"unexpected output: $other")
    }
  }

  test("lib fetch fails on cached dependency version mismatch") {
    val result = for {
      setup <- stateWithConfiguredCachedDep("main = 1\n", "dep", "5.0.0")
      (state, _) = setup
      fetched <- runWithState(List("lib", "fetch", "--repo_root", "repo"), state)
    } yield fetched

    result match {
      case Right((_, out)) =>
        fail(s"expected lib fetch failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("dep 5.0.0:"), rendered)
        assert(rendered.contains("cached library descriptor mismatch"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib fetch fails on cached dependency name mismatch") {
    val result = for {
      setup <- stateWithConfiguredCachedDep("main = 1\n", "not_dep", "0.0.1")
      (state, _) = setup
      fetched <- runWithState(List("lib", "fetch", "--repo_root", "repo"), state)
    } yield fetched

    result match {
      case Right((_, out)) =>
        fail(s"expected lib fetch failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("not_dep 0.0.1:"), rendered)
        assert(rendered.contains("cached library descriptor mismatch"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib check fails on cached dependency version mismatch") {
    val appSrc =
      """from Dep/Foo import dep
|
|main = dep
|""".stripMargin

    val result = for {
      setup <- stateWithConfiguredCachedDep(appSrc, "dep", "5.0.0")
      (state, _) = setup
      checked <- runWithState(List("lib", "check", "--repo_root", "repo"), state)
    } yield checked

    result match {
      case Right((_, out)) =>
        fail(s"expected lib check failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(
          rendered.contains("version mismatch: dependency=5.0.0, cached=0.0.1"),
          rendered
        )
        assert(!rendered.contains("name mismatch:"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib check reports both cached dependency version mismatches") {
    val appSrc =
      """from DepA/Foo import depA
|
|from DepB/Foo import depB
|
|main = depA.add(depB)
|""".stripMargin

    val result = for {
      state <- stateWithTwoVersionMismatchedCachedDeps(appSrc)
      checked <- runWithState(List("lib", "check", "--repo_root", "repo"), state)
    } yield checked

    result match {
      case Right((_, out)) =>
        fail(s"expected lib check failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(
          rendered.contains("version mismatch: dependency=5.0.0, cached=0.0.1"),
          rendered
        )
        assert(
          rendered.contains("version mismatch: dependency=6.0.0, cached=0.0.2"),
          rendered
        )
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("lib fetch reports network failures with detailed reason") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
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
          "https://example.com/network.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runAndReportWithState(
        List("lib", "fetch", "--repo_root", "repo"),
        state1
      )
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val errOutput = state.stdErr.render(200)
        assert(errOutput.contains("failed: blake3:222"), errOutput)
        assert(errOutput.contains("download failed (1 failure)"), errOutput)
        assert(errOutput.contains("uri=https://example.com/network.bosatsu_lib"), errOutput)
        assert(errOutput.contains("reason=network error"), errOutput)
        assert(errOutput.contains("message=connection reset by peer"), errOutput)
    }
  }

  test("lib fetch reports hash mismatch with expected and found hashes") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_hash_mismatch",
          "--version",
          "0.0.1",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/hash-mismatch.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runAndReportWithState(
        List("lib", "fetch", "--repo_root", "repo"),
        state1
      )
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val errOutput = state.stdErr.render(200)
        assert(errOutput.contains("download failed (1 failure)"), errOutput)
        assert(errOutput.contains("reason=hash mismatch"), errOutput)
        assert(errOutput.contains(show"expected=$validHash2"), errOutput)
        assert(
          errOutput.contains("found=blake3:0000000000000000000000000000000000000000000000000000000000000000"),
          errOutput
        )
    }
  }

  test("lib fetch reports http status failures") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_http_404",
          "--version",
          "0.0.1",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/404.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        s0
      )
      (state1, _) = s1
      s2 <- runAndReportWithState(
        List("lib", "fetch", "--repo_root", "repo"),
        state1
      )
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val errOutput = state.stdErr.render(200)
        assert(errOutput.contains("reason=http status"), errOutput)
        assert(errOutput.contains("status=404 Not Found"), errOutput)
    }
  }

  test("lib fetch uses grammatical singular and plural failure wording") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "deps",
          "add",
          "--repo_root",
          "repo",
          "--dep",
          "dep_one_failure",
          "--version",
          "0.0.1",
          "--hash",
          validHash2,
          "--uri",
          "https://example.com/network.bosatsu_lib",
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
          "dep_two_failures",
          "--version",
          "0.0.1",
          "--hash",
          validHash3,
          "--uri",
          "https://example.com/404-first.bosatsu_lib",
          "--uri",
          "https://example.com/network-second.bosatsu_lib",
          "--public",
          "--no-fetch"
        ),
        state1
      )
      (state2, _) = s2
      s3 <- runAndReportWithState(
        List("lib", "fetch", "--repo_root", "repo"),
        state2
      )
    } yield s3

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val errOutput = state.stdErr.render(200)
        assert(errOutput.contains("download failed (1 failure)"), errOutput)
        assert(errOutput.contains("download failed (2 failures)"), errOutput)
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

    runWithFiles(files)(List("lib", "fetch", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected previous fetch failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to fetch previous"), msg)
    }
  }

  test("lib publish --dry-run validates without mutating config, CAS, or default cache") {
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "publish",
          "--repo_root",
          "repo",
          "--outdir",
          "out",
          "--git_sha",
          "deadbeef",
          "--dry-run"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out)) =>
        val confPath = Chain("repo", "src", "mylib_conf.json")
        assertEquals(readStringFile(state, confPath), renderJson(conf))

        val publishedPath = Chain("out", "mylib-v0.0.1.bosatsu_lib")
        val publishedLib = readLibraryFile(state, publishedPath)
        assertNoFile(state, casPathFor(Chain("repo"), publishedLib))
        assertEquals(
          filePathsUnder(state, Chain("repo", ".bosatsuc", "infer-cache")),
          Set.empty
        )

        out match {
          case Output.Many(items) =>
            assert(
              items.toList.forall {
                case Output.Library(_, _) => true
                case _                    => false
              }
            )
          case other =>
            fail(s"expected publish output list, found: $other")
        }
    }
  }

  test("lib publish updates config and CAS when not in dry-run mode") {
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "lib",
          "publish",
          "--repo_root",
          "repo",
          "--outdir",
          "out",
          "--git_sha",
          "deadbeef"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _)) =>
        val publishedPath = Chain("out", "mylib-v0.0.1.bosatsu_lib")
        val publishedLib = readLibraryFile(state, publishedPath)

        val confAfter =
          readJsonFile[LibConfig](state, Chain("repo", "src", "mylib_conf.json"))
        assertEquals(confAfter.nextVersion, Version(0, 0, 2))
        confAfter.previous match {
          case Some(prev) =>
            assertEquals(prev.version, Some(Version(0, 0, 1).toProto))
            assert(prev.hashes.contains(publishedLib.hash.toIdent))
          case None       =>
            fail("expected previous descriptor to be written during publish")
        }

        val cachedLib = readLibraryFile(state, casPathFor(Chain("repo"), publishedLib))
        assertEquals(cachedLib.hash, publishedLib.hash)
    }
  }

  test("lib build without --main_pack reports missing main") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(
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
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
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

  test("lib build defaults to tree-shaking unused external packages") {
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
|from Bosatsu/Prog import Main
|
|main = Main(1)
|""".stripMargin
    val lazySrc =
      """package Bosatsu/Lazy
|
|external struct LazyInt
|external def mk_LazyInt(i: Int) -> LazyInt
|
|ignored = mk_LazyInt(1)
|""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("repo", "src", "Bosatsu", "Lazy.bosatsu") -> lazySrc,
      Chain("repo", "src", "MyLib", "Main.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
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
        s0
      )
      (state1, _) = s1
      s2 <- runWithState(
        List(
          "lib",
          "build",
          "--repo_root",
          "repo",
          "--outdir",
          "out_all",
          "-m",
          "MyLib/Main",
          "--emitmode",
          "all"
        ),
        state1
      )
    } yield (s1, s2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right(((stateDefault, outDefault), (stateAll, outAll))) =>
        outDefault match {
          case Output.Basic(_, _) => ()
          case other              => fail(s"unexpected output: $other")
        }
        outAll match {
          case Output.Basic(_, _) => ()
          case other              => fail(s"unexpected output: $other")
        }
        val lazyHeaderInclude = "#include \"bosatsu_ext_Bosatsu_l_Lazy.h\""
        val outputCDefault =
          readStringFile(stateDefault, Chain("out", "output.c"))
        assert(
          !outputCDefault.contains(lazyHeaderInclude),
          outputCDefault
        )
        val outputCAll = readStringFile(stateAll, Chain("out_all", "output.c"))
        assert(outputCAll.contains(lazyHeaderInclude), outputCAll)
    }
  }

  test(
    "lib test runs runtime readiness preflight before executing tests"
  ) {
    val targetSrc =
      """test_one = Assertion(True, "ok")
"""
    val files =
      baseLibFiles(targetSrc) :+ (
        Chain(".", ".git", "HEAD") -> "ref: refs/heads/main\n"
      )

    runWithFiles(files)(
      List("lib", "test", "--repo_root", "repo")
    ) match {
      case Right(out) =>
        fail(s"expected preflight failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains(
            "runtime readiness preflight failed before running `lib test`"
          ),
          msg
        )
        assert(msg.contains("missing default c runtime config"), msg)
        assert(msg.contains("expected artifact: cc_conf.json"), msg)
        assert(msg.contains("runtime hash:"), msg)
        assert(!msg.contains("system not supported in memory mode"), msg)
    }
  }

  test(
    "lib test with missing --cc_conf reports a concise validation error"
  ) {
    val targetSrc =
      """test_one = Assertion(True, "ok")
"""
    val files = baseLibFiles(targetSrc)

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--cc_conf",
        "repo/does_not_exist_cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected missing cc_conf failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains(
            "runtime readiness preflight failed before running `lib test`"
          ),
          msg
        )
        assert(
          msg.contains("cc_conf file not found: repo/does_not_exist_cc_conf.json"),
          msg
        )
        assert(
          msg.contains(
            "Run `bosatsu c-runtime install` or provide a valid `--cc_conf` path."
          ),
          msg
        )
        assert(!msg.contains("unknown error"), msg)
        assert(!msg.contains("NoSuchFileException"), msg)
    }
  }

  test("lib test rejects --value with --filter") {
    module.run(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Euler/One::test_one",
        "--filter",
        "MyLib/Euler/.*"
      )
    ) match {
      case Left(_)  => ()
      case Right(_) =>
        fail("expected parser error when both --value and --filter are supplied")
    }
  }

  test("lib test --value accepts exported plain Test values") {
    val targetSrc =
      """export test_one
|
|test_one = Assertion(True, "ok")
|""".stripMargin
    val unrelatedBrokenSrc =
      """bad = does_not_exist
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin
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

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Euler/One::test_one",
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
        assert(!msg.contains("invalid test value"), msg)
    }
  }

  test("lib test --value accepts exported ProgTest values") {
    val targetSrc =
      """from Bosatsu/Prog import ProgTest, pure
|
|export prog_tests
|
|prog_tests = ProgTest(_ -> pure(Assertion(True, "ok")))
|""".stripMargin
    val unrelatedBrokenSrc =
      """bad = does_not_exist
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin
    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("repo", "src", "MyLib", "Euler", "ProgSel.bosatsu") -> targetSrc,
      Chain("repo", "src", "MyLib", "ReproMin8.bosatsu") -> unrelatedBrokenSrc,
      Chain("repo", "cc_conf.json") -> ccConfJson
    )

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Euler/ProgSel::prog_tests",
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
        assert(!msg.contains("invalid test value"), msg)
    }
  }

  test("lib test --value rejects non-exported test values") {
    val targetSrc =
      """export visible
|
|visible = Assertion(True, "ok")
|hidden = Assertion(True, "hidden")
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin
    val files =
      baseLibFiles(targetSrc) :+ (Chain("repo", "cc_conf.json") -> ccConfJson)

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::hidden",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected invalid value selection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid test value `MyLib/Foo::hidden`"), msg)
        assert(msg.contains("value is not exported"), msg)
        assert(msg.contains("exported test values: [visible]"), msg)
    }
  }

  test("lib test --value rejects exported values that are not test values") {
    val targetSrc =
      """export not_test
|
|not_test = 1
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin
    val files =
      baseLibFiles(targetSrc) :+ (Chain("repo", "cc_conf.json") -> ccConfJson)

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::not_test",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected invalid value selection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid test value `MyLib/Foo::not_test`"), msg)
        assert(msg.contains("exported value is not a test value"), msg)
        assert(msg.contains("Bosatsu/Predef::Test"), msg)
    }
  }

  test("lib test --value reports unknown package with a clear error") {
    val targetSrc =
      """export test_one
|
|test_one = Assertion(True, "ok")
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin
    val files =
      baseLibFiles(targetSrc) :+ (Chain("repo", "cc_conf.json") -> ccConfJson)

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "Nope/Missing::test_one",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected invalid value selection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid test value `Nope/Missing::test_one`"), msg)
        assert(msg.contains("unknown package"), msg)
    }
  }

  test("lib test --value reports packages with no exported test values") {
    val targetSrc =
      """export main
|
|main = 42
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin
    val files =
      baseLibFiles(targetSrc) :+ (Chain("repo", "cc_conf.json") -> ccConfJson)

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::missing",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected invalid value selection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid test value `MyLib/Foo::missing`"), msg)
        assert(msg.contains("has no exported test values"), msg)
    }
  }

  test(
    "lib test --filter reports no matching packages and suggests package typo fixes"
  ) {
    val targetSrc =
      """export hedgehog_tests
|
|hedgehog_tests = Assertion(True, "ok")
|""".stripMargin
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
|""".stripMargin

    val libs = Libraries(SortedMap(Name("mylib") -> "src"))
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> renderJson(libs),
      Chain("repo", "src", "mylib_conf.json") -> renderJson(conf),
      Chain("repo", "src", "MyLib", "Testing", "HedgeHog.bosatsu") -> targetSrc,
      Chain("repo", "cc_conf.json") -> ccConfJson
    )

    runWithFiles(files)(
      List(
        "lib",
        "test",
        "--repo_root",
        "repo",
        "--filter",
        "MyLib/Testing/Hedgehog",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected no matching packages error, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("no packages found matching regex"), msg)
        assert(msg.contains("MyLib/Testing/Hedgehog"), msg)
        assert(msg.contains("Did you mean: MyLib/Testing/HedgeHog ?"), msg)
        assert(!msg.contains("no tests found"), msg)
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

    runWithFiles(files)(
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

    runWithFiles(files)(List("lib", "check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected unfiltered check failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("does_not_exist"), msg)
    }

    runWithFiles(files)(
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

  test("lib check rejects --value") {
    module.run(
      List(
        "lib",
        "check",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Euler/One::test_one"
      )
    ) match {
      case Left(_)  => ()
      case Right(_) =>
        fail("expected parser error for unsupported --value on lib check")
    }
  }

  test("tool test --quiet sets quiet mode in output") {
    val src =
      """tests = TestSuite("quiet", [
|  Assertion(True, "pass one")
|])
|""".stripMargin

    runWithFiles(List(Chain("Package0") -> src))(
      List(
        "tool",
        "test",
        "--quiet",
        "--test_package",
        "Package0",
        "--input",
        "Package0"
      )
    ) match {
      case Right(Output.TestOutput(_, _, quiet)) =>
        assert(quiet)
      case Right(other) =>
        fail(s"expected test output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool test output includes package and total timings") {
    val src =
      """tests = TestSuite("timed", [
|  Assertion(True, "pass one")
|])
|""".stripMargin
    val cmd = List(
      "tool",
      "test",
      "--test_package",
      "Package0",
      "--input",
      "Package0"
    )

    val result = for {
      s0 <- stateFromFiles(List(Chain("Package0") -> src))
      out <- runWithStateAndExit(cmd, s0)
    } yield out

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _, exitCode)) =>
        val out = state.stdOut.render(120)
        assertEquals(exitCode, ExitCode.Success)
        assert(out.matches("(?s).*Package0: \\d+\\.\\d{3}s.*"), out)
        assert(out.matches("(?s).*\\d+ test[s]?, .* in \\d+\\.\\d{3}s.*"), out)
    }
  }

  test("tool test --quiet only prints failures and summary") {
    val src =
      """tests = TestSuite("quiet", [
|  Assertion(True, "pass one"),
|  Assertion(False, "boom")
|])
|""".stripMargin
    val cmd = List(
      "tool",
      "test",
      "--quiet",
      "--test_package",
      "Package0",
      "--input",
      "Package0"
    )

    val result = for {
      s0 <- stateFromFiles(List(Chain("Package0") -> src))
      out <- runWithStateAndExit(cmd, s0)
    } yield out

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, _, exitCode)) =>
        val out = state.stdOut.render(120)
        assertEquals(exitCode, ExitCode.Error)
        assert(out.contains("boom"), out)
        assert(out.contains("passed"), out)
        assert(out.contains("failed"), out)
        assert(out.matches("(?s).*Package0: \\d+\\.\\d{3}s.*"), out)
        assert(out.matches("(?s).*\\d+ test[s]?, .* in \\d+\\.\\d{3}s.*"), out)
        assert(!out.contains("pass one"), out)
    }
  }

  test("tool test runs ProgTest values") {
    val appSrc =
      """package App/ProgTest
|
|from Bosatsu/Prog import ProgTest, pure
|
|prog_tests = ProgTest(_ -> pure(
|  TestSuite("prog", [Assertion(True, "ok")])
|))
|""".stripMargin
    val files = List(
      Chain("Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("App", "ProgTest.bosatsu") -> appSrc
    )
    val cmd = List(
      "tool",
      "test",
      "--input",
      "Bosatsu/Prog.bosatsu",
      "--input",
      "App/ProgTest.bosatsu"
    )

    val result = for {
      s0 <- stateFromFiles(files)
      out <- runWithStateAndExit(cmd, s0)
    } yield out

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, Output.TestOutput(tests, _, _), exitCode)) =>
        assertEquals(exitCode, ExitCode.Success)
        val appTest = tests.collectFirst {
          case (pn, Some(test))
              if pn == PackageName.parts("App", "ProgTest") =>
            test.value
        }.getOrElse(fail("missing App/ProgTest test output"))
        assertEquals(appTest.assertions, 1)
        assertEquals(appTest.failureCount, 0)
      case Right((_, other, _)) =>
        fail(s"expected test output, got: $other")
    }
  }

  test("tool test reports discovery error when Test appears after ProgTest") {
    val appSrc =
      """package App/BadOrder
|
|from Bosatsu/Prog import ProgTest, pure
|
|prog_tests = ProgTest(_ -> pure(Assertion(True, "prog test")))
|tests = Assertion(True, "plain test after prog test")
|""".stripMargin
    val files = List(
      Chain("Bosatsu", "Prog.bosatsu") -> minimalProgModuleSrc,
      Chain("App", "BadOrder.bosatsu") -> appSrc
    )
    val cmd = List(
      "tool",
      "test",
      "--input",
      "Bosatsu/Prog.bosatsu",
      "--input",
      "App/BadOrder.bosatsu"
    )

    runWithFiles(files)(cmd) match {
      case Right(out) =>
        fail(s"expected discovery error, got: $out")
      case Left(err)  =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("invalid test discovery"), msg)
        assert(msg.contains("App/BadOrder"), msg)
        assert(msg.contains("prog_tests"), msg)
        assert(msg.contains("tests"), msg)
    }
  }

  test("tool test converts uncaught ProgTest errors into failures") {
    val progSrc =
      """package Bosatsu/Prog
|
|export Prog(), ProgTest(), pure, raise_error
|
|enum Prog[e, a]:
|  Pure(get: a)
|  Raise(get: e)
|
|def pure[a](a: a) -> forall e. Prog[e, a]:
|  Pure(a)
|
|def raise_error[e, a](e: e) -> Prog[e, a]:
|  Raise(e)
|
|struct ProgTest(test_fn: List[String] -> Prog[String, Test])
|""".stripMargin
    val appSrc =
      """package App/ProgBoom
|
|from Bosatsu/Prog import ProgTest, raise_error
|
|prog_tests = ProgTest(_ -> raise_error("boom"))
|""".stripMargin
    val files = List(
      Chain("Bosatsu", "Prog.bosatsu") -> progSrc,
      Chain("App", "ProgBoom.bosatsu") -> appSrc
    )
    val cmd = List(
      "tool",
      "test",
      "--input",
      "Bosatsu/Prog.bosatsu",
      "--input",
      "App/ProgBoom.bosatsu"
    )

    val result = for {
      s0 <- stateFromFiles(files)
      out <- runWithStateAndExit(cmd, s0)
    } yield out

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, Output.TestOutput(tests, _, _), exitCode)) =>
        assertEquals(exitCode, ExitCode.Error)
        val appTest = tests.collectFirst {
          case (pn, Some(test))
              if pn == PackageName.parts("App", "ProgBoom") =>
            test.value
        }.getOrElse(fail("missing App/ProgBoom test output"))
        assertEquals(appTest.assertions, 1)
        assertEquals(appTest.failureCount, 1)
        appTest match {
          case Test.Assertion(false, msg) =>
            assert(msg.contains("uncaught error"), msg)
            assert(msg.contains("boom"), msg)
          case other =>
            fail(s"expected synthetic assertion, got: $other")
        }
      case Right((_, other, _)) =>
        fail(s"expected test output, got: $other")
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
      s0 <- stateFromFiles(Nil)
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
