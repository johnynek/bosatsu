package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.implicits._
import dev.bosatsu.edn.Edn
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.library.{LibConfig, Libraries, Name, Version}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.tool.{
  CliException,
  CompilerApi,
  ExitCode,
  GraphOutput,
  LintMode,
  Output,
  PackageResolver,
  ShowEdn
}
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import munit.FunSuite
import org.typelevel.paiges.Doc
import scala.collection.immutable.SortedMap

class ToolAndLibCommandTest extends FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type StateIO[A] = MemoryMain.StateF[ErrorOr][A]
  private val module = MemoryMain[ErrorOr]
  private val validHash1 = "blake3:" + ("1" * 64)
  private val validHash2 = "blake3:" + ("2" * 64)
  private val validHash3 = "blake3:" + ("3" * 64)

  private def renderJson[A: Json.Writer](value: A): String =
    Json.Writer.write(value).render

  private def packageName(str: String): PackageName =
    PackageName.parse(str).getOrElse(fail(s"invalid package name: $str"))

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
      prefix: Chain[String]
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
    allFilePaths(state, Chain.empty).filter { path =>
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

  private def typeCheckWithLintMode(
      state: MemoryMain.State,
      inputs: NonEmptyList[Chain[String]],
      compileOptions: CompileOptions,
      lintMode: LintMode,
      compileCacheDirOpt: Option[Chain[String]]
  ): ErrorOr[(MemoryMain.State, (PackageMap.Compiled, NonEmptyList[
    (Chain[String], PackageName)
  ]))] =
    Par.noParallelism {
      given Par.EC = summon[Par.EC]
      CompilerApi
        .typeCheckWithLintMode[StateIO, Chain[String]](
          module.platformIO,
          inputs,
          Nil,
          Colorize.None,
          PackageResolver.ExplicitOnly(),
          compileOptions,
          lintMode,
          compileCacheDirOpt
        )
        .run(state)
    }

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

  private def resetLogs(state: MemoryMain.State): MemoryMain.State =
    state.copy(stdOut = Doc.empty, stdErr = Doc.empty)

  private def writeStringFile(
      state: MemoryMain.State,
      path: Chain[String],
      content: String
  ): MemoryMain.State =
    state
      .withFile(
        path,
        MemoryMain.FileContent.Str(withExplicitPackage(path, content))
      )
      .getOrElse(fail(s"failed to write ${path.mkString_("/")}"))

  private val lintOnlyLibSrc =
    """def keep(x: Int) -> Int:
      |  unused = x.add(1)
      |  x
      |
      |main = keep(1)
      |""".stripMargin

  private val unreachableLintLibSrc =
    """enum Value: V
      |
      |enum Result[e, r]: Err(err: e), Ok(ok: r)
      |
      |def has_unreachable(y: Result[Value, Value]) -> Value:
      |  match y:
      |    case _:
      |      V
      |    case Ok(_):
      |      V
      |
      |main = has_unreachable(Err(V))
      |""".stripMargin

  private val nonTotalLibSrc =
    """enum Value: V
      |
      |enum Result[e, r]: Err(err: e), Ok(ok: r)
      |
      |def non_total(y: Result[Value, Value]) -> Value:
      |  match y:
      |    case Ok(v):
      |      v
      |
      |main = non_total(Ok(V))
      |""".stripMargin

  private val mixedLintAndHardLibSrc =
    """enum Value: V
      |
      |enum Result[e, r]: Err(err: e), Ok(ok: r)
      |
      |def mixed(y: Result[Value, Value]) -> Value:
      |  unused = V
      |  match y:
      |    case Ok(v):
      |      v
      |
      |main = mixed(Ok(V))
      |""".stripMargin

  private val invalidPatternLibSrc =
    """x = "foo bar"
      |
      |main = match x:
      |  case "${_}${_}":
      |    "bad"
      |  case _:
      |    "still bad"
      |""".stripMargin

  private val recursionLintLibSrc =
    """def fn(x):
      |  recur x:
      |    case _:
      |      0
      |
      |main = fn(1)
      |""".stripMargin

  private val exposedDepLibSrc =
    """export Dep()
      |
      |struct Dep
      |""".stripMargin

  private val exposesMismatchLibSrc =
    """from Dep/Api import Dep
      |export Wrapped()
      |
      |struct Wrapped(value: Dep)
      |""".stripMargin

  private val hardRecursionErrorLibSrc =
    """enum Nat: Zero, Succ(prev: Nat)
      |
      |def len(lst):
      |  loop lst:
      |    case []: Zero
      |    case [_, *tail]: Succ(len(tail))
      |
      |main = len([])
      |""".stripMargin

  private val lintOnlyLibTestSrc =
    """export test_one
      |
      |def make_test():
      |  unused = 1
      |  Assertion(True, "ok")
      |
      |test_one = make_test()
      |""".stripMargin

  private val topLevelLintLibTestSrc =
    """from MyLib/Dep import dep
      |
      |export test_one
      |
      |helper = dep
      |test_one = Assertion(True, "ok")
      |""".stripMargin

  private val simpleOkLibSrc =
    """main = 1
      |""".stripMargin

  private val hardTypeErrorLibSrc =
    """main = missing_value
      |""".stripMargin

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

  private val fullProgModuleSrc: String =
    Predef.loadFileInCompile("test_workspace/Prog.bosatsu")

  private val progVarMainBody: String =
    """from Bosatsu/Prog import Main, await, get, new_var, pure, set, swap, update
|
|main = Main(_ -> (
|  v <- new_var(10).await()
|  _ <- set(v, 12).await()
|  swapped <- swap(v, 20).await()
|  summary <- update(v, current -> (mul(current, 2), add(current, swapped))).await()
|  final <- get(v).await()
|  pure(add(summary, sub(final, 30)))
|))
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

    val rendered = ShowEdn.showDoc(packs, Nil, packageNamesOnly = false).render(120)
    val parsed = Edn.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse show output: $err")
    }

    val packageEdn = parsed match {
      case EList(
            ESymbol("show") :: EKeyword("ir") :: _ :: EKeyword(
              "typed-passes"
            ) :: _ :: EKeyword("interfaces") :: _ :: EKeyword(
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

  private def typedShowValue(
      show: Output.ShowValue
  ): Output.ShowValue.Typed =
    show match {
      case typed: Output.ShowValue.Typed => typed
      case other                         => fail(s"expected typed show output, found: $other")
    }

  private def typedShowPackages(
      show: Output.ShowValue
  ): List[Package.Typed[Any]] =
    typedShowValue(show).packages

  private def typedShowInterfaces(
      show: Output.ShowValue
  ): List[Package.Interface] =
    typedShowValue(show).interfaces

  @annotation.unused
  private def matchlessShowValue(
      show: Output.ShowValue
  ): Output.ShowValue.Matchless =
    show match {
      case matchless: Output.ShowValue.Matchless => matchless
      case other                                => fail(s"expected Matchless show output, found: $other")
    }

  private def renderShow(show: Output.ShowValue): String =
    ShowEdn.showDoc(show).render(120)

  private def matchlessDefExpr(
      show: Output.ShowValue,
      packName: String,
      bindableName: String
  ): Matchless.Expr[?] = {
    val matchless = matchlessShowValue(show)
    val pack = matchless.packages.find(_.name.asString == packName).getOrElse {
      fail(s"missing Matchless package $packName")
    }

    pack.defs.collectFirst {
      case (name, expr) if name.sourceCodeRepr == bindableName => expr
    }.getOrElse(fail(s"missing Matchless def $packName::$bindableName"))
  }

  private def containsGlobalExpr(
      expr: Matchless.Expr[?],
      packName: String,
      bindableName: String
  ): Boolean = {
    val targetPack = packageName(packName)
    val targetName = Identifier.Name(bindableName)

    def loopExpr(ex: Matchless.Expr[?]): Boolean =
      ex match {
        case Matchless.Global(_, `targetPack`, `targetName`) =>
          true
        case Matchless.Lambda(captures, _, _, body) =>
          captures.exists(loopExpr) || loopExpr(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond) || loopExpr(effectExpr)
        case Matchless.App(fn, args) =>
          loopExpr(fn) || args.exists(loopExpr)
        case Matchless.Let(_, value, in) =>
          loopExpr(value) || loopExpr(in)
        case Matchless.LetMut(_, in) =>
          loopExpr(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond) || loopExpr(thenExpr) || loopExpr(elseExpr)
        case Matchless.SwitchVariant(on, _, cases, default) =>
          loopExpr(on) || cases.exists { case (_, branch) =>
            loopExpr(branch)
          } || default.exists(loopExpr)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond) || loopExpr(thenExpr)
        case Matchless.PrevNat(of) =>
          loopExpr(of)
        case ge: Matchless.GetEnumElement[?] =>
          loopExpr(ge.arg)
        case gs: Matchless.GetStructElement[?] =>
          loopExpr(gs.arg)
        case _ =>
          false
      }

    def loopBool(ex: Matchless.BoolExpr[?]): Boolean =
      ex match {
        case Matchless.EqualsLit(arg, _) =>
          loopExpr(arg)
        case Matchless.LtEqLit(arg, _) =>
          loopExpr(arg)
        case Matchless.EqualsNat(arg, _) =>
          loopExpr(arg)
        case Matchless.And(left, right) =>
          loopBool(left) || loopBool(right)
        case Matchless.CheckVariant(arg, _, _, _) =>
          loopExpr(arg)
        case Matchless.CheckVariantSet(arg, _, _, _) =>
          loopExpr(arg)
        case Matchless.SetMut(_, value) =>
          loopExpr(value)
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value) || loopBool(in)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in)
        case Matchless.TrueConst =>
          false
      }

    loopExpr(expr)
  }

  private def showJsonPackageNames(json: Json): List[String] =
    def jsonNameAtom(value: Json): Option[String] =
      value match {
        case Json.JString(name) => Some(name)
        case Json.JObject(("$str", Json.JString(name)) :: Nil) =>
          Some(name)
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

  private def showJsonIr(json: Json): String =
    json match {
      case Json.JObject(fields) =>
        fields.toMap.get("ir") match {
          case Some(Json.JString(value)) =>
            value
          case Some(Json.JObject(("$sym", Json.JString(value)) :: Nil)) =>
            value
          case other =>
            fail(s"expected show ir symbol, found: $other")
        }
      case other =>
        fail(s"expected show json object, found: $other")
    }

  private def showJsonFieldKeys(json: Json): Set[String] =
    json match {
      case Json.JObject(fields) => fields.iterator.map(_._1).toSet
      case other                => fail(s"expected show json object, found: $other")
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
        "show",
        "--repo_root",
        "repo",
        "--value",
        value,
        "--ir",
        "matchless",
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

  test("root eval parses to library mode") {
    module.run(List("eval", "--main", "MyLib/Foo")) match {
      case Left(help) =>
        fail(s"expected root eval to parse, got help: $help")
      case Right(_) =>
        ()
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

  test("eval/json/show use library context") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List("eval", "--repo_root", "repo", "--main", "MyLib/Foo")
    ) match {
      case Right(Output.EvaluationResult(_, _, _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    runWithFiles(files)(
      List("json", "write", "--repo_root", "repo", "--main", "MyLib/Foo")
    ) match {
      case Right(Output.JsonOutput(Json.JNumberStr("42"), _)) => ()
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    runWithFiles(files)(
      List("show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
      case Right(other) => fail(s"unexpected output: $other")
      case Left(err)    => fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
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

  test("show --package-names only emits package names") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--package-names"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val rendered = ShowEdn.showDoc(show).render(120)
        assert(rendered.contains("""(package :name "MyLib/Foo")"""), rendered)
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

  test("show --ir matchless quotes package names consistently in globals") {
    val helperSrc =
      """package MyLib/Helper
|
|export one
|
|def one(i: Int) -> Int:
|  i
|""".stripMargin
    val deepHelperSrc =
      """package MyLib/Deep/Helper
|
|export two
|
|def two(i: Int) -> Int:
|  i
|""".stripMargin
    val callerSrc =
      """package MyLib/Foo
|
|from MyLib/Helper import one
|from MyLib/Deep/Helper import two
|
|export use
|
|def use(flag: Bool, i: Int) -> Int:
|  if flag:
|    one(i)
|  else:
|    two(i)
|""".stripMargin
    val files =
      baseLibFiles(callerSrc) ++
        List(
          Chain("repo", "src", "MyLib", "Helper.bosatsu") -> helperSrc,
          Chain("repo", "src", "MyLib", "Deep", "Helper.bosatsu") -> deepHelperSrc
        )

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--ir",
        "matchless",
        "--disable-matchless-pass",
        "global-inlining"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val rendered = renderShow(show)
        assert(
          containsGlobalExpr(matchlessDefExpr(show, "MyLib/Foo", "use"), "MyLib/Helper", "one")
        )
        assert(
          containsGlobalExpr(
            matchlessDefExpr(show, "MyLib/Foo", "use"),
            "MyLib/Deep/Helper",
            "two"
          )
        )
        assert(rendered.contains("""(global "MyLib/Helper" one)"""), rendered)
        assert(rendered.contains("""(global "MyLib/Deep/Helper" two)"""), rendered)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test(
    "eval --main scopes local typechecking to selected package roots and transitive deps"
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
    "show --package scopes local typechecking to selected package roots and transitive deps"
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
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
        val imports = importItems(packs.headOption.getOrElse(fail("expected one package")))
        assert(imports.exists(_._1 == "MyLib/Dep"), imports.toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(Option(err.getMessage).getOrElse(err.toString))
    }
  }

  test("show invalid package name parse error includes package hint") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List("show", "--repo_root", "repo", "--package", "euler1")
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

  test("show includes original and local import names") {
    val src =
      """from Bosatsu/Predef import add as operator +, mul as operator *
|
|main = 1 + (2 * 3)
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List("show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
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

  test("show separates exported types and values to avoid name collisions") {
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
      List("show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
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

  test("show supports --type and --value selectors") {
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
        "show",
        "--repo_root",
        "repo",
        "--type",
        "MyLib/Foo::Box",
        "--value",
        "MyLib/Foo::helper"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
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

  test("show --value validates that the selected value exists") {
    val src =
      """main = 42
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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

  test("show --no-opt retains values removed by optimization") {
    val src =
      """helper = 1
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::helper",
        "--no-opt"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assert(packageDefNames(pack).contains("helper"), packageDefNames(pack).toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("show reports typed IR metadata and explicit typed pass disables") {
    val src =
      """helper = 1
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List("show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        assertEquals(
          typedShowValue(show).typedPasses.map(_.cliName),
          List("loop-recur-lowering", "normalize", "discard-unused")
        )
        val rendered = renderShow(show)
        assert(rendered.contains(":ir"), rendered)
        assert(rendered.contains("typedexpr"), rendered)
        assert(rendered.contains(":typed-passes"), rendered)
        assert(rendered.contains("loop-recur-lowering"), rendered)
        assert(rendered.contains("normalize"), rendered)
        assert(rendered.contains("discard-unused"), rendered)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::helper",
        "--disable-typed-pass",
        "discard-unused"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        assertEquals(
          typedShowValue(show).typedPasses.map(_.cliName),
          List("loop-recur-lowering", "normalize")
        )
        val packs = typedShowPackages(show)
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assertEquals(packageDefNames(pack), List("helper"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("show --validate-typedexpr validates typed IR before rendering") {
    val src =
      """helper = 1
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--validate-typedexpr"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        assertEquals(packs.map(_.name.asString), List("MyLib/Foo"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("show --validate-typedexpr rejects --ir matchless") {
    val src =
      """main = 42
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--ir",
        "matchless",
        "--validate-typedexpr"
      )
    ) match {
      case Right(other) =>
        fail(s"expected validation option error, got output: $other")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("--validate-typedexpr requires --ir typedexpr"), msg)
    }
  }

  test(
    "show --validate-typedexpr validates dependency packages against their scoped dependency versions"
  ) {
    val rootMainSrc =
      """from Foo/Util import helper
|
|main = helper
|""".stripMargin
    val rootUtilSrc =
      """export helper
|
|helper = "root"
|""".stripMargin
    val depUtilSrc =
      """export helper
|
|helper = 1
|""".stripMargin
    val depClientSrc =
      """from Foo/Util import helper
|
|main = helper
|""".stripMargin

    val files = baseLibFiles(rootMainSrc) ++ List(
      Chain("repo", "src", "Foo", "Util.bosatsu") -> rootUtilSrc,
      Chain("dep_src", "Foo", "Util.bosatsu") -> depUtilSrc,
      Chain("dep_src", "Foo", "Client.bosatsu") -> depClientSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
          "tool",
          "check",
          "--input",
          "dep_src/Foo/Util.bosatsu",
          "--input",
          "dep_src/Foo/Client.bosatsu",
          "--output",
          "out/dep.packages"
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
          "out/dep.packages",
          "--output",
          "out/dep.bosatsu_lib"
        ),
        state1
      )
      (state2, _) = s2
      depLib = readLibraryFile(state2, Chain("out", "dep.bosatsu_lib"))
      s3 <- runWithState(
        List(
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
      s4 <- runWithState(
        List(
          "show",
          "--repo_root",
          "repo",
          "--package",
          "MyLib/Foo",
          "--package",
          "Foo/Client",
          "--validate-typedexpr"
        ),
        state4
      )
    } yield s4

    result match {
      case Left(err) =>
        fail(Option(err.getMessage).getOrElse(err.toString))
      case Right((_, Output.ShowOutput(show, _))) =>
        val names = typedShowPackages(show).map(_.name.asString).toSet
        assertEquals(names, Set("MyLib/Foo", "Foo/Client"))
      case Right((_, other)) =>
        fail(s"unexpected output: $other")
    }
  }

  test("show avoids exploding repeated non-lambda global calls") {
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

  test("show --value includes local value dependencies and only needed imports") {
    val src =
      """from Bosatsu/Predef import add, mul
|
|def main(x):
|  add(x, 2)
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::main"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
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

  test("show --value includes local types required by constructor usage") {
    val src =
      """struct Box(value: Int)
|
|helper = Box(1)
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Foo::main"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        val pack = packs.headOption.getOrElse(fail("expected one package"))
        assert(packageTypeNames(pack).contains("Box"), packageTypeNames(pack).toString)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("show --externals only includes external values with types") {
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
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--externals"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        val interfaces = typedShowInterfaces(show)
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

  test("library show --ir matchless omits typed sections and reports matchless json") {
    val src =
      """struct Box(value: Int)
|
|helper = Box(1)
|main = helper
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--ir",
        "matchless"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val matchless = matchlessShowValue(show)
        assertEquals(
          matchless.matchlessPasses.map(_.cliName),
          List(
            "hoist-invariant-loop-lets",
            "reuse-constructors",
            "global-inlining"
          )
        )
        val rendered = renderShow(show)
        assert(rendered.contains(":ir"), rendered)
        assert(rendered.contains("matchless"), rendered)
        assert(!rendered.contains(":interfaces"), rendered)
        assert(!rendered.contains(":types"), rendered)
        assert(rendered.contains("(def main"), rendered)
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
        "show",
        "--repo_root",
        "repo",
        "--package",
        "MyLib/Foo",
        "--ir",
        "matchless",
        "--json"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(showJsonIr(json), "matchless")
        assertEquals(showJsonPackageNames(json), List("MyLib/Foo"))
        assert(!showJsonFieldKeys(json).contains("interfaces"))
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
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
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
      case Right(Output.ShowOutput(show, _)) =>
        val packs = typedShowPackages(show)
        val interfaces = typedShowInterfaces(show)
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

  test("tool show --ir matchless can disable global inlining") {
    val helperSrc =
      """package App/Helper
|
|export choose
|
|def choose(flag: Bool, on_true: Int) -> Int:
|  if flag:
|    on_true
|  else:
|    0
|""".stripMargin
    val callerSrc =
      """package App/Caller
|
|from App/Helper import choose
|
|def expensive(i: Int) -> Int:
|  if False:
|    0
|  else:
|    i
|
|def use(i: Int) -> Int:
|  choose(False, expensive(i))
|""".stripMargin
    val files = List(
      Chain("src", "App", "Helper.bosatsu") -> helperSrc,
      Chain("src", "App", "Caller.bosatsu") -> callerSrc
    )

    runWithFiles(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Helper.bosatsu",
        "--input",
        "src/App/Caller.bosatsu",
        "--value",
        "App/Caller::use",
        "--ir",
        "matchless"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val rendered = renderShow(show)
        assert(rendered.contains(":matchless-passes"), rendered)
        assert(
          !containsGlobalExpr(matchlessDefExpr(show, "App/Caller", "use"), "App/Helper", "choose")
        )
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Helper.bosatsu",
        "--input",
        "src/App/Caller.bosatsu",
        "--value",
        "App/Caller::use",
        "--ir",
        "matchless",
        "--disable-matchless-pass",
        "global-inlining"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        assert(
          containsGlobalExpr(matchlessDefExpr(show, "App/Caller", "use"), "App/Helper", "choose")
        )
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }

    runWithFiles(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Helper.bosatsu",
        "--input",
        "src/App/Caller.bosatsu",
        "--value",
        "App/Caller::use",
        "--ir",
        "matchless",
        "--json"
      )
    ) match {
      case Right(Output.JsonOutput(json, _)) =>
        assertEquals(showJsonIr(json), "matchless")
        assertEquals(showJsonPackageNames(json), List("App/Caller"))
      case Right(other) =>
        fail(s"unexpected output: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("tool show --ir matchless inlines tiny pure value helpers") {
    val helperSrc =
      """package App/Helper
|
|export Pair(), pair
|
|struct Pair(left: Int, right: Int)
|
|pair = Pair(1, 2)
|""".stripMargin
    val callerSrc =
      """package App/Caller
|
|from App/Helper import Pair, pair
|
|use =
|  match pair:
|    case Pair(_, x):
|      x
|""".stripMargin
    val files = List(
      Chain("src", "App", "Helper.bosatsu") -> helperSrc,
      Chain("src", "App", "Caller.bosatsu") -> callerSrc
    )

    runWithFiles(files)(
      List(
        "tool",
        "show",
        "--input",
        "src/App/Helper.bosatsu",
        "--input",
        "src/App/Caller.bosatsu",
        "--value",
        "App/Caller::use",
        "--ir",
        "matchless"
      )
    ) match {
      case Right(Output.ShowOutput(show, _)) =>
        val expr = matchlessDefExpr(show, "App/Caller", "use")
        assert(!containsGlobalExpr(expr, "App/Helper", "pair"))
        assertEquals(expr, Matchless.Literal(Lit.fromInt(2)))
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

  test("eval missing value reports CliException without stack trace") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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

  test("eval handles parse/sort repro without StackOverflowError") {
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
|  loop xs:
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
|  def go(i: Int, acc: String) -> String:
|    loop i:
|      case _ if cmp_Int(i, 0) matches GT:
|        part = quoted(i)
|        next =
|          if acc matches "":
|            part
|          else:
|            "${part},${acc}"
|        go(i.sub(1), next)
|      case _:
|        acc
|
|  go(n, "")
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

  test("tool eval --run supports Bosatsu/Prog Var in Main") {
    val appSrc = s"package Tool/VarMain\n\n$progVarMainBody"
    val files = List(
      Chain("src", "Bosatsu", "Prog.bosatsu") -> fullProgModuleSrc,
      Chain("src", "Tool", "VarMain.bosatsu") -> appSrc
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
          "tool",
          "eval",
          "--run",
          "--main",
          "Tool/VarMain",
          "--input",
          "src/Bosatsu/Prog.bosatsu",
          "--input",
          "src/Tool/VarMain.bosatsu"
        ),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((_, out, exitCode)) =>
        assertEquals(exitCode, ExitCode.fromInt(42))
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
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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
    "eval --run executes Bosatsu/Prog::Main and includes synthetic argv[0]"
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

  test("eval --run merges positional and -- passthrough args in order") {
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

  test("eval without --run rejects -- passthrough args") {
    val src =
      """main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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

  test("eval --run supports Bosatsu/Prog observe in Main") {
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

  test("eval --run supports Bosatsu/Prog Var in Main") {
    val files =
      baseLibFiles(progVarMainBody) :+ (
        Chain("repo", "src", "Bosatsu", "Prog.bosatsu") -> fullProgModuleSrc
      )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithStateAndExit(
        List(
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
        assertEquals(exitCode, ExitCode.fromInt(42))
        out match {
          case Output.RunMainResult(_) => ()
          case other                   => fail(s"unexpected output: $other")
        }
    }
  }

  test("eval --run reads stdin for IO/Std read_line and read_all_stdin") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export (Handle, stdin, stdout, read_utf8, write_utf8, flush)
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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

  test("eval --run handles now_mono duration_to_nanos newtype representation") {
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
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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

  test("eval --run handles now_wall instant newtype representation") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export Instant, now_wall, instant_to_nanos
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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

  test("eval --run handles list_dir path newtype representation") {
    val ioCoreSrc =
      """package Bosatsu/IO/Core
|
|from Bosatsu/Prog import Prog
|from Bosatsu/IO/Error import IOError
|
|export Path, string_to_Path, path_to_String, list_dir
|exposes Bosatsu/IO/Error, Bosatsu/Prog
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

  test("eval constructor main reports actionable parse error") {
    val src =
      """enum Flag: True, False
main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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

  test("json constructor main reports actionable parse error") {
    val src =
      """enum Flag: True, False
main = 42
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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
exposes Dep/Util

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
|exposes Dep/Util
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
|exposes Dep/Util
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

  test("doc writes markdown in package directory layout") {
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

  test("doc --source_repo_url includes source code links") {
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

  test("doc --remote_doc_links_html rewrites dependency links to .html") {
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
|exposes Dep/Util
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

  test("doc keeps dependency links as .md when html rewrite flag is off") {
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
|exposes Dep/Util
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

  test("doc prefers local Predef links when included and remote when not") {
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
        assert(predefDoc.contains("<a id=\"type-bool\"></a>"), predefDoc)
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
        List(
          "[`int_loop`](#value-int-loop)",
          "[`range_fold`](#value-range-fold)",
          "[`uncurry2`](#value-uncurry2)",
          "[`uncurry3`](#value-uncurry3)",
          "<a id=\"value-int-loop\"></a>",
          "def int_loop",
          "def range_fold",
          "def uncurry2",
          "def uncurry3"
        ).foreach { removed =>
          assert(!predefDoc.contains(removed), predefDoc)
        }
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

  test("doc --include_predef includes Bosatsu/Predef markdown") {
    val src =
      """export main,
main = 1
"""
    val files = baseLibFiles(src)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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

  test("doc labels private packages and can exclude them") {
    val publicPack = PackageName.parts("MyLib", "Public")
    val privatePack = PackageName.parts("MyLib", "Private")
    val conf = LibConfig(
      name = Name("mylib"),
      repo_uri = "https://example.com",
      next_version = Version(0, 0, 1),
      previous = None,
      exported_packages = LibConfig.PackageFilter.Name(publicPack) :: Nil,
      all_packages =
        LibConfig.PackageFilter.Name(publicPack) ::
          LibConfig.PackageFilter.Name(privatePack) ::
          Nil,
      public_deps = Nil,
      private_deps = Nil,
      default_main = None
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
          case Output.ShowOutput(show, _) =>
            val packs = typedShowPackages(show)
            assertEquals(packs.map(_.name.asString), List("Dep/Foo"))
          case other =>
            fail(s"unexpected show output: $other")
        }
    }
  }

  test("trimmed core_alpha exports stay visible through dependency interfaces") {
    val exportedPackages = List(
      "Bosatsu/Char",
      "Bosatsu/Collection/Array",
      "Bosatsu/Eval",
      "Bosatsu/IO/Bytes",
      "Bosatsu/IO/Core",
      "Bosatsu/IO/Error",
      "Bosatsu/IO/Std",
      "Bosatsu/Json",
      "Bosatsu/Lazy",
      "Bosatsu/Num/Float64",
      "Bosatsu/Prog"
    ).map(packageName)

    val conf = LibConfig
      .init(Name("core_alpha"), "https://example.com", Version(6, 0, 0))
      .copy(
        exported_packages = exportedPackages.map(LibConfig.PackageFilter.Name(_))
      )

    val allowedSrc =
      """package App/Allowed
|
|from Bosatsu/IO/Core import core_error
|from Bosatsu/IO/Std import std_summary
|from Bosatsu/Json import JNull
|from Bosatsu/Prog import Main
|
|_ = JNull
|_ = std_summary
|_ = core_error
|main = Main(1)
|""".stripMargin
    val blockedSrc =
      """package App/Blocked
|
|from Bosatsu/Num/Nat import Nat, Zero
|
|bad: Nat = Zero
|main = bad
|""".stripMargin
    val files = List(
      Chain("app", "App", "Allowed.bosatsu") -> allowedSrc,
      Chain("app", "App", "Blocked.bosatsu") -> blockedSrc,
      Chain("repo", "bosatsu_libs.json") -> renderJson(
        Libraries(SortedMap(Name("core_alpha") -> "src"))
      ),
      Chain("repo", "src", "core_alpha_conf.json") -> renderJson(conf),
      Chain("repo", "src", "Bosatsu", "Char.bosatsu") ->
        """package Bosatsu/Char
|
|export char_tag
|
|char_tag = .'x'
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Collection", "Array.bosatsu") ->
        """package Bosatsu/Collection/Array
|
|from Bosatsu/Char import char_tag
|
|export array_char
|
|array_char = char_tag
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Eval.bosatsu") ->
        """package Bosatsu/Eval
|
|export EvalInt(), eval_value
|
|struct EvalInt(value: Int)
|
|eval_value = EvalInt(1)
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "IO", "Bytes.bosatsu") ->
        """package Bosatsu/IO/Bytes
|
|export Bytes(), bytes_value
|
|struct Bytes(size: Int)
|
|bytes_value = Bytes(1)
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "IO", "Core.bosatsu") ->
        """package Bosatsu/IO/Core
|
|from Bosatsu/Char import char_tag
|from Bosatsu/IO/Error import IOError, sample_error
|
|export core_char, core_error
|exposes Bosatsu/IO/Error
|
|core_char = char_tag
|core_error: IOError = sample_error
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "IO", "Error.bosatsu") ->
        """package Bosatsu/IO/Error
|
|export IOError(), sample_error
|
|enum IOError:
|  Sample
|
|sample_error = Sample
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "IO", "Std.bosatsu") ->
      """package Bosatsu/IO/Std
|
|from Bosatsu/IO/Core import core_char, core_error
|
|export std_summary
|exposes Bosatsu/IO/Error
|
|std_summary = (core_char, core_error)
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Json.bosatsu") ->
        """package Bosatsu/Json
|
|from Bosatsu/Char import char_tag
|
|export Json(), Optional(), Nullable(), json_char
|
|enum Json:
|  JNull
|
|enum Optional[a]:
|  Missing
|  Set(value: a)
|
|enum Nullable[a]:
|  Null
|  NonNull(value: a)
|
|json_char = char_tag
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Lazy.bosatsu") ->
        """package Bosatsu/Lazy
|
|export LazyInt(), lazy_value
|
|struct LazyInt(value: Int)
|
|lazy_value = LazyInt(1)
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Num", "Float64.bosatsu") ->
        """package Bosatsu/Num/Float64
|
|export float_tag
|
|float_tag = 1.0
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Num", "Nat.bosatsu") ->
        """package Bosatsu/Num/Nat
|
|export Nat()
|
|enum Nat:
|  Zero
|""".stripMargin,
      Chain("repo", "src", "Bosatsu", "Prog.bosatsu") ->
        """package Bosatsu/Prog
|
|export Main()
|
|struct Main(value: Int)
|""".stripMargin
    )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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
      (state1, _) = s1
      depPath = Chain("out", "core_alpha-v6.0.0.bosatsu_lib")
      s2 <- runWithState(
        List(
          "tool",
          "check",
          "--input",
          "app/App/Allowed.bosatsu",
          "--pub_dep",
          "out/core_alpha-v6.0.0.bosatsu_lib",
          "--output",
          "out/App.Allowed.bosatsu_package"
        ),
        state1
      )
      (state2, _) = s2
    } yield (state2, depPath)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, depPath)) =>
        val _ = readLibraryFile(state, depPath)

        runWithState(
          List(
            "tool",
            "check",
            "--input",
            "app/App/Blocked.bosatsu",
            "--pub_dep",
            "out/core_alpha-v6.0.0.bosatsu_lib",
            "--output",
            "out/App.Blocked.bosatsu_package"
          ),
          state
        ) match {
          case Right((_, out)) =>
            fail(
              s"expected dependency interface to hide Bosatsu/Num/Nat, got: $out"
            )
          case Left(err) =>
            val msg = Option(err.getMessage).getOrElse(err.toString)
            assert(msg.contains("Bosatsu/Num/Nat"), msg)
            assert(msg.toLowerCase.contains("unknown package"), msg)
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
exposes Dep/Foo

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

  test("tool check rejects todo and tool show still rejects it") {
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
      case Right(out) =>
        fail(s"expected strict todo rejection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("Unknown name"), msg)
        assert(msg.contains("todo"), msg)
        assert(msg.contains("check --warn"), msg)
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
        assert(msg.contains("check --warn"), msg)
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
            val appName = PackageName.parts("Cache", "App")
            def appPack(packs: List[Package.Compiled]): Package.Compiled =
              packs.find(_.name == appName).getOrElse {
                fail(s"missing Cache/App in ${packs.map(_.name)}")
              }

            def mainRegion(pack: Package.Compiled): Region =
              pack.lets
                .collectFirst {
                  case (Identifier.Name("main"), _, expr) =>
                    HasRegion.region(expr)
                }
                .getOrElse(fail(s"missing Cache/App::main in ${pack.name}"))

            assertEquals(packs2.map(_.name), packs1.map(_.name))
            assertEquals(mainRegion(appPack(packs2)), mainRegion(appPack(packs1)))
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
        val keyFiles1 = filePathsUnder(state1, keyPrefix)
        val keyFiles2 = filePathsUnder(state2, keyPrefix)
        val casFiles1 = filePathsUnder(state1, casPrefix)
        val casFiles2 = filePathsUnder(state2, casPrefix)
        assertNotEquals(keyFiles2, keyFiles1)
        assert(casFiles1.subsetOf(casFiles2), (casFiles1, casFiles2).toString)
        assert(casFiles2.size > casFiles1.size, (casFiles1, casFiles2).toString)
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

  test("check --cache_dir writes cache artifacts and reuses them") {
    val files = baseLibFiles("main = 1\n")
    val cmd = List(
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
    "check without cache flags writes default cache artifacts and reuses them"
  ) {
    val files = baseLibFiles("main = 1\n")
    val cmd = List(
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

  test("check --no_cache does not write infer cache artifacts") {
    val files = baseLibFiles("main = 1\n")
    val cmd = List(
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

  test("check rejects --cache_dir with --no_cache") {
    val cmd = List(
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

  test("check --warn reports lint warnings on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(lintOnlyLibSrc))
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        val err1 = state1.stdErr.render(200)
        val err2 = state2.stdErr.render(200)
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assert(err1.contains("unused value 'unused'"), err1)
        assert(err1.contains("1 warning: 1 unused value"), err1)
        assertEquals(err2, err1)
    }
  }

  test("check --lax suppresses lint warnings on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--lax"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(lintOnlyLibSrc))
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assertEquals(state1.stdErr.render(200), "")
        assertEquals(state2.stdErr.render(200), "")
    }
  }

  test("check --warn reports recursion-form warnings on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(recursionLintLibSrc))
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        val err1 = state1.stdErr.render(400)
        val err2 = state2.stdErr.render(400)
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assert(err1.contains("recur but no recursive call to fn."), err1)
        assert(err1.contains("1 warning: 1 recursion form"), err1)
        assertEquals(err2, err1)
    }
  }

  test("check --warn reports exposes mismatches on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )

    val files =
      baseLibFiles(exposesMismatchLibSrc) :+
        (Chain("repo", "src", "Dep", "Api.bosatsu") -> exposedDepLibSrc)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        val err1 = state1.stdErr.render(400)
        val err2 = state2.stdErr.render(400)
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assert(err1.contains("declared `exposes` does not match"), err1)
        assert(err1.contains("declared here:"), err1)
        assert(err1.contains("no `exposes` declaration found."), err1)
        assert(err1.contains("canonical fix:"), err1)
        assert(err1.contains("exposes Dep/Api."), err1)
        assert(!err1.contains("\nactual:"), err1)
        assertEquals(err2, err1)
    }
  }

  test("check --lax suppresses exposes mismatch warnings on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--lax"
    )

    val files =
      baseLibFiles(exposesMismatchLibSrc) :+
        (Chain("repo", "src", "Dep", "Api.bosatsu") -> exposedDepLibSrc)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assertEquals(state1.stdErr.render(200), "")
        assertEquals(state2.stdErr.render(200), "")
    }
  }

  test("check --lax suppresses recursion-form warnings on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--lax"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(recursionLintLibSrc))
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assertEquals(state1.stdErr.render(200), "")
        assertEquals(state2.stdErr.render(200), "")
    }
  }

  test("check --warn keeps todo warnings stable on cache hits") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles("main = todo(1)\n"))
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        val err1 = state1.stdErr.render(400)
        val err2 = state2.stdErr.render(400)
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assert(err1.contains("temporary `todo` placeholder used here"), err1)
        assert(err1.contains("1 warning: 1 todo usage"), err1)
        assert(err1.contains("check --warn"), err1)
        assertEquals(err2, err1)
    }
  }

  test("check --lax allows todo without warnings") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--lax"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles("main = todo(1)\n"))
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      s2 <- runWithState(cmd, resetLogs(state1))
      (state2, out2) = s2
    } yield (state1, state2, out1, out2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2, out1, out2)) =>
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assertEquals(out2, Output.Basic(Doc.text(""), None))
        assertEquals(state1.stdErr.render(200), "")
        assertEquals(state2.stdErr.render(200), "")
    }
  }

  test("check strict mode still fails on lint after a prior --lax cache hit") {
    val laxCmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--lax"
    )
    val strictCmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(lintOnlyLibSrc))
      s1 <- runWithState(laxCmd, s0)
      (state1, _) = s1
      s2 <- runWithState(strictCmd, resetLogs(state1))
    } yield s2

    result match {
      case Right((_, out)) =>
        fail(s"expected strict lint failure after cache warmup, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("unused value 'unused'"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("check strict mode still fails on recursion-form lint after a prior --warn cache hit") {
    val warnCmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )
    val strictCmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(recursionLintLibSrc))
      s1 <- runWithState(warnCmd, s0)
      (state1, _) = s1
      s2 <- runAndReportWithState(strictCmd, resetLogs(state1))
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        val errOutput = state.stdErr.render(400)
        assertEquals(exitCode, ExitCode.Error)
        assert(errOutput.contains("recur but no recursive call to fn."), errOutput)
        assert(errOutput.contains("Use `match` for non-recursive branching."), errOutput)
    }
  }

  test("check strict mode still rejects todo after a prior --warn cache hit") {
    val warnCmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )
    val strictCmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles("main = todo(1)\n"))
      s1 <- runWithState(warnCmd, s0)
      (state1, _) = s1
      s2 <- runAndReportWithState(strictCmd, resetLogs(state1))
    } yield s2

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        val errOutput = state.stdErr.render(400)
        assertEquals(exitCode, ExitCode.Error)
        assert(errOutput.contains("Unknown name"), errOutput)
        assert(errOutput.contains("todo"), errOutput)
        assert(errOutput.contains("check --warn"), errOutput)
    }
  }

  test("check rejects --warn with --lax") {
    module.run(
      List(
        "check",
        "--repo_root",
        "repo",
        "--warn",
        "--lax"
      )
    ) match {
      case Left(_)  => ()
      case Right(_) =>
        fail("expected parser error when both --warn and --lax are supplied")
    }
  }

  test("check --warn postpones unreachable branch diagnostics") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--warn"
    )

    val result = for {
      s0 <- stateFromFiles(baseLibFiles(unreachableLintLibSrc))
      s1 <- runWithState(cmd, s0)
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out)) =>
        val errOutput = state.stdErr.render(200)
        assertEquals(out, Output.Basic(Doc.text(""), None))
        assert(errOutput.contains("unreachable branches"), errOutput)
        assert(errOutput.contains("1 warning: 1 unreachable branch"), errOutput)
    }
  }

  test("check --warn keeps non-total matches fatal") {
    runWithFiles(baseLibFiles(nonTotalLibSrc))(
      List("check", "--repo_root", "repo", "--warn")
    ) match {
      case Right(out) =>
        fail(s"expected non-total match failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("non-total match"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("check --warn prints lint warnings before failing on hard errors") {
    val result =
      for {
        s0 <- stateFromFiles(baseLibFiles(mixedLintAndHardLibSrc))
        s1 <- runAndReportWithState(
          List("check", "--repo_root", "repo", "--warn"),
          s0
        )
      } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, exitCode)) =>
        val errOutput = state.stdErr.render(200)
        assertEquals(exitCode, ExitCode.Error)
        assert(errOutput.contains("unused value 'unused'"), errOutput)
        assert(errOutput.contains("non-total match"), errOutput)
        val warningIdx = errOutput.indexOf("unused value 'unused'")
        val errorIdx = errOutput.indexOf("non-total match")
        assert(warningIdx >= 0 && warningIdx < errorIdx, errOutput)
    }
  }

  test("check --lax keeps invalid-pattern totality errors fatal") {
    runWithFiles(baseLibFiles(invalidPatternLibSrc))(
      List("check", "--repo_root", "repo", "--lax")
    ) match {
      case Right(out) =>
        fail(s"expected invalid pattern failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("invalid string pattern"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("check --lax keeps recursion errors fatal for recursion-form sources") {
    runWithFiles(baseLibFiles(hardRecursionErrorLibSrc))(
      List("check", "--repo_root", "repo", "--lax")
    ) match {
      case Right(out) =>
        fail(s"expected recursion failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(
          rendered.contains("loop requires all recursive calls to len to be in tail position."),
          rendered
        )
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("check --lax keeps recursion errors fatal") {
    runWithFiles(baseLibFiles(hardRecursionErrorLibSrc))(
      List("check", "--repo_root", "repo", "--lax")
    ) match {
      case Right(out) =>
        fail(s"expected recursion failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(
          rendered.contains("loop requires all recursive calls to len to be in tail position."),
          rendered
        )
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("test warn and lax modes continue past lint-only compile diagnostics") {
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
      baseLibFiles(lintOnlyLibTestSrc) :+ (Chain("repo", "cc_conf.json") -> ccConfJson)

    runWithFiles(files)(
      List("test", "--repo_root", "repo", "--cc_conf", "repo/cc_conf.json")
    ) match {
      case Right(out) =>
        fail(s"expected strict lint failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("unused value 'unused'"), msg)
    }

    runWithFiles(files)(
      List(
        "test",
        "--repo_root",
        "repo",
        "--cc_conf",
        "repo/cc_conf.json",
        "--warn"
      )
    ) match {
      case Right(out) =>
        fail(s"expected memory-mode runtime failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("system not supported in memory mode"), msg)
        assert(!msg.contains("unused value 'unused'"), msg)
    }

    runWithFiles(files)(
      List(
        "test",
        "--repo_root",
        "repo",
        "--cc_conf",
        "repo/cc_conf.json",
        "--lax"
      )
    ) match {
      case Right(out) =>
        fail(s"expected memory-mode runtime failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("system not supported in memory mode"), msg)
        assert(!msg.contains("unused value 'unused'"), msg)
    }
  }

  test("warn lint replay stays stable across optimize cache hits used by test") {
    val inputs = NonEmptyList.of(
      Chain("repo", "src", "MyLib", "Foo.bosatsu"),
      Chain("repo", "src", "MyLib", "Dep.bosatsu")
    )
    val files =
      baseLibFiles(topLevelLintLibTestSrc) ++
        List(
          Chain("repo", "src", "MyLib", "Dep.bosatsu") ->
            """export dep
              |
              |dep = 1
              |""".stripMargin
        )

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- typeCheckWithLintMode(
        s0,
        inputs,
        CompileOptions.Default,
        LintMode.Warn,
        Some(Chain("cache"))
      )
      (state1, _) = s1
      s2 <- typeCheckWithLintMode(
        resetLogs(state1),
        inputs,
        CompileOptions.Default,
        LintMode.Warn,
        Some(Chain("cache"))
      )
      (state2, _) = s2
    } yield (state1, state2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, state2)) =>
        val err1 = state1.stdErr.render(400)
        val err2 = state2.stdErr.render(400)
        assert(err1.contains("unused value 'helper'"), err1)
        assert(err2.contains("unused value 'helper'"), err2)
        assert(!err1.contains("unused import"), err1)
        assert(!err2.contains("unused import"), err2)
    }
  }

  test("check --warn still prints cached lint when another package fails hard") {
    val cmd = List(
      "check",
      "--repo_root",
      "repo",
      "--cache_dir",
      "cache",
      "--warn"
    )
    val initialFiles =
      baseLibFiles(lintOnlyLibSrc) :+
        (Chain("repo", "src", "MyLib", "Bar.bosatsu") -> simpleOkLibSrc)

    val result = for {
      s0 <- stateFromFiles(initialFiles)
      s1 <- runWithState(cmd, s0)
      (state1, out1) = s1
      updated = writeStringFile(
        resetLogs(state1),
        Chain("repo", "src", "MyLib", "Bar.bosatsu"),
        hardTypeErrorLibSrc
      )
      s2 <- runAndReportWithState(cmd, updated)
    } yield (state1, out1, s2)

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state1, out1, (state2, exitCode))) =>
        val err1 = state1.stdErr.render(200)
        val err2 = state2.stdErr.render(400)
        assertEquals(out1, Output.Basic(Doc.text(""), None))
        assert(err1.contains("unused value 'unused'"), err1)
        assertEquals(exitCode, ExitCode.Error)
        assert(err2.contains("unused value 'unused'"), err2)
        assert(err2.contains("missing_value"), err2)
        val warningIdx = err2.indexOf("unused value 'unused'")
        val errorIdx = err2.indexOf("missing_value")
        assert(warningIdx >= 0 && warningIdx < errorIdx, err2)
    }
  }

  test("test strict, warn, and lax modes still reject todo") {
    val ccConfJson =
      """{
        |  "cc_path": "cc",
        |  "flags": [],
        |  "iflags": [],
        |  "libs": [],
        |  "os": "test"
        |}
        |""".stripMargin
    val todoTestSrc =
      """export test_one
        |
        |test_one = todo(Assertion(True, "later"))
        |""".stripMargin

    val files =
      baseLibFiles(todoTestSrc) :+ (Chain("repo", "cc_conf.json") -> ccConfJson)

    List(
      "strict" -> Nil,
      "warn" -> List("--warn"),
      "lax" -> List("--lax")
    ).foreach { case (modeLabel, modeFlags) =>
      runWithFiles(files)(
        List(
          "test",
          "--repo_root",
          "repo",
          "--cc_conf",
          "repo/cc_conf.json"
        ) ::: modeFlags
      ) match {
        case Right(out) =>
          fail(s"expected todo failure in $modeLabel mode, got: $out")
        case Left(err) =>
          val msg = Option(err.getMessage).getOrElse(err.toString)
          assert(msg.contains("todo"), msg)
          assert(msg.contains("check --warn"), msg)
      }
    }
  }

  test("deps list text output includes public and private sections") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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
        List("deps", "list", "--repo_root", "repo"),
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

  test("json apply and traverse use CliException paths") {
    val src =
      """main = (x) -> x.add(1)
"""
    val files = baseLibFiles(src) :+ (Chain("repo", "in.json") -> "[41]")

    runWithFiles(files)(
      List(
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

  test("json write/apply/traverse support --yaml output") {
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

  test("json write rejects unsupported function output") {
    val src =
      """main = (x: Int) -> x
"""
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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

  test("json write supports Char, Array, and Bytes external values") {
    val charSrc =
      """main = match int_to_Char(127):
  case Some(c): c
  case None: .'?'
"""
    val charFiles = baseLibFiles(charSrc)

    runWithFiles(charFiles)(
      List(
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

  test("json write/apply supports Bosatsu/Json::Json directly") {
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
|exposes Bosatsu/Json
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

  test("json apply accepts Float64 encoded as strings") {
    val src =
      """main = (x: Float64) -> x
|""".stripMargin
    val files = baseLibFiles(src)

    runWithFiles(files)(
      List(
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

  test("json Optional fields omit absent keys and decode missing keys") {
    val src =
      """from Bosatsu/Json import Optional, Missing, Set
|
|export Payload(), absent_payload, present_payload, echo
|exposes Bosatsu/Json
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

  test("json Nullable flattens nested nullability") {
    val src =
      """from Bosatsu/Json import Nullable, Null, NonNull
|
|export flat_null, echo
|exposes Bosatsu/Json
|
|flat_null: Nullable[Nullable[Int]] = NonNull(Null)
|echo = (n: Nullable[Nullable[Int]]) -> n
|""".stripMargin
    val files = withBosatsuJsonModule(baseLibFiles(src))

    runWithFiles(files)(
      List(
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

  test("json apply supports Char, Array, and Bytes arguments") {
    val charFnSrc =
      """main = (c: Char) -> (c, c)
"""
    val charFnFiles = baseLibFiles(charFnSrc)

    runWithFiles(charFnFiles)(
      List(
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

  test("json write reports unknown package clearly") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(
      List(
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

  test("show reports unknown package clearly") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(
      List(
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

  test("json write reports missing value in a known package") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(
      List(
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

  test("json write missing-value suggestions are filtered and sorted") {
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
    "check renders previous descriptor details when previous is missing"
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

    runWithFiles(files)(List("check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected missing-previous failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("previous not in cas"), msg)
    }
  }

  test("check --warn accepts todo but show rejects it") {
    val files = baseLibFiles("main = todo(1)\n")

    runWithFiles(files)(List("check", "--repo_root", "repo", "--warn")) match {
      case Right(Output.Basic(_, _)) => ()
      case Right(other)              => fail(s"unexpected output: $other")
      case Left(err)                 => fail(err.getMessage)
    }

    runWithFiles(files)(
      List("show", "--repo_root", "repo", "--package", "MyLib/Foo")
    ) match {
      case Right(out) =>
        fail(s"expected emit-mode rejection, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("todo"), msg)
        assert(msg.contains("check --warn"), msg)
    }
  }

  test("check --warn only warns on built-in todo") {
    val src =
      """from MyLib/Helper import todo as imported_todo
        |
        |todo = (x) -> x.add(1)
        |main = todo(1).add(imported_todo(2))
        |""".stripMargin
    val helperSrc =
      """export todo
        |
        |todo = (x) -> x.add(2)
        |""".stripMargin
    val files =
      baseLibFiles(src) :+
        (Chain("repo", "src", "MyLib", "Helper.bosatsu") -> helperSrc)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List("check", "--repo_root", "repo", "--warn"),
        s0
      )
    } yield s1

    result match {
      case Left(err) =>
        fail(err.getMessage)
      case Right((state, out)) =>
        assertEquals(out, Output.Basic(Doc.text(""), None))
        assertEquals(state.stdErr.render(200), "")
    }
  }

  test("check malformed bosatsu_libs.json reports parse error without stack trace") {
    val files = List(
      Chain("repo", "bosatsu_libs.json") -> "{ bad json"
    )

    val result = for {
      state0 <- stateFromFiles(files)
      stateAndExit <- runAndReportWithState(
        List("check", "--color", "none", "--repo_root", "repo"),
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

  test("check malformed <lib>_conf.json reports parse error without stack trace") {
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
        List("check", "--color", "none", "--repo_root", "repo"),
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

  test("fetch reports total fetched objects by default") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(List("fetch", "--repo_root", "repo")) match {
      case Right(Output.Basic(doc, None)) =>
        val rendered = doc.render(120)
        assert(rendered.contains("fetched 0 objects."), rendered)
      case Right(other) =>
        fail(s"expected basic output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("fetch --quiet suppresses successful output") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(
      List("fetch", "--repo_root", "repo", "--quiet")
    ) match {
      case Right(Output.Basic(doc, None)) =>
        assertEquals(doc.render(120), "")
      case Right(other) =>
        fail(s"expected basic output, got: $other")
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("fetch accepts matching descriptor on cached dependency hit") {
    val result = for {
      setup <- stateWithConfiguredCachedDep("main = 1\n", "dep", "0.0.1")
      (state, _) = setup
      fetched <- runWithState(List("fetch", "--repo_root", "repo"), state)
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

  test("fetch fails on cached dependency version mismatch") {
    val result = for {
      setup <- stateWithConfiguredCachedDep("main = 1\n", "dep", "5.0.0")
      (state, _) = setup
      fetched <- runWithState(List("fetch", "--repo_root", "repo"), state)
    } yield fetched

    result match {
      case Right((_, out)) =>
        fail(s"expected fetch failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("dep 5.0.0:"), rendered)
        assert(rendered.contains("cached library descriptor mismatch"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("fetch fails on cached dependency name mismatch") {
    val result = for {
      setup <- stateWithConfiguredCachedDep("main = 1\n", "not_dep", "0.0.1")
      (state, _) = setup
      fetched <- runWithState(List("fetch", "--repo_root", "repo"), state)
    } yield fetched

    result match {
      case Right((_, out)) =>
        fail(s"expected fetch failure, got: $out")
      case Left(err: CliException) =>
        val rendered = err.errDoc.render(120)
        assert(rendered.contains("not_dep 0.0.1:"), rendered)
        assert(rendered.contains("cached library descriptor mismatch"), rendered)
      case Left(err) =>
        fail(err.getMessage)
    }
  }

  test("check fails on cached dependency version mismatch") {
    val appSrc =
      """from Dep/Foo import dep
|
|main = dep
|""".stripMargin

    val result = for {
      setup <- stateWithConfiguredCachedDep(appSrc, "dep", "5.0.0")
      (state, _) = setup
      checked <- runWithState(List("check", "--repo_root", "repo"), state)
    } yield checked

    result match {
      case Right((_, out)) =>
        fail(s"expected check failure, got: $out")
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

  test("check reports both cached dependency version mismatches") {
    val appSrc =
      """from DepA/Foo import depA
|
|from DepB/Foo import depB
|
|main = depA.add(depB)
|""".stripMargin

    val result = for {
      state <- stateWithTwoVersionMismatchedCachedDeps(appSrc)
      checked <- runWithState(List("check", "--repo_root", "repo"), state)
    } yield checked

    result match {
      case Right((_, out)) =>
        fail(s"expected check failure, got: $out")
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

  test("fetch reports network failures with detailed reason") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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
        List("fetch", "--repo_root", "repo"),
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

  test("fetch reports hash mismatch with expected and found hashes") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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
        List("fetch", "--repo_root", "repo"),
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

  test("fetch reports http status failures") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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
        List("fetch", "--repo_root", "repo"),
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

  test("fetch uses grammatical singular and plural failure wording") {
    val files = baseLibFiles("main = 1\n")

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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
        List("fetch", "--repo_root", "repo"),
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

  test("fetch reports previous fetch failures") {
    val previousDesc = proto.LibDescriptor(
      version = Some(Version(0, 0, 0).toProto),
      hashes = validHash3 :: Nil,
      uris = Nil
    )
    val conf = LibConfig
      .init(Name("mylib"), "https://example.com", Version(0, 0, 1))
      .copy(previous = Some(previousDesc))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    runWithFiles(files)(List("fetch", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected previous fetch failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to fetch previous"), msg)
    }
  }

  test("publish --dry-run validates without mutating config, CAS, or default cache") {
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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

  test("publish updates config and CAS when not in dry-run mode") {
    val conf =
      LibConfig.init(Name("mylib"), "https://example.com", Version(0, 0, 1))
    val files = baseLibFilesWithConf("main = 1\n", conf)

    val result = for {
      s0 <- stateFromFiles(files)
      s1 <- runWithState(
        List(
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

  test("build without --main_pack reports missing main") {
    val files = baseLibFiles("main = 1\n")

    runWithFiles(files)(
      List("build", "--repo_root", "repo", "--outdir", "out")
    ) match {
      case Right(out) =>
        fail(s"expected no-main build failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("no main defined"), msg)
    }
  }

  test(
    "build succeeds when importing package from private dependency in CAS"
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
      s4 <- runWithState(List("check", "--repo_root", "repo"), state4)
      (state5, _) = s4
      s5 <- runWithState(
        List(
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

  test("build defaults to tree-shaking unused external packages") {
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
    "test runs runtime readiness preflight before executing tests"
  ) {
    val targetSrc =
      """test_one = Assertion(True, "ok")
"""
    val files =
      baseLibFiles(targetSrc) :+ (
        Chain(".", ".git", "HEAD") -> "ref: refs/heads/main\n"
      )

    runWithFiles(files)(
      List("test", "--repo_root", "repo")
    ) match {
      case Right(out) =>
        fail(s"expected preflight failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(
          msg.contains(
            "runtime readiness preflight failed before running `test`"
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
    "test with missing --cc_conf reports a concise validation error"
  ) {
    val targetSrc =
      """test_one = Assertion(True, "ok")
"""
    val files = baseLibFiles(targetSrc)

    runWithFiles(files)(
      List(
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
            "runtime readiness preflight failed before running `test`"
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

  test("test rejects --value with --filter") {
    module.run(
      List(
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

  test("test --value accepts exported plain Test values") {
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

  test("test --value accepts exported ProgTest values") {
    val targetSrc =
      """from Bosatsu/Prog import ProgTest, pure
|
|export prog_tests
|exposes Bosatsu/Prog
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

  test("test --value rejects non-exported test values") {
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

  test("test --value rejects exported values that are not test values") {
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

  test("test --value reports unknown package with a clear error") {
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

  test("test --value reports packages with no exported test values") {
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
    "test --filter reports no matching packages and suggests package typo fixes"
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
    "test --filter fails when any source header fails to parse"
  ) {
    val noTestsSrc =
      """value = 1
"""
    val brokenSrc =
      """package MyLib/ReproMin8/
|
|broken = Assertion(True, "oops")
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
      Chain("repo", "src", "MyLib", "Testing", "HedgeHog.bosatsu") -> noTestsSrc,
      Chain("repo", "src", "MyLib", "ReproMin8.bosatsu") -> brokenSrc,
      Chain("repo", "cc_conf.json") -> ccConfJson
    )

    runWithFiles(files)(
      List(
        "test",
        "--repo_root",
        "repo",
        "--filter",
        "MyLib/Testing/HedgeHog",
        "--cc_conf",
        "repo/cc_conf.json"
      )
    ) match {
      case Right(out) =>
        fail(s"expected parse failure for malformed header, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("failed to parse"), msg)
        assert(msg.contains("ReproMin8.bosatsu"), msg)
        assert(!msg.contains("no tests found"), msg)
    }
  }

  test(
    "test --filter scopes local typechecking to matching package roots"
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
    "check --filter scopes local typechecking to matching package roots"
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

    runWithFiles(files)(List("check", "--repo_root", "repo")) match {
      case Right(out) =>
        fail(s"expected unfiltered check failure, got: $out")
      case Left(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("does_not_exist"), msg)
    }

    runWithFiles(files)(
      List(
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

  test("check rejects --value") {
    module.run(
      List(
        "check",
        "--repo_root",
        "repo",
        "--value",
        "MyLib/Euler/One::test_one"
      )
    ) match {
      case Left(_)  => ()
      case Right(_) =>
        fail("expected parser error for unsupported --value on check")
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
      Colorize.None,
      quiet = false
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
