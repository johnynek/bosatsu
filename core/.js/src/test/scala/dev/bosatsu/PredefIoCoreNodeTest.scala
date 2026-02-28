package dev.bosatsu

import Value._
import scala.scalajs.js

class PredefIoCoreNodeTest extends munit.FunSuite {

  private val createNewOpenMode: Value = SumValue(3, UnitValue)
  private val ioErrAlreadyExists = 2
  private val ioErrInvalidArgument = 12
  private val ioErrUnsupported = 19

  private lazy val fs: js.Dynamic = js.Dynamic.global.require("fs")
  private lazy val pathMod: js.Dynamic = js.Dynamic.global.require("path")
  private lazy val os: js.Dynamic = js.Dynamic.global.require("os")

  private def runProg(prog: Value): Either[Value, Value] =
    PredefImpl.runProg(prog).result

  private def expectRight(result: Either[Value, Value]): Value =
    result match {
      case Right(v) => v
      case Left(err) =>
        fail(s"expected success, got error: $err")
    }

  private def expectLeft(result: Either[Value, Value]): Value =
    result match {
      case Left(err) => err
      case Right(v)  =>
        fail(s"expected error, got success: $v")
    }

  private def ioErrorTagAndContext(err: Value): (Int, String) =
    err match {
      case s: SumValue =>
        if (s.value.values.nonEmpty) {
          s.value.get(0) match {
            case Str(context) => (s.variant, context)
            case other        =>
              fail(s"expected IOError context String, found: $other")
          }
        } else fail(s"expected IOError payload, found: $err")
      case other =>
        fail(s"expected IOError sum value, found: $other")
    }

  private def asPath(pathValue: Value): String =
    pathValue match {
      case Str(path) => path
      case p: ProductValue if p.values.length == 1 =>
        p.get(0) match {
          case Str(path) => path
          case other     => fail(s"expected Path string field, found: $other")
        }
      case other =>
        fail(s"expected Path value, found: $other")
    }

  private def asTuple2(value: Value): (Value, Value) =
    value match {
      case p: ProductValue if p.values.length == 2 =>
        (p.get(0), p.get(1))
      case other =>
        fail(s"expected tuple2 value, found: $other")
    }

  private def mkTempDir(prefix: String): String =
    fs.mkdtempSync(
      pathMod.join(os.tmpdir().asInstanceOf[String], prefix)
    ).asInstanceOf[String]

  private def pathJoin(left: String, right: String): String =
    pathMod.join(left, right).asInstanceOf[String]

  private def pathDirName(path: String): String =
    pathMod.dirname(path).asInstanceOf[String]

  private def pathResolve(path: String): String =
    pathMod.resolve(path).asInstanceOf[String]

  private def exists(path: String): Boolean =
    fs.existsSync(path).asInstanceOf[Boolean]

  private def isDirectory(path: String): Boolean =
    fs
      .lstatSync(path)
      .asInstanceOf[js.Dynamic]
      .applyDynamic("isDirectory")()
      .asInstanceOf[Boolean]

  private def deleteTree(path: String): Unit = {
    if (exists(path)) {
      val _ = fs.rmSync(
        path,
        js.Dynamic.literal(recursive = true, force = true)
      )
      ()
    }
  }

  private def deleteFile(path: String): Unit = {
    if (exists(path)) {
      val _ = fs.rmSync(path)
      ()
    }
  }

  test("prog_core_open_file CreateNew is atomic and returns context-rich errors") {
    val tempDir = mkTempDir("bosatsu-open-file-")
    try {
      val path = pathJoin(tempDir, "lock")
      val handle = expectRight(
        runProg(
          PredefIoCorePlatform.prog_core_open_file(Str(path), createNewOpenMode)
        )
      )
      val _ = expectRight(runProg(PredefIoCorePlatform.prog_core_close(handle)))

      val alreadyExistsErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_open_file(Str(path), createNewOpenMode)
        )
      )
      val (alreadyExistsTag, alreadyExistsContext) =
        ioErrorTagAndContext(alreadyExistsErr)
      assertEquals(alreadyExistsTag, ioErrAlreadyExists)
      assert(
        alreadyExistsContext.contains("open_file(") &&
          alreadyExistsContext.contains("mode=CreateNew") &&
          alreadyExistsContext.contains("opening file failed"),
        alreadyExistsContext
      )

      val invalidModeErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_open_file(
            Str(pathJoin(tempDir, "lock2")),
            SumValue(99, UnitValue)
          )
        )
      )
      val (invalidModeTag, invalidModeContext) = ioErrorTagAndContext(
        invalidModeErr
      )
      assertEquals(invalidModeTag, ioErrInvalidArgument)
      assert(
        invalidModeContext.contains("mode=<invalid OpenMode>"),
        invalidModeContext
      )

      val invalidPathErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_open_file(
            Str("\u0000bad-path"),
            createNewOpenMode
          )
        )
      )
      val (invalidPathTag, invalidPathContext) = ioErrorTagAndContext(
        invalidPathErr
      )
      assertEquals(invalidPathTag, ioErrInvalidArgument)
      assert(
        invalidPathContext.contains("open_file(") &&
          invalidPathContext.contains("mode=CreateNew"),
        invalidPathContext
      )

      val invalidPathTypeErr = expectLeft(
        runProg(PredefIoCorePlatform.prog_core_open_file(VInt(1), Str("not-a-mode")))
      )
      val (invalidPathTypeTag, invalidPathTypeContext) = ioErrorTagAndContext(
        invalidPathTypeErr
      )
      assertEquals(invalidPathTypeTag, ioErrInvalidArgument)
      assert(invalidPathTypeContext.nonEmpty, invalidPathTypeContext)
    } finally deleteTree(tempDir)
  }

  test("prog_core_create_temp_file supports default and explicit dirs with rich argument validation") {
    val tempDir = mkTempDir("bosatsu-temp-file-")
    var explicitTemp: Option[String] = None
    var defaultTemp: Option[String] = None

    try {
      val explicitResult = expectRight(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.some(Str(tempDir)),
            Str("bo"),
            Str(".tmp")
          )
        )
      )
      val (explicitPathValue, explicitHandle) = asTuple2(explicitResult)
      val explicitPath = asPath(explicitPathValue)
      explicitTemp = Some(explicitPath)
      assert(exists(explicitPath), explicitPath)
      assertEquals(pathResolve(pathDirName(explicitPath)), pathResolve(tempDir))
      val _ = expectRight(runProg(PredefIoCorePlatform.prog_core_close(explicitHandle)))

      val defaultResult = expectRight(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.none,
            Str(""),
            Str(".tmp")
          )
        )
      )
      val (defaultPathValue, defaultHandle) = asTuple2(defaultResult)
      val defaultPath = asPath(defaultPathValue)
      defaultTemp = Some(defaultPath)
      assert(exists(defaultPath), defaultPath)
      val _ = expectRight(runProg(PredefIoCorePlatform.prog_core_close(defaultHandle)))

      val badDirOptionErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            Str("not-an-option"),
            Str("abc"),
            Str(".tmp")
          )
        )
      )
      val (badDirOptionTag, badDirOptionContext) = ioErrorTagAndContext(
        badDirOptionErr
      )
      assertEquals(badDirOptionTag, ioErrInvalidArgument)
      assert(
        badDirOptionContext.contains("invalid temp file dir"),
        badDirOptionContext
      )

      val badPrefixTypeErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.none,
            VInt(7),
            Str(".tmp")
          )
        )
      )
      val (badPrefixTypeTag, badPrefixTypeContext) = ioErrorTagAndContext(
        badPrefixTypeErr
      )
      assertEquals(badPrefixTypeTag, ioErrInvalidArgument)
      assert(
        badPrefixTypeContext.contains("prefix=<invalid String>"),
        badPrefixTypeContext
      )

      val badSuffixTypeErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.none,
            Str("abc"),
            VInt(9)
          )
        )
      )
      val (badSuffixTypeTag, badSuffixTypeContext) = ioErrorTagAndContext(
        badSuffixTypeErr
      )
      assertEquals(badSuffixTypeTag, ioErrInvalidArgument)
      assert(
        badSuffixTypeContext.contains("suffix=<invalid String>"),
        badSuffixTypeContext
      )

      val badPrefixErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.none,
            Str("bad/name"),
            Str(".tmp")
          )
        )
      )
      val (badPrefixTag, badPrefixContext) = ioErrorTagAndContext(badPrefixErr)
      assertEquals(badPrefixTag, ioErrInvalidArgument)
      assert(
        badPrefixContext.contains("invalid temp file prefix"),
        badPrefixContext
      )

      val badSuffixErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.none,
            Str("abc"),
            Str("bad/name")
          )
        )
      )
      val (badSuffixTag, badSuffixContext) = ioErrorTagAndContext(badSuffixErr)
      assertEquals(badSuffixTag, ioErrInvalidArgument)
      assert(
        badSuffixContext.contains("invalid temp file suffix"),
        badSuffixContext
      )

      val badDirPathErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_file(
            VOption.some(Str("\u0000bad-dir")),
            Str("abc"),
            Str(".tmp")
          )
        )
      )
      val (badDirPathTag, badDirPathContext) = ioErrorTagAndContext(
        badDirPathErr
      )
      assertEquals(badDirPathTag, ioErrInvalidArgument)
      assert(
        badDirPathContext.contains("Files.createTempFile failed"),
        badDirPathContext
      )
    } finally {
      explicitTemp.foreach(deleteFile)
      defaultTemp.foreach(deleteFile)
      deleteTree(tempDir)
    }
  }

  test("prog_core_create_temp_dir supports default and explicit dirs with rich argument validation") {
    val tempDir = mkTempDir("bosatsu-temp-dir-")
    var explicitTempDir: Option[String] = None
    var defaultTempDir: Option[String] = None

    try {
      val explicitResult = expectRight(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_dir(
            VOption.some(Str(tempDir)),
            Str("xy")
          )
        )
      )
      val explicitCreatedDir = asPath(explicitResult)
      explicitTempDir = Some(explicitCreatedDir)
      assert(isDirectory(explicitCreatedDir), explicitCreatedDir)
      assertEquals(pathResolve(pathDirName(explicitCreatedDir)), pathResolve(tempDir))

      val defaultResult = expectRight(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_dir(
            VOption.none,
            Str("")
          )
        )
      )
      val defaultCreatedDir = asPath(defaultResult)
      defaultTempDir = Some(defaultCreatedDir)
      assert(isDirectory(defaultCreatedDir), defaultCreatedDir)

      val badDirOptionErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_dir(
            Str("not-an-option"),
            Str("abc")
          )
        )
      )
      val (badDirOptionTag, badDirOptionContext) = ioErrorTagAndContext(
        badDirOptionErr
      )
      assertEquals(badDirOptionTag, ioErrInvalidArgument)
      assert(
        badDirOptionContext.contains("invalid temp dir"),
        badDirOptionContext
      )

      val badPrefixTypeErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_dir(
            VOption.none,
            VInt(1)
          )
        )
      )
      val (badPrefixTypeTag, badPrefixTypeContext) = ioErrorTagAndContext(
        badPrefixTypeErr
      )
      assertEquals(badPrefixTypeTag, ioErrInvalidArgument)
      assert(
        badPrefixTypeContext.contains("prefix=<invalid String>"),
        badPrefixTypeContext
      )

      val badPrefixErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_dir(
            VOption.none,
            Str("bad/name")
          )
        )
      )
      val (badPrefixTag, badPrefixContext) = ioErrorTagAndContext(badPrefixErr)
      assertEquals(badPrefixTag, ioErrInvalidArgument)
      assert(
        badPrefixContext.contains("invalid temp dir prefix"),
        badPrefixContext
      )

      val badDirPathErr = expectLeft(
        runProg(
          PredefIoCorePlatform.prog_core_create_temp_dir(
            VOption.some(Str("\u0000bad-dir")),
            Str("abc")
          )
        )
      )
      val (badDirPathTag, badDirPathContext) = ioErrorTagAndContext(
        badDirPathErr
      )
      assertEquals(badDirPathTag, ioErrInvalidArgument)
      assert(
        badDirPathContext.contains("Files.createTempDirectory failed"),
        badDirPathContext
      )
    } finally {
      explicitTempDir.foreach(deleteTree)
      defaultTempDir.foreach(deleteTree)
      deleteTree(tempDir)
    }
  }

  test("browser runtime returns IOError.Unsupported instead of missing externals") {
    val result =
      PredefIoCorePlatform.withNodeRuntimeForTest(false) {
        runProg(
          PredefIoCorePlatform.prog_core_open_file(
            Str("foo"),
            createNewOpenMode
          )
        )
      }

    val err = expectLeft(result)
    val (tag, context) = ioErrorTagAndContext(err)
    assertEquals(tag, ioErrUnsupported)
    assert(context.contains("requires Node.js runtime host"), context)
  }
}
