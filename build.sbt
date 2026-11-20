import com.github.sbt.git.SbtGit.GitKeys
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.protocol.testing.TestResult
import Dependencies._
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import sbt.util.Digest
import scala.sys.process._
import scala.util.Try

lazy val generateCoreAlphaParadoxDocs =
  taskKey[Unit](
    "Generate core_alpha markdown docs (including predef) and copy them into paradox sources"
  )

lazy val versionString = "3.8.2"

ThisBuild / scalaVersion := versionString
ThisBuild / crossScalaVersions := Seq(versionString)
ThisBuild / dynverVTagPrefix := true
// sbt-dynver sets the version; we only use sbt-git for the head commit.
// sbt-git 3 runs `git describe --tags` with the system git while loading,
// which logs an error on tag-less CI checkouts, so skip that lookup.
ThisBuild / GitKeys.gitDescribedVersion := None

// sbt 2 caches task results, such as compiled classes, by their inputs. With
// BUILDBUDDY_API_KEY set (CI passes it from a repository secret), that cache
// is also shared through BuildBuddy, so a build reuses results from other
// machines. Without the key, sbt uses only its local disk cache.
lazy val buildBuddyApiKey: Option[String] =
  sys.env.get("BUILDBUDDY_API_KEY").map(_.trim).filter(_.nonEmpty)
Global / remoteCache ~= { configured =>
  buildBuddyApiKey match {
    case Some(_) => Some(uri("grpcs://remote.buildbuddy.io"))
    case None    => configured
  }
}
Global / remoteCacheHeaders ++=
  buildBuddyApiKey.map(key => s"x-buildbuddy-api-key=$key").toList
ThisBuild / organization := "dev.bosatsu"
ThisBuild / homepage := Some(uri("https://github.com/johnynek/bosatsu"))
ThisBuild / licenses := List(
  "Apache-2.0" -> uri("https://www.apache.org/licenses/LICENSE-2.0.txt")
)
ThisBuild / developers := List(
  Developer(
    "johnynek",
    "Oscar Boykin",
    "oscar.boykin@gmail.com",
    uri("https://github.com/johnynek")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    uri("https://github.com/johnynek/bosatsu"),
    "scm:git:git@github.com:johnynek/bosatsu.git"
  )
)

// Repo files that tests read while they run. sbt 2's `test` skips a suite
// whose last pass is cached. That cache key covers the suite's classes and
// everything they depend on, classpath resources and test options, but not
// files read at runtime, so these are hashed into every suite's key: a change
// to any of them reruns the tests.
lazy val testDataPaths =
  List("test_workspace", ".github/workflows", "bosatsu_libs.json")

lazy val testDataDigest =
  taskKey[Digest]("Hash of the repo files that tests read while they run")

// Hashes the paths and current contents of the files under `paths`. git lists
// them, so ignored build outputs don't count; outside a git checkout, every
// file under `paths` counts.
def repoFilesDigest(root: File, paths: Seq[String]): Digest = {
  val gitCommand = Seq(
    "git",
    "ls-files",
    "-z",
    "--cached",
    "--others",
    "--exclude-standard",
    "--"
  ) ++ paths
  val gitListed = Try(Process(gitCommand, root).!!).toOption
    .map(_.split('\u0000').toList.filter(_.nonEmpty))
  val relPaths = gitListed.getOrElse {
    paths.flatMap { p =>
      (root / p).allPaths.get().filter(_.isFile).flatMap(IO.relativize(root, _))
    }
  }
  val sha = MessageDigest.getInstance("SHA-256")
  relPaths.distinct.sorted.foreach { rel =>
    sha.update(rel.getBytes(StandardCharsets.UTF_8))
    sha.update(0.toByte)
    val file = root / rel
    if (file.isFile) sha.update(IO.readBytes(file))
    sha.update(0.toByte)
  }
  Digest.sha256Hash(sha.digest())
}

ThisBuild / testDataDigest := Def.uncached(
  repoFilesDigest((ThisBuild / baseDirectory).value, testDataPaths)
)

// Scala.js tests also depend on two things outside their classpath: the
// linker settings they're linked with and the Node.js that runs them. The JS
// projects hash both into their test cache keys (see commonJsSettings).
lazy val nodeVersion =
  taskKey[String]("Version of the Node.js that runs the Scala.js tests")

lazy val jsTestRuntimeDigest =
  taskKey[Digest]("Hash of the linker settings and Node.js version JS tests run with")

ThisBuild / nodeVersion := Def.uncached(
  Try(Seq("node", "--version").!!.trim).getOrElse("unknown")
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "utf-8",
    "-feature",
    "-unchecked",
    "-language:strictEquality",
    "-Ycheck-all-patmat",
    "-explain",
    "-Wunused:all",
    "-Wnonunit-statement",
    "-Wvalue-discard",
    "-Werror"
  ),
  Test / testOptions += Tests.Argument("-oDF"),
  // munit reports only failing tests.
  Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "--log=failure"),
  Test / extraTestDigests += (ThisBuild / testDataDigest).value
)

lazy val commonJsSettings = Seq(
  Global / scalaJSStage := FastOptStage,
  parallelExecution := false,
  jsEnv := Def.uncached(new org.scalajs.jsenv.nodejs.NodeJSEnv()),
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSLinkerConfig := scalaJSLinkerConfig.value
    .withBatchMode(scala.sys.env.get("BOSATSU_CI").isDefined)
    .withModuleKind(ModuleKind.CommonJSModule),
  coverageEnabled := false,
  scalaJSUseMainModuleInitializer := false,
  jsTestRuntimeDigest := Def.uncached {
    // Batch mode only changes how much memory the linker uses.
    val linkerConfig =
      (Test / fastLinkJS / scalaJSLinkerConfig).value.withBatchMode(false)
    val node = (ThisBuild / nodeVersion).value
    Digest.sha256Hash(s"$linkerConfig\n$node".getBytes(StandardCharsets.UTF_8))
  },
  Test / extraTestDigests += jsTestRuntimeDigest.value
)

lazy val root =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("."))
    .aggregate(core)
    .settings(
      commonSettings,
      name := "bosatsu",
      publish / skip := true
    )
    .jsSettings(commonJsSettings)

lazy val docs = (project in file("docs"))
  .enablePlugins(ParadoxPlugin)
  .settings(
    name := "paradox docs",
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxNavigationDepth := 1,
    Compile / paradoxProperties ++= Map(
      "empty" -> "",
      "version" -> version.value
    ),
    // Runs bosatsuj and writes files, so it must run every time rather than
    // being restored from the sbt 2 task cache.
    Compile / generateCoreAlphaParadoxDocs := Def.uncached {
      val log = streams.value.log
      val repoRoot = (LocalRootProject / baseDirectory).value
      val paradoxGeneratedRoot =
        (Compile / sourceDirectory).value / "paradox" / "generated" / "core_alpha"
      val generatedDocsRoot = repoRoot / "core_alpha_docs"
      val sourceRepoUrl = "https://github.com/johnynek/bosatsu/blob/main"
      val disableParadoxSourceMarkdownLink =
        """---
          |github.base_url=
          |---
          |
          |""".stripMargin
      def rewriteMarkdownLinksToHtml(content: String): String = {
        val linkPattern = raw"\]\(([^)]+)\)".r
        linkPattern.replaceAllIn(content, m => {
          val target = m.group(1)
          val hashIdx = target.indexOf('#')
          val (path, suffix) =
            if (hashIdx >= 0) {
              (target.substring(0, hashIdx), target.substring(hashIdx))
            } else (target, "")

          if (path.endsWith(".md") && !path.contains("://")) {
            s"](${path.stripSuffix(".md")}.html$suffix)"
          } else {
            m.matched
          }
        })
      }

      // Ensure bosatsuj has an up-to-date CLI assembly before generating docs.
      val _ = (cli / assembly).value

      val fetchCmd = Seq("./bosatsuj", "fetch")
      log.info(fetchCmd.mkString("running: ", " ", ""))
      val fetchExit = Process(fetchCmd, repoRoot).!
      if (fetchExit != 0) {
        sys.error(
          s"fetch failed with exit code $fetchExit: ${fetchCmd.mkString(" ")}"
        )
      }

      val docCmd = Seq(
        "./bosatsuj",
        "doc",
        "--outdir",
        "core_alpha_docs",
        "--include_predef",
        "--source_repo_url",
        sourceRepoUrl
      )
      log.info(docCmd.mkString("running: ", " ", ""))
      val docExit = Process(docCmd, repoRoot).!
      if (docExit != 0) {
        sys.error(
          s"doc generation failed with exit code $docExit: ${docCmd.mkString(" ")}"
        )
      }

      if (!generatedDocsRoot.exists()) {
        sys.error(
          s"expected generated docs at $generatedDocsRoot, but directory was missing"
        )
      }

      IO.delete(paradoxGeneratedRoot)
      IO.createDirectory(paradoxGeneratedRoot)
      IO.copyDirectory(generatedDocsRoot, paradoxGeneratedRoot)

      val markdownFiles =
        (paradoxGeneratedRoot ** "*.md").get()
          .filterNot(_.getName == "index.md")
          .sortBy(f =>
            IO.relativize(paradoxGeneratedRoot, f).getOrElse(f.getPath)
          )

      val tocLinkLines = markdownFiles.map { file =>
        val relPath = IO
          .relativize(paradoxGeneratedRoot, file)
          .getOrElse(file.getName)
          .replace(java.io.File.separatorChar, '/')
        val title = relPath.stripSuffix(".md")
        s"* [$title]($relPath)"
      }

      val pageLinkLines = markdownFiles.map { file =>
        val relPath = IO
          .relativize(paradoxGeneratedRoot, file)
          .getOrElse(file.getName)
          .replace(java.io.File.separatorChar, '/')
        val title = relPath.stripSuffix(".md")
        val htmlPath = s"${title}.html"
        s"* [$title]($htmlPath)"
      }

      val generatedIndex =
        s"""# Core Alpha API
           |
           |This section is generated from `test_workspace` using:
           |`./bosatsuj doc --outdir core_alpha_docs --include_predef --source_repo_url $sourceRepoUrl`
           |
           |@@@ index
           |${tocLinkLines.mkString("\n")}
           |@@@
           |
           |## Browse all generated docs
           |
           |${pageLinkLines.mkString("\n")}
           |""".stripMargin

      markdownFiles.foreach { file =>
        val markdown = IO.read(file)
        IO.write(
          file,
          disableParadoxSourceMarkdownLink + rewriteMarkdownLinksToHtml(markdown)
        )
      }
      IO.write(
        paradoxGeneratedRoot / "index.md",
        disableParadoxSourceMarkdownLink + generatedIndex
      )
      log.info(
        s"generated ${markdownFiles.size} markdown files into $paradoxGeneratedRoot"
      )
    },
    // Copies generated markdown into the site, so it must not be cached either.
    Compile / paradox := Def.uncached {
      val paradoxResult = (Compile / paradox)
        .dependsOn(Compile / generateCoreAlphaParadoxDocs)
        .value
      val log = streams.value.log
      val generatedSourceRoot =
        (Compile / sourceDirectory).value / "paradox" / "generated" / "core_alpha"
      val paradoxSiteRoot = (Compile / paradox / target).value
      val generatedSiteRoot = paradoxSiteRoot / "generated" / "core_alpha"

      val markdownFiles = (generatedSourceRoot ** "*.md").get()
      markdownFiles.foreach { sourceFile =>
        IO.relativize(generatedSourceRoot, sourceFile) match {
          case Some(relPath) =>
            val targetFile = generatedSiteRoot / relPath
            IO.createDirectory(targetFile.getParentFile)
            IO.copyFile(sourceFile, targetFile)
          case None =>
            sys.error(
              s"failed to relativize generated markdown path: $sourceFile against $generatedSourceRoot"
            )
        }
      }
      log.info(
        s"copied ${markdownFiles.size} generated markdown files into $generatedSiteRoot"
      )
      paradoxResult
    },
    publish / skip := true
  )

lazy val rootJVM = root.jvm
lazy val rootJS = root.js

lazy val headCommit = git.gitHeadCommit

// BuildInfo holds the version and git commit, which change with every commit.
// Only the entry point projects (cli, cliJS, jsapi and jsui) generate it and
// hand it to core as a dev.bosatsu.CompilerBuildInfo, so a new commit doesn't change the
// compiled output of core and the other shared projects, and sbt's build cache
// can reuse it.
lazy val compilerBuildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    version,
    scalaVersion,
    headCommit,
    BuildInfoKey.action("cRuntimeArchiveHash") {
      sys.env.getOrElse("BOSATSU_C_RUNTIME_HASH", "")
    }
  ),
  buildInfoPackage := "dev.bosatsu",
  buildInfoOptions += BuildInfoOption.Traits("dev.bosatsu.CompilerBuildInfo")
)

lazy val base =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("base"))
    .settings(
      commonSettings,
      name := "bosatsu-base",
      moduleName := "compiler-base"
    )
    .jsSettings(commonJsSettings)

lazy val baseJS = base.js
lazy val baseJVM = base.jvm

lazy val cli = (project in file("cli"))
  .enablePlugins(NativeImagePlugin, BuildInfoPlugin)
  .settings(
    commonSettings,
    compilerBuildInfoSettings,
    name := "bosatsu-cli",
    moduleName := "compiler-cli",
    Compile / javacOptions ++= Seq("--release", "17"),
    assembly / test := TestResult.Passed,
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class") =>
        // fat jars shouldn't include module metadata
        MergeStrategy.discard
      case PathList("META-INF", "versions", _, "module-info.class") =>
        // drop multi-release module metadata to avoid duplicate conflicts
        MergeStrategy.discard
      case PathList("scala", "annotation", "nowarn$.class" | "nowarn.class") =>
        // this is duplicated in scala-collection-compat
        MergeStrategy.last
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },
    assembly / mainClass := Some("dev.bosatsu.Main"),
    Compile / mainClass := Some("dev.bosatsu.Main"),
    libraryDependencies ++=
      Seq(
        catsEffect.value,
        http4sEmber.value,
        fs2core.value,
        fs2io.value,
        slf4jNop.value,
        jawnParser.value % Test,
        jawnAst.value % Test,
        "us.bpsm" % "edn-java" % "0.7.1" % Test,
        jacksonYaml.value % Test,
        jython.value % Test,
        jna.value,
        nativeImageSdk.value,
        protobufJava.value,
        munit.value % Test,
        munitScalaCheck.value % Test
      ),
    // static linking doesn't work with macos or with linux http4s on the path
    nativeImageOptions ++= {
      val nativeImageBuilderXmx =
        sys.env
          .get("BOSATSU_NATIVE_IMAGE_BUILD_JAVA_XMX")
          .filter(_.nonEmpty)
          .getOrElse("12g")
      val watchdogOpts =
        sys.env
          .get("BOSATSU_NATIVE_IMAGE_DEADLOCK_WATCHDOG_INTERVAL_MINUTES")
          .filter(_.nonEmpty)
          .toList
          .map(minutes => s"-H:DeadlockWatchdogInterval=$minutes") ++
          sys.env
            .get("BOSATSU_NATIVE_IMAGE_DEADLOCK_WATCHDOG_EXIT_ON_TIMEOUT")
            .collect {
              case value if value.equalsIgnoreCase("true") =>
                "-H:+DeadlockWatchdogExitOnTimeout"
              case value if value.equalsIgnoreCase("false") =>
                "-H:-DeadlockWatchdogExitOnTimeout"
            }
            .toList
      val nativeImageThreadOpts =
        sys.env
          .get("BOSATSU_NATIVE_IMAGE_THREADS")
          .filter(_.nonEmpty)
          .toList
          .map(threads => s"-H:NumberOfThreads=$threads")
      val staticOpt =
        if (sys.env.get("BOSATSU_STATIC_NATIVE_IMAGE").exists(_.nonEmpty))
          List("--static")
        else
          Nil
      val muslOpt =
        if (
          sys.env.get("BOSATSU_STATIC_NATIVE_IMAGE").exists(_.nonEmpty) &&
          sys.env.get("BOSATSU_NATIVE_IMAGE_LIBC").contains("musl")
        )
          List("--libc=musl")
        else
          Nil
      val clibPaths =
        sys.env
          .get("BOSATSU_NATIVE_IMAGE_CLIB_PATH")
          .toList
          .flatMap(_.split(java.io.File.pathSeparator).toList)
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(p => s"-H:CLibraryPath=$p")
      List(
        "--no-fallback",
        "--verbose",
        "-O2",
        s"-J-Xmx$nativeImageBuilderXmx",
        "-H:IncludeResources=dev/bosatsu/scalawasiz3/aot/.*\\.meta",
        "-H:+RemoveUnusedSymbols",
        "-H:CompilationExpirationPeriod=0",
        "-H:CompilationNoProgressPeriod=0"
      ) ++ watchdogOpts ++ nativeImageThreadOpts ++ staticOpt ++ muslOpt ++ clibPaths
    },
    nativeImageJvm := "graalvm-java23",
    nativeImageVersion := "23.0.2"
  )
  .dependsOn(protoJVM, coreJVM % "compile->compile;test->test")

lazy val proto =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("proto"))
    .settings(
      name := "bosatsu-proto",
      moduleName := "compiler-proto",
      // ScalaPB-generated sources currently trigger -Wvalue-discard on builder APIs.
      // Keep strict warnings for handwritten code while silencing warnings in managed code.
      Compile / scalacOptions += "-Wconf:src=.*[/\\\\]src_managed[/\\\\].*:s",
      // sbt-protoc adds PB.protoSources to the resource directories, so the
      // .proto files are packaged without listing the directory again here.
      Compile / PB.targets := Seq(
        scalapb.gen(scala3Sources = true) -> (Compile / sourceManaged).value
      ),
      // The trick is in this line:
      Compile / PB.protoSources := Seq(
        (ThisBuild / baseDirectory).value / "proto" / "src" / "main" / "protobuf"
      ),
      libraryDependencies ++= Seq(
        "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion
      ),
      PB.protocVersion := "3.19.1",
      commonSettings
    )

lazy val protoJs = proto.js
lazy val protoJVM = proto.jvm

lazy val core =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core")).settings(
    commonSettings,
    name := "bosatsu-core",
    moduleName := "compiler-core",
    assembly / test := TestResult.Passed,
    scalacOptions += "-Yexplicit-nulls",
    libraryDependencies ++=
      Seq(
        blake3.value,
        sha.value,
        cats.value,
        catsCol.value,
        catsParse.value,
        decline.value,
        paiges.value,
        scalawasiz3.value,
        scalaCheck.value % Test,
        munit.value % Test,
        munitScalaCheck.value % Test,
        "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion
      )
  ).dependsOn(base, proto)
    .jsSettings(
      commonJsSettings
    )

lazy val coreJVM = core.jvm
lazy val coreJS =
  core.js.enablePlugins(ScalaJSPlugin)

lazy val cliJS =
  crossProject(JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("cliJS"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      commonJsSettings,
      compilerBuildInfoSettings,
      name := "bosatsu-clijs",
      moduleName := "compiler-clijs",
      assembly / test := TestResult.Passed,
      mainClass := Some("dev.bosatsu.tool.Fs2Main"),
      libraryDependencies ++= Seq(
        fs2core.value,
        fs2io.value,
        catsEffect.value,
        http4sCore.value,
        http4sEmber.value,
        munit.value % Test
      )
    )
    .jsSettings(
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
      scalaJSLinkerConfig ~= { _.withSourceMap(false).withOptimizer(true) },
      mainClass := Some("dev.bosatsu.tool.Fs2Main"),
      scalaJSUseMainModuleInitializer := true
    )
    .dependsOn(base, core)

lazy val jsapi =
  crossProject(JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("jsapi"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      commonJsSettings,
      compilerBuildInfoSettings,
      name := "bosatsu-jsapi",
      moduleName := "compiler-jsapi",
      assembly / test := TestResult.Passed,
      libraryDependencies ++=
        Seq(
          cats.value,
          decline.value,
          munit.value % Test,
          scalaCheck.value % Test
        )
    )
    .dependsOn(base, core)

lazy val jsapiJS = jsapi.js

// jsui is browser-only: ff4s has no JVM build.
lazy val jsui =
  crossProject(JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("jsui"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      commonJsSettings,
      compilerBuildInfoSettings,
      name := "bosatsu-jsui",
      moduleName := "compiler-jsui",
      libraryDependencies ++=
        Seq(
          cats.value,
          ff4s.value,
          scalaCheck.value % Test,
          munit.value % Test,
          munitScalaCheck.value % Test
        )
    )
    .jsSettings(
      scalaJSUseMainModuleInitializer := true,
      // index.html loads the UI with a plain <script> tag, and the linked
      // code has no npm imports, so Scala.js can emit the browser script
      // directly without a bundler.
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
      // The deployed page is the single fullLinkJS file, with no source map.
      Compile / fullLinkJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) }
    )
    .dependsOn(base, core)

lazy val jsuiJS = jsui.js

lazy val bench = project
  .dependsOn(core.jvm)
  .settings(moduleName := "bosatsu-bench")
  .settings(commonSettings)
  .settings(
    publish := {},
    publishLocal := {},
    libraryDependencies ++=
      Seq(),
    publishArtifact := false
  )
  .settings(coverageEnabled := false)
  .enablePlugins(JmhPlugin)
