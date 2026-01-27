import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Dependencies._

lazy val versionString = "3.8.1"

ThisBuild / scalaVersion := versionString
ThisBuild / crossScalaVersions := Seq(versionString)
ThisBuild / dynverVTagPrefix := true

lazy val commonSettings = Seq(
  organization := "org.bykn",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "utf-8",
    "-feature",
    "-unchecked",
  ),
  Test / testOptions += Tests.Argument("-oDF")
)

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSLinkerConfig := scalaJSLinkerConfig.value
    .withBatchMode(scala.sys.env.get("BOSATSU_CI").isDefined)
    .withModuleKind(ModuleKind.CommonJSModule),
  coverageEnabled := false,
  scalaJSUseMainModuleInitializer := false
)

lazy val root =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("."))
    .aggregate(core)
    .settings(
      commonSettings,
      name := "bosatsu"
    )
    .jsSettings(commonJsSettings)

lazy val docs = (project in file("docs"))
  .enablePlugins(ParadoxPlugin)
  .settings(
    name := "paradox docs",
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxProperties ++= Map(
      "empty" -> "",
      "version" -> version.value
    ),
    publish / skip := true
  )

lazy val rootJVM = root.jvm
lazy val rootJS = root.js

lazy val headCommit = git.gitHeadCommit

lazy val base =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(
    "base"
  )).enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      name := "bosatsu-base",
      buildInfoKeys := Seq[BuildInfoKey](
        name,
        version,
        scalaVersion,
        sbtVersion,
        headCommit,
        BuildInfoKey.action("cRuntimeArchiveHash") {
          sys.env.getOrElse("BOSATSU_C_RUNTIME_HASH", "")
        }
      ),
      buildInfoPackage := "dev.bosatsu"
    )
    .jsSettings(commonJsSettings)

lazy val baseJS = base.js
lazy val baseJVM = base.jvm

lazy val cli = (project in file("cli"))
  .enablePlugins(NativeImagePlugin)
  .settings(
    commonSettings,
    name := "bosatsu-cli",
    assembly / test := {},
    assembly / assemblyMergeStrategy := {
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
        jawnParser.value % Test,
        jawnAst.value % Test,
        jython.value % Test,
        munit.value % Test,
        munitScalaCheck.value % Test
      ),
    // static linking doesn't work with macos or with linux http4s on the path
    nativeImageOptions ++= List(
      "--no-fallback",
      "--verbose",
    ) ++ {
      val staticOpt =
        if (sys.env.get("BOSATSU_STATIC_NATIVE_IMAGE").exists(_.nonEmpty))
          List("--static")
        else
          Nil
      val muslOpt =
        if (sys.env.get("BOSATSU_STATIC_NATIVE_IMAGE").exists(_.nonEmpty) &&
            sys.env.get("BOSATSU_NATIVE_IMAGE_LIBC").contains("musl"))
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
      staticOpt ++ muslOpt ++ clibPaths
    },
    nativeImageJvm := "graalvm-java21",
    nativeImageVersion := "21.0.2",
  )
  .dependsOn(protoJVM, coreJVM % "compile->compile;test->test")

lazy val simulationCli = (project in file("simulation-cli"))
  .settings(
    commonSettings,
    name := "bosatsu-sim",
    assembly / mainClass := Some("dev.bosatsu.simulation.Main"),
    Compile / mainClass := Some("dev.bosatsu.simulation.Main"),
    libraryDependencies ++= Seq(
      catsEffect.value,
      decline.value,
      fs2io.value,
      munit.value % Test,
      munitScalaCheck.value % Test
    )
  )
  .dependsOn(coreJVM % "compile->compile;test->test")

lazy val proto =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(
    "proto"
  ))
    .settings(
     
      Compile / PB.targets := Seq(
        scalapb.gen(scala3Sources = true) -> (Compile / sourceManaged).value
      ),
      // The trick is in this line:
      Compile / PB.protoSources := Seq(
        (ThisBuild / baseDirectory).value / "proto/src/main/protobuf"
      ),
      libraryDependencies ++= Seq(
        "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion
      ),
      PB.protocVersion := "3.19.1",
      commonSettings
    )

lazy val protoJs = proto.js
lazy val protoJVM = proto.jvm

lazy val core =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(
    "core"
  )).settings(
    commonSettings,
    name := "bosatsu-core",
    assembly / test := {},
    libraryDependencies ++=
      Seq(
        blake3.value,
        cats.value,
        catsCol.value,
        catsParse.value,
        decline.value,
        paiges.value,
        scalaCheck.value % Test,
        munit.value % Test,
        munitScalaCheck.value % Test,
        "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion
      )
  ).dependsOn(base, proto)
    .jsSettings(
      commonJsSettings
    )

lazy val coreJVM = core.jvm
lazy val coreJS =
  core.js.enablePlugins(ScalaJSPlugin).enablePlugins(ScalaJSBundlerPlugin)

lazy val cliJS =
  (crossProject(JSPlatform).crossType(CrossType.Pure) in file("cliJS"))
    .settings(
      commonSettings,
      commonJsSettings,
      name := "bosatsu-clijs",
      assembly / test := {},
      mainClass := Some("dev.bosatsu.tool.Fs2Main"),
      libraryDependencies ++= Seq(
        fs2core.value,
        fs2io.value,
        catsEffect.value,
        http4sCore.value,
        http4sEmber.value
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
  (crossProject(JSPlatform).crossType(CrossType.Pure) in file("jsapi"))
    .settings(
      commonSettings,
      commonJsSettings,
      name := "bosatsu-jsapi",
      assembly / test := {},
      libraryDependencies ++=
        Seq(
          cats.value,
          decline.value,
          scalaCheck.value % Test
        )
    )
    .dependsOn(base, core)

lazy val jsapiJS = jsapi.js

lazy val jsui =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(
    "jsui"
  ))
    .settings(
      commonSettings,
      commonJsSettings,
      name := "bosatsu-jsui",
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
      scalaJSUseMainModuleInitializer := true
    )
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(ScalaJSBundlerPlugin)
    .dependsOn(base, core)

lazy val jsuiJS = jsui.js
lazy val jsuiJVM = jsui.jvm

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
