import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Dependencies._

lazy val commonSettings = Seq(
  organization := "org.bykn",
  version := "0.0.7",
  addCompilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  scalaVersion := "2.13.16",
  crossScalaVersions := Seq("2.13.16"),
  // from: https://tpolecat.github.io/2017/04/25/scalac-flags.html
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:higherKinds", // Allow higher-kinded types
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    // "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    // "-Ywarn-dead-code",                  // Warn when dead code is identified. // this kills ability to use ???
    // "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused",
    // "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    /* "-Ywarn-unused:locals",              // Warn if a local definition is unused */
    /* "-Ywarn-unused:params",              // Warn if a value parameter is unused. */
    /* "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused. */
    /* "-Ywarn-unused:privates",            // Warn if a private member is unused. */
    "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
    "-Xsource:3",
    "-Ypatmat-exhaust-depth",
    "40",
    "-Wconf:cat=deprecation&msg=.*Stream.*:s"
  ),
  Compile / console / scalacOptions --= Seq(
    "-Ywarn-unused:imports",
    "-Xfatal-warnings"
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

lazy val scalaReflect = Def.setting {
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
}

lazy val headCommit = git.gitHeadCommit

lazy val base =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(
    "base"
  )).enablePlugins(BuildInfoPlugin)
    .settings(
      commonSettings,
      name := "bosatsu-base",
      libraryDependencies += scalaReflect.value,
      buildInfoKeys := Seq[BuildInfoKey](
        name,
        version,
        scalaVersion,
        sbtVersion,
        headCommit
      ),
      buildInfoPackage := "org.bykn.bosatsu"
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
    assembly / mainClass := Some("org.bykn.bosatsu.Main"),
    Compile / mainClass := Some("org.bykn.bosatsu.Main"),
    libraryDependencies ++=
      Seq(
        catsEffect.value,
        jawnParser.value % Test,
        jawnAst.value % Test,
        jython.value % Test,
        munit.value % Test,
        munitScalaCheck.value % Test
      ),
    nativeImageOptions ++= {
      val common =
        List("--no-fallback", "--verbose", "--initialize-at-build-time")
      if (Option(System.getProperty("os.name")).exists(_.contains("Mac OS")))
        common
      else ("--static" :: common)
    },
    nativeImageVersion := "22.3.0"
  )
  .dependsOn(protoJVM, coreJVM % "compile->compile;test->test")

lazy val proto =
  (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file(
    "proto"
  ))
    .settings(
      Compile / PB.targets := Seq(
        scalapb.gen() -> (Compile / sourceManaged).value
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
        cats.value,
        catsParse.value,
        decline.value,
        paiges.value,
        scalaCheck.value % Test,
        scalaTest.value % Test,
        scalaTestPlusScalacheck.value % Test,
        munit.value % Test,
        munitScalaCheck.value % Test,
        "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion,

        // needed for acyclic which we run periodically, not all the time
        "com.lihaoyi" % "acyclic_2.13.12" % "0.3.15" % "provided"
      )
    // periodically we use acyclic to ban cyclic dependencies and make compilation faster
    ,
    autoCompilerPlugins := true,
    addCompilerPlugin("com.lihaoyi" % "acyclic_2.13.12" % "0.3.15"),
    scalacOptions += "-P:acyclic:force"
  ).dependsOn(base, proto)
    .jsSettings(
      commonJsSettings,
      Compile / npmDependencies += "js-sha256" -> "0.11.0"
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
      mainClass := Some("org.bykn.bosatsu.tool.Fs2Main"),
      libraryDependencies ++= Seq(fs2core.value, fs2io.value, catsEffect.value)
    )
    .jsSettings(
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
      scalaJSLinkerConfig ~= { _.withSourceMap(false).withOptimizer(true) },
      mainClass := Some("org.bykn.bosatsu.tool.Fs2Main"),
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
          scalaCheck.value % Test,
          scalaTest.value % Test,
          scalaTestPlusScalacheck.value % Test
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
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++=
        Seq(
          cats.value,
          ff4s.value,
          scalaCheck.value % Test,
          munit.value % Test,
          munitScalaCheck.value % Test
        )
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
