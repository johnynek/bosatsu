import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import Dependencies._


lazy val commonSettings = Seq(
  organization := "org.bykn",
  version      := "0.1.0-SNAPSHOT",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  scalaVersion := "2.12.11",
  crossScalaVersions := Seq("2.12.11"),
  // from: https://tpolecat.github.io/2017/04/25/scalac-flags.html
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    //"-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    //"-Ywarn-dead-code",                  // Warn when dead code is identified. // this kills ability to use ???
    //"-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused-import",             // Warn if an import selector is not referenced.
    //"-Ywarn-unused",
    //"-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    /* "-Ywarn-unused:locals",              // Warn if a local definition is unused. */
    /* "-Ywarn-unused:params",              // Warn if a value parameter is unused. */
    /* "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused. */
    /* "-Ywarn-unused:privates",            // Warn if a private member is unused. */
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ),

  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),

  testOptions in Test += Tests.Argument("-oDF"),

  testFrameworks += new TestFramework("munit.Framework")
)

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
  // batch mode decreases the amount of memory needed to compile scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(scala.sys.env.get("TRAVIS").isDefined),
  coverageEnabled := false,
  scalaJSUseMainModuleInitializer := false,
  scalacOptions += "-P:scalajs:sjsDefinedByDefault"
)

lazy val root = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("."))
  .aggregate(core)
  .settings(
    commonSettings,
    name := "bosatsu",
  )

lazy val docs = (project in file("docs"))
  .enablePlugins(ParadoxPlugin)
  .settings(
    name := "paradox docs",
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    paradoxProperties in Compile ++= Map(
      "empty" -> "",
      "version" -> version.value
    ),
    publish / skip := true
  )

lazy val rootJVM = root.jvm
lazy val rootJS = root.js

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }

lazy val headCommit = git.gitHeadCommit

lazy val base = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("base")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    name := "bosatsu-base",
    libraryDependencies += scalaReflect.value,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, headCommit),
    buildInfoPackage := "org.bykn.bosatsu",
  )

lazy val baseJS = base.js
lazy val baseJVM = base.jvm

lazy val cli = (project in file("cli")).
  settings(
    commonSettings,
    name := "bosatsu-cli",
    test in assembly := {},
    mainClass in assembly := Some("org.bykn.bosatsu.Main"),
    libraryDependencies ++=
      Seq(
        catsEffect.value,
        jawnParser.value % Test,
        jawnAst.value % Test,
        jython.value % Test,
        munit.value % Test,
      ),
    PB.targets in Compile := Seq(
     scalapb.gen() -> (sourceManaged in Compile).value
   )
  ).dependsOn(coreJVM % "compile->compile;test->test")

lazy val parser = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("parser")).
  settings(
    commonSettings,
    name := "bosatsu-parser",
    test in assembly := {},
    libraryDependencies ++=
      Seq(
        cats.value,
        munit.value % Test
      )
  )
  .jsSettings(commonJsSettings)

lazy val core = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("core")).
  settings(
    commonSettings,
    name := "bosatsu-core",
    test in assembly := {},
    libraryDependencies ++=
      Seq(
        alleycats.value,
        cats.value,
        decline.value,
        fastparse.value,
        paiges.value,
        scalaCheck.value % Test,
        scalaTest.value % Test,
        munit.value % Test,
        // needed for acyclic which we run periodically, not all the time
        //"com.lihaoyi" %% "acyclic" % "0.1.7" % "provided"
      )
      /*
      // periodically we use acyclic to ban cyclic dependencies and make compilation faster
      ,
      autoCompilerPlugins := true,

      addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.7"),

      scalacOptions += "-P:acyclic:force"
      */
  )
  .dependsOn(base)
  .jsSettings(commonJsSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val jsapi = (crossProject(JSPlatform).crossType(CrossType.Pure) in file("jsapi")).
  settings(
    commonSettings,
    commonJsSettings,
    name := "bosatsu-jsapi",
    test in assembly := {},
    libraryDependencies ++=
      Seq(
        alleycats.value,
        cats.value,
        decline.value,
        scalaCheck.value % Test,
        scalaTest.value % Test,
      )
  )
  .dependsOn(base, core)

lazy val jsapiJS = jsapi.js

lazy val bench = project
  .dependsOn(core.jvm)
  .settings(moduleName := "bosatsu-bench")
  .settings(commonSettings)
  .settings(
    publish := {},
    publishLocal := {},
    publishArtifact := false)
  .settings(coverageEnabled := false)
  .enablePlugins(JmhPlugin)
