import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val blake3 = Def.setting("pt.kcry" %%% "blake3" % "3.1.2")
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
  lazy val catsCol =
    Def.setting("org.typelevel" %%% "cats-collections-core" % "0.9.10")
  lazy val catsEffect =
    Def.setting("org.typelevel" %%% "cats-effect" % "3.6.3")
  lazy val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "1.1.0")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "2.6.0")
  lazy val ff4s = Def.setting("io.github.buntec" %%% "ff4s" % "0.26.1")
  lazy val fs2core = Def.setting("co.fs2" %%% "fs2-core" % "3.12.2")
  lazy val fs2io = Def.setting("co.fs2" %%% "fs2-io" % "3.12.2")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "1.6.0")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "1.6.0")
  lazy val jacksonYaml = Def.setting(
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.21.1"
  )
  lazy val jython = Def.setting("org.python" % "jython-standalone" % "2.7.4")
  lazy val http4sCore = Def.setting("org.http4s" %%% "http4s-core" % "0.23.33")
  lazy val http4sEmber =
    Def.setting("org.http4s" %%% "http4s-ember-client" % "0.23.33")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.2.2")
  lazy val munitScalaCheck =
    Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.2.0")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.4.4")
  lazy val scalaCheck =
    Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0")
  lazy val slf4jNop = Def.setting("org.slf4j" % "slf4j-nop" % "2.0.17")
}
