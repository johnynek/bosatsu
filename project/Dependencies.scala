import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val blake3 = Def.setting("pt.kcry" %%% "blake3" % "3.1.2")
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
  lazy val catsEffect =
    Def.setting("org.typelevel" %%% "cats-effect" % "3.6.3")
  lazy val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "1.1.0")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "2.5.0")
  lazy val ff4s = Def.setting("io.github.buntec" %%% "ff4s" % "0.25.0")
  lazy val fs2core = Def.setting("co.fs2" %%% "fs2-core" % "3.12.2")
  lazy val fs2io = Def.setting("co.fs2" %%% "fs2-io" % "3.12.2")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "1.6.0")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "1.6.0")
  lazy val jython = Def.setting("org.python" % "jython-standalone" % "2.7.4")
  lazy val http4sBlaze =
    Def.setting("org.http4s" %% "http4s-blaze-client" % "0.23.17")
  lazy val http4sCore = Def.setting("org.http4s" %%% "http4s-core" % "0.23.32")
  lazy val http4sEmber =
    Def.setting("org.http4s" %%% "http4s-ember-client" % "0.23.32")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.1.1")
  lazy val munitScalaCheck =
    Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.1.0")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.4.4")
  lazy val scalaCheck =
    Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0")
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.2.19")
  lazy val scalaTestPlusScalacheck =
    Def.setting("org.scalatestplus" %%% "scalacheck-1-17" % "3.2.18.0")
}
