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
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "2.5.0")
  lazy val ff4s = Def.setting("io.github.buntec" %%% "ff4s" % "0.26.1")
  lazy val fs2core = Def.setting("co.fs2" %%% "fs2-core" % "3.12.2")
  lazy val fs2io = Def.setting("co.fs2" %%% "fs2-io" % "3.12.2")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "1.6.0")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "1.6.0")
  lazy val jython = Def.setting("org.python" % "jython-standalone" % "2.7.4")
  lazy val http4sCore = Def.setting("org.http4s" %%% "http4s-core" % "0.23.33")
  lazy val http4sEmberClient =
    Def.setting("org.http4s" %%% "http4s-ember-client" % "0.23.33")
  lazy val http4sEmberServer =
    Def.setting("org.http4s" %%% "http4s-ember-server" % "0.23.33")
  lazy val http4sDsl = Def.setting("org.http4s" %%% "http4s-dsl" % "0.23.33")
  lazy val http4sCirce = Def.setting("org.http4s" %%% "http4s-circe" % "0.23.33")
  lazy val circeCore = Def.setting("io.circe" %%% "circe-core" % "0.14.10")
  lazy val circeGeneric = Def.setting("io.circe" %%% "circe-generic" % "0.14.10")
  lazy val circeParser = Def.setting("io.circe" %%% "circe-parser" % "0.14.10")
  lazy val munit = Def.setting("org.scalameta" %%% "munit" % "1.2.2")
  lazy val munitScalaCheck =
    Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.2.0")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.4.4")
  lazy val scalaCheck =
    Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0")
  lazy val slf4jNop = Def.setting("org.slf4j" % "slf4j-nop" % "1.7.36")
}
