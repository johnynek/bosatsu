import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val alleycats = Def.setting("org.typelevel" %%% "alleycats-core" % "2.0.0")
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.0.0")
  lazy val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "2.0.0")
  lazy val dagon = Def.setting("com.stripe" %%% "dagon-core" % "0.2.2")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "1.0.0")
  lazy val fastparse = Def.setting("com.lihaoyi" %%% "fastparse" % "1.0.0")
  lazy val fastparseCats = Def.setting("org.bykn" %%% "fastparse-cats-core" % "0.1.0")
  lazy val http4sDsl = Def.setting("org.http4s" %%% "http4s-dsl" % "0.21.4")
  lazy val http4sBlazeServer = Def.setting("org.http4s" %%% "http4s-blaze-server" % "0.21.4")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "0.14.1")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "0.14.1")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.3.0")
  lazy val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.13.5")
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.0.3")
}
