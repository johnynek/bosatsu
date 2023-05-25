import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.9.0")
  lazy val catsEffect =
    Def.setting("org.typelevel" %%% "cats-effect" % "3.4.10")
  lazy val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "0.3.9")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "2.4.1")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "1.4.0")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "1.4.0")
  lazy val jython = Def.setting("org.python" % "jython-standalone" % "2.7.3")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.4.2")
  lazy val scalaCheck =
    Def.setting("org.scalacheck" %%% "scalacheck" % "1.17.0")
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.2.16")
  lazy val scalaTestPlusScalacheck =
    Def.setting("org.scalatestplus" %%% "scalacheck-1-17" % "3.2.15.0")
}
