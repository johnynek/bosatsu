import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.4.2")
  lazy val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "2.3.3")
  lazy val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "0.3.1")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "1.3.0")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "1.0.3")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "1.0.3")
  lazy val jython = Def.setting("org.python" % "jython-standalone" % "2.7.2")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.4.0")
  lazy val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.15.3")
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.2.3")
  lazy val scalaTestPlusScalacheck = Def.setting("org.scalatestplus" %%% "scalacheck-1-14" % "3.2.2.0")
}
