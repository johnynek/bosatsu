import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val alleycats = Def.setting("org.typelevel" %%% "alleycats-core" % "2.1.1")
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.1.1")
  lazy val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "2.2.0")
  lazy val dagon = Def.setting("com.stripe" %%% "dagon-core" % "0.2.2")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "1.2.0")
  lazy val fastparse = Def.setting("com.lihaoyi" %%% "fastparse" % "1.0.0")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "1.0.0")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "1.0.0")
  lazy val jython = Def.setting("org.python" % "jython-standalone" % "2.7.2")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.3.0")
  lazy val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.14.3")
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.0.8")
}
