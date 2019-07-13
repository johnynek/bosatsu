import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  lazy val alleycats = Def.setting("org.typelevel" %%% "alleycats-core" % "1.6.0")
  lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "1.6.0")
  lazy val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "1.3.0")
  lazy val dagon = Def.setting("com.stripe" %%% "dagon-core" % "0.2.2")
  lazy val decline = Def.setting("com.monovore" %%% "decline" % "0.4.2")
  lazy val fastparse = Def.setting("com.lihaoyi" %%% "fastparse" % "1.0.0")
  lazy val fastparseCats = Def.setting("org.bykn" %%% "fastparse-cats-core" % "0.1.0")
  lazy val jawnParser = Def.setting("org.typelevel" %%% "jawn-parser" % "0.14.1")
  lazy val jawnAst = Def.setting("org.typelevel" %%% "jawn-ast" % "0.14.1")
  lazy val paiges = Def.setting("org.typelevel" %%% "paiges-core" % "0.2.0")
  lazy val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.13.5")
  lazy val scalaTest = Def.setting("org.scalatest" %%% "scalatest" % "3.0.3")

  lazy val scalatra = Def.setting("org.scalatra" %% "scalatra" % "2.6.3")
  lazy val scalatraForms = Def.setting("org.scalatra" %% "scalatra-forms" % "2.6.3")
  lazy val jettyWebapp = Def.setting("org.eclipse.jetty" % "jetty-webapp" % jettyVersion)
  lazy val jettyPlus = Def.setting("org.eclipse.jetty" % "jetty-plus" % jettyVersion)
  lazy val circeCore = Def.setting("io.circe" %% "circe-core" % circeVersion)
  lazy val circeGeneric = Def.setting("io.circe" %% "circe-generic" % circeVersion)
  lazy val circeParser = Def.setting("io.circe" %% "circe-parser" % circeVersion)

  private val jettyVersion = "9.4.6.v20170531"
  private val circeVersion = "0.8.0"
}
