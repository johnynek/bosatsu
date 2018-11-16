import sbt._

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core" % "1.0.0-MF"
  lazy val dagon = "com.stripe" %% "dagon-core" % "0.2.2"
  lazy val decline = "com.monovore" %% "decline" % "0.4.0-M2"
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "1.0.0"
  lazy val paiges = "org.typelevel" %% "paiges-core" % "0.2.0"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.5"
  lazy val betterFiles = "com.github.pathikrit"  %% "better-files" % "3.5.0"
  lazy val scalatra = "org.scalatra" %% "scalatra" % "2.6.3"
  lazy val scalatraForms = "org.scalatra" %% "scalatra-forms" % "2.6.3"
  lazy val jettyWebapp = "org.eclipse.jetty" % "jetty-webapp" % jettyVersion
  lazy val jettyPlus = "org.eclipse.jetty" % "jetty-plus" % jettyVersion

  private val jettyVersion = "9.4.6.v20170531"
}
