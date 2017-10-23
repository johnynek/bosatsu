import sbt._

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core" % "1.0.0-MF"
  lazy val dagon = "com.stripe" %% "dagon-core" % "0.2.2"
  lazy val decline = "com.monovore" %% "decline" % "0.4.0-M2"
  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "1.0.0"
  lazy val paiges = "org.typelevel" %% "paiges-core" % "0.2.0"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.5"
}
