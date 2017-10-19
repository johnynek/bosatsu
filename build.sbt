import Dependencies._

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalacOptions += "-Ypartial-unification"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.bykn",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "edgemar",
    libraryDependencies ++=
      Seq(
        cats,
        scalaTest % Test
      )
  )
