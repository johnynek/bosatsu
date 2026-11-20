addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.5.0")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.2.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.14.0")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox" % "0.11.1")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.1.0-RC2")
addSbtPlugin("com.github.sbt" % "sbt-git" % "3.0.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.23.0")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.6.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("com.github.sbt" % "sbt-jmh" % "0.4.9")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.1")

// This is adding this compiler plugin as a dependency for the build, not the code itself
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.21"
