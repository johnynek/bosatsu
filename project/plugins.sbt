addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox" % "0.10.7")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.8")
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.1.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.21.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.2")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.2")

// This is adding this compiler plugin as a dependency for the build, not the code itself
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.20"
