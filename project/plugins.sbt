addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.0.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox" % "0.9.2")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.4")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.6.0")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.1")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.8.2")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")

// This is adding this compiler plugin as a dependency for the build, not the code itself
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.4"
