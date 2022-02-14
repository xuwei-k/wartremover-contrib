addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.11")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.1.1")

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.1.0")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.3")

libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value
