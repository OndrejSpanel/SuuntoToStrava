name := "SuuntoToStrava"

version := "1.0"

scalaVersion := "2.11.8"

val log4jVersion = "2.5"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % log4jVersion

libraryDependencies += "org.apache.commons" % "commons-math" % "2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "com.typesafe.akka" %% "akka-http-experimental" % "2.4.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"
