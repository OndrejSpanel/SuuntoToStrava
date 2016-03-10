name := "SuuntoToStrava"

version := "1.0"

scalaVersion := "2.11.8"

val log4jVersion = "2.5"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % log4jVersion

libraryDependencies += "org.apache.commons" % "commons-math" % "2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.2.1"
