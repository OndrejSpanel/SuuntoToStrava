name := "SuuntoToStrava"

version := "0.8.0-alpha"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.7")

javacOptions ++= Seq("-source", "1.7", "-target", "1.7", "-Xlint")

val appMain = "net.suunto3rdparty.Main"

mainClass in (Compile, run) := Some(appMain)

mainClass in (Compile, packageBin) := Some(appMain)

val log4jVersion = "2.5"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % log4jVersion

libraryDependencies += "org.apache.commons" % "commons-math" % "2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "joda-time" % "joda-time" % "2.9.4"

libraryDependencies += "org.joda" % "joda-convert" % "1.8.1"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.11"

libraryDependencies += "com.typesafe.akka" %% "akka-http-experimental" % "2.4.11"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.2"

libraryDependencies += "org.apache.httpcomponents" % "fluent-hc" % "4.3.6"

libraryDependencies += "org.apache.httpcomponents" % "httpmime" % "4.3.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"
