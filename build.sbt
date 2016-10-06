name := "SuuntoToStrava"

version := "0.6.0-alpha"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.8")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

val appMain = "net.suunto3rdparty.Main"

mainClass in (Compile, run) := Some(appMain)

mainClass in (Compile, packageBin) := Some(appMain)

initialize := {
  val _ = initialize.value
  val specVersion = sys.props("java.specification.version")
  if (Set("1.5", "1.6", "1.7") contains specVersion) {
    val javaHome = sys.props("java.home")
    sys.error(s"Java 8 or higher is required for this project, found $specVersion (from $javaHome).")
  }
}

val log4jVersion = "2.5"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion

libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % log4jVersion

libraryDependencies += "org.apache.commons" % "commons-math" % "2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.11"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.2"

libraryDependencies += "org.apache.httpcomponents" % "fluent-hc" % "4.3.6"

libraryDependencies += "org.apache.httpcomponents" % "httpmime" % "4.3.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"
