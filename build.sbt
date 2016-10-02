name := "mara2-lang"

version := "0.1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= List(
  // Scala Dependencies
  "org.parboiled" %% "parboiled" % "2.1.3",
  "org.scalactic" %% "scalactic" % "3.0.0"
) ++ List (
  // Java Dependencies
  "io.netty" % "netty-all" % "4.1.4.Final",
  "ch.qos.logback" %  "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
) ++ List(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)