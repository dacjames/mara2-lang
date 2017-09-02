lazy val commonSettings = Seq(
  organization := "io.dac",
  version := "0.1.0",
  scalaVersion := "2.12.3",
  libraryDependencies ++= Seq(
    // Scala Dependencies
    "org.parboiled" %% "parboiled" % "2.1.4",
    "org.scalactic" %% "scalactic" % "3.0.0",
    "org.typelevel" %% "cats" % "0.9.0",

    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
  ) ++ Seq(
    // Java Dependencies
    "ch.qos.logback" %  "logback-classic" % "1.1.7"
  ) ++ Seq(
    "org.scalatest" %% "scalatest" % "3.0.0"  % "test"
  )
)

lazy val macros =
  (project in file("./macros")).
    settings(commonSettings: _*).
    settings(
      name := "mara-macros"
    )

lazy val core =
  (project in file("./core")).
  dependsOn(macros).
  settings(commonSettings: _*).
  settings(
    name := "mara-core",
    libraryDependencies ++= Seq(
      "io.netty" % "netty-all" % "4.1.4.Final"
    )
  )

lazy val all =
  (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "mara-lang"
  ).aggregate(macros, core)




