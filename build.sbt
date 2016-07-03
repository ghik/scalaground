name in ThisBuild := "scalaground"
version in ThisBuild := "1.0"
scalaVersion in ThisBuild := "2.11.8"

lazy val scalaground = project.in(file("."))
  .aggregate(macros, core)

lazy val macros = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = project.dependsOn(macros).settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,

    "com.avsystem.commons" %% "commons-core" % "1.14.0",
    "org.apache.commons" % "commons-lang3" % "3.4",
    "com.chuusai" %% "shapeless" % "2.2.5",
    "org.jsoup" % "jsoup" % "1.8.3",
    "com.typesafe.akka" %% "akka-actor" % "2.4.4",
    "org.reactivemongo" %% "reactivemongo" % "0.11.11",
    "org.slf4j" % "slf4j-api" % "1.7.21",
    "com.lihaoyi" %% "pprint" % "0.4.0",
    "com.lihaoyi" %% "upickle" % "0.4.0",
    "org.imgscalr" % "imgscalr-lib" % "4.2"
  )
)
