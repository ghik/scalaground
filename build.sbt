import sbt.Keys.version

inThisBuild(Seq(
  name := "scalaground",
  version := "1.0",
  scalaVersion := "2.12.3",
  resolvers += Resolver.sonatypeRepo("releases")
))

lazy val scalaground = project.in(file("."))
  .aggregate(macros, core)

lazy val macros = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = project.dependsOn(macros).settings(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,

    "com.avsystem.commons" %% "commons-core" % "1.25.4",
    "org.apache.commons" % "commons-lang3" % "3.4",
    "com.chuusai" %% "shapeless" % "2.3.2",
    "org.jsoup" % "jsoup" % "1.8.3",
    "org.reactivemongo" %% "reactivemongo" % "0.12.1",
    "org.slf4j" % "slf4j-api" % "1.7.21",
    "com.lihaoyi" %% "pprint" % "0.4.4",
    "com.lihaoyi" %% "upickle" % "0.4.4",
    "org.imgscalr" % "imgscalr-lib" % "4.2",
    "io.circe" %% "circe-core" % "0.8.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )
)
