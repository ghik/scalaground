import sbt.Keys.version

inThisBuild(Seq(
  name := "scalaground",
  version := "1.0",
  scalaVersion := "2.13.14",
  resolvers ++= Resolver.sonatypeOssRepos("releases"),

  Compile / scalacOptions ++= Seq(
    "-encoding", "utf-8",
    "-Yrangepos",
    "-explaintypes",
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-Xfatal-warnings",
    "-Xlint:-missing-interpolator,-adapted-args,-unused,_",
    "-Ycache-plugin-class-loader:last-modified",
    "-Ycache-macro-class-loader:last-modified",
    "-Xnon-strict-patmat-analysis",
    "-Xlint:-strict-unsealed-patmat"
  ),
))

lazy val scalaground = project.in(file("."))
  .aggregate(macros, core)

lazy val macros = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = project.dependsOn(macros).settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,

    "com.avsystem.commons" %% "commons-core" % "2.16.0",
    "com.avsystem.commons" %% "commons-mongo" % "2.16.0",
    "org.mongodb" % "mongodb-driver-reactivestreams" % "5.1.0",
    "com.google.guava" % "guava" % "31.1-jre",
    "org.apache.commons" % "commons-lang3" % "3.14.0",
    "org.jsoup" % "jsoup" % "1.17.2",
    "org.slf4j" % "slf4j-api" % "2.0.12",
    "org.imgscalr" % "imgscalr-lib" % "4.2",
    "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
)
