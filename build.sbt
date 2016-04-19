name in ThisBuild := "scalaground"
version in ThisBuild := "1.0"
scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file("."))
  .aggregate(macros, core)

lazy val macros = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = project.dependsOn(macros).settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,

    "com.avsystem.commons" %% "commons-core" % "1.14.0",
    "com.chuusai" %% "shapeless" % "2.2.5",
    "org.jsoup" % "jsoup" % "1.8.3",
    "com.typesafe.akka" %% "akka-actor" % "2.3.15"
  )
)
