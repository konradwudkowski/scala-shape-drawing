import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "drawing",
    trapExit := false,
    libraryDependencies ++= List(
      cats,
      scalaTest % Test
    ),
    scalacOptions ++= Seq("-feature", "-Xfatal-warnings", "-Ywarn-dead-code", "-Ywarn-unused:imports"),
    scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
  )
