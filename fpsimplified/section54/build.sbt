import Dependencies._

ThisBuild / scalaVersion     := "2.12.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "section54",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.typelevel" %% "cats" % "0.9.0",
      "org.typelevel" %% "cats-effect" % "0.3"
    )
  )
scalacOptions += "-deprecation"
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
