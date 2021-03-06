import Dependencies._

//ThisBuild / scalaVersion := "3.1.0"
ThisBuild / scalaVersion := "2.13.7"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "adventofcode",
    scalafmtOnCompile := true,
    libraryDependencies += "dev.zio" %% "zio-streams" % "2.0.0-M6-2",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
