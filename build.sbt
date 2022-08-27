import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "ru.otus"
ThisBuild / organizationName := "otus"

lazy val root = (project in file("."))
  .settings(
    name := "scala-dev-mooc-2022-06",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += catsCore,
    libraryDependencies ++= zio,
    addCompilerPlugin(Dependencies.kindProjector)
  )