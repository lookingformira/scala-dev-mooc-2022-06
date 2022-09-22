import sbt._

object Dependencies {
  lazy val KindProjectorVersion = "0.10.3"
  lazy val kindProjector =
    "org.typelevel" %% "kind-projector" % KindProjectorVersion

  lazy val ZioVersion = "1.0.4"
  lazy val CatsEffectVersion = "3.3.12"
  lazy val PureconfigVersion = "0.17.1"
  lazy val FS2Version = "3.2.10"
  lazy val Http4sVersion = "0.23.13"
  lazy val CirceVersion = "0.14.2"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.3.0"

  lazy val catsEffect: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-effect" % CatsEffectVersion,
    "org.typelevel" %% "cats-effect-kernel" % CatsEffectVersion,
    "org.typelevel" %% "cats-effect-std" % CatsEffectVersion
  )

  lazy val fs2: Seq[ModuleID] = Seq(
    "co.fs2" %% "fs2-core" % FS2Version,
    "co.fs2" %% "fs2-io" % FS2Version
  )

  lazy val http4s: Seq[ModuleID] = Seq(
    "org.http4s" %% "http4s-dsl" % Http4sVersion,
    "org.http4s" %% "http4s-circe" % Http4sVersion,
    "org.http4s" %% "http4s-ember-server" % Http4sVersion,
    "org.http4s" %% "http4s-ember-client" % Http4sVersion
  )

  lazy val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core" % CirceVersion,
    "io.circe" %% "circe-parser" % CirceVersion,
    "io.circe" %% "circe-generic" % CirceVersion,
    "io.circe" %% "circe-literal" % CirceVersion
  )

  lazy val zio: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio" % ZioVersion,
    "dev.zio" %% "zio-test" % ZioVersion,
    "dev.zio" %% "zio-test-sbt" % ZioVersion,
    "dev.zio" %% "zio-macros" % ZioVersion
  )

  lazy val zioConfig: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio-config" % "1.0.5",
    "dev.zio" %% "zio-config-magnolia" % "1.0.5",
    "dev.zio" %% "zio-config-typesafe" % "1.0.5"
  )

}
