name := "exploding-cats"

version := "1.0"

scalaVersion := "2.13.6"

val scalaTestVersion = "3.2.10"
val circeVersion = "0.14.1"
val catsVersion = "2.3.0"
val catsCollectionsVersion = "0.9.0"
val catsEffectsVersion = "3.2.9"
// val fs2Version = "3.2.0"
val http4sVersion = "0.23.7"
// val http4sVersion = "0.21.22"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-collections-core" % catsCollectionsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectsVersion,

  // "co.fs2" %% "fs2-core" % fs2Version,

  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,

  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)