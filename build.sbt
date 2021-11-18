name := "exploding-cats"

version := "1.0"

scalaVersion := "2.13.6"

val catsVersion = "2.3.0"
val catsEffectsVersion = "3.2.9"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectsVersion
)