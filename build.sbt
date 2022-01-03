import Dependencies._

name := "five_hundred_game"

version := "0.1"

scalaVersion := "2.13.7"

val Http4sVersion = "0.22.8"
val catsVersion = "2.7.0"
val catsEfVersion = "2.5.4"

scalacOptions := Seq(
  "-Xsource:3",
//  "-Wconf:msg=\\$implicit\\$:s", // https://github.com/oleg-py/better-monadic-for/issues/50#issuecomment-788150296
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % catsEfVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
  "org.http4s" %% "http4s-dsl" % Http4sVersion,
  "org.http4s" %% "http4s-circe" % Http4sVersion,
  "org.http4s" %% "http4s-server" % Http4sVersion,
  Circe.Generic,
)
