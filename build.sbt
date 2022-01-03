import Dependencies._

name := "five_hundred_game"

version := "0.1"

scalaVersion := "2.13.7"

scalacOptions := Seq(
  "-Xsource:3",
)

libraryDependencies ++= Seq(
  Cats.Core,
  Cats.Effect,
  Http4s.BlazeServer,
  Http4s.BlazeClient,
  Http4s.Server,
  Http4s.Circe,
  Http4s.Dsl,
  Circe.Generic,
)
