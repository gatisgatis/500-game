import sbt._



object Dependencies {
  val CirceVersion = "0.14.1"
  object Circe {
    val Generic = "io.circe" %% "circe-generic" % CirceVersion
    val GenericExtras = "io.circe" %% "circe-generic-extras" % CirceVersion
    val Parser = "io.circe" %% "circe-parser" % CirceVersion
    val Optics = "io.circe" %% "circe-optics" % CirceVersion
  }
}