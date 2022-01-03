import sbt._

object Dependencies {
  val CirceVersion = "0.14.1"
  object Circe {
    val Generic = "io.circe" %% "circe-generic" % CirceVersion
    val GenericExtras = "io.circe" %% "circe-generic-extras" % CirceVersion
    val Parser = "io.circe" %% "circe-parser" % CirceVersion
    val Optics = "io.circe" %% "circe-optics" % CirceVersion
  }
  val CatsVersion = "2.7.0"
  val CatsEffectVersion = "2.5.4"
  object Cats {
    val Effect = "org.typelevel" %% "cats-effect" % CatsEffectVersion
    val Core = "org.typelevel" %% "cats-core" % CatsVersion
  }
  val Http4sVersion = "0.22.8"
  object Http4s {
    val BlazeServer = "org.http4s" %% "http4s-blaze-server" % Http4sVersion
    val BlazeClient = "org.http4s" %% "http4s-blaze-client" % Http4sVersion
    val Dsl = "org.http4s" %% "http4s-dsl" % Http4sVersion
    val Circe = "org.http4s" %% "http4s-circe" % Http4sVersion
    val Server = "org.http4s" %% "http4s-server" % Http4sVersion
  }

}
