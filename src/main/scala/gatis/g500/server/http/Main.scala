package gatis.g500.server.http

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.ExecutionContext.global

object G500 extends IOApp {
  override def run(
    args: List[String],
  ): IO[ExitCode] = {
    val registry = new Registry[IO]
    HttpServer
      .make[IO](registry, global)
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
}
