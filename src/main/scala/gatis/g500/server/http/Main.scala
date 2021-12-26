package gatis.g500.server.http

import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.websocket.*
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.implicits.*
import fs2.*
import fs2.concurrent.Queue
import org.http4s.dsl.Http4sDsl
import cats.syntax.all.*
import gatis.g500.server.http.Command.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

object G500 extends IOApp {
  override def run(
    args: List[String],
  ): IO[ExitCode] = {
    val registry = new Registry[IO]
    for {
      _ <- Server.run[IO](registry, global)
    } yield ExitCode.Success
  }
}

object Server {

//  object PlayerNameQueryParamMatcher extends QueryParamDecoderMatcher[String]("playerName")

  def run[F[_]: Timer: ContextShift: ConcurrentEffect](
    registry: Registry[F],
    executionContext: ExecutionContext,
  ): F[Unit] = {

    val dsl = new Http4sDsl[F] {}
    import dsl.*
    val service: HttpRoutes[F] = HttpRoutes.of[F] {

      case GET -> Root / "players" => Ok(registry.playersListJson.toString)

      case GET -> Root / "tables" => Ok(registry.tablesJson.toString)

      case req @ POST -> Root / "test-post-route" =>
        req.as[String].flatMap(body => Ok(s"Test $body"))

      case GET -> Root / playerName / "ws" =>
        def fc: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case Text(t, _) =>
            val command = WsMsgParser.parse(t)
            command match {
              case OpenTable => registry.addTable(PlayerName(playerName)).void
              case JoinTable(tableId) => registry.joinTable(tableId, PlayerName(playerName)).void
              case LeaveTable(tableId) => registry.leaveTable(tableId, PlayerName(playerName)).void
              case _ => Sync[F].delay(println(s"INVALID INPUT - $playerName: $t"))
            }
          case Close(_) => registry.disconnect(PlayerName(playerName)).void // change status to OFFLINE
          case f => Sync[F].delay(println(s"Unknown type: $f"))
        }

        for {
          queue <- Queue.unbounded[F, String]
          toClient = queue.dequeue.through(
            _.evalMap(m => WebSocketFrame.Text(m).pure[F]), // m.asJson.spaces2
          )
          fromClient = fc
          _ <- registry.updatePlayersList(PlayerName(playerName), queue.enqueue1)
          response <- WebSocketBuilder[F].build(toClient, fromClient)
        } yield response
    }

    val httpApp = Router("/" -> service).orNotFound

    BlazeServerBuilder[F](executionContext)
      .withWebSockets(true)
      .bindHttp(port = 9000, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain

  }
}
