package gatis.g500.server.http

import cats.effect.{ConcurrentEffect, ContextShift, Resource, Sync, Timer}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.HttpRoutes
import org.http4s.server.{Router, Server}
import org.http4s.websocket.*
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.implicits.*
import fs2.*
import fs2.concurrent.Queue
import org.http4s.dsl.Http4sDsl
import cats.syntax.all.*
import gatis.g500.server.http.Command.*
import org.http4s.server.middleware.*
import scala.concurrent.ExecutionContext

object HttpServer {

  def make[F[_]: Timer: ContextShift: ConcurrentEffect](
    registry: Registry[F],
    executionContext: ExecutionContext,
  ): Resource[F, Server] = {

    val dsl = new Http4sDsl[F] {}
    import dsl.*
    val service: HttpRoutes[F] = HttpRoutes.of[F] {

      case GET -> Root / "players" => Ok(registry.playersListJson)

      case GET -> Root / "tables" => Ok(registry.tablesJson)

      case GET -> Root / "results" / tableId =>
        registry.tablesMap.get(TableId(tableId)) match {
          case Some(table) => Ok(registry.resultsJson(table))
          case None => Ok("Table does not exist")
        }

      case req @ POST -> Root / "login" =>
        req
          .as[String]
          .flatMap { body =>
            registry.playersList.find(_.name.value == body) match {
              case Some(player) => if (player.isOnline) Ok(false) else Ok(true)
              case _ => Ok(true)
            }
          }

      case GET -> Root / playerName / "ws" =>
        def fc: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case Text(t, _) =>
            val command = WsMsgParser.parse(t)
            command match {
              case OpenTable => registry.addTable(PlayerName(playerName)).void
              case JoinTable(tableId) => registry.joinTable(tableId, PlayerName(playerName)).void
              case LeaveTable(tableId) => registry.leaveTable(tableId, PlayerName(playerName)).void
              case PlayTurn(tableId, input) => registry.playTurn(tableId, PlayerName(playerName), input).void
              case _ => Sync[F].delay(println(s"INVALID INPUT - $playerName: $t"))
            }
          case Close(_) => registry.disconnect(PlayerName(playerName)).void
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

    val httpApp = CORS.policy.withAllowOriginAll.httpApp(Router("/" -> service).orNotFound)

    BlazeServerBuilder[F](executionContext)
      .withWebSockets(true)
      .bindHttp(port = 9000, host = "localhost")
      .withHttpApp(httpApp)
      .resource
  }
}
