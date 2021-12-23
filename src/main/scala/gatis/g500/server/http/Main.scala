package gatis.g500.server.http

import cats.Monad
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.websocket.*
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.circe.*
import org.http4s.implicits.*
import fs2.*
import fs2.concurrent.Queue
import org.http4s.implicits.*

import scala.concurrent.duration.*
import org.http4s.dsl.Http4sDsl
import cats.syntax.all.*
import gatis.g500.server.http.Methods.{Player, TableRegistry}
import org.http4s.dsl.impl.QueryParamDecoderMatcher

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.util.Random

object G500 extends IOApp {
  override def run(
    args: List[String],
  ): IO[ExitCode] = {
    // šeit izveidoju table registrāciju un tad mutēju to tad, kad vajadzīgs.
    // TrieMap
    val tableRegistry = new TableRegistry[IO]

    for {
      _ <- Server.run[IO](tableRegistry, global)
    } yield ExitCode.Success
  }
}

object Server {

  object PlayerNameQueryParamMatcher extends QueryParamDecoderMatcher[String]("playerName")

  def run[F[_]: Timer: ContextShift: ConcurrentEffect](
    tableRegistry: TableRegistry[F],
    executionContext: ExecutionContext,
  ): F[Unit] = {

    val dsl = new Http4sDsl[F] {}
    import dsl.*
    val service: HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root => Ok("This Route Does Nothing")

//      case GET -> Root / username / "open_table" =>
////        val id = Random.between(0, Int.MaxValue).toString
//        val id = Random.alphanumeric.take(10).mkString("")
//        val result = tableRegistry.add(id, username)
//        result match {
//          case Right(value) =>
//            tableRegistry = value
//            Ok(s"$username opened table with id $id")
//          case Left(msg) => Ok(s"Error. $msg")
//        }

//      case GET -> Root / username / "join_table" / id =>
//        val result = tableRegistry.join(id, username)
//        result match {
//          case Right(value) =>
//            tableRegistry = value
//            Ok(s"$username joined table with id $id")
//          case Left(msg) => Ok(s"Error. $msg")
//        }

//      case GET -> Root / username / "leave_table" / id =>
//        val result = tableRegistry.unJoin(id, username)
//        result match {
//          case Right(value) =>
//            tableRegistry = value
//            Ok(s"$username left table with id $id")
//          case Left(msg) => Ok(s"Error. $msg")
//        }

      case GET -> Root / "tables" =>
        Ok(tableRegistry.tables.map { t =>
          (t._1, t._2.players.size)
        })

      case req @ POST -> Root / "first-post-route" =>
        req.as[String].flatMap(body => Ok(s"dsdasd $body"))

      case GET -> Root / tableName / "ws" :? PlayerNameQueryParamMatcher(playerName) =>
        def fc: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
          case Text(t, _) => tableRegistry.post(tableName, t, playerName).void
          // izstrādāt post metodi, kas atrod vajadzīgo galdu un nohandlo, kuram spēlētam queue pievieno ziņu utt
          case Close(_) => tableRegistry.leave(tableName, playerName).void
          case f =>
            Sync[F].delay(
              println(s"Unknown type: $f"),
            ) // saņemot Close, nozīmē, ka pazudis connection. jāizdomā, ko darīt tad
        }

        for {
          queue <- Queue.unbounded[F, String]
          toClient = queue.dequeue.through(
            _.evalMap(m => WebSocketFrame.Text(m).pure[F]), // m.asJson.spaces2
          )
          fromClient = fc
          player = Player(playerName, queue.enqueue1)
          _ = tableRegistry.add(tableName, player)
          _ <- tableRegistry.join(tableName, player)
          response <- WebSocketBuilder[F].build(toClient, fromClient)
        } yield response

//        val toClient: Stream[F, WebSocketFrame] =
//          Stream.awakeEvery[F](10.seconds).map(d => Text(s"Ping! $d"))
//        val fromClient: Pipe[F, WebSocketFrame, Unit] = _.evalMap {
//          case Text(t, _) => Sync[F].delay(println(t))
//          case f => Sync[F].delay(println(s"Unknown type: $f"))
//        }
//        WebSocketBuilder[F].build(toClient, fromClient)
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
