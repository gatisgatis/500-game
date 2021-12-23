package gatis.g500.server.http

import cats.Applicative

import scala.collection.concurrent.TrieMap
import cats.syntax.all.*

object Methods {
  type Game = String

  case class Player[F[_]](id: String, toClient: String => F[Unit])

  case class Table[F[_]](players: List[Player[F]], id: String, game: Game)

  class TableRegistry[F[_]: Applicative] {

    val tables: TrieMap[String, Table[F]] = TrieMap.empty

    def add(tableId: String, player: Player[F]): Boolean =
      tables.get(tableId) match {
        case None => tables.put(tableId, Table(List(player), tableId, "todo")); true
        case _ => false
      }

    def join(tableId: String, player: Player[F]): F[String] =
      tables.get(tableId) match {
        case Some(table) if table.players.length >= 3 => "Table full".pure[F]
        case Some(table) =>
          if (table.players.contains(player)) {
            s"${player.id} already at table $tableId".pure[F]
          } else {
            tables.update(tableId, table.copy(players = table.players :+ player))
            // remove when not needed
            table.players.map(_.toClient(s"${player.id} joined this table")).sequence.map(_ => "Ok")
          }
        case _ => "Table not found".pure[F]
      }

    def leave(tableId: String, playerName: String): F[String] =
      tables.get(tableId) match {
        case Some(table) =>
          if (table.players.exists(_.id == playerName)) {
            val ps = table.players.filter(_.id != playerName)
            tables.update(tableId, table.copy(players = ps))
            // remove when not needed
            ps.map(_.toClient(s"$playerName left this table")).sequence.map(_ => "Ok")

          } else {
            s"$playerName not at this table".pure[F]
          }
        case _ => "Table not found".pure[F]
      }

    def post(tableName: String, t: String, playerName: String): F[String] =
      tables.get(tableName) match {
        case Some(table) =>
          if (table.players.exists(_.id == playerName)) {
            val (me, others) = table.players.partition(_.id == playerName)
            // remove when not needed
            others.map(_.toClient(s"$playerName: $t")).sequence *>
              me.map(_.toClient(s"Msg Sent")).sequence *> "Ok".pure[F]

          } else {
            s"$playerName not at this table".pure[F]
          }
        case _ => "Table not found".pure[F]
      }

    //    def remove(id: String): Either[String, TableRegistry[F]] =
//      tables.get(id) match {
//        case Some(_) =>
//          Right(this.copy(tables = tables - id))
//        case _ => Left("Table not found")
//      }
  }

}
