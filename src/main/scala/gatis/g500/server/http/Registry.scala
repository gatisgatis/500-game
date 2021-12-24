package gatis.g500.server.http

import cats.Applicative

import scala.collection.mutable.ListBuffer
import cats.syntax.all.*
import main.{Deck, Game, PlayerIndex}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}

import scala.collection.concurrent.TrieMap
import scala.util.Random

final case class TableId(value: String) extends AnyVal {
  override def toString = value
}

final case class PlayerName(value: String) extends AnyVal {
  override def toString = value
}

final case class Player[F[_]](name: PlayerName, tableId: TableId, toClient: String => F[Unit], isOnline: Boolean)

final case class PlayerInfo[F[_]](player: Player[F], playerIndex: PlayerIndex)

final case class Table[F[_]](players: List[PlayerInfo[F]], id: TableId, game: String) {
  def getPlayerInfoFromIndex(playerIndex: PlayerIndex): PlayerInfo[F] =
    players.find(_.playerIndex == playerIndex).get
}

class Registry[F[_]: Applicative] {

  val playersList: ListBuffer[Player[F]] = ListBuffer.empty

  val tablesMap: TrieMap[TableId, Table[F]] = TrieMap.empty

  def updatePlayersList(name: PlayerName, toClient: String => F[Unit]): F[Unit] = {
    val index = playersList.indexWhere(_.name == name)
    if (index >= 0) {
      playersList(index) = playersList(index).copy(isOnline = true)
      // TODO. send msg to players on table if player was on table
      playersList.map(_.toClient(s"$name back online")).toList.sequence.map(_ => ()) // maybe not needed
      //      ().pure[F]
    } else {
      playersList.append(Player(name, tableId = TableId(""), toClient, isOnline = true))
      playersList.map(_.toClient(s"$name joined")).toList.sequence.map(_ => ()) //  maybe not needed
    }
  }

  def disconnect(name: PlayerName): F[Unit] = {
    val index = playersList.indexWhere(_.name == name)
    playersList(index) = playersList(index).copy(isOnline = false)
    // TODO. send msg to players on table if player was on table
    playersList.map(_.toClient(s"$name offline")).toList.sequence.map(_ => ()) // maybe not needed
  }

  def addTable(playerName: PlayerName): F[String] = {
    // generate random tableId
    val tableId = TableId(Random.alphanumeric.take(10).mkString(""))
    tablesMap.get(tableId) match {
      case None =>
        playersList.find(_.name == playerName) match {
          case None => s"$playerName not found in players list".pure[F]
          case Some(p) =>
            tablesMap.put(tableId, Table(List(PlayerInfo(p, FirstPlayer)), tableId, game = "todo"))
            val index = playersList.indexWhere(_.name == playerName)
            playersList(index) = playersList(index).copy(tableId = tableId)
            playersList
              .map(_.toClient(s"new table opened $tableId"))
              .toList
              .sequence
              .map(_ => s"Table crated. ID: $tableId") // maybe not needed
        }
      case _ => s"Cannot create table with same ID: $tableId".pure[F]
    }
  }

  def joinTable(tableId: TableId, playerName: PlayerName): F[String] =
    tablesMap.get(tableId) match {
      case Some(table) if table.players.length >= 3 => "Table full".pure[F] // notify player why he could not join
      case Some(table) =>
        if (table.players.exists(_.player.name == playerName)) {
          s"${playerName} already at table $tableId".pure[F]
        } else {
          playersList.find(_.name == playerName) match {
            case None => s"$playerName not found in players list".pure[F]
            case Some(p) =>
              // if player already at other table, leave it before joining this one
              leaveTable(p.tableId, p.name)
              // update tablesMap
              val pi: PlayerIndex = if (table.players.size == 1) SecondPlayer else ThirdPlayer
              val t = table.copy(players = table.players :+ PlayerInfo(p, pi))
              tablesMap.update(tableId, t)

              // update player at playerList
              val index = playersList.indexWhere(_.name == playerName)
              playersList(index) = playersList(index).copy(tableId = tableId)

              if (t.players.size == 3) {
                // If 3 players joined, then should init game
                val game = Game.init(1, FirstPlayer, Nil, Deck.shuffle)

                // Send player specific game object to all players at table
                val firstPlayerCards = game.players(FirstPlayer).cards.toString()
                val secondPlayerCards = game.players(SecondPlayer).cards.toString()
                val thirdPlayerCards = game.players(ThirdPlayer).cards.toString()

                val msg1 = t.getPlayerInfoFromIndex(FirstPlayer).player.toClient(firstPlayerCards)
                val msg2 = t.getPlayerInfoFromIndex(SecondPlayer).player.toClient(secondPlayerCards)
                val msg3 = t.getPlayerInfoFromIndex(ThirdPlayer).player.toClient(thirdPlayerCards)

                List(msg1, msg2, msg3).sequence *> table.players
                  .map(_.player.toClient("sending cards now"))
                  .sequence *> "Ok".pure[F]
              } else {
                // Send all other players at this table msg about new player joining
                table.players
                  .map(_.player.toClient(s"${playerName} joined this table"))
                  .sequence
                  .map(_ => s"$playerName joined table $tableId")
              }
          }
        }
      case _ => "Table not found".pure[F]
    }

  def leaveTable(tableId: TableId, playerName: PlayerName): F[String] =
    tablesMap.get(tableId) match {
      case Some(table) =>
        if (table.players.exists(_.player.name == playerName)) {
          val ps = table.players.filter(_.player.name != playerName)
          val index = playersList.indexWhere(_.name == playerName)
          playersList(index) = playersList(index).copy(tableId = TableId(""))
          if (ps.size.isEmpty) {
            tablesMap.remove(tableId)
            "Info".pure[F]
          } else {
            tablesMap.update(tableId, table.copy(players = ps))
            // Send all other players at this table msg about player leaving
            ps.map(_.player.toClient(s"$playerName left this table"))
              .sequence
              .map(_ => s"$playerName left table $tableId")
          }

        } else {
          s"$playerName not at this table".pure[F]
        }
      case _ => "Table not found".pure[F]
    }

}
