package gatis.g500.server.http

import cats.Applicative

import scala.collection.mutable.ListBuffer
import cats.syntax.all.*
import main.{Actions, Card, Deck, Game, PlayerIndex}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}
import io.circe.*
import io.circe.Decoder.Result
import io.circe.generic.JsonCodec
import io.circe.syntax.*
import main.Main.{gameEither, resultsString}
import main.Phase.{Bidding, GameEnd, PassCards, PlayCards, RoundEnd, TakeCards}

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

final case class Table[F[_]](players: List[PlayerInfo[F]], id: TableId, game: Option[Game]) {
  def getPlayerInfoFromIndex(playerIndex: PlayerIndex): PlayerInfo[F] =
    players.find(_.playerIndex == playerIndex).get
}

class Registry[F[_]: Applicative] {

  val playersList: ListBuffer[Player[F]] = ListBuffer.empty

  def playersListJson: Json =
    Json.obj(
      "type" -> "playersList".asJson,
      "list" ->
        playersList.map { p =>
          Json.obj(
            "name" -> p.name.value.asJson,
            "isOnline" -> p.isOnline.asJson,
            "tableId" -> (if (p.tableId == TableId("")) Json.Null else p.tableId.value.asJson),
          )
        }.asJson,
    )

  val tablesMap: TrieMap[TableId, Table[F]] = TrieMap.empty

  def tablesJson: Json = Json.obj(
    "type" -> "tablesList".asJson,
    "list" -> tablesMap.map { kv =>
      val (_, t) = kv
      Json.obj(
        "id" -> t.id.value.asJson,
        "players" -> t.players.map { p =>
          p.player.name.value.asJson
        }.asJson,
      )
    }.asJson,
  )

  def generalInfoJson(info: String): String = Json
    .obj(
      "type" -> "generalInfo".asJson,
      "info" -> info.asJson,
    )
    .toString

  def tableJson(table: Table[F], playerIndex: PlayerIndex): String =
    Json
      .obj(
        "type" -> "tableInfo".asJson,
        "tableId" -> table.id.value.asJson,
        "gameInfo" -> (if (table.game.isDefined) gameObjectJson(table.game.get) else Json.Null),
        "players" -> table.players.map { p =>
          Json.obj(
            "name" -> p.player.name.value.asJson,
            "isOnline" -> p.player.isOnline.asJson,
            "playerIndex" -> p.playerIndex.toString.asJson,
            "cards" -> getCards(table, playerIndex, p.playerIndex),
            "cardsFromTable" -> Json.Null, // TODO. send these only when take cards phase. maybe not needed
            "cardFromBidWinner" -> Json.Null, // TODO. send it only when pass cards phase. maybe not needed
            "bid" -> 0.asJson, // TODO. send this only when bidding phase
            "trickCount" -> 0.asJson, // TODO.
            "pointsCollected" -> 0.asJson, // TODO. send this only when round end
          )
        }.asJson,
      )
      .toString

  def getCards(table: Table[F], playerIndexReceivingInfo: PlayerIndex, playerIndexInfoAbout: PlayerIndex): Json =
    table.game match {
      case None => Json.Null
      case Some(game) =>
        if (playerIndexReceivingInfo == playerIndexInfoAbout)
          game
            .players(playerIndexInfoAbout)
            .cardsSorted
            .foldLeft("")((acc, cur) => s"$acc${cur.toStringNormal} ")
            .trim
            .asJson
        else
          game
            .players(playerIndexInfoAbout)
            .cardsSorted
            .foldLeft("")((acc, cur) => s"${acc}X ")
            .trim
            .asJson
    }

  def gameObjectJson(game: Game): Json =
    Json
      .obj(
        "type" -> "gameInfo".asJson,
        "roundNumber" -> game.roundNumber.asJson,
        "phase" -> game.phase.toString.asJson,
        "activePlayerIndex" -> game.activePlayerIndex.toString.asJson,
        "highestBid" -> game.highestBid.asJson,
        "bidWinner" -> (if (game.biddingWinnerIndex.isDefined) game.biddingWinnerIndex.get.toString.asJson
                        else Json.Null),
        "cardsPlayed" -> game.cardsOnBoard.foldLeft("")((acc, cur) => s"$acc${cur.toStringNormal} ").trim.asJson,
        "trumpSuit" -> (if (game.trump.isDefined) game.trump.get.toString.asJson else Json.Null),
      )

  def updatePlayersList(name: PlayerName, toClient: String => F[Unit]): F[Unit] = {
    val index = playersList.indexWhere(_.name == name)
    if (index >= 0) {
      val p = playersList(index).copy(isOnline = true)
      playersList(index) = p
      val tId = p.tableId
      // update tablesMap, message player's table about player being back online and send new tableInfo object
      if (tId.value != "") {
        val t = tablesMap(tId)
        val i = t.players.indexWhere(_.player.name == name)
        val pi = t.players(i).copy(player = p)
        val ps: List[PlayerInfo[F]] = t.players.updated(i, pi)
        tablesMap.update(tId, t.copy(players = ps))
        t.players
          .map(_.player.toClient(generalInfoJson(s"${p.name} is back online")))
          .sequence *> t.players
          .map(ply => ply.player.toClient(tableJson(tablesMap(tId), ply.playerIndex)))
          .sequence *> ()
          .pure[F]
      } else {
        ().pure[F]
      }
    } else {
      playersList.append(Player(name, tableId = TableId(""), toClient, isOnline = true))
      // message all players about new player joining
      playersList.map(_.toClient(generalInfoJson(s"$name joined the site"))).toList.sequence.map(_ => ())
    }
  }

  def disconnect(name: PlayerName): F[Unit] = {
    val index = playersList.indexWhere(_.name == name)
    val p = playersList(index).copy(isOnline = false)
    playersList(index) = p
    // update tablesMap, message player's table about player going offline and send new tableInfo object
    val tId = p.tableId
    if (tId.value != "") {
      val t = tablesMap(tId)
      val i = t.players.indexWhere(_.player.name == name)
      val pi = t.players(i).copy(player = p)
      val ps: List[PlayerInfo[F]] = t.players.updated(i, pi)
      tablesMap.update(tId, t.copy(players = ps))
      t.players
        .map(_.player.toClient(generalInfoJson(s"${p.name} went offline")))
        .sequence *> t.players
        .map(ply => ply.player.toClient(tableJson(tablesMap(tId), ply.playerIndex)))
        .sequence *> ()
        .pure[F]
    } else {
      ().pure[F]
    }
  }

  def addTable(playerName: PlayerName): F[String] = {
    // generate random tableId
    val tableId = TableId(Random.alphanumeric.take(10).mkString(""))
    tablesMap.get(tableId) match {
      case None =>
        playersList.find(_.name == playerName) match {
          case None => s"$playerName not found in players list".pure[F]
          case Some(p) =>
            // if player already at other table, leave it before joining this one
            // TODO. Not sending leaving table messages. why?
            leaveTable(p.tableId, p.name)
            tablesMap.put(tableId, Table(List(PlayerInfo(p, FirstPlayer)), tableId, game = None))
            val index = playersList.indexWhere(_.name == playerName)
            playersList(index) = p.copy(tableId = tableId)
            playersList
              .map(_.toClient(generalInfoJson(s"New table $tableId just opened")))
              .toList
              .sequence
              .map(_ => s"Table crated. ID: $tableId") // maybe not needed
        }
      case _ => s"Cannot create table with same ID: $tableId".pure[F]
    }
  }

  def joinTable(tableId: TableId, playerName: PlayerName): F[String] =
    tablesMap.get(tableId) match {
      case Some(table) if table.players.length >= 3 =>
        "Table full".pure[F] // TODO. message player why he could not join maybe?
      case Some(table) =>
        if (table.players.exists(_.player.name == playerName)) {
          s"$playerName already at table $tableId".pure[F]
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
                // INIT GAME
                val game = Game.init(1, FirstPlayer, Nil, Deck.shuffle)

                // update tablesMap
                val tt = t.copy(game = Some(game))
                tablesMap.update(tableId, tt)

                t.players
                  .map(_.player.toClient(generalInfoJson(s"$playerName joined your table")))
                  .sequence *> t.players
                  .map(ply => ply.player.toClient(tableJson(tt, ply.playerIndex)))
                  .sequence *> s"$playerName joined table $tableId".pure[F]
              } else {
                // message and tableInfo object
                table.players
                  .map(_.player.toClient(generalInfoJson(s"$playerName joined your table")))
                  .sequence *> t.players
                  .map(ply => ply.player.toClient(tableJson(t, ply.playerIndex)))
                  .sequence *> s"$playerName joined table $tableId".pure[F]
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
            s"Table ${tableId.value} removed".pure[F]
          } else {
            tablesMap.update(tableId, table.copy(players = ps, game = None))
            ps
              .map(_.player.toClient(generalInfoJson(s"$playerName left your table")))
              .sequence *> ps
              .map(ply => ply.player.toClient(tableJson(tablesMap(tableId), ply.playerIndex)))
              .sequence *> s"$playerName left table $tableId"
              .pure[F]
          }

        } else {
          s"$playerName not at this table".pure[F]
        }
      case _ => "Table not found".pure[F]
    }

  def playTurn(tableId: TableId, playerName: PlayerName, input: String): F[String] =
    tablesMap.get(tableId) match {
      case Some(table) =>
        table.game match {
          case Some(game) =>
            val ply = table.players.find(_.player.name == playerName).get
            if (ply.playerIndex != game.activePlayerIndex) {
              List(ply.player.toClient(generalInfoJson("It's not your turn!"))).sequence.map(_ =>
                "Not players turn",
              ) // TODO. No List needed
            } else {
              // HERE ALL THE GAME MAGIC HAPPENS
              val newGame = game.phase match {
                case Bidding =>
                  for {
                    bid <- input.toIntOption.toRight("Invalid bid")
                    newRound <- Actions.makeBid(game, bid)
                  } yield newRound
                case TakeCards => Actions.takeCards(game)
                case PassCards =>
                  val inputSplit = input.split(" ")
                  for {
                    _ <- if (inputSplit.length == 2) Right(()) else Left("Wrong Input as 2 cards")
                    cardLeft <- Card.fromString(inputSplit(0)).toRight("Card Left invalid")
                    cardRight <- Card.fromString(inputSplit(1)).toRight("Card Right invalid")
                    newRound <- Actions.passCards(game, cardLeft, cardRight)
                  } yield newRound
                case PlayCards =>
                  for {
                    card <- Card.fromString(input).toRight("Invalid card typed")
                    newRound <- Actions.playCard(game, card)
                  } yield newRound
                case RoundEnd =>
                  for {
                    newGame <- Actions.updateGameAfterRound(game)
                  } yield newGame
                case GameEnd => Left("Game End") // TODO. What Now?
              }

              println(newGame)

              newGame match {
                case Right(value) =>
                  val t = table.copy(game = Some(value))
                  tablesMap.update(tableId, t)
                  // TODO. send general msg about what just happened
                  t.players
                    .map(plyr => plyr.player.toClient(tableJson(t, plyr.playerIndex)))
                    .sequence *> s"$playerName played a turn".pure[F]
                case Left(value) =>
                  ply.player.toClient(generalInfoJson(value))
                  value.pure[F]
              }
            }
          case _ => "Game not found".pure[F]
        }
      case _ => "Table not found".pure[F]
    }

}
