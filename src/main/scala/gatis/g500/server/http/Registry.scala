package gatis.g500.server.http

import cats.Applicative

import scala.collection.mutable.ListBuffer
import cats.syntax.all.*
import gatis.g500.game.{Actions, Card, Deck, Game, PlayerIndex}
import gatis.g500.game.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}
import io.circe.*
import io.circe.syntax.*
import gatis.g500.game.Phase.{Bidding, GameEnd, PassCards, PlayCards, RoundEnd, TakeCards}

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
  val tablesMap: TrieMap[TableId, Table[F]] = TrieMap.empty

  def updatePlayersList(name: PlayerName, toClient: String => F[Unit]): F[Unit] = {
    val index = playersList.indexWhere(_.name == name)
    if (index >= 0) {
      val p = playersList(index).copy(isOnline = true, toClient = toClient)
      playersList(index) = p
      val tId = p.tableId

      if (tId.value != "") {
        val t = tablesMap(tId)
        val i = t.players.indexWhere(_.player.name == name)
        val pi = t.players(i).copy(player = p)
        val ps: List[PlayerInfo[F]] = t.players.updated(i, pi)
        tablesMap.update(tId, t.copy(players = ps))
        // players at table -> generalInfo
        // players at table -> tableInfo
        // playersList -> playersList
        // me -> tablesList
        ps
          .map(_.player.toClient(generalInfoJson(s"${p.name} is back online")))
          .sequence *> ps
          .map(ply => ply.player.toClient(tableJson(tablesMap(tId), ply.playerIndex)))
          .sequence *> playersList
          .map(_.toClient(playersListJson))
          .toList
          .sequence *> List(p).map(_.toClient(tablesJson)).sequence *> ()
          .pure[F]
      } else {
        // playersList -> playersList
        // me -> tablesList
        playersList
          .map(_.toClient(playersListJson))
          .toList
          .sequence *> List(p).map(_.toClient(tablesJson)).sequence *> ()
          .pure[F]
      }
    } else {
      val p = Player(name, tableId = TableId(""), toClient, isOnline = true)
      playersList.append(p)
      // playersList -> playersList
      // playersList -> generalInfo
      playersList
        .map(_.toClient(playersListJson))
        .toList
        .sequence *>
        playersList.map(_.toClient(generalInfoJson(s"$name joined the site"))).toList.sequence *>
        List(p.toClient(tablesJson)).sequence *> ()
          .pure[F]
    }
  }

  def playersListJson: String =
    Json
      .obj(
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
      .toString

  def tablesJson: String = Json
    .obj(
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
    .toString

  def disconnect(name: PlayerName): F[Unit] = {
    val index = playersList.indexWhere(_.name == name)
    val p = playersList(index).copy(isOnline = false)
    playersList(index) = p
    val tId = p.tableId
    if (tId.value != "") {
      val t = tablesMap(tId)
      val i = t.players.indexWhere(_.player.name == name)
      val pi = t.players(i).copy(player = p)
      val ps: List[PlayerInfo[F]] = t.players.updated(i, pi)
      tablesMap.update(tId, t.copy(players = ps))
      // other players at table -> generalInfo
      // other players at table -> tableInfo
      // playersList -> playersList
      ps
        .map(_.player.toClient(generalInfoJson(s"${p.name} went offline")))
        .sequence *> ps
        .map(ply => ply.player.toClient(tableJson(tablesMap(tId), ply.playerIndex)))
        .sequence *> playersList
        .map(_.toClient(playersListJson))
        .toList
        .sequence *> ()
        .pure[F]
    } else {
      playersList
        .map(_.toClient(playersListJson))
        .toList
        .sequence *> ().pure[F]
    }
  }

  def meJson(tableId: String): String =
    Json
      .obj(
        "type" -> "meInfo".asJson,
        "tableId" -> tableId.asJson,
      )
      .toString

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
            val t = Table(List(PlayerInfo(p, FirstPlayer)), tableId, game = None)
            tablesMap.put(tableId, t)
            val index = playersList.indexWhere(_.name == playerName)
            playersList(index) = p.copy(tableId = tableId)
            // playersList -> generalInfo
            // playersList -> tablesList
            // me -> tableInfo
            playersList
              .map(_.toClient(generalInfoJson(s"New table $tableId just opened")))
              .toList
              .sequence *> playersList
              .map(_.toClient(tablesJson))
              .toList
              .sequence *> playersList
              .map(_.toClient(playersListJson))
              .toList
              .sequence *> List(p).map(_.toClient(tableJson(t, FirstPlayer))).sequence *> s"Table crated. ID: $tableId"
              .pure[F]
        }
      case _ => s"Cannot create table with same ID: $tableId".pure[F]
    }
  }

  def leaveTable(tableId: TableId, playerName: PlayerName): F[String] =
    tablesMap.get(tableId) match {
      case Some(table) =>
        if (table.players.exists(_.player.name == playerName)) {
          val ps = table.players.filter(_.player.name != playerName)
          val index = playersList.indexWhere(_.name == playerName)
          val p = playersList(index)
          playersList(index) = p.copy(tableId = TableId(""))
          if (ps.size.isEmpty) {
            tablesMap.remove(tableId)
            // playersList -> tablesList
            // me -> meInfo
            playersList
              .map(_.toClient(tablesJson))
              .toList
              .sequence *> playersList
              .map(_.toClient(playersListJson))
              .toList
              .sequence *> List(p.toClient(meJson(""))).sequence *> s"Table removed: $tableId"
              .pure[F]
          } else {
            tablesMap.update(tableId, table.copy(players = ps, game = None))
            // other players at table -> generalInfo
            // other players at table -> tableInfo
            // playersList -> tableList
            // playersList -> playersList
            // me -> meJson
            ps
              .map(_.player.toClient(generalInfoJson(s"$playerName left your table")))
              .sequence *> ps
              .map(ply => ply.player.toClient(tableJson(tablesMap(tableId), ply.playerIndex)))
              .sequence *> playersList
              .map(_.toClient(tablesJson))
              .toList
              .sequence *>
              playersList
                .map(_.toClient(playersListJson))
                .toList
                .sequence *> List(p.toClient(meJson(""))).sequence *>
              s"$playerName left table $tableId"
                .pure[F]
          }

        } else {
          s"$playerName not at this table".pure[F]
        }
      case _ => "Table not found".pure[F]
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
              val pi: PlayerIndex =
                if (!table.players.exists(p => p.playerIndex == FirstPlayer)) FirstPlayer
                else if (!table.players.exists(p => p.playerIndex == SecondPlayer)) SecondPlayer
                else ThirdPlayer
              val t = table.copy(players = table.players :+ PlayerInfo(p, pi))
              tablesMap.update(tableId, t)

              // update player at playerList
              val index = playersList.indexWhere(_.name == playerName)
              playersList(index) = playersList(index).copy(tableId = tableId)

              if (t.players.size == 3) {
                // INIT GAME
                val game = Game.init(1, FirstPlayer, Nil, Deck.shuffle)

                val tt = t.copy(game = Some(game))
                tablesMap.update(tableId, tt)

                t.players
                  .map(_.player.toClient(generalInfoJson(s"$playerName joined your table")))
                  .sequence *> t.players
                  .map(ply => ply.player.toClient(tableJson(tt, ply.playerIndex)))
                  .sequence *> playersList
                  .map(_.toClient(tablesJson))
                  .toList
                  .sequence *> playersList
                  .map(_.toClient(playersListJson))
                  .toList
                  .sequence *> s"$playerName joined table $tableId".pure[F]
              } else {
                // message and tableInfo object
                table.players
                  .map(_.player.toClient(generalInfoJson(s"$playerName joined your table")))
                  .sequence *> t.players
                  .map(ply => ply.player.toClient(tableJson(t, ply.playerIndex)))
                  .sequence *> playersList
                  .map(_.toClient(tablesJson))
                  .toList
                  .sequence *> playersList
                  .map(_.toClient(playersListJson))
                  .toList
                  .sequence *> s"$playerName joined table $tableId".pure[F]
              }
          }
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

              newGame match {
                case Right(value) =>
                  val t = table.copy(game = Some(value))
                  tablesMap.update(tableId, t)
                  // TODO. send general msg about what just happened. or other type of msg maybe...
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
            "playedCard" -> getPlayedCard(table, p.playerIndex),
            "cardsFromTable" -> Json.Null, // TODO. send these only when take cards phase. just for better ui. maybe not needed.
            "cardFromBidWinner" -> Json.Null, // TODO. send it only when pass cards phase. just for better ui. maybe not needed
            "bid" -> getBid(table, p.playerIndex),
            "trickCount" -> getTrickCount(table, p.playerIndex),
            "pointsCollected" -> getPoints(table, p.playerIndex),
          )
        }.asJson,
      )
      .toString

  def getPoints(table: Table[F], playerIndexInfoAbout: PlayerIndex): Json =
    table.game match {
      case None => Json.Null
      case Some(game) =>
        if (game.phase == RoundEnd) game.players(playerIndexInfoAbout).points.asJson else Json.Null
    }

  def getPlayedCard(table: Table[F], playerIndexInfoAbout: PlayerIndex): Json =
    table.game match {
      case None => Json.Null
      case Some(game) =>
        game.players(playerIndexInfoAbout).playedCard match {
          case None => Json.Null
          case Some(card) => card.toStringNormal.asJson
        }
    }

  def getTrickCount(table: Table[F], playerIndexInfoAbout: PlayerIndex): Json =
    table.game match {
      case None => Json.Null
      case Some(game) => game.players(playerIndexInfoAbout).trickCount.asJson
    }

  def getBid(table: Table[F], playerIndexInfoAbout: PlayerIndex): Json =
    table.game match {
      case None => Json.Null
      case Some(game) =>
        if (game.phase == Bidding) game.players(playerIndexInfoAbout).bid.asJson else Json.Null
    }

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
        "cardsPrevTrick" -> game.previousTrick.foldLeft("")((acc, cur) => s"$acc${cur.toStringNormal} ").trim.asJson,
        "trumpSuit" -> (if (game.trump.isDefined) game.trump.get.toString.asJson else Json.Null),
        "marriagePoints" -> game.marriagePoints.asJson,
      )

  def resultsJson(table: Table[F]): String =
    table.game match {
      case Some(game) =>
        Json
          .obj(
            "type" -> "results".asJson,
            "isStarted" -> true.asJson,
            "list" -> game.results.map { r =>
              Json.obj(
                "winningBid" -> r.winningBid.asJson,
                "winnerIndex" -> r.playerIndexWinningBid.toString.asJson,
                "Player_1" -> r.pointsGame(FirstPlayer).asJson,
                "Player_2" -> r.pointsGame(SecondPlayer).asJson,
                "Player_3" -> r.pointsGame(ThirdPlayer).asJson,
              )
            }.asJson,
          )
          .toString
      case None =>
        Json
          .obj(
            "type" -> "results".asJson,
            "isStarted" -> false.asJson,
            "list" -> List.empty[String].asJson,
          )
          .toString
    }
}
