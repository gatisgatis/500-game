package main

import main.Phase.{Bidding, PassCards, PlayCards, TakeCards}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}
import main.Rank.{King, Queen}

object Actions {

  private def checkForInvalidBid(game: Game, bid: Int): Either[String, Unit] =
    if (bid >= 0 && bid % 5 != 0) Left("Invalid bid. Bid must be with a step of 5")
    else if (bid >= 0 && bid < 60) Left(s"Bid must be greater or equal to 60")
    else if (bid > 205) Left(s"Bid is too high")
    else if (bid >= 0 && bid <= game.highestBid)
      Left(s"Bid must be greater than bid form previous bidder (${game.highestBid})")
    else Right(())

  private def nextToBidPlayerIndex(game: Game, bid: Int): Either[String, PlayerIndex] = {
    val nextIndex = game.activePlayerIndex.next
    val prevIndex = game.activePlayerIndex.previous
    val nextPlayer = game.players(nextIndex)
    val prevPlayer = game.players(prevIndex)

    if (nextPlayer.bid >= 0) Right(nextIndex)
    else if (prevPlayer.bid >= 0) Right(prevIndex)
    else if (bid >= 0) Right(game.activePlayerIndex)
    else Left("All players have passed")

  }

  def makeBid(game: Game, bid: Int): Either[String, Game] =
    for {
      _ <- if (game.phase != Bidding) Left("Cannot make a bid. No bidding phase now") else Right(())
      player = game.players(game.activePlayerIndex)
      _ <- if (player.bid >= 0) Right(()) else Left(s"${game.activePlayerIndex} has already passed this round")
      playerGamePoints = game.results.lastOption match {
        case None => 0
        case Some(line) => line.pointsGame(game.activePlayerIndex)
      }
      _ <-
        if (playerGamePoints < 1000) Right(())
        else Left(s"${game.activePlayerIndex} are not allowed to bid if total points above 1000. Only pass allowed.")
      _ <- checkForInvalidBid(game, bid)
    } yield {

      val nextToBidPlayerIndexEither = nextToBidPlayerIndex(game, bid)

      // change Phase if all players have passed
      val newPhase = if (nextToBidPlayerIndexEither.isRight) game.phase else game.phase.nextPhase

      // if no next player to bid then last non-passing bidder starts next phase. it's always current bidder
      val newActivePlayerIndex = nextToBidPlayerIndexEither.getOrElse(game.activePlayerIndex)

      val newHighestBid = if (bid > game.highestBid) bid else game.highestBid

      val newBiddingWinnerIndex = if (bid > game.highestBid) Some(game.activePlayerIndex) else game.biddingWinnerIndex

      val newActivePlayer = player.copy(bid = bid)

      val newPlayers = game.players.updated(game.activePlayerIndex, newActivePlayer)

      // Info msg about what happened this turn
      val msg = s"ACTION: ${game.activePlayerIndex} ${if (bid > 0) s"made a bid of $bid" else "passed"}"
      println(msg)
      println()

      game.copy(
        phase = newPhase,
        activePlayerIndex = newActivePlayerIndex,
        players = newPlayers,
        highestBid = newHighestBid,
        biddingWinnerIndex = newBiddingWinnerIndex,
      )
    }

  def takeCards(game: Game): Either[String, Game] =
    if (game.phase != TakeCards) Left("Cannot take cards. No take-cards phase now.")
    else {
      val player = game.players(game.activePlayerIndex)
      val newCards = player.cards ::: game.cardsToTake
      val newPlayer = player.copy(cards = newCards)
      val newPlayers = game.players.updated(game.activePlayerIndex, newPlayer)

      // Info msg about what happened this turn
      val msg =
        s"ACTION: ${game.activePlayerIndex} took ${game.cardsToTake.foldLeft("")((acc, cur) => acc + cur.toString + " ")} from the board"
      println(msg)
      println()

      Right(game.copy(players = newPlayers, cardsToTake = Nil, phase = game.phase.nextPhase))
    }

  def passCards(game: Game, cardToLeft: Card, cardToRight: Card): Either[String, Game] =
    for {
      _ <- if (game.phase != PassCards) Left("Cannot pass cards. No pass-cards phase now") else Right(())
      activePlayer = game.players(game.activePlayerIndex)
      _ <-
        if (!activePlayer.cards.contains(cardToLeft) || !activePlayer.cards.contains(cardToRight))
          Left("Player's hand does not contain picked cards")
        else Right(())
      playerOnLeft = game.players(game.activePlayerIndex.next)
      playerOnRight = game.players(game.activePlayerIndex.previous)
    } yield {
      val newCardsActivePlayer = activePlayer.cards.filter(card => card != cardToRight && card != cardToLeft)
      val newActivePlayer = activePlayer.copy(cards = newCardsActivePlayer)

      val newCardsPlayerLeft = playerOnLeft.cards :+ cardToLeft
      val newPlayerOnLeft = playerOnLeft.copy(cards = newCardsPlayerLeft)

      val newCardsOnRight = playerOnRight.cards :+ cardToRight
      val newPlayerOnRight = playerOnRight.copy(cards = newCardsOnRight)

      val newPlayers = Map(
        game.activePlayerIndex -> newActivePlayer,
        game.activePlayerIndex.next -> newPlayerOnLeft,
        game.activePlayerIndex.previous -> newPlayerOnRight,
      )

      // Info msg about what happened this turn
      val msg: String =
        s"ACTION: ${game.activePlayerIndex} passed $cardToRight to ${game.activePlayerIndex.previous} and $cardToLeft to ${game.activePlayerIndex.next}"
      println(msg)
      println()

      game.copy(players = newPlayers, phase = game.phase.nextPhase)
    }

  private def getCardsAllowedToPlay(cards: List[Card], requiredSuit: Option[Suit]): List[Card] =
    requiredSuit match {
      case Some(suit) =>
        val requiredSuitCards = cards.filter(_.suit == suit)
        if (requiredSuitCards.isEmpty) cards
        else requiredSuitCards
      case _ => cards
    }

  private def getWinnerCardOfTrick(cards: List[Card], requiredSuit: Suit, trump: Suit): Card = {
    val trumpsPlayed = cards.filter(_.suit == trump)
    if (trumpsPlayed.nonEmpty) {
      trumpsPlayed.sortWith(_.value > _.value).head
    } else {
      val requiredSuitCardsPlayed = cards.filter(_.suit == requiredSuit)
      requiredSuitCardsPlayed.sortWith(_.value > _.value).head
    }
  }

  def playCard(game: Game, card: Card): Either[String, Game] = {
    for {
      _ <- if (game.phase != PlayCards) Left("Cannot play cards. No play-cards phase now") else Right(())
      activePlayer = game.players(game.activePlayerIndex)
      _ <- if (!activePlayer.cards.contains(card)) Left("Player's hand does not contain picked card") else Right(())
      cardsAllowedToPlay = getCardsAllowedToPlay(activePlayer.cards, game.requiredSuit)
      _ <- if (!cardsAllowedToPlay.contains(card)) Left("Not allowed to play this card") else Right(())
    } yield {

      val newCardsOnBoard = game.cardsOnBoard :+ card

      val newActivePlayerCards = activePlayer.cards.filter(_ != card)

      val newActivePlayersPlayedCard = Some(card)

      val newActivePlayer = activePlayer.copy(cards = newActivePlayerCards, playedCard = newActivePlayersPlayedCard)

      game.requiredSuit match {
        // This means it's not first card on the board
        // If it's third card then must determine:
        // - player taking trick
        // - points for taking trick
        // - if it was last card played this round
        case Some(suit) =>
          // 3rd card player
          if (newCardsOnBoard.length >= 3) {
            val pointsFromTrick = newCardsOnBoard.foldLeft(0)((acc, cur) => acc + cur.value)

            val cardTakingTrick = getWinnerCardOfTrick(newCardsOnBoard, suit, game.trump.get)

            // if index of the card taking the trick is known, it's possible to determine playerIndex who played this card
            val index = newCardsOnBoard.indexOf(cardTakingTrick)
            val playerIndexTakingTrick = {
              if (index == 0) game.activePlayerIndex.next
              else if (index == 1) game.activePlayerIndex.previous
              else game.activePlayerIndex
            }

            // What if player taking trick is the same as active player? How to update points and cards then...?
            val playerTakingTrick = {
              if (playerIndexTakingTrick == game.activePlayerIndex) newActivePlayer
              // this could throw?
              else game.players(playerIndexTakingTrick)
            }

            val newPlayerTakingTrick = playerTakingTrick.copy(
              points = playerTakingTrick.points + pointsFromTrick,
              trickCount = playerTakingTrick.trickCount + 1,
            )

            // update players Map. Order matters
            val newPlayers = game.players
              .updated(game.activePlayerIndex, newActivePlayer)
              .updated(playerIndexTakingTrick, newPlayerTakingTrick)

            // change phase if last card played
            val newPhase = if (newActivePlayer.cards.isEmpty) game.phase.nextPhase else game.phase

            // Info msg about what happened this turn
            val msg: String =
              s"ACTION: ${game.activePlayerIndex} played $card. $playerIndexTakingTrick took down a trick of " +
                s"${newCardsOnBoard.foldLeft("")((acc, cur) => acc + cur.toString + " ")}and collected $pointsFromTrick points"
            println(msg)
            println()

            game.copy(
              players = newPlayers,
              phase = newPhase,
              cardsOnBoard = Nil,
              previousTrick = newCardsOnBoard,
              requiredSuit = None,
              activePlayerIndex = playerIndexTakingTrick,
            )

            // 2nd card played
          } else {

            val newPlayers = game.players.updated(game.activePlayerIndex, newActivePlayer)

            // Info msg about what happened this turn
            val msg: String = s"ACTION: ${game.activePlayerIndex} played $card"
            println(msg)
            println()

            game.copy(
              cardsOnBoard = newCardsOnBoard,
              activePlayerIndex = game.activePlayerIndex.next,
              players = newPlayers,
            )
          }
        // First card on the board. New required suit, possibly new trump, possibly bonus points must be determined
        // This means it's first card on the board. Must determine:
        // - new required suit
        // - bonus points for marriages
        // - possibly new trump if it's first card this round
        case None =>
          // updates trump
          val newTrump = if (game.trump.isEmpty) Some(card.suit) else game.trump

          // updates points if has 'marriage'. 40 for trump, 20 for non-trump marriage
          val pointsFromMarriage = if (card.rank == Queen) {
            val hasSameSuitKing = newActivePlayerCards.contains(Card(card.suit, King))
            if (hasSameSuitKing) {
              if (card.suit == newTrump.get) 40 else 20
            } else 0
          } else if (card.rank == King) {
            val hasSameSuitQueen = newActivePlayerCards.contains(Card(card.suit, Queen))
            if (hasSameSuitQueen) {
              if (card.suit == newTrump.get) 40 else 20
            } else 0
          } else 0

          val nextPlayer = game.players(game.activePlayerIndex.next)
          val prevPlayer = game.players(game.activePlayerIndex.previous)

          val newPlayers = game.players
            .updated(game.activePlayerIndex, newActivePlayer)
            .updated(game.activePlayerIndex, newActivePlayer.copy(points = newActivePlayer.points + pointsFromMarriage))
            .updated(game.activePlayerIndex.next, nextPlayer.copy(playedCard = None))
            .updated(game.activePlayerIndex.previous, prevPlayer.copy(playedCard = None))

          // Info msg about what happened this turn
          val optionalMsgAboutBonusPoints =
            if (pointsFromMarriage > 0) s" and announced marriage collecting additional $pointsFromMarriage points"
            else ""
          val msg: String = s"ACTION: ${game.activePlayerIndex} played $card$optionalMsgAboutBonusPoints"
          println(msg)
          println()

          game.copy(
            cardsOnBoard = newCardsOnBoard,
            requiredSuit = Some(card.suit),
            trump = newTrump,
            activePlayerIndex = game.activePlayerIndex.next,
            players = newPlayers,
          )
      }
    }
  }

  def updateGameAfterRound(game: Game): Either[String, Game] = {

    val winningBid = game.highestBid
    // should be safe because this method wont be called when bidding winner is not known
    val playerIndexWinningBid = game.biddingWinnerIndex.get

    def determineRoundPoints(playerIndex: PlayerIndex): Int = {
      val points = game.players(playerIndex).points
      if (playerIndexWinningBid == playerIndex) {
        if (winningBid <= points) winningBid
        else -winningBid
      } else {
        val diffOfFive = points % 5
        val resultPoints =
          if (diffOfFive > 2) points - diffOfFive + 5
          else points - diffOfFive
        // If player's gamePonts is under 100, only way to decrease gamePoints is by bidding..
        game.results.lastOption match {
          case None => resultPoints
          case Some(line) =>
            val gamePoints = line.pointsGame(playerIndex)
            if (gamePoints < 100) 0 else resultPoints
        }
      }
    }

    val firstPlayerRoundPoints = determineRoundPoints(FirstPlayer)
    val secondPlayerRoundPoints = determineRoundPoints(SecondPlayer)
    val thirdPlayerRoundPoints = determineRoundPoints(ThirdPlayer)

    val newPointsRound: Map[PlayerIndex, Int] = Map(
      FirstPlayer -> firstPlayerRoundPoints,
      SecondPlayer -> secondPlayerRoundPoints,
      ThirdPlayer -> thirdPlayerRoundPoints,
    )

    val newPointsGame: Map[PlayerIndex, Int] = game.results match {
      case Nil =>
        Map(
          FirstPlayer -> (500 - firstPlayerRoundPoints),
          SecondPlayer -> (500 - secondPlayerRoundPoints),
          ThirdPlayer -> (500 - thirdPlayerRoundPoints),
        )
      case _ =>
        val actualResults = game.results.last
        val newFirstPlayerPoints = actualResults.pointsGame(FirstPlayer) - firstPlayerRoundPoints
        val newSecondPlayerPoints = actualResults.pointsGame(SecondPlayer) - secondPlayerRoundPoints
        val newThirdPlayerPoints = actualResults.pointsGame(ThirdPlayer) - thirdPlayerRoundPoints
        Map(
          FirstPlayer -> newFirstPlayerPoints,
          SecondPlayer -> newSecondPlayerPoints,
          ThirdPlayer -> newThirdPlayerPoints,
        )
    }

    val result = new Results {
      val winningBid: Int = game.highestBid
      val playerIndexWinningBid: PlayerIndex = game.biddingWinnerIndex.get
      val pointsRound: Map[PlayerIndex, Int] = newPointsRound
      val pointsGame: Map[PlayerIndex, Int] = newPointsGame
    }

    val newResults = game.results :+ result

    // Info msg about what happened this turn
    val msg = playerIndexWinningBid match {
      case FirstPlayer =>
        s"$FirstPlayer played a game ($winningBid) and ${if (firstPlayerRoundPoints > 0) "won" else "lost"}"
      case SecondPlayer =>
        s"$SecondPlayer played a game ($winningBid) and ${if (secondPlayerRoundPoints > 0) "won" else "lost"}"
      case ThirdPlayer =>
        s"$ThirdPlayer played a game ($winningBid) and ${if (thirdPlayerRoundPoints > 0) "won" else "lost"}"
    }
    println(msg)
    println()

    def determineWinnerOfGame: Option[PlayerIndex] =
      if (result.pointsGame(FirstPlayer) <= 0) Some(FirstPlayer)
      else if (result.pointsGame(SecondPlayer) <= 0) Some(SecondPlayer)
      else if (result.pointsGame(ThirdPlayer) <= 0) Some(ThirdPlayer)
      else None

    // if there is winner, change game phase of previous game-state else init new game to start new round
    determineWinnerOfGame match {
      case None =>
        Right(Game.init(game.roundNumber + 1, game.playerIndexStartingThisRound.next, newResults, Deck.shuffle))
      case Some(winnerIndex) =>
        println(s"$winnerIndex won the game!!!")
        println()
        Right(game.copy(results = newResults, phase = game.phase.nextPhase))
    }

  }

}
