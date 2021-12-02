package main

import main.GamePhase.{Bidding, PassCards, PlayCards, RoundEnd, TakeCards}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}
import main.Rank.{King, Queen}

object Actions {

  private def getPlayer(round: Round, playerIndex: PlayerIndex): Either[String, Player] = {
    round.players.get(playerIndex) match {
      case Some(player) => Right(player)
      case None => Left(s"No Player with index '$playerIndex' found")
    }
  }

  private def checkForInvalidBid(round: Round, bid: Int): Either[String, Unit] = {
    if (bid >= 0 && bid % 5 != 0) Left("Invalid bid. Bid must be with a step of 5")
    else if (bid >= 0 && bid < 60) Left(s"Bid must be greater or equal to 60")
    else if (bid > 205) Left(s"Bid is too high")
    else if (bid >= 0 && bid <= round.highestBid) Left(s"Bid must be greater than bid form previous bidder (${round.highestBid})")
    else Right()
  }

  private def nextToBidPlayerIndex(round: Round, bid: Int): Either[String, PlayerIndex] = {
    val nextIndex = round.activePlayerIndex.next
    val prevIndex = round.activePlayerIndex.previous

    for {
      nextPlayer <- getPlayer(round, nextIndex)
      prevPlayer <- getPlayer(round, prevIndex)
      next <- {
        if (nextPlayer.bid >= 0) Right(nextIndex)
        else if (prevPlayer.bid >= 0) Right(prevIndex)
        else if (bid >= 0) Right(round.activePlayerIndex)
        else Left("All players have passed")
      }
    } yield next
  }

  def makeBid(round: Round, bid: Int): Either[String, Round] = {

    for {
      // TODO. Is this the right way to do it?
      _ <- if (round.phase != Bidding) Left("Cannot make a bid. No bidding phase now") else Right()
      player <- getPlayer(round, round.activePlayerIndex)
      _ <- if (player.bid >= 0) Right() else Left(s"${round.activePlayerIndex} has already passed this round")
      _ <- checkForInvalidBid(round, bid)
    } yield {

      val nextToBidPlayerIndexEither = nextToBidPlayerIndex(round, bid)

      // change GamePhase if all players have passed
      val newPhase = if (nextToBidPlayerIndexEither.isRight) round.phase else round.phase.nextPhase

      // if no next player to bid then last non-passing bidder starts next phase. it's always current bidder
      val newActivePlayerIndex = nextToBidPlayerIndexEither.getOrElse(round.activePlayerIndex)

      val newHighestBid = if (bid > round.highestBid) bid else round.highestBid

      val newBiddingWinnerIndex = if (bid > round.highestBid) Some(round.activePlayerIndex) else round.biddingWinnerIndex

      val newActivePlayer = player.copy(bid = bid)

      val newPlayers = round.players.updated(round.activePlayerIndex, newActivePlayer)

      // Info msg about what happened this turn
      val msg = s"ACTION: ${round.activePlayerIndex} ${if(bid > 0) s"did bid $bid" else "passed"}"
      println(msg)
      println()

      round.copy(
        phase = newPhase,
        activePlayerIndex = newActivePlayerIndex,
        players = newPlayers,
        highestBid = newHighestBid,
        biddingWinnerIndex = newBiddingWinnerIndex
      )
    }
  }

  def takeCards(round: Round): Either[String, Round] = {
    if (round.phase != TakeCards) Left("Cannot take cards. No take-cards phase now.")
    else {
      getPlayer(round, round.activePlayerIndex).flatMap(player => {
        val newCards = player.cards ::: round.cardsToTake
        val newPlayer = player.copy(cards = newCards)
        val newPlayers = round.players.updated(round.activePlayerIndex, newPlayer)

        // Info msg about what happened this turn
        val msg = s"ACTION: ${round.activePlayerIndex} took ${round.cardsToTake.foldLeft("")((acc, cur) => acc + cur.toString + " ")} from the board"
        println(msg)
        println()

        Right(round.copy(players = newPlayers, cardsToTake = Nil, phase = round.phase.nextPhase))
      })
    }
  }

  def passCards(round: Round, cardToLeft: Card, cardToRight: Card): Either[String, Round] = {
    for {
      _ <- if (round.phase != PassCards) Left("Cannot pass cards. No pass-cards phase now") else Right()
      activePlayer <- getPlayer(round, round.activePlayerIndex)
      _ <- if (!activePlayer.cards.contains(cardToLeft) || !activePlayer.cards.contains(cardToRight)) Left("Player's hand does not contain picked cards") else Right()
      playerOnLeft <- getPlayer(round, round.activePlayerIndex.next)
      playerOnRight <- getPlayer(round, round.activePlayerIndex.previous)
    } yield {
      val newCardsActivePlayer = activePlayer.cards.filter(card => card != cardToRight && card != cardToLeft)
      val newActivePlayer = activePlayer.copy(cards = newCardsActivePlayer)

      val newCardsPlayerLeft = playerOnLeft.cards :+ cardToLeft
      val newPlayerOnLeft = playerOnLeft.copy(cards = newCardsPlayerLeft)

      val newCardsOnRight = playerOnRight.cards :+ cardToRight
      val newPlayerOnRight = playerOnRight.copy(cards = newCardsOnRight)

      val newPlayers = Map(
        round.activePlayerIndex -> newActivePlayer,
        round.activePlayerIndex.next -> newPlayerOnLeft,
        round.activePlayerIndex.previous -> newPlayerOnRight
      )

      // Info msg about what happened this turn
      val msg: String = s"ACTION: ${round.activePlayerIndex} passed $cardToRight to ${round.activePlayerIndex.previous} and $cardToLeft to ${round.activePlayerIndex.next}"
      println(msg)
      println()

      round.copy(players = newPlayers, phase = round.phase.nextPhase)
    }
  }

  private def getCardsAllowedToPlay(cards: List[Card], requiredSuit: Option[Suit]): List[Card] = {
    requiredSuit match {
      case Some(suit) => {
        val requiredSuitCards = cards.filter(_.suit == suit)
        if(requiredSuitCards.isEmpty) cards
        else requiredSuitCards
      }
      case _ => cards
    }
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

  def playCard(round: Round, card: Card): Either[String, Round] = {
    for {
      _ <- if (round.phase != PlayCards) Left("Cannot play cards. No play-cards phase now") else Right()
      activePlayer <- getPlayer(round, round.activePlayerIndex)
      _ <- if (!activePlayer.cards.contains(card)) Left("Player's hand does not contain picked card") else Right()
      cardsAllowedToPlay = getCardsAllowedToPlay(activePlayer.cards, round.requiredSuit)
      _ <- if (!cardsAllowedToPlay.contains(card)) Left("Not allowed to play this card") else Right()
    } yield {

      val newCardsOnBoard = round.cardsOnBoard :+ card

      val newActivePlayerCards = activePlayer.cards.filter(_ != card)

      val newActivePlayer = activePlayer.copy(cards = newActivePlayerCards)

      round.requiredSuit match {
        // This means it's not first card on the board
        // If it's third card then must determine:
        // - player taking trick
        // - points for taking trick
        // - if it was last card played this round
        case Some(suit) => {

          // 3rd card player
          if(newCardsOnBoard.length >= 3) {
            val pointsFromTrick = newCardsOnBoard.foldLeft(0)((acc, cur) => acc + cur.value)

            val cardTakingTrick = getWinnerCardOfTrick(newCardsOnBoard, suit, round.trump.get)

            // if index of the card taking the trick is known, it's possible to determine playerIndex who played this card
            val index = newCardsOnBoard.indexOf(cardTakingTrick)
            val playerIndexTakingTrick = {
              if(index == 0) round.activePlayerIndex.next
              else if (index == 1) round.activePlayerIndex.previous
              else round.activePlayerIndex
            }

            // What if player taking trick is the same as active player? How to update points and cards then...?
            val playerTakingTrick = {
              if(playerIndexTakingTrick == round.activePlayerIndex) newActivePlayer
              // this could throw?
              else round.players(playerIndexTakingTrick)
            }

            val newPlayerTakingTrick = playerTakingTrick.copy(points = playerTakingTrick.points + pointsFromTrick)

            // update players Map. Order matters
            val newPlayers = round.players.updated(round.activePlayerIndex, newActivePlayer).updated(playerIndexTakingTrick, newPlayerTakingTrick)

            // change phase if last card played
            val newPhase = if(newActivePlayer.cards.isEmpty) round.phase.nextPhase else round.phase

            // Info msg about what happened this turn
            val msg: String = s"ACTION: ${round.activePlayerIndex} played $card. ${playerIndexTakingTrick} took down a trick of " +
              s"${newCardsOnBoard.foldLeft("")((acc, cur) => acc + cur.toString + " ")}and collected $pointsFromTrick points"
            println(msg)
            println()

            round.copy(players = newPlayers, phase = newPhase, cardsOnBoard = Nil, requiredSuit = None, activePlayerIndex = playerIndexTakingTrick)

            // 2nd card played
          } else {

            val newPlayers = round.players.updated(round.activePlayerIndex, newActivePlayer)

            // Info msg about what happened this turn
            val msg: String = s"ACTION: ${round.activePlayerIndex} played $card"
            println(msg)
            println()

            round.copy(cardsOnBoard = newCardsOnBoard, activePlayerIndex = round.activePlayerIndex.next, players = newPlayers)
          }
        }
        // First card on the board. New required suit, possibly new trump, possibly bonus points must be determined
        // This means it's first card on the board. Must determine:
        // - new required suit
        // - bonus points for marriages
        // - possibly new trump if it's first card this round
        case None => {

          // updates trump
          val newTrump = if(round.trump.isEmpty) Some(card.suit) else round.trump

          // updates points if has 'marriage'. 40 for trump, 20 for non-trump marriage
          val pointsFromMarriage = if(card.rank == Queen) {
            val hasSameSuitKing = newActivePlayerCards.contains(Card(card.suit, King))
            if(hasSameSuitKing) {
              if (card.suit == newTrump.get) 40 else 20
            } else 0
          } else if(card.rank == King) {
            val hasSameSuitQueen = newActivePlayerCards.contains(Card(card.suit, Queen))
            if(hasSameSuitQueen) {
              if (card.suit == newTrump.get) 40 else 20
            } else 0
          } else 0

          val newPlayers = round.players.updated(round.activePlayerIndex, newActivePlayer).updated(round.activePlayerIndex, newActivePlayer.copy(points = newActivePlayer.points + pointsFromMarriage))

          // Info msg about what happened this turn
          val optionalMsgAboutBonusPoints = if(pointsFromMarriage > 0) s" and announced marriage collecting additional $pointsFromMarriage points" else ""
          val msg: String = s"ACTION: ${round.activePlayerIndex} played ${card}${optionalMsgAboutBonusPoints}"
          println(msg)
          println()

          round.copy(cardsOnBoard = newCardsOnBoard, requiredSuit = Some(card.suit), trump = newTrump, activePlayerIndex = round.activePlayerIndex.next, players = newPlayers)
        }
      }
    }
  }

  // List[(WinningBid, PlayerIndexWhoWonBidding, Map[PlayerIndex, PointsThisRound])]

  def updateGame(round: Round, game: Game): Game = {
    val winningBid = round.highestBid
    val playerIndexWinningBid = round.biddingWinnerIndex.get

    def determineRoundPoints(points: Int, playerIndex: PlayerIndex): Int = {
      if(playerIndexWinningBid == playerIndex) {
        if(winningBid <= points) winningBid
        else -winningBid
      } else points
    }

    // TODO. Round pointsForRound to nearest 5

    // TODO. Method to check for GAME winner. If someone's total score is 0 or less

    val firstPlayerRoundPoints = determineRoundPoints(round.players(FirstPlayer).points, FirstPlayer)
    val secondPlayerRoundPoints = determineRoundPoints(round.players(SecondPlayer).points, SecondPlayer)
    val thirdPlayerRoundPoints = determineRoundPoints(round.players(ThirdPlayer).points, ThirdPlayer)

    // What is PlayerIndex with Product ???
    val resultsLine: Map[PlayerIndex, Int] = Map(FirstPlayer -> firstPlayerRoundPoints, SecondPlayer -> secondPlayerRoundPoints, ThirdPlayer -> thirdPlayerRoundPoints)
    Game(game.results :+ (winningBid, playerIndexWinningBid, resultsLine))
  }



}
