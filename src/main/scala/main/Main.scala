package main

import main.Cards.Rank.{Jack, King, Ten, Ace, Queen, Nine}
import main.Cards.Suit.{Club, Heart, Spade, Diamond}
import main.Cards.{Card, Deck, Suit}

object Main extends App {

  type RoundNumber = Int // positive integers
  type PlayerIndex = Int // 0, 1, 2
  type Score = Int // positive/negative integers
  type Bid = Int
  type ErrorMsg = String
  type RoundPhase = String // bidding, take-cards, pass-cards, play, round-ending

  case class Board(cardsPlayed: List[Card], requiredSuit: Option[Suit])

  case class Hand(cards: List[Card], bid: Bid, points: Score) {
    override def toString: String = {
      val _hand = "[ " + cards.foldLeft("")((acc, cur) => acc + cur.toString + ", ") + "]"
      val _bid = if(bid < 0) "Passed" else if (bid == 0) "No bid made" else bid.toString
      val _points = points.toString
      "Cards: " + _hand + " | Bid: " + _bid + " | Points: " + _points
    }
  }

  case class Round(
      board: Board,
      cardsToTake: List[Card],
      trump: Option[Suit],
      highestBid: Option[Bid],
      playerOnActionIndex: PlayerIndex,
      roundNumber: RoundNumber,
      players: Map[PlayerIndex, Hand],
      phase: RoundPhase
  ) {
    override def toString: String = {
      val _cardsOnBoard = "[ " + board.cardsPlayed.foldLeft("")((acc, cur) => acc + cur.toString + ", ") + "]"
      val _requiredSuit = board.requiredSuit match {
        case Some(suit) => suit match {
          case Club => Console.GREEN + "club" + Console.RESET
          case Diamond => Console.BLUE + "diamond" + Console.RESET
          case Heart => Console.RED + "heart" + Console.RESET
          case Spade => Console.WHITE + "spade" + Console.RESET
        }
        case _ => "none"
      }
      val _cardsToTake = "[ " + cardsToTake.foldLeft("")((acc, cur) => acc + cur.toString + ", ") + "]"
      val _trump = trump match {
        case Some(suit) => suit match {
          case Club => Console.GREEN + "club" + Console.RESET
          case Diamond => Console.BLUE + "diamond" + Console.RESET
          case Heart => Console.RED + "heart" + Console.RESET
          case Spade => Console.WHITE + "spade" + Console.RESET
        }
        case _ => "none"
      }
      val _highestBid = highestBid match {
        case Some(bid) => s"$bid"
        case None => "-"
      }
      val _playerIndex = Console.BOLD + playerOnActionIndex.toString + Console.RESET
      val _roundNumber = roundNumber.toString

      val _player0 = players.get(0)
      val _player1 = players.get(1)
      val _player2 = players.get(2)

      s"""|Round Number: ${_roundNumber} | Phase: ${phase} | Cards To Take: ${_cardsToTake} | HighestBid: ${_highestBid} | Trump: ${_trump}
         |Player to Act: ${_playerIndex} | RequiredSuit: ${_requiredSuit} | Cards Played: ${_cardsOnBoard}
         |Players:
         |Player 0: ${_player0}
         |Player 1: ${_player1}
         |Player 2: ${_player2}
         |
         |""".stripMargin
    }
  }

  case class Game(currentResults: Map[PlayerIndex, Score], history: Map[RoundNumber, Map[PlayerIndex, (List[Card], Score, Bid)]])


  def initNewRound(roundNumber: RoundNumber, startPlayer: PlayerIndex, deck: List[Card]): Round = {
    val board = Board(Nil, None)
    val cardsToTake = deck.slice(0,3)
    val hand1 = Hand(deck.slice(3,10), bid = 0, points = 0)
    val hand2 = Hand(deck.slice(10,17), bid = 0, points = 0)
    val hand3 = Hand(deck.slice(17,24), bid = 0, points = 0)
    val players = Map(0 -> hand1, 1 -> hand2, 2 -> hand3)
    Round(board, cardsToTake,None, None, startPlayer, roundNumber, players, "bidding")
  }

  def currentHandToAct(round: Round): Option[Hand] = {
    round.players.get(round.playerOnActionIndex)
  }

  def indexOnTheLeft(currentIndex: PlayerIndex): PlayerIndex = {
    if(currentIndex >= 2) 0
    else currentIndex + 1
  }

  def indexOnTheRight(currentIndex: PlayerIndex): PlayerIndex = {
    if(currentIndex <= 0) 2
    else currentIndex - 1
  }

  def nextToActPlayerIndex(round: Round): Option[PlayerIndex] = {
    val phase = round.phase

    if(phase == "bidding") {
      val playersNotPassedIndexes = round.players.filter(_._2.bid >= 0).keys
      if(playersNotPassedIndexes.isEmpty) {
        None
      } else {
        // TODO.
        Some(1)
      }
    } else None
  }

  def makeBid(round: Round, newBid: Bid): Either[ErrorMsg, Round] = {
    if(newBid % 5 != 0) {
      Left("Invalid bid. Bid must be with a step of 5")
    } else {
      val handToAct = currentHandToAct(round)
      handToAct match {
        case Some(hand) =>
          if(hand.bid < 0) {
            Left(s"Player ${round.playerOnActionIndex} has already passed this round")
          } else if (newBid >= 0 && newBid <= round.highestBid.getOrElse(0)) {
            Left(s"Bid must be greater than previous bid (${round.highestBid.getOrElse(0)})")
          } else if (newBid >= 0 && newBid < 60) {
            Left(s"Bid must be greater or equal to 60")
          } else if (newBid > 225) {
            Left(s"Bid is too high")
          } else {

            val nextToActPlayerIndexOption = nextToActPlayerIndex(round)

            val phase = nextToActPlayerIndexOption match {
              case None => "take-cards"
              case _ => "bidding"
            }

            val nextIndex = nextToActPlayerIndexOption.getOrElse(round.playerOnActionIndex)

            val newHighestBid = Some(newBid)

            val newHand = hand.copy(bid = newBid)

            val newPlayers = round.players.updated(round.playerOnActionIndex, newHand)

            Right(round.copy(highestBid = newHighestBid, phase = phase, playerOnActionIndex = nextIndex, players = newPlayers))
          }
        case None => Left("Error. makeBid. Hand not found")
      }
    }
  }


  def getCardsFromBoard(round: Round): Either[ErrorMsg, Round] = {
    val hand = currentHandToAct(round)

    val newCards = hand.get.cards ::: round.cardsToTake

    val newHand = hand.get.copy(cards = newCards)

    val newPlayers = round.players.updated(round.playerOnActionIndex, newHand)

    Right(round.copy(players = newPlayers, cardsToTake = Nil, phase = "pass-cards"))
  }

  def passCards(round: Round, cardToRight: Card, cardToLeft: Card): Either[ErrorMsg, Round] = {
    val hand = currentHandToAct(round)

    if(!hand.get.cards.contains(cardToRight) || !hand.get.cards.contains(cardToLeft)) {
      Left(s"Invalid cards picked")
    } else {
      val newCards = hand.get.cards.filter(card => card != cardToRight && card != cardToLeft)
      val newHand = hand.get.copy(cards = newCards)

      val indexRight = indexOnTheRight(round.playerOnActionIndex)
      val handRight = round.players.get(indexRight)
      val newCardsRight = handRight.get.cards :+ cardToRight
      val newHandRight = handRight.get.copy(cards = newCardsRight)

      val indexLeft = indexOnTheLeft(round.playerOnActionIndex)
      val handLeft = round.players.get(indexLeft)
      val newCardsLeft = handLeft.get.cards :+ cardToLeft
      val newHandLeft = handLeft.get.copy(cards = newCardsLeft)

      val newPlayers = Map(indexLeft -> newHandLeft, indexRight -> newHandRight, round.playerOnActionIndex -> newHand)

      Right(round.copy(players = newPlayers, phase = "play"))
    }

  }

  def playCard(round: Round, playedCard: Card): Either[ErrorMsg, Round] = {
    val hand = currentHandToAct(round)
    if(!hand.get.cards.contains(playedCard)) Left("No Such card in hand")
    else {
      round.board.requiredSuit match {
        case Some(suit) => {
          val hasRequiredSuitCardsInTheHand: Boolean = hand.get.cards.exists(_.suit == suit)
          if (hasRequiredSuitCardsInTheHand && playedCard.suit != suit) Left("Played card does not match required suit")
          else {
            // update players cards in hand
            val newCards: List[Card] = hand.get.cards.filter(_ != playedCard)

            // update cards on board
            val newCardsPlayed: List[Card] = round.board.cardsPlayed :+ playedCard

            val isThreeCardsOnBoard: Boolean = newCardsPlayed.length >= 3

            def calculatePointsFromTrick(cards: List[Card]): Int = cards.foldLeft(0)((acc, cur) => {
              acc + cur.value
            })

            def getWinnerCardOfTrick(cards: List[Card], requiredSuit: Suit, trump: Suit): Card = {
              val trumpsPlayed = cards.filter(_.suit == trump)
              if (trumpsPlayed.nonEmpty) {
                trumpsPlayed.sortWith(_.value > _.value).head
              } else {
                val requiredSuitCardsPlayed = cards.filter(_.suit == requiredSuit)
                requiredSuitCardsPlayed.sortWith(_.value > _.value).head
              }
            }

            if (isThreeCardsOnBoard) {

              val cardTakingTrick = getWinnerCardOfTrick(newCardsPlayed, round.board.requiredSuit.get, round.trump.get)

              val index = newCardsPlayed.indexOf(cardTakingTrick)

              val playerIndexTakingTrick = if (index == 2) round.playerOnActionIndex
                else if (index == 1) indexOnTheRight(round.playerOnActionIndex)
                else indexOnTheLeft(round.playerOnActionIndex)

              val pointsForTrick = calculatePointsFromTrick(newCardsPlayed)

              val newHand = hand.get.copy(cards = newCards)
              val newHand2 = round.players(playerIndexTakingTrick).copy(points = pointsForTrick)

              val newPlayers = round.players.updated(round.playerOnActionIndex, newHand)
              val newPlayers_ = newPlayers.updated(playerIndexTakingTrick, newHand2)

              val newBoard = round.board.copy(cardsPlayed = Nil, requiredSuit = None)

              // TODO
              // PĀRBAUDĪT, VAI NAV IZSPĒLĒTA PĒDĒJĀ KĀRTS UN NAV JĀIET UZ SCORING FĀZI??

              Right(round.copy(players = newPlayers_, playerOnActionIndex = playerIndexTakingTrick, board = newBoard))

            } else {
              val nextPlayerIndex = indexOnTheLeft(round.playerOnActionIndex)

              val newHand = hand.get.copy(cards = newCards)

              val newPlayers = round.players.updated(round.playerOnActionIndex, newHand)

              val newBoard = round.board.copy(cardsPlayed = newCardsPlayed)

              Right(round.copy(players = newPlayers, playerOnActionIndex = nextPlayerIndex, board = newBoard))
            }
          }
        }
        case _ => {
          // update players cards in hand
          val newCards = hand.get.cards.filter(_ != playedCard)

          // update cards on board
          val newCardsPlayed = List(playedCard)

          // update required suit
          val newRequiredSuit = Some(playedCard.suit)

          // updates trump
          val newTrump = if(round.trump.isEmpty) Some(playedCard.suit) else round.trump

          // updates points if has 'marriage'. 40 for trump, 20 for non-trump marriage
          val pointsFromMarriage = if(playedCard.rank == Queen) {
            val hasSameSuitKing = newCards.contains(Card(playedCard.suit, King))
            if(hasSameSuitKing) {
              if (playedCard.suit == newTrump.get) 40 else 20
            } else 0
          } else if(playedCard.rank == King) {
            val hasSameSuitQueen = newCards.contains(Card(playedCard.suit, Queen))
            if(hasSameSuitQueen) {
              if (playedCard.suit == newTrump.get) 40 else 20
            } else 0
          } else 0

          val nextPlayerIndex = indexOnTheLeft(round.playerOnActionIndex)

          val newPoints = hand.get.points + pointsFromMarriage

          val newHand = hand.get.copy(cards = newCards, points = newPoints)

          val newPlayers = round.players.updated(round.playerOnActionIndex, newHand)

          val newBoard = round.board.copy(cardsPlayed = newCardsPlayed, requiredSuit = newRequiredSuit)

          Right(round.copy(players = newPlayers, playerOnActionIndex = nextPlayerIndex, board = newBoard, trump = newTrump))
        }
      }
    }
  }


  val round = initNewRound(1, 0, Deck.deck)
  println(round)

  val round2 = playCard(round, Card(Club, King))
  println(round2)

  val round3 = playCard(round2.getOrElse(round), Card(Club, Ace))
  println(round3)

  val round4 = playCard(round3.getOrElse(round), Card(Club, Queen))
  println(round4)

  val round5 = playCard(round4.getOrElse(round), Card(Spade, Jack))
  println(round5)

}
