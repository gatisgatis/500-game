package main

import main.Phase.{Bidding, GameEnd, PassCards, PlayCards, RoundEnd, TakeCards}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}

final case class Game(
  cardsOnBoard: List[Card],
  requiredSuit: Option[Suit],
  cardsToTake: List[Card],
  trump: Option[Suit],
  highestBid: Int,
  activePlayerIndex: PlayerIndex,
  biddingWinnerIndex: Option[PlayerIndex],
  roundNumber: Int,
  playerIndexStartingThisRound: PlayerIndex,
  players: Map[PlayerIndex, Player],
  phase: Phase,
  results: List[Results],
) {
  override def toString: String = {
    val cardsOnBoardAsString: String =
      if (cardsOnBoard.isEmpty) "---"
      else cardsOnBoard.foldLeft("")((acc, cur) => acc + cur.toString + " ")
    val requiredSuitAsString: String = requiredSuit match {
      case Some(suit) => suit.color + suit.fullName + Console.RESET
      case _ => "---"
    }
    val cardsToTakeAsString: String = if (cardsToTake.isEmpty) "taken" else "hidden"
    val highestBidAsString: String = if (highestBid == 0) "No Bids yet" else highestBid.toString
    val trumpAsString: String = trump match {
      case Some(suit) => suit.color + suit.fullName + Console.RESET
      case _ => "---"
    }
    val activePlayerAsString: String = Console.BOLD + activePlayerIndex + Console.RESET
    val biddingWinnerAsString: String = biddingWinnerIndex match {
      case Some(index) => index.toString
      case _ => "---"
    }
    val prefixForActivePlayer = Console.GREEN + "* " + Console.RESET

    val player1LineAsString = if (activePlayerIndex == FirstPlayer) {
      s"$prefixForActivePlayer$FirstPlayer: ${players(FirstPlayer)}"
    } else s"  $FirstPlayer: ${players(FirstPlayer).toStringHidden}"

    val player2LineAsString = if (activePlayerIndex == SecondPlayer) {
      s"$prefixForActivePlayer$SecondPlayer: ${players(SecondPlayer)}"
    } else s"  $SecondPlayer: ${players(SecondPlayer).toStringHidden}"

    val player3LineAsString = if (activePlayerIndex == ThirdPlayer) {
      s"$prefixForActivePlayer$ThirdPlayer: ${players(ThirdPlayer)}"
    } else s"  $ThirdPlayer: ${players(ThirdPlayer).toStringHidden}"

    val info = phase match {
      case Bidding => s"$activePlayerIndex must bid. Enter positive integer to bid, negative to pass"
      case TakeCards =>
        s"""|All players have passed.
                            |$activePlayerIndex won bidding with bid ${highestBidAsString} and must take cards from the board
                            |Enter any symbol to take cards
                            |""".stripMargin
      case PassCards => s"""|$activePlayerIndex must pass 2 cards.
                            |First card to player on the left, second card to player on the right
                            |Enter 2 valid cards separated by space
                            |""".stripMargin
      case PlayCards => s"$activePlayerIndex must play a card. Enter valid card"
      case RoundEnd => "Round Finished. Enter any symbol to see round result"
      case GameEnd => "Game Ended. Enter 'start' to start new one or 'exit' to exit program"
      case _ => "Should Not Be Here..."
    }

    s"""|CURRENT GAME STATE:
        |Round Number: $roundNumber| Phase: $phase| Active Player: $activePlayerAsString
        |HighestBid: $highestBidAsString| Bidding Winner: $biddingWinnerAsString| Cards To Take: $cardsToTakeAsString
        |Cards Played: $cardsOnBoardAsString| RequiredSuit: $requiredSuitAsString| Trump: $trumpAsString
        |Players:
        |$player1LineAsString
        |$player2LineAsString
        |${player3LineAsString}
        |
        |GAME INFO: $info
        |""".stripMargin
  }
}

object Game {

  def init(roundNumber: Int, startPlayerIndex: PlayerIndex, results: List[Results], deck: List[Card]): Game = {

    val cardsToTake = deck.slice(0, 3)
    val player1 = Player(deck.slice(3, 10), bid = 0, points = 0)
    val player2 = Player(deck.slice(10, 17), bid = 0, points = 0)
    val player3 = Player(deck.slice(17, 24), bid = 0, points = 0)
    val players: Map[PlayerIndex, Player] = Map(FirstPlayer -> player1, SecondPlayer -> player2, ThirdPlayer -> player3)
    Game(
      cardsOnBoard = Nil,
      requiredSuit = None,
      cardsToTake = cardsToTake,
      players = players,
      roundNumber = roundNumber,
      trump = None,
      highestBid = 0,
      activePlayerIndex = startPlayerIndex,
      phase = Bidding,
      biddingWinnerIndex = None,
      results = results,
      playerIndexStartingThisRound = startPlayerIndex,
    )
  }
}
