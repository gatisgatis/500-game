package main

import main.GamePhase.{Bidding, PassCards, PlayCards, RoundEnd, TakeCards}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}

case class Round(
                  cardsOnBoard: List[Card],
                  requiredSuit: Option[Suit],
                  cardsToTake: List[Card],
                  trump: Option[Suit],
                  highestBid: Int,
                  activePlayerIndex: PlayerIndex,
                  biddingWinnerIndex: Option[PlayerIndex],
                  roundNumber: Int,
                  players: Map[PlayerIndex, Player],
                  phase: GamePhase
                ) {
  override def toString: String = {
    val cardsOnBoardAsString: String = if (cardsOnBoard.isEmpty) "---"
      else cardsOnBoard.foldLeft("")((acc, cur) => acc + cur.toString + " ")
    val requiredSuitAsString: String = requiredSuit match {
      case Some(suit) => suit.color + suit.fullName + Console.RESET
      case _ => "---"
    }
    val cardsToTakeAsString: String = if(cardsToTake.isEmpty) "---" else  cardsToTake.foldLeft("")((acc, cur) => acc + cur.toString + " ")
    val highestBidAsString: String = if(highestBid == 0) "No Bids Made" else highestBid.toString
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

    val player1asString = players.get(FirstPlayer) match {
      case Some(player) => player.toString
      case None => "---"
    }
    val player1prefix = if(activePlayerIndex == FirstPlayer) prefixForActivePlayer else "  "
    val player2asString = players.get(SecondPlayer) match {
      case Some(player) => player.toString
      case None => "---"
    }
    val player2prefix = if(activePlayerIndex == SecondPlayer) prefixForActivePlayer else "  "
    val player3asString = players.get(ThirdPlayer) match {
      case Some(player) => player.toString
      case None => "---"
    }
    val player3prefix = if(activePlayerIndex == ThirdPlayer) prefixForActivePlayer else "  "

    val info = phase match {
      case Bidding => s"$activePlayerIndex must bid"
      case TakeCards => s"$activePlayerIndex must take cards from the board"
      case PassCards => s"$activePlayerIndex must pass 2 cards"
      case PlayCards => s"$activePlayerIndex must play a card"
      case RoundEnd => "Score points, Start new Round"
      case _ => ""
    }

    s"""|CURRENT GAME STATE:
        |Round Number: $roundNumber | Phase: $phase | Active Player: $activePlayerAsString
        |HighestBid: $highestBidAsString | Bidding Winner: $biddingWinnerAsString | Cards To Take: $cardsToTakeAsString
        |Cards Played: $cardsOnBoardAsString | RequiredSuit: $requiredSuitAsString | Trump: $trumpAsString
        |Players:
        |${player1prefix}Player 1: $player1asString
        |${player2prefix}Player 2: $player2asString
        |${player3prefix}Player 3: $player3asString
        |
        |TO DO: $info
        |""".stripMargin
  }
}

object Round {

  def init(roundNumber: Int, activePlayerIndex: PlayerIndex, deck: List[Card]): Round = {

    val cardsToTake = deck.slice(0,3)
    val player1 = Player(deck.slice(3,10), bid = 0, points = 0)
    val player2 = Player(deck.slice(10,17), bid = 0, points = 0)
    val player3 = Player(deck.slice(17,24), bid = 0, points = 0)
    val players: Map[PlayerIndex, Player] = Map(FirstPlayer -> player1, SecondPlayer -> player2, ThirdPlayer -> player3)
    Round(
      cardsOnBoard = Nil,
      requiredSuit = None,
      cardsToTake = cardsToTake,
      players = players,
      roundNumber = roundNumber,
      trump = None,
      highestBid = 0,
      activePlayerIndex = activePlayerIndex,
      phase = Bidding,
      biddingWinnerIndex = None
    )
  }
}
