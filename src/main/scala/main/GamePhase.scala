package main

sealed trait GamePhase {
  def toString: String
  def nextPhase: GamePhase
}

object GamePhase {
  case object Bidding extends GamePhase {
    override def toString: String = "Bidding"
    override def nextPhase: GamePhase = TakeCards
  }
  case object TakeCards extends GamePhase {
    override def toString: String = "Take Cards"
    override def nextPhase: GamePhase = PassCards
  }
  case object PassCards extends GamePhase {
    override def toString: String = "Pass Cards"
    override def nextPhase: GamePhase = PlayCards
  }
  case object PlayCards extends GamePhase {
    override def toString: String = "Play Cards"
    override def nextPhase: GamePhase = RoundEnd
  }
  case object RoundEnd extends GamePhase {
    override def toString: String = "Round Ending"
    override def nextPhase: GamePhase = Bidding
  }
  case object GameEnd extends GamePhase {
    override def toString: String = "Game Ending"
    override def nextPhase: GamePhase = GameEnd
  }
}
