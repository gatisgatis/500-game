package main

sealed trait Phase {
  def toString: String
  def nextPhase: Phase
}

object Phase {
  case object Bidding extends Phase {
    override def toString: String = "Bidding"
    override def nextPhase: Phase = TakeCards
  }
  case object TakeCards extends Phase {
    override def toString: String = "Take Cards"
    override def nextPhase: Phase = PassCards
  }
  case object PassCards extends Phase {
    override def toString: String = "Pass Cards"
    override def nextPhase: Phase = PlayCards
  }
  case object PlayCards extends Phase {
    override def toString: String = "Play Cards"
    override def nextPhase: Phase = RoundEnd
  }
  case object RoundEnd extends Phase {
    override def toString: String = "Round Ending"
    override def nextPhase: Phase = Bidding
  }
  case object GameEnd extends Phase {
    override def toString: String = "Game Ending"
    override def nextPhase: Phase = GameEnd
  }
}
