package main

sealed trait PlayerIndex {
  def toString: String
  def next: PlayerIndex
  def previous: PlayerIndex
}

object PlayerIndex {
  case object FirstPlayer extends PlayerIndex {
    override def next: PlayerIndex = SecondPlayer
    override def previous: PlayerIndex = ThirdPlayer
    override def toString: String = "Player 1"
  }
  case object SecondPlayer extends PlayerIndex {
    override def next: PlayerIndex = ThirdPlayer
    override def previous: PlayerIndex = FirstPlayer
    override def toString: String = "Player 2"
  }
  case object ThirdPlayer extends PlayerIndex {
    override def next: PlayerIndex = FirstPlayer
    override def previous: PlayerIndex = SecondPlayer
    override def toString: String = "Player 3"
  }
}
