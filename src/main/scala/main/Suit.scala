package main

sealed trait Suit {
  def toString: String
  val color: String
  val fullName: String
}

object Suit {
  object Club extends Suit {
    override def toString: String = "c"
    val color: String = Console.GREEN
    val fullName: String = "CLUB"
  }
  object Diamond extends Suit {
    override def toString: String = "d"
    val color: String = Console.BLUE
    val fullName: String = "DIAMOND"
  }
  object Heart extends Suit {
    override def toString: String = "h"
    val color: String = Console.RED
    val fullName: String = "HEART"
  }
  object Spade extends Suit {
    override def toString: String = "s"
    val color: String = Console.WHITE
    val fullName: String = "SPADE"
  }
  val all: Set[Suit] = Set(Club, Diamond, Heart, Spade)
}
