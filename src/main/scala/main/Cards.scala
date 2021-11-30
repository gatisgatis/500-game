package main

import scala.util.Random

object Cards {

  trait Rank {
    def toString: String
    val value: Int
  }

  object Rank {
    object Nine extends Rank {
      override def toString: String = "9"
      val value: Int = 0
    }
    object Ten extends Rank {
      override def toString: String = "T"
      val value: Int = 10
    }
    object Jack extends Rank {
      override def toString: String = "J"
      val value: Int = 2
    }
    object Queen extends Rank {
      override def toString: String = "Q"
      val value: Int = 3
    }
    object King extends Rank {
      override def toString: String = "K"
      val value: Int = 4
    }
    object Ace extends Rank {
      override def toString: String = "A"
      val value: Int = 11
    }
  }

  trait Suit {
    def toString: String
    val color: String
  }

  object Suit {
    object Club extends Suit {
      override def toString: String = "c"
      val color: String = Console.GREEN
    }
    object Diamond extends Suit {
      override def toString: String = "d"
      val color: String = Console.BLUE
    }
    object Heart extends Suit {
      override def toString: String = "h"
      val color: String = Console.RED
    }
    object Spade extends Suit {
      override def toString: String = "s"
      val color: String = Console.WHITE
    }
  }

  case class Card(suit: Suit, rank: Rank) {
    def value: Int = rank.value
    override def toString:String = suit.color + rank.toString + suit.toString + Console.RESET
  }

  import Suit._
  import Rank._

  object Deck {
    private val ranks: Set[Rank] = Set(Nine, Ten, Jack, Queen, King, Ace)
    private val suits: Set[Suit] = Set(Club, Diamond, Heart, Spade)
    val deck: List[Card] = (for {
      r <- ranks
      s <- suits
    } yield Card(s, r)).toList

    def shuffle: List[Card] = Random.shuffle(deck)
  }

}
