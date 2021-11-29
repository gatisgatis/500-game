package main

import scala.util.Random

object Cards {

  trait Rank {
    def toString: String
  }

  object Rank extends Rank {
    object Nine extends Rank {
      override def toString: String = "9"
    }
    object Ten extends Rank {
      override def toString: String = "T"
    }
    object Jack extends Rank {
      override def toString: String = "J"
    }
    object Queen extends Rank {
      override def toString: String = "Q"
    }
    object King extends Rank {
      override def toString: String = "K"
    }
    object Ace extends Rank {
      override def toString: String = "A"
    }
  }

  trait Suit {
    def toString: String
  }

  object Suit extends Suit {
    object Club extends Suit {
      override def toString: String = "c"
    }
    object Diamond extends Suit {
      override def toString: String = "d"
    }
    object Heart extends Suit {
      override def toString: String = "h"
    }
    object Spade extends Suit {
      override def toString: String = "s"
    }
  }

  case class Card(suit: Suit, rank: Rank) {
    import Rank._
    def value: Int = rank match {
      case Ace => 11
      case Ten => 10
      case King => 4
      case Queen => 3
      case Jack => 2
      case Nine => 0
    }

    override def toString:String = {
      val color: String = suit match {
        case Suit.Club => Console.GREEN
        case Suit.Diamond => Console.BLUE
        case Suit.Heart => Console.RED
        case Suit.Spade => Console.WHITE
        case _ => Console.BLACK
      }
      color + rank.toString + suit.toString + Console.RESET
    }
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


//  def toHumanReadableCards(cards: List[Card]): String = {
//    cards.foldLeft("")((acc, cur) => acc + " " + cur.toString)
//  }

}
