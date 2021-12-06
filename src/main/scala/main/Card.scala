package main

case class Card(suit: Suit, rank: Rank) {
  def value: Int = rank.value
  override def toString: String = suit.color + rank.toString + suit.toString + Console.RESET
}

import Suit._
import Rank._

object Card {
  def fromString(input: String): Option[Card] =
    if (input.length != 2) None
    else {
      val rankPart = input(0).toUpper
      val suitPart = input(1).toUpper
      val rank = rankPart match {
        case '9' => Some(Nine)
        case 'T' => Some(Ten)
        case 'J' => Some(Jack)
        case 'Q' => Some(Queen)
        case 'K' => Some(King)
        case 'A' => Some(Ace)
        case _ => None
      }
      val suit = suitPart match {
        case 'C' => Some(Club)
        case 'D' => Some(Diamond)
        case 'H' => Some(Heart)
        case 'S' => Some(Spade)
        case _ => None
      }
      for {
        s <- suit
        r <- rank
      } yield Card(s, r)
    }
}
