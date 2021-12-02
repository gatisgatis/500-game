package main

import scala.util.Random

object Deck {
  val deck: List[Card] = (for {
    r <- Rank.all
    s <- Suit.all
  } yield Card(s, r)).toList

  def shuffle: List[Card] = Random.shuffle(deck)
}
