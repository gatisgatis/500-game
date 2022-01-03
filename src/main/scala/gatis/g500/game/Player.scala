package gatis.g500.game

final case class Player(
  cards: List[Card],
  bid: Int,
  points: Int,
  playerIndex: PlayerIndex,
  trickCount: Int,
  playedCard: Option[Card],
) {
  def cardsSorted: List[Card] = (for {
    s <- Suit.all
    sameSuitCards = cards.filter(_.suit == s)
    sorted = sameSuitCards.sortWith(_.value > _.value)
  } yield sorted).toList.flatten

  override def toString: String = {
    val cardsAsString = if (cards.isEmpty) "---" else cardsSorted.foldLeft("")((acc, cur) => acc + cur.toString + " ")
    val bidAsString = if (bid < 0) "Passed" else if (bid == 0) "No Bid Made" else bid
    "Cards: " + cardsAsString + " | Bid: " + bidAsString + " | Points: " + points
  }
  def toStringHidden: String = {
    val bidAsString = if (bid < 0) "Passed" else if (bid == 0) "No Bid Made" else bid
    "Cards: hidden | Bid: " + bidAsString + " | Points: " + points
  }
}
