package main

// List[(WinningBid, PlayerIndexWhoWonBidding, Map[PlayerIndex, PointsThisRound])]

case class Game(results: List[(Int, PlayerIndex, Map[PlayerIndex, Int])])

// TODO. add toString method to display results table



