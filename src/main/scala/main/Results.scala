package main

import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}

trait Results {
  val winningBid: Int
  val playerIndexWinningBid: PlayerIndex
  val pointsRound: Map[PlayerIndex, Int]
  val pointsGame: Map[PlayerIndex, Int]

  override def toString: String = {

    val firstPlayerBid = if (playerIndexWinningBid == FirstPlayer) s"($winningBid)" else "     "
    val secondPlayerBid = if (playerIndexWinningBid == SecondPlayer) s"($winningBid)" else "     "
    val thirdPlayerBid = if (playerIndexWinningBid == ThirdPlayer) s"($winningBid)" else "     "

    s"""|   $firstPlayerBid     $secondPlayerBid     $thirdPlayerBid
        |   ${pointsGame(FirstPlayer)}     ${pointsGame(SecondPlayer)}    ${pointsGame(ThirdPlayer)}
        |""".stripMargin
  }
}
