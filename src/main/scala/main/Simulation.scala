package main

import main.Actions.{makeBid, passCards, playCard, takeCards, updateGame}
import main.PlayerIndex.{FirstPlayer, SecondPlayer}
import main.Rank.{Ace, Jack, King, Nine, Queen, Ten}
import main.Suit.{Club, Diamond, Heart, Spade}

object Simulation extends App {

  // ONE FULL ROUND SIMULATION
  val round = Round.init(1, FirstPlayer, Deck.deck)
  println(round)

  val round2 = makeBid(round, 80)
  println(round2.getOrElse(round))

  val round3 = makeBid(round2.getOrElse(round), 90)
  println(round3.getOrElse(round))

  val round4 = makeBid(round3.getOrElse(round), bid = 120)
  println(round4.getOrElse(round))

  val round5 = makeBid(round4.getOrElse(round), bid = -5)
  println(round5.getOrElse(round))

  val round6 = makeBid(round5.getOrElse(round), bid = -5)
  println(round6.getOrElse(round))

  val round7 = makeBid(round6.getOrElse(round), bid = -5)
  println(round7.getOrElse(round))

  val round8 = takeCards(round7.getOrElse(round))
  println(round8.getOrElse(round))

  val round9 = passCards(round8.getOrElse(round), Card(Club, King), Card(Spade, Jack))
  println(round9.getOrElse(round))

  val round10 = playCard(round9.getOrElse(round), Card(Heart, Ace))
  println(round10.getOrElse(round))

  val round11 = playCard(round10.getOrElse(round), Card(Heart, Nine))
  println(round11.getOrElse(round))

  val round12 = playCard(round11.getOrElse(round), Card(Heart, King))
  println(round12.getOrElse(round))

  val round13 = playCard(round12.getOrElse(round), Card(Diamond, Nine))
  val round14 = playCard(round13.getOrElse(round), Card(Diamond, Queen))
  val round15 = playCard(round14.getOrElse(round), Card(Diamond, Jack))
  println(round15.getOrElse(round))

  val round16 = playCard(round15.getOrElse(round), Card(Club, King))
  val round17 = playCard(round16.getOrElse(round), Card(Club, Jack))
  val round18 = playCard(round17.getOrElse(round), Card(Heart, Queen))
  println(round18.getOrElse(round))

  val round19 = playCard(round18.getOrElse(round), Card.fromString("Td").get)
  val round20 = playCard(round19.getOrElse(round), Card(Heart, Ten))
  val round21 = playCard(round20.getOrElse(round), Card(Diamond, Ace))
  println(round21.getOrElse(round))

  val round22 = playCard(round21.getOrElse(round), Card(Club, Ace))
  val round23 = playCard(round22.getOrElse(round), Card(Spade, Jack))
  val round24 = playCard(round23.getOrElse(round), Card(Heart, Jack))
  println(round24.getOrElse(round))

  val round25 = playCard(round24.getOrElse(round), Card(Diamond, King))
  val round26 = playCard(round25.getOrElse(round), Card(Club, Nine))
  val round27 = playCard(round26.getOrElse(round), Card(Spade, Queen))
  println(round27.getOrElse(round))

  val round28 = playCard(round27.getOrElse(round), Card(Spade, Nine))
  val round29 = playCard(round28.getOrElse(round), Card(Club, Queen))
  val round30 = playCard(round29.getOrElse(round), Card(Spade, Ace))
  println(round30.getOrElse(round))

  val round31 = playCard(round30.getOrElse(round), Card(Spade, King))
  val round32 = playCard(round31.getOrElse(round), Card(Spade, Ten))
  val round33 = playCard(round32.getOrElse(round), Card(Club, Ten))
  println(round33.getOrElse(round))

  val game = updateGame(round33.getOrElse(round), Game(Nil))
  println(game)
  println()

  val round34 = Round.init(2, SecondPlayer, Deck.shuffle)
  println(round34)

}
