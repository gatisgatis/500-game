package main

import main.GamePhase.{Bidding, PassCards, PlayCards, RoundEnd, TakeCards}
import main.PlayerIndex.FirstPlayer

import scala.io.StdIn.readLine

object Main extends App {

  println("WELCOME TO 500 CARD GAME.")
  println("start - to start new game")
  println("exit - to exit program")
  println("results - to see current results")
  println()

  // uzlabot TO DO
  // again - parāda vēlreiz GameState
  // slēpt neaktīvo spēlētāju kārtis, slēpt kārtis uz boarda

  var appRunning = true
  var roundEither: Either[String, Round] = Left("Waiting for game to start")

  var legitRound: Option[Round] = None

  while (appRunning) {
    val gameState = roundEither match {
      case Left(str) => str
      case Right(round) => {
        legitRound = Some(round)
        round.toString
      }
    }
    println(gameState)
    val userInput = readLine()

    userInput.trim match {
      case "exit" => appRunning = false
      case "start" => roundEither = Right(Round.init(1, FirstPlayer, Deck.shuffle))
      case "results" => println("No Results For Now")
      case input => {
        legitRound match {
          case None => ()
          case Some(round) => {
            round.phase match {
              case Bidding => {
                roundEither = for {
                  bid <- input.toIntOption.toRight("Invalid bid")
                  newRound <- Actions.makeBid(round, bid)
                } yield newRound
              }
              case TakeCards => roundEither = Actions.takeCards(round)
              case PassCards => {
                val inputSplit = input.split(" ")
                roundEither = for {
                  _ <- if(inputSplit.length == 2) Right() else Left("Wrong Input as 2 cards")
                  cardLeft <- Card.fromString(inputSplit(0)).toRight("Card Left invalid")
                  cardRight <- Card.fromString(inputSplit(1)).toRight("Card Right invalid")
                  newRound <- Actions.passCards(round, cardLeft, cardRight)
                } yield newRound
              }
              case PlayCards => {
                roundEither = for {
                  card <- Card.fromString(input).toRight("Invalid card typed")
                  newRound <- Actions.playCard(round, card)
                } yield newRound
              }
              case RoundEnd => {
                println("round end")
                // update game method
                // init new round
              }
            }
          }
        }
      }
    }
  }

  println("EXIT GAME")
}
