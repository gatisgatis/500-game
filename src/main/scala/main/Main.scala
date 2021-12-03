package main

import main.Phase.{Bidding, GameEnd, PassCards, PlayCards, RoundEnd, TakeCards}
import main.PlayerIndex.{FirstPlayer, SecondPlayer, ThirdPlayer}

import scala.io.StdIn.readLine

object Main extends App {

  val info:String = {
    s"""|Valid commands:
        |start - starts a new game
        |exit - exits program
        |results - shows results for current game
        |again - shows game state again
        |info - shows valid commands
        |""".stripMargin
  }

  def resultsString: String = {
    val top =
      s"""|${FirstPlayer} ${SecondPlayer} ${ThirdPlayer}
          |   500      500      500
          |""".stripMargin
    if(legitGame.isEmpty) "Game not started yet! No results to show"
    else legitGame.get.results.foldLeft(top)((acc, result) => {
      acc + result.toString
    })
  }

  println("WELCOME TO 500 CARD GAME.")
  println(info)
  println()

  var appRunning = true
  var gameEither: Either[String, Game] = Left("Enter 'start' to start a new game")

  var legitGame: Option[Game] = None

  while (appRunning) {
    val gameState = gameEither match {
      case Left(str) => str
      case Right(game) =>
        legitGame = Some(game)
        game.toString
    }
    println(gameState)
    val userInput = readLine()

    userInput.trim match {
      case "exit" => appRunning = false
      case "start" => gameEither = Right(Game.init(1, FirstPlayer, Nil, Deck.shuffle))
      case "results" => println(resultsString)
      case "again" => () // nothing. it will just reprint gameState...
      case "info" => println(info)
      case input => {
        legitGame match {
          case None => ()
          case Some(game) => {
            game.phase match {
              case Bidding => {
                gameEither = for {
                  bid <- input.toIntOption.toRight("Invalid bid")
                  newRound <- Actions.makeBid(game, bid)
                } yield newRound
              }
              case TakeCards => gameEither = Actions.takeCards(game)
              case PassCards => {
                val inputSplit = input.split(" ")
                gameEither = for {
                  _ <- if(inputSplit.length == 2) Right() else Left("Wrong Input as 2 cards")
                  cardLeft <- Card.fromString(inputSplit(0)).toRight("Card Left invalid")
                  cardRight <- Card.fromString(inputSplit(1)).toRight("Card Right invalid")
                  newRound <- Actions.passCards(game, cardLeft, cardRight)
                } yield newRound
              }
              case PlayCards => {
                gameEither = for {
                  card <- Card.fromString(input).toRight("Invalid card typed")
                  newRound <- Actions.playCard(game, card)
                } yield newRound
              }
              case RoundEnd => {
                gameEither = for {
                  newGame <- Actions.updateGameAfterRound(game)
                } yield newGame
              }
              case GameEnd => {
                println("Game Finished")
                println(resultsString)
              }
            }
          }
        }
      }
    }
  }

  println("Thank you for playing.")
}
