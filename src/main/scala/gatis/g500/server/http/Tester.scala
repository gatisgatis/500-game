package gatis.g500.server.http

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxOptionId, none}
import gatis.g500.server.http.Exercise1_Common.response

import scala.annotation.tailrec
import scala.io.StdIn

object Tester extends IOApp {

  private val sideEfectfulProgramm: IO[String] = {
    println("Printing")
    IO.pure("")
  }

  def run(args: List[String]): IO[ExitCode] =
//    sideEfectfulProgramm.as(ExitCode.Success)
    sideEfectfulProgramm.map(_ => ExitCode.Success)

}

object Exercise1_Common {
  def response(animal: String): Option[String] = animal.trim match {
    case "cat" | "cats" => "In ancient times cats were worshipped as gods; they have not forgotten this.".some
    case "dog" | "dogs" => "Be the person your dog thinks you are.".some
    case x if x.nonEmpty => s"I don't know what to say about '$x'.".some
    case _ => none
  }
}

object Exercise1_Imperative {
  private var counter: Int = 0

  @tailrec
  def main(args: Array[String]): Unit = {
    println("What is your favourite animal?")
    val animal = StdIn.readLine()
    val output = response(animal)
    output match {
      case Some(x) =>
        println(x)

      case None =>
        if (counter >= 2) {
          println("I am disappoint. You have failed to answer too many times.")
          sys.exit(1)
        } else {
          counter += 1
          println("Empty input is not valid, try again...")
          main(args)
        }
    }
  }
}

trait Console {
  def putStrLn(value: String): IO[Unit]
  def readStrLn: IO[String]
}

object Console {
  object Real extends Console {
    def putStrLn(value: String): IO[Unit] = IO(println(value))
    def readStrLn: IO[String] = IO(StdIn.readLine())
  }
}

object Exercise1_Functional extends IOApp {
  import Exercise1_Common.response

  def process(console: Console, counter: Int = 0): IO[ExitCode] = {
    for {
      _ <- console.putStrLn("do somethings")
      input <- console.readStrLn
      output = response(input)
      exitCode <- output
        .map(result => console.putStrLn(result).as(ExitCode.Success))
        .getOrElse{
          if(counter > 2) {
            console.putStrLn("failed").as(ExitCode.Error)
          } else {
            console.putStrLn("try again") *> process(console, counter + 1)
          }
        }
    } yield exitCode
  }

  override def run(args: List[String]): IO[ExitCode] = process(Console.Real)
}
