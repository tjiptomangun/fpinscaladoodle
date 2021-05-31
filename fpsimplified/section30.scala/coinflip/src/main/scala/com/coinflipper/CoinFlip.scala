package com.flipcoinpper

import scala.util.Random
import scala.annotation.tailrec
case class GameState(
                      flips: Int,
                      numCorrect: Int
                    )

object CoinFlip extends App {
  val s = GameState(0, 0)
  val r = new Random();

  def getUserInput(): String = scala.io.StdIn.readLine.trim.toUpperCase
  val hist: List[GameState] = List();
  mainLoop(s, hist, r)
  @tailrec
  def mainLoop  (gameState: GameState, history: List[GameState], random: Random): Unit = {
    println(s"#flips ${gameState.flips} #correct ${gameState.numCorrect}")
    println("please type h for head, t for tail, n for new game or other to exit")
    val ui = getUserInput()
    ui match {

      case "H" | "T" => {
        val newFlips = gameState.flips + 1;
        val toss = random.nextInt(2) match {
          case 0 => "H"
          case 1 => "T"
        }
        println(s"toss ${toss} user ${ui}")
        if (toss == ui) {
          val newNumCorrect = gameState.numCorrect + 1;
          val newGameState = gameState.copy(flips = newFlips, numCorrect = newNumCorrect)
          mainLoop(newGameState, history, random)
        }
        else {
          val newGameState = gameState.copy(flips = newFlips)
          mainLoop(newGameState, history, random)
        }
      }
      case "N" => {
        mainLoop(s, gameState :: history , r)
      }
      case _ => {
          (s :: history).foreach((gs) =>  {
          println(s"#flips ${gs.flips} #correct ${gs.numCorrect}")
        })

      }

    }

  }

}