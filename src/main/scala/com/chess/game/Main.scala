package com.chess.game

object Main extends App {
  val sampleMoves = "src/main/resources/com.chess.data/sample-moves.txt"
  val sampleMoves_Invalid = "src/main/resources/com.chess.data/sample-moves-invalid.txt"
  val sampleMoves_checkMate = "src/main/resources/com.chess.data/checkmate.txt"
  val double_check = "src/main/resources/com.chess.data/double-check-moves.txt"

  new Game(double_check).initiateTheGame() match {
    case Left(e)  => println(e)
    case Right(_) => println("End Game")
  }

}
