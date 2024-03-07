package com.chess.game

import com.whitehatgaming.UserInputFile

object Main extends App {
  val sampleMoves = "src/main/resources/com.chess.data/sample-moves.txt"
  val sampleMoves_Invalid = "src/main/resources/com.chess.data/sample-moves-invalid.txt"
  val sampleMoves_checkMate = "src/main/resources/com.chess.data/checkmate.txt"
  val double_check = "src/main/resources/com.chess.data/double-check-moves.txt"
// use Source/Scanner to read moves from input

//  new Game(sampleMoves).initiateTheGame() match {
//    case Left(e)  => println(e)
//    case Right(_) => println("End Game")
//  }

  println(new Game(sampleMoves_checkMate).letTheGameBegin())
//  println(new Game(sampleMoves).readMove())
}
