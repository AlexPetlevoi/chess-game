package util

import com.chess.game.Board.updateBoard
import model.Position
import model.pieces.Piece

object TestUtils {

  def updateTestBoard(tail: List[(Position, Position)], currentBoard: Map[Position, Piece]): Map[Position, Piece] = {
    tail match {
      case Nil => currentBoard
      case head :: tail => updateTestBoard(tail, updateBoard(head._1, head._2, currentBoard(head._1), currentBoard))
    }
  }
}
