package chess.board.pieces

import com.chess.game.Board
import com.chess.game.Board.{preMoveValidation, renderBoard, updateBoard}
import model.{Black, Position, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RookTest extends AnyWordSpec with Matchers {

  "Rook validate" should {

    val initBoard = Board.createBoard
    val rookPosition = Position(3, 3)
    val (from, to) = (Position(0, 7), rookPosition)
    val board = updateBoard(from, to, initBoard(from), initBoard)

    "wrong turn move" in {
      renderBoard(board)
      val rook = board(rookPosition)
      val blackTurn = Black
      val dest = Position(3, 4)
      preMoveValidation(false, blackTurn, rook, rookPosition, dest, board) mustBe Left(
        "Not valid turn move. Move from d5 to d4"
      )
    }

    "move taking own piece" in {
      renderBoard(board)
      val rook = board(rookPosition)
      val dest = Position(3, 6)
      preMoveValidation(false, White, rook, rookPosition, dest, board) mustBe Left(
        "Destination color must not be same color as own color. Move from d5 to d2"
      )
    }

    "invalid Move" in {
      val rook = board(rookPosition)
      val wrongDestinationPosition = Position(1, 4)
      preMoveValidation(false, White, rook, rookPosition, wrongDestinationPosition, board) mustBe Left(
        "Invalid Rook(White) Move. Move from d5 to d2"
      )
    }

    "move taking king piece" in {
      val rook = board(rookPosition)
      val fromKingPosition = Position(4, 0)
      val toKingPosition = Position(3, 2)
      val king = board(fromKingPosition)
      val kingBoard = updateBoard(fromKingPosition, toKingPosition, king, board)
      renderBoard(kingBoard)
      preMoveValidation(false, White, rook, rookPosition, toKingPosition, kingBoard) mustBe Left(
        "Destination Move cannot be King. Move from d5 to d6"
      )
    }

    "diagonal move" in {
      renderBoard(board)
      val rook = board(rookPosition)
      val diagonalMoves = List(Position(1, 4), Position(1, 2), Position(5, 2), Position(3, 4))
      diagonalMoves.forall(dest => rook.validateMove(rookPosition, dest, board)) mustBe false
    }

    "vertical move" in {
      renderBoard(board)
      val rook = board(rookPosition)
      val diagonalMoves = List(Position(3, 2), Position(3, 5))
      diagonalMoves.forall(dest => rook.validateMove(rookPosition, dest, board)) mustBe true
    }

    "horizontal move" in {
      renderBoard(board)
      val rook = board(rookPosition)
      val diagonalMoves = List(Position(0, 3), Position(7, 3))
      diagonalMoves.forall(dest => rook.validateMove(rookPosition, dest, board)) mustBe true
    }
  }
}
