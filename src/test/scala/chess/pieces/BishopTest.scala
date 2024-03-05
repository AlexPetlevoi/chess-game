package chess.board.pieces

import com.chess.game.Board
import com.chess.game.Board.{preMoveValidation, renderBoard, updateBoard}
import model.{Black, Position, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BishopTest extends AnyWordSpec with Matchers {

  "Bishop validate" should {

    val initBoard = Board.createBoard
    val bishopPosition = Position(2, 3)
    val (from, to) = (Position(2, 7), bishopPosition)
    val board = updateBoard(from, to, initBoard(from), initBoard)

    "wrong turn move" in {
      renderBoard(board)
      val bishop = board(bishopPosition)
      val blackTurn = Black
      val dest = Position(0, 5)
      preMoveValidation(false, blackTurn, bishop, bishopPosition, dest, board) mustBe Left(
        "Not valid turn move. Move from c5 to a3"
      )
    }

    "move taking own piece" in {
      renderBoard(board)
      val bishop = board(bishopPosition)
      val dest = Position(5, 6)
      preMoveValidation(false, White, bishop, bishopPosition, dest, board) mustBe Left(
        "Destination color must not be same color as own color. Move from c5 to f2"
      )
    }

    "move taking king piece" in {
      renderBoard(board)
      val bishop = board(bishopPosition)
      val fromKingPosition = Position(4, 0)
      val toKingPosition = Position(3, 2)
      val king = board(fromKingPosition)
      val kingBoard = updateBoard(fromKingPosition, toKingPosition, king, board)
      preMoveValidation(false, White, bishop, bishopPosition, toKingPosition, kingBoard) mustBe Left(
        "Destination Move cannot be King. Move from c5 to d6"
      )
    }

    "diagonal move" in {
      renderBoard(board)
      val bishop = board(bishopPosition)
      val diagonalMoves = List(Position(0, 5), Position(0, 1), Position(4, 1), Position(4, 5))

      diagonalMoves.forall(dest =>
        preMoveValidation(false, White, bishop, bishopPosition, dest, board) == Right(true)
      ) mustBe true
    }

    "vertical move" in {
      renderBoard(board)
      val bishop = board(bishopPosition)
      val diagonalMoves = List(Position(2, 1), Position(2, 5))
      diagonalMoves.forall(dest => bishop.validateMove(bishopPosition, dest, board)) mustBe false
    }

    "horizontal move" in {
      renderBoard(board)
      val bishop = board(bishopPosition)
      val diagonalMoves = List(Position(0, 3), Position(7, 3))
      diagonalMoves.forall(dest => bishop.validateMove(bishopPosition, dest, board)) mustBe false
    }
  }
}
