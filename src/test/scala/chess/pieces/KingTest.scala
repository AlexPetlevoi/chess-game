package chess.board.pieces

import com.chess.game.Board
import com.chess.game.Board.{preMoveValidation, renderBoard, updateBoard}
import model.{Position, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Right

class KingTest extends AnyWordSpec with Matchers {
  "King validate" should {
    val initBoard = Board.createBoard
    val kingPosition = Position(3, 4)
    val (from, to) = (Position(4, 7), kingPosition)
    val board = updateBoard(from, to, initBoard(from), initBoard)

    "move 1 step around itself" in {
      renderBoard(board)
      val king = board(kingPosition)
      val possibleValidMoves = List(
        Position(2, 3),
        Position(3, 3),
        Position(4, 3),
        Position(4, 4),
        Position(4, 5),
        Position(3, 5),
        Position(2, 5),
        Position(2, 4)
      )
      possibleValidMoves.forall(dest =>
        preMoveValidation(false, White, king, kingPosition, dest, board) == Right(true)
      ) mustBe true
    }

    "move vertical and take own piece" in {
      renderBoard(board)
      val king = board(kingPosition)
      val ownPawnPosition = Position(0, 6)
      val updatedPawnPosition = Position(3, 5)
      val ownPawn = board(ownPawnPosition)
      val updatedBoard = updateBoard(ownPawnPosition, updatedPawnPosition, ownPawn, board)
      renderBoard(updatedBoard)
      preMoveValidation(true, White, king, kingPosition, updatedPawnPosition, updatedBoard) mustBe Left(
        "Destination color must not be same color as own color. Move from d4 to d3"
      )

    }

    "move horizontal and take own piece" in {
      renderBoard(board)
      val king = board(kingPosition)
      val ownPawnPosition = Position(0, 6)
      val updatedPawnPosition = Position(2, 4)
      val ownPawn = board(ownPawnPosition)
      val updatedBoard = updateBoard(ownPawnPosition, updatedPawnPosition, ownPawn, board)
      renderBoard(updatedBoard)
      preMoveValidation(true, White, king, kingPosition, updatedPawnPosition, updatedBoard) mustBe Left(
        "Destination color must not be same color as own color. Move from d4 to c4"
      )

    }

    "move diagonal and take own piece" in {
      renderBoard(board)
      val king = board(kingPosition)
      val ownPawnPosition = Position(0, 6)
      val updatedPawnPosition = Position(2, 5)
      val ownPawn = board(ownPawnPosition)
      val updatedBoard = updateBoard(ownPawnPosition, updatedPawnPosition, ownPawn, board)
      renderBoard(updatedBoard)
      preMoveValidation(true, White, king, kingPosition, updatedPawnPosition, updatedBoard) mustBe Left(
        "Destination color must not be same color as own color. Move from d4 to c3"
      )
    }

    "invalid 2 square move" in {
      renderBoard(board)
      val king = board(kingPosition)
      val invalidPosition = Position(1, 4)
      preMoveValidation(true, White, king, kingPosition, invalidPosition, board) mustBe Left(
        "Invalid King(White) Move. Move from d4 to b4"
      )
    }
  }
}
