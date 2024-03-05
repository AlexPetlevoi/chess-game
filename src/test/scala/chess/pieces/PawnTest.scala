package chess.board.pieces

import com.chess.game.Board
import com.chess.game.Board.{preMoveValidation, renderBoard, updateBoard}
import model.{Black, Position, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import util.TestUtils.updateTestBoard

class PawnTest extends AnyWordSpec with Matchers {

  "pawn validate" should {
    val board = Board.createBoard
    val pawnPosition = Position(4, 6)

    "one step Move" in {
      val pawn = board(pawnPosition)
      val destination = Position(4, 5)
      preMoveValidation(false, White, pawn, pawnPosition, destination, board) mustBe Right(true)
    }

    "two step Move" in {
      val pawn = board(pawnPosition)
      val destination = Position(4, 4)
      preMoveValidation(false, White, pawn, pawnPosition, destination, board) mustBe Right(true)
    }

    "diagonal take opponent piece move" in {
      val pawn = board(pawnPosition)
      val destination1 = Position(3, 5)
      val destination2 = Position(5, 5)
      val opponentPawn1 = Position(3, 1)
      val opponentPawn2 = Position(5, 1)
      val zippedData = List(opponentPawn1, opponentPawn2).zip(List(destination1, destination2))
      val updatedBoard = updateTestBoard(zippedData, board)
      renderBoard(updatedBoard)
      preMoveValidation(false, White, pawn, pawnPosition, destination1, updatedBoard) mustBe Right(true)
      preMoveValidation(false, White, pawn, pawnPosition, destination2, updatedBoard) mustBe Right(true)
    }

    "diagonal move to empty cell" in {
      val pawn = board(pawnPosition)
      val destination1 = Position(3, 5)
      val destination2 = Position(5, 5)
      preMoveValidation(false, White, pawn, pawnPosition, destination1, board) mustBe Left(
        "Invalid Pawn(White) Move. Move from e2 to d3"
      )
      preMoveValidation(false, White, pawn, pawnPosition, destination2, board) mustBe Left(
        "Invalid Pawn(White) Move. Move from e2 to f3"
      )
    }

    "diagonal move to take opponent piece more than 1 square" in {
      val pawn = board(pawnPosition)
      val destination1 = Position(2, 4)
      val destination2 = Position(6, 4)
      val opponentPawn1 = Position(3, 1)
      val opponentPawn2 = Position(5, 1)
      val zippedData = List(opponentPawn1, opponentPawn2).zip(List(destination1, destination2))
      val updatedBoard = updateTestBoard(zippedData, board)
      renderBoard(updatedBoard)
      preMoveValidation(false, White, pawn, pawnPosition, destination1, board) mustBe Left(
        "Invalid Pawn(White) Move. Move from e2 to c4"
      )
      preMoveValidation(false, White, pawn, pawnPosition, destination2, board) mustBe Left(
        "Invalid Pawn(White) Move. Move from e2 to g4"
      )
    }

    "wrong turn move" in {
      renderBoard(board)
      val pawn = board(pawnPosition)
      val blackTurn = Black
      val dest = Position(4, 5)
      preMoveValidation(false, blackTurn, pawn, pawnPosition, dest, board) mustBe Left(
        "Not valid turn move. Move from e2 to e3"
      )
    }

    "move taking own piece" in {
      renderBoard(board)
      val pawn = board(pawnPosition)
      val dest = Position(3, 5)
      val ownPawnPosition = Position(0, 6)
      val ownPawn = board(ownPawnPosition)
      val updatedBoard = updateBoard(ownPawnPosition, dest, ownPawn, board)
      preMoveValidation(false, White, pawn, pawnPosition, dest, updatedBoard) mustBe Left(
        "Destination color must not be same color as own color. Move from e2 to d3"
      )
    }

    "move taking king piece" in {
      val pawn = board(pawnPosition)
      val fromKingPosition = Position(4, 0)
      val toKingPosition = Position(3, 5)
      val king = board(fromKingPosition)
      val kingBoard = updateBoard(fromKingPosition, toKingPosition, king, board)
      renderBoard(kingBoard)
      preMoveValidation(false, White, pawn, pawnPosition, toKingPosition, kingBoard) mustBe Left(
        "Destination Move cannot be King. Move from e2 to d3"
      )
    }

    "backward move" in {
      val pawn = board(pawnPosition)
      val updatedPawnPosition = Position(4, 4)
      val backWardPosition = Position(4, 6)
      val updatedBoard = updateBoard(pawnPosition, updatedPawnPosition, pawn, board)
      val updatedPawn = updatedBoard(updatedPawnPosition)
      renderBoard(updatedBoard)
      preMoveValidation(false, White, updatedPawn, updatedPawnPosition, backWardPosition, updatedBoard) mustBe Left(
        "Invalid Pawn(White) Move. Move from e4 to e2"
      )
    }
  }
}
