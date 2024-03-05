package chess.board.pieces

import com.chess.game.Board
import com.chess.game.Board.{preMoveValidation, renderBoard, updateBoard}
import model.pieces.Pawn
import model.{Black, Position, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import util.TestUtils.updateTestBoard

import scala.util.Right

class QueenTest extends AnyWordSpec with Matchers {

  "Queen validate" should {
    val initBoard = Board.createBoard
    val queenPosition = Position(3, 3)
    val (from, to) = (Position(3, 7), queenPosition)
    val board = updateBoard(from, to, initBoard(from), initBoard)

    "vertical move" in {
      renderBoard(board)
      val queen = board(queenPosition)
      val diagonalMoves = List(Position(3, 1), Position(3, 5))
      diagonalMoves.forall(dest =>
        preMoveValidation(true, White, queen, queenPosition, dest, board) == Right(true)
      ) mustBe true
    }

    "horizontal move" in {
      renderBoard(board)
      val queen = board(queenPosition)
      val diagonalMoves = List(Position(0, 3), Position(7, 3))
      diagonalMoves.forall(dest =>
        preMoveValidation(true, White, queen, queenPosition, dest, board) == Right(true)
      ) mustBe true
    }

    "diagonal move" in {
      renderBoard(board)
      val queen = board(queenPosition)
      val diagonalMoves = List(Position(1, 1), Position(5, 1), Position(1, 5), Position(5, 5))
      diagonalMoves.forall(dest =>
        preMoveValidation(false, White, queen, queenPosition, dest, board) == Right(true)
      ) mustBe true
    }

    "1 step to each direction around the queen" in {
      renderBoard(board)
      val queen = board(queenPosition)
      val diagonalMoves = List(
        Position(2, 2),
        Position(3, 2),
        Position(4, 2),
        Position(4, 3),
        Position(4, 4),
        Position(3, 4),
        Position(2, 4),
        Position(2, 3)
      )
      diagonalMoves.forall(dest =>
        preMoveValidation(false, White, queen, queenPosition, dest, board) == Right(true)
      ) mustBe true
    }

    "vertical move and take own piece" in {
      renderBoard(board)
      val queen = board(queenPosition)
      val ownPiecePosition = Position(3, 6)
      preMoveValidation(true, White, queen, queenPosition, ownPiecePosition, board) mustBe Left(
        "Destination color must not be same color as own color. Move from d5 to d2"
      )
    }

    "horizontal move and take own piece" in {
      renderBoard(board)
      val queen = board(queenPosition)
      val ownPawnPosition = Position(0, 6)
      val updatedPawnPosition = Position(0, 3)
      val ownPawn = board(ownPawnPosition)
      val updatedBoard = updateBoard(ownPawnPosition, updatedPawnPosition, ownPawn, board)
      renderBoard(updatedBoard)
      preMoveValidation(true, White, queen, queenPosition, updatedPawnPosition, updatedBoard) mustBe Left(
        "Destination color must not be same color as own color. Move from d5 to a5"
      )
    }

    "should not jump over pieces" in {
      val queen = board(queenPosition)
      val posAroundQueen = List(
        Position(4, 4),
        Position(3, 4),
        Position(2, 4),
        Position(2, 3),
        Position(4, 3),
        Position(2, 4),
        Position(2, 2),
        Position(3, 2)
      )
      val allPawns = List(
        Position(0, 1),
        Position(1, 1),
        Position(2, 1),
        Position(3, 1),
        Position(4, 1),
        Position(5, 1),
        Position(6, 1),
        Position(7, 1)
      )
      val allPossibleMove = List(
        Position(1, 4),
        Position(1, 1),
        Position(2, 1),
        Position(3, 1),
        Position(4, 1),
        Position(5, 1),
        Position(5, 2),
        Position(5, 3),
        Position(5, 4),
        Position(5, 5),
        Position(4, 5),
        Position(3, 5),
        Position(2, 5),
        Position(1, 5),
        Position(1, 4),
        Position(1, 3),
        Position(1, 2)
      )
      val zippedMoves = allPawns.zip(posAroundQueen)
      val boardForQueenJumpValidation = updateTestBoard(zippedMoves, board) + (Position(4, 2) -> Pawn(Black))
      renderBoard(boardForQueenJumpValidation)
      allPossibleMove.forall(dest => queen.validateMove(queenPosition, dest, boardForQueenJumpValidation)) mustBe false
    }

    "move taking king piece" in {
      val queen = board(queenPosition)
      val fromKingPosition = Position(4, 0)
      val toKingPosition = Position(3, 2)
      val king = board(fromKingPosition)
      val kingBoard = updateBoard(fromKingPosition, toKingPosition, king, board)
      renderBoard(kingBoard)
      preMoveValidation(false, White, queen, queenPosition, toKingPosition, kingBoard) mustBe Left(
        "Destination Move cannot be King. Move from d5 to d6"
      )
    }

    "invalid move" in {
      val queen = board(queenPosition)
      val invalidPosition = Position(1, 4)
      renderBoard(board)
      preMoveValidation(false, White, queen, queenPosition, invalidPosition, board) mustBe Left(
        "Invalid Queen(White) Move. Move from d5 to b4"
      )
    }

  }
}
