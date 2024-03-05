package chess.board.pieces

import com.chess.game.Board
import com.chess.game.Board.{preMoveValidation, renderBoard, updateBoard}
import model.pieces.Pawn
import model.{Black, Position, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import util.TestUtils.updateTestBoard

class KnightTest extends AnyWordSpec with Matchers {

  "Knight validate" should {

    val initBoard = Board.createBoard
    val knightPosition = Position(3, 3)
    val (from, to) = (Position(1, 7), knightPosition)
    val board = updateBoard(from, to, initBoard(from), initBoard)

    "all possible move" in {
      renderBoard(board)
      val knight = board(knightPosition)
      val allPossibleMove = List(
        Position(1, 4),
        Position(1, 2),
        Position(2, 5),
        Position(2, 1),
        Position(4, 5),
        Position(4, 1),
        Position(5, 4),
        Position(5, 2)
      )
      allPossibleMove.forall(dest => knight.validateMove(knightPosition, dest, board)) mustBe true
    }

    "able to move over pieces" in {
      renderBoard(board)
      val knight = board(knightPosition)
      val posAroundKing = List(
        Position(4, 4),
        Position(3, 4),
        Position(2, 4), //
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
      val zippedMoves = allPawns.zip(posAroundKing)
      val allPossibleMove = List(
        Position(1, 4),
        Position(1, 2),
        Position(2, 5),
        Position(2, 1),
        Position(4, 5),
        Position(4, 1),
        Position(5, 4),
        Position(5, 2)
      )

      val boardForKnight = updateTestBoard(zippedMoves, board) + (Position(4, 2) -> Pawn(Black))
      renderBoard(boardForKnight)
      allPossibleMove.forall(dest => knight.validateMove(knightPosition, dest, boardForKnight)) mustBe true
    }

    "invalid horizontal move" in {
      renderBoard(board)
      val knight = board(knightPosition)
      val invalidHorizontalPosition1 = Position(0, 3)
      val invalidHorizontalPosition2 = Position(7, 3)
      preMoveValidation(false, White, knight, knightPosition, invalidHorizontalPosition1, board) mustBe Left(
        "Invalid Knight(White) Move. Move from d5 to a5"
      )
      preMoveValidation(false, White, knight, knightPosition, invalidHorizontalPosition2, board) mustBe Left(
        "Invalid Knight(White) Move. Move from d5 to h5"
      )
    }

    "invalid vertical move" in {
      renderBoard(board)
      val knight = board(knightPosition)
      val invalidHorizontalPosition1 = Position(3, 1)
      val invalidHorizontalPosition2 = Position(3, 5)
      preMoveValidation(false, White, knight, knightPosition, invalidHorizontalPosition1, board) mustBe Left(
        "Invalid Knight(White) Move. Move from d5 to d7"
      )
      preMoveValidation(false, White, knight, knightPosition, invalidHorizontalPosition2, board) mustBe Left(
        "Invalid Knight(White) Move. Move from d5 to d3"
      )
    }

    "move taking king piece" in {
      val knight = board(knightPosition)
      val fromKingPosition = Position(4, 0)
      val toKingPosition = Position(5, 2)
      val king = board(fromKingPosition)
      val kingBoard = updateBoard(fromKingPosition, toKingPosition, king, board)
      renderBoard(kingBoard)
      preMoveValidation(false, White, knight, knightPosition, toKingPosition, kingBoard) mustBe Left(
        "Destination Move cannot be King. Move from d5 to f6"
      )
    }

    "move taking own piece" in {
      val knight = board(knightPosition)
      val fromOwnPawnPosition = Position(2, 6)
      val toOwnPosition = Position(2, 5)
      val ownPawn = board(fromOwnPawnPosition)
      val kingBoard = updateBoard(fromOwnPawnPosition, toOwnPosition, ownPawn, board)
      renderBoard(kingBoard)
      preMoveValidation(false, White, knight, knightPosition, toOwnPosition, kingBoard) mustBe Left(
        "Destination color must not be same color as own color. Move from d5 to c3"
      )
    }

  }
}
