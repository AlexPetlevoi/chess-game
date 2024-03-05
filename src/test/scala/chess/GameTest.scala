package chess

import com.chess.game.Game
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GameTest extends AnyWordSpec with Matchers {
  val sampleMoves = "src/test/resource/sample-moves-valid.txt"
  val sampleMovesInvalid = "src/test/resource/sample-moves-valid.txt"
  val checkMate = "src/test/resource/checkmate.txt"

  "Game" should {
    "perform the game with valid moves" in {
      val game = new Game(sampleMoves).initiateTheGame()
    }

    "perform the game with invalid moves" in {
      val game = new Game(sampleMovesInvalid).initiateTheGame()
      game mustBe Left("Invalid Rook(Black) Move. Move from h8 to h4")
    }

    "perform the game with checkmate result" in {
      val game = new Game(checkMate).initiateTheGame()
      game mustBe Left("Invalid Rook(Black) Move. Move from h8 to h4")
    }
  }
}
