package chess.board

import com.chess.game.Board
import model.{Black, White}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoardTest extends AnyWordSpec with Matchers {

  "Board" should {
    "create board with 32 pieces" in {
      val board = Board.createBoard
      board.size mustBe 32
      board.values.count(p => p.color == White) mustBe 16
      board.values.count(p => p.color == Black) mustBe 16
    }
  }
}
