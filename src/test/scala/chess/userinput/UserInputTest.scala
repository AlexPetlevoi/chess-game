package chess.userinput

import com.chess.game.Game
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UserInputTest extends AnyWordSpec with Matchers {
  val emptyFile = "src/test/resource/empty-file.txt"

  "UserInput" should {
    "handle non existing file" in {
      val nonExistentFile = "non-existent"
      val game = new Game(nonExistentFile)

      game.initiateTheGame() mustBe Left(s"Specified File $nonExistentFile doesn't exist")
    }

    "handle end of file" in {
      val game = new Game(emptyFile)

      game.initiateTheGame() mustBe Left("Specified file src/test/resource/empty-file.txt is empty")
    }
  }
}
