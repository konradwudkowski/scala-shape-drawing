package db

import org.scalatest.{Matchers, WordSpec}

class UserCommandSpec extends WordSpec with Matchers {

  "Parsing user commands" should {
    "work for creating new canvas" in {
      val userInput = "C 12 14"
      UserCommand.parse(userInput) shouldBe Right(CreateCanvas(12, 14))
    }
    "fail for zero sized canvas" in {
      val userInputs = List("C 0 0", "C 1 0", "C 0 1")
      userInputs.foreach { u =>
        UserCommand.parse(u) shouldBe Left(EmptyCanvas)
      }
    }
    "work for drawing lines" in {
      val userInput = "L 1 2 1 5"
      UserCommand.parse(userInput) shouldBe Right(DrawLine(1, 2, 1, 5))
    }
    "fail to accept a line if points don't form a line" in {
      val userInput = "L 1 2 3 4"
      UserCommand.parse(userInput) shouldBe Left(InvalidLineCoordinates)
    }
    "work for drawing rectangles" in {
      val userInput = "R 1 5 2 4"
      UserCommand.parse(userInput) shouldBe Right(DrawRectangle(1, 5, 2, 4))
    }
    "fail to accept a rectangle if upper left point not provided first" in {
      val userInputs = List("R 1 5 1 6", "R 3 5 1 1")
      userInputs.foreach { u =>
        UserCommand.parse(u) shouldBe Left(InvalidRectangleCoordinates)
      }
    }
    "work for quitting" in {
      val userInput = "Q"
      UserCommand.parse(userInput) shouldBe Right(Quit)
    }
    "work for empty input" in {
      val userInput = ""
      UserCommand.parse(userInput) shouldBe Right(ReturnPressed)
    }
    "fail for unrecognized commands" in {
      val userInput = "not a valid command"
      UserCommand.parse(userInput) shouldBe Left(UnknownCommand)
    }

  }

}
