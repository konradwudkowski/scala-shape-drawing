package db

import org.scalatest.{Matchers, OptionValues, WordSpec}

class CommandSpec extends WordSpec with Matchers with OptionValues {

  "Parsing user commands" should {
    "work for creating new canvas" in {
      val cmd = "C 12 14"
      Command.parse(cmd).value shouldBe CreateCanvas(12, 14)
    }
    "work for drawing lines" in {
      val cmd = "L 1 2 5 5"
      Command.parse(cmd).value shouldBe DrawLine(1, 2, 5, 5)
    }
    "work for drawing rectangles" in {
      val cmd = "R 1 2 5 5"
      Command.parse(cmd).value shouldBe DrawRectangle(1, 2, 5, 5)
    }
    "work for initiating exit" in {
      val cmd = "Q"
      Command.parse(cmd).value shouldBe Quit
    }
    "work for empty input" in {
      val cmd = ""
      Command.parse(cmd).value shouldBe ReturnPressed
    }
    "return None for unrecognized commands" in {
      val cmd = "not a valid command"
      Command.parse(cmd) shouldBe None
    }
  }

}
