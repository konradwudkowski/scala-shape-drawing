package db

import db.Canvas._
import org.scalatest.{Matchers, WordSpec}

class CanvasSpec extends WordSpec with Matchers {

  val ─ = "-"
  val │ = "|"
  val □ = " "
  val ⨯ = "x"

  "Canvas" should {
    "have specified dimensions and borders" in {
      val width  = 2
      val height = 3

      addBorders(createGrid(width, height)) shouldBe Vector(
        Vector(─, ─, ─, ─),
        Vector(│, □, □, │),
        Vector(│, □, □, │),
        Vector(│, □, □, │),
        Vector(─, ─, ─, ─)
      )
    }

    "draw a horizontal line" in {
      val grid = createGrid(10, 5)

      val point1 = Point(1, 3)
      val point2 = Point(7, 3)
      val result = drawLine(point1, point2)(grid)

      withClue(showIncorrectPattern(result)) {
        result shouldBe Vector(
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(⨯, ⨯, ⨯, ⨯, ⨯, ⨯, ⨯, □, □, □),
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, □, □, □, □, □, □, □)
        )
      }
    }

    "draw a horizontal line if order of points is reversed" in {
      val grid = createGrid(10, 5)

      val point1 = Point(1, 3)
      val point2 = Point(7, 3)
      val result = drawLine(point2, point1)(grid)

      withClue(showIncorrectPattern(result)) {
        result shouldBe Vector(
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(⨯, ⨯, ⨯, ⨯, ⨯, ⨯, ⨯, □, □, □),
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, □, □, □, □, □, □, □)
        )
      }
    }

    "draw a vertical line" in {
      val grid = createGrid(10, 5)

      val result = drawLine(Point(5, 1), Point(5, 4))(grid)

      withClue(showIncorrectPattern(result)) {
        result shouldBe Vector(
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □)
        )
      }
    }

    "draw a vertical line if order of points is reversed" in {
      val grid = createGrid(10, 5)

      val point1 = Point(5, 1)
      val point2 = Point(5, 4)
      val result = drawLine(point2, point1)(grid)

      withClue(showIncorrectPattern(result)) {
        result shouldBe Vector(
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □),
          Vector(□, □, □, □, ⨯, □, □, □, □, □)
        )
      }
    }

    "draw a rectangle" in {
      val grid = createGrid(10, 5)

      val result = drawRectangle(Point(4, 4), Point(10, 2))(grid)

      withClue(showIncorrectPattern(result)) {
        result shouldBe Vector(
          Vector(□, □, □, □, □, □, □, □, □, □),
          Vector(□, □, □, ⨯, ⨯, ⨯, ⨯, ⨯, ⨯, ⨯),
          Vector(□, □, □, ⨯, □, □, □, □, □, ⨯),
          Vector(□, □, □, ⨯, ⨯, ⨯, ⨯, ⨯, ⨯, ⨯),
          Vector(□, □, □, □, □, □, □, □, □, □)
        )
      }
    }
  }

  def showIncorrectPattern(grid: Grid): String =
    s"Expected a different pattern but got: \n ${showGrid(grid)} \n "

}
