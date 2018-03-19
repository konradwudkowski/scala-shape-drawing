package db

import scala.math.{max, min}

object Canvas {

  type Grid = Vector[Vector[String]]

  final case class Point(x: Int, y: Int)

  def createGrid(width: Int, height: Int): Grid =
    Vector.fill(height)(Vector.fill(width)(" "))

  def drawLine(p1: Point, p2: Point)(grid: Grid): Grid =
    fillValues(allPointsBetweenCoordinates(p1, p2))(grid)

  private def allPointsBetweenCoordinates(p1: Point, p2: Point): Seq[Point] = {
    val isVertical = p1.x == p2.x

    if (isVertical) {
      val verticalRange = min(p1.y, p2.y) to max(p1.y, p2.y)
      verticalRange.map(y => Point(p1.x, y))
    } else {
      val horizontalRange = min(p1.x, p2.x) to max(p2.x, p1.x)
      horizontalRange.map(x => Point(x, p1.y))
    }
  }

  def drawRectangle(upperLeft: Point, lowerRight: Point)(grid: Grid): Grid = {
    val line1 = allPointsBetweenCoordinates(upperLeft, Point(upperLeft.x, lowerRight.y))
    val line2 = allPointsBetweenCoordinates(upperLeft, Point(lowerRight.x, upperLeft.y))
    val line3 = allPointsBetweenCoordinates(lowerRight, Point(upperLeft.x, lowerRight.y))
    val line4 = allPointsBetweenCoordinates(lowerRight, Point(lowerRight.x, upperLeft.y))

    val points = line1 ++ line2 ++ line3 ++ line4

    fillValues(points)(grid)
  }

  def fillValues(points: Seq[Point])(grid: Grid): Grid =
    points.foldLeft(grid) {
      case (acc, point) =>
        val colPosition = point.x - 1
        val rowPosition = acc.length - point.y
        val updatedRow  = acc(rowPosition).updated(colPosition, "x")
        acc.updated(rowPosition, updatedRow)
    }

  def printWithBorders(grid: Grid): Unit =
    println(showGrid(addBorders(grid)))

  def showGrid(grid: Grid): String =
    grid.map(_.mkString).mkString("\n")

  def addBorders(grid: Grid): Grid = {
    val withVerticalBorders = grid.map { rows =>
      "|" +: rows :+ "|"
    }

    val horizontalBorder = Vector.fill(grid.head.length + 2)("-")
    horizontalBorder +: withVerticalBorders :+ horizontalBorder
  }

}
