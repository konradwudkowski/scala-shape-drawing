package db

import db.Canvas._

import scala.util.control.NonFatal

object Main extends App {
  val scanner = new java.util.Scanner(System.in)

  loop()

  def usage(): Unit = {
    def padded(s: String): String = s.padTo(15, " ").mkString

    val q1 = "Following commands are available:"
    val q2 = padded("C w h") + "Create a new canvas of width w and height h"
    val q3 = padded("L x1 y1 x2 y2") + "Draw a new line from coordinates (x1, y1) to (x2, y2) horizontally or vertically."
    val q4 = padded("R x1 y1 x2 y2") + "Draw a new rectangle, with upper left corner at coordinate (x1, y1) and lower right coordinate at (x2, y2)."
    val q5 = padded("Q") + "Quit the program"
    println(List(q1, q2, q3, q4, q5).mkString("\n"))
  }

  def loop(state: Grid = Vector.empty): Grid = {
    print("enter command: ")

    def handle(nextGrid: => Grid): Grid =
      try {
        val grid = nextGrid
        printWithBorders(grid)
        loop(grid)
      } catch {
        case NonFatal(_) =>
          println(red("Unable to apply command to current canvas"))
          loop(state)
      }

    UserCommand.parse(scanner.nextLine()) match {

      case Right(Quit) =>
        println("Bye!")
        sys.exit()

      case Right(ReturnPressed) =>
        println
        loop(state)

      case Right(CreateCanvas(width, height)) =>
        handle {
          createGrid(width, height)
        }

      case Right(DrawLine(x1, y1, x2, y2)) =>
        handle {
          drawLine(Point(x1, y1), Point(x2, y2))(state)
        }

      case Right(DrawRectangle(x1, y1, x2, y2)) =>
        handle {
          drawRectangle(Point(x1, y1), Point(x2, y2))(state)
        }

      case Left(EmptyCanvas) =>
        println(red("Please create a non-empty canvas"))
        usage()
        loop(state)

      case Left(InvalidLineCoordinates) =>
        println(red("Line coordinates not valid to form a horizontal or vertical line"))
        usage()
        loop(state)

      case Left(InvalidRectangleCoordinates) =>
        println(red("Rectangle coordinates not valid, please make sure to put upper left point first"))
        usage()
        loop(state)

      case Left(UnknownCommand) =>
        println(red("Command unrecognized, please try again"))
        usage()
        loop(state)
    }

  }

  def red(s: String): String = Console.RED + s + Console.RESET

}
