package db

import scala.util.Try

object Command {

  private val CreateInput        = """C\s+(\d+)\s+(\d+)\s*""".r
  private val DrawLineInput      = """L\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*""".r
  private val DrawRectangleInput = """R\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*""".r

  def parse(s: String): Option[Command] =
    s match {
      case CreateInput(width, height) =>
        Try(CreateCanvas(width.toInt, height.toInt)).toOption

      case DrawLineInput(x1, x2, y1, y2) =>
        Try(DrawLine(x1.toInt, x2.toInt, y1.toInt, y2.toInt)).toOption

      case DrawRectangleInput(x1, x2, y1, y2) =>
        Try(DrawRectangle(x1.toInt, x2.toInt, y1.toInt, y2.toInt)).toOption

      case "Q" => Some(Quit)
      case ""  => Some(ReturnPressed)
      case _   => None
    }
}

sealed trait Command extends Product with Serializable

final case class CreateCanvas(width: Int, height: Int) extends Command

final case class DrawLine(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

final case class DrawRectangle(x1: Int, y1: Int, x2: Int, y2: Int) extends Command

case object Quit extends Command

case object ReturnPressed extends Command
