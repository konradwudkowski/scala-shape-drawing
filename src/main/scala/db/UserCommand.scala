package db

object UserCommand {

  private val CreateInput        = """C\s+(\d{1,9})\s+(\d{1,9})\s*""".r
  private val DrawLineInput      = """L\s+(\d{1,9})\s+(\d{1,9})\s+(\d{1,9})\s+(\d{1,9})\s*""".r
  private val DrawRectangleInput = """R\s+(\d{1,9})\s+(\d{1,9})\s+(\d{1,9})\s+(\d{1,9})\s*""".r

  def parse(s: String): Either[InvalidCommand, UserCommand] =
    s match {
      case CreateInput(width, height) =>
        if (isNonEmptyCanvas(width, height)) {
          Right(CreateCanvas(width.toInt, height.toInt))
        } else {
          Left(EmptyCanvas)
        }

      case DrawLineInput(x1, x2, y1, y2) =>
        if (canFormALine(x1, x2, y1, y2)) {
          Right(DrawLine(x1.toInt, x2.toInt, y1.toInt, y2.toInt))
        } else {
          Left(InvalidLineCoordinates)
        }

      case DrawRectangleInput(x1, x2, y1, y2) =>
        if (upperLeftFirst(x1, x2, y1, y2)) {
          Right(DrawRectangle(x1.toInt, x2.toInt, y1.toInt, y2.toInt))
        } else {
          Left(InvalidRectangleCoordinates)
        }

      case "Q" => Right(Quit)
      case ""  => Right(ReturnPressed)
      case _   => Left(UnknownCommand)
    }

  def isNonEmptyCanvas(w: String, h: String): Boolean =
    w.toInt > 0 && h.toInt > 0

  def canFormALine(x1: String, y1: String, x2: String, y2: String): Boolean =
    x1 == x2 || y1 == y2

  def upperLeftFirst(x1: String, y1: String, x2: String, y2: String): Boolean =
    x1.toInt <= x2.toInt && y1.toInt >= y2.toInt
}

sealed trait UserCommand extends Product with Serializable
final case class CreateCanvas(width: Int, height: Int) extends UserCommand
final case class DrawLine(x1: Int, y1: Int, x2: Int, y2: Int) extends UserCommand
final case class DrawRectangle(x1: Int, y1: Int, x2: Int, y2: Int) extends UserCommand
case object Quit extends UserCommand
case object ReturnPressed extends UserCommand

sealed trait InvalidCommand extends Product with Serializable
case object InvalidLineCoordinates extends InvalidCommand
case object InvalidRectangleCoordinates extends InvalidCommand
case object EmptyCanvas extends InvalidCommand
case object UnknownCommand extends InvalidCommand
