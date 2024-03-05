package model.pieces
import model.{Black, Color, Position, White}

case class Pawn( override val color: Color) extends Piece(color) {

  override def toString = s"Pawn($color)"

  override def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean = {
    val rr = Math.abs(to.rank - from.rank)
    val ff = Math.abs(to.file - from.file)
    val isDestinationEmptyCell = emptyCell(Position(to.rank, to.file),pieces)


    val isMoveValid = color match {
      case White =>
        if (ff == 1 && rr == 0 && isDestinationEmptyCell) true
        else if (from.file == 6 && ff == 2 && rr == 0 && isDestinationEmptyCell && emptyCell(Position(from.file -1, from.rank),pieces)) true
        else if (ff == 1 && rr == 1 && !isDestinationEmptyCell) true
        else false
      case Black =>
        if (ff == 1 && rr == 0 && isDestinationEmptyCell) true
        else if (from.file == 1 && ff == 2 && rr == 0 && isDestinationEmptyCell && emptyCell(Position(from.file +1, from.rank),pieces)) true
        else if (ff == 1 && rr == 1 && !isDestinationEmptyCell) true
        else false
    }
    isMoveValid
  }

  def emptyCell(position: Position,  pieces: Map[Position, Piece])={
    !pieces.contains(position)
  }
}
