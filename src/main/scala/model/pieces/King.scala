package model.pieces

import model.{Color, Position}

case class King(override val color: Color) extends Piece(color) {
  override def toString = s"King($color)"

  override def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean = {
    val ff = Math.abs(from.file - to.file)
    val rr = Math.abs(from.rank - to.rank)
    (rr == 1 & ff == 0) || (rr == 0 & ff == 1) || (rr == 1 & ff == 1)
  }
}
