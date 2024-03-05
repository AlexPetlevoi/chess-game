package model.pieces

import model.{Color, Position}

case class Knight(override val color: Color) extends Piece(color) {
  override def toString = s"Knight($color)"

  override def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean = {
    val rr = Math.abs(to.rank - from.rank)
    val ff = Math.abs(to.file - from.file)
    (ff == 2 && rr == 1) || (ff == 1 && rr == 2)
  }
}
