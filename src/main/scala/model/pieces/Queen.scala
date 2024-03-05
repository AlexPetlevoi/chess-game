package model.pieces

import model.pieces.Piece.{validateDiagonalMove, validateHorizontalMove}
import model.{Color, Position}

case class Queen(override val color: Color) extends Piece(color) {

  override def toString = s"Queen($color)"

  override def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean = {
    val ff = Math.abs(from.file - to.file)
    val rr = Math.abs(from.rank - to.rank)
    if (ff == 0 || rr == 0) validateHorizontalMove(from, to, pieces)
    else validateDiagonalMove(from, to, pieces)
  }
}
