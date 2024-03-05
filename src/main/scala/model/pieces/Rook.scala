package model.pieces

import model.pieces.Piece.validateHorizontalMove
import model.{Color, Position}

case class Rook(override val color: Color) extends Piece(color) {
  override def toString = s"Rook($color)"

  override def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean =
    validateHorizontalMove(from, to, pieces)
}
