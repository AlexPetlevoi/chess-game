package model.pieces

import model.pieces.Piece.validateDiagonalMove
import model.{Color, Position}

case class Bishop(override val color: Color) extends Piece(color) {

  override def toString = s"Bishop($color)"

  override def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean =
    validateDiagonalMove(from, to, pieces)
}
