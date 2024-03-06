package model.pieces

import model.{Color, Position, White}

abstract class Piece(val color: Color = White) {

  def validateMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean

  override def toString = s"Piece($color)"
}

object Piece {
  def checkIfColorValid(source: Color, destination: Color) = source != destination

  def checkIfDestinationIsKing(piece: Option[Piece]) = piece match {
    case Some(value) =>
      value match {
        case _ @King(_) => true
        case _          => false
      }
    case None => false
  }

  def validateDiagonalMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean = {
    val ff = Math.abs(from.file - to.file)
    val rr = Math.abs(from.rank - to.rank)
    if (ff == rr) {
      val isPathEmpty: Boolean = (ff, rr) match {
        case (x, y) if x > 1 || y > 1 =>
          val (rStep, fStep) = generateSequence(from, to)
          val list = rStep.zipAll(fStep, fStep.head, rStep.head)
          list.forall { case (r, f) => !pieces.contains(Position(r, f)) }
        case _ => true
      }
      isPathEmpty
    } else false
  }

  def validateHorizontalMove(from: Position, to: Position, pieces: Map[Position, Piece]): Boolean = {
    val isSameFile = from.file == to.file
    val isSameRank = from.rank == to.rank
    if (isSameFile || isSameRank) {
      val seq = if (isSameFile) {
        (if (from.rank > to.rank) (from.rank - 1) until (to.rank, -1)
         else (from.rank + 1 until to.rank)).map(r => (r, from.file))
      } else {
        (if (from.file > to.file) (from.file - 1) until (to.file, -1)
         else ((from.file + 1) until to.file)).map(f => (from.rank, f))
      }
      seq.forall { case (rank, file) => !pieces.contains(Position(rank, file)) }
    } else false
  }

  def generateSequence(from: Position, to: Position) = {
    val rSeq = if (from.rank > to.rank)((from.rank - 1).until(to.rank, -1)) else ((from.rank + 1) until to.rank)
    val fSeq = if (from.file > to.file)((from.file - 1).until(to.file, -1)) else ((from.file + 1) until to.file)
    (rSeq.toList, fSeq.toList)
  }
}
