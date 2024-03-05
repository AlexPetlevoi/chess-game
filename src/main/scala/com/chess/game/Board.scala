package com.chess.game

import model.pieces.Piece.{checkIfColorValid, checkIfDestinationIsKing}
import model.pieces._
import model.{Black, Color, Position, White}

object Board {

  val listOfChars = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

  def mapToPosition(st: List[Int]) = {
    val fromRank = st(0)
    val fromFile = st(1)
    val toRank = st(2)
    val toFile = st(3)
    (Position(fromRank, fromFile), Position(toRank, toFile))
  }

  def updateBoard(from: Position, to: Position, sourcePiece: Piece, pieces: Map[Position, Piece]) =
    (pieces - from) + ((to, sourcePiece))

  def isBoundaryValid(list: List[Int]): Either[String, Boolean] = {
    val boundary = 0 to 7
    Either.cond(list.forall(boundary.contains), true, "Out of boundary move")
  }

  def preMoveValidation(
    isFirstMove: Boolean,
    turnColorMove: Color,
    piece: Piece,
    from: Position,
    to: Position,
    pieces: Map[Position, Piece]
  ) = {
    val coordinates = s"Move from $from to $to"
    val destinationPiece = pieces.get(Position(to.rank, to.file))
    val validationResult = for {
      _ <-
        if (isFirstMove) Either.cond(piece.color == White, true, s"Not valid turn move. $coordinates") else Right(())
      _ <- Either.cond(turnColorMove == piece.color, true, s"Not valid turn move. $coordinates")
      _ <- Either.cond(
        destinationPiece.forall(dest => !checkIfDestinationIsKing(Some(dest))),
        true,
        s"Destination Move cannot be King. ${coordinates}"
      )
      _ <- Either.cond(
        destinationPiece.forall(dest => checkIfColorValid(piece.color, dest.color)),
        true,
        s"Destination color must not be same color as own color. ${coordinates}"
      )
      _ <- Either.cond(piece.validateMove(from, to, pieces), true, s"Invalid $piece Move. ${coordinates}")
    } yield true
    validationResult
  }

  def replaceCharToInt(st: String) =
    st.map(e => if (listOfChars.contains(e)) ((e.toInt) - 48).toChar else e)

  def createPawns() = {
    val vertical = 0 to 7
    val horizontal = Seq(1, 6)
    (for {
      v <- vertical
      h <- horizontal
    } yield if (h == 6) (Position(v, h) -> Pawn(White)) else Position(v, h) -> Pawn(Black)).toMap
  }

  def createBoard: Map[Position, Piece] =
    Map(
      Position(0, 0) -> Rook(Black),
      Position(1, 0) -> Knight(Black),
      Position(2, 0) -> Bishop(Black),
      Position(3, 0) -> Queen(Black),
      Position(4, 0) -> King(Black),
      Position(5, 0) -> Bishop(Black),
      Position(6, 0) -> Knight(Black),
      Position(7, 0) -> Rook(Black)
    ) ++ Map(
      Position(0, 7) -> Rook(White),
      Position(1, 7) -> Knight(White),
      Position(2, 7) -> Bishop(White),
      Position(3, 7) -> Queen(White),
      Position(4, 7) -> King(White),
      Position(5, 7) -> Bishop(White),
      Position(6, 7) -> Knight(White),
      Position(7, 7) -> Rook(White)
    ) ++ createPawns

  def renderBoard(pieces: Map[Position, Piece]) = {
    val horizontal = 0 to 7
    val vertical = 0 to 7
    val rankLabel = listOfChars.mkString("  | ", " | ", " | ")
    val deliminator = "--+"+ List.fill(9)("---+").mkString
    val rows = horizontal
      .zip(horizontal.reverse)
      .map { case (r, rankRepresentation) =>
        vertical
          .map { v =>
            val pieceVal = pieces.get(Position(v, r)) match {
              case Some(p) => getPieceName(p)
              case _       => "_"
            }
            s"| $pieceVal "
          }
          .mkString(s"${rankRepresentation + 1} ", "", s"| ${rankRepresentation + 1}\n")
      }
      .mkString
    println(rankLabel + "\n" + deliminator + "\n" + rows + deliminator + "\n" + rankLabel + "\n")
  }

  def getPieceName(piece: Piece): String = {
    val pieceSymbol = piece match {
      case _: Pawn   => "p"
      case _: Knight => "n"
      case _: Bishop => "b"
      case _: Rook   => "r"
      case _: Queen  => "q"
      case _: King   => "k"
    }
    if (piece.color == White) pieceSymbol.toUpperCase else pieceSymbol
  }

}
