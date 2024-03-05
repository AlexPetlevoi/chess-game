package com.chess.game

import com.chess.game.Board._
import com.whitehatgaming.UserInputFile
import model.pieces.{King, Piece}
import model.{Black, Color, Position, White}

import scala.util.Try

class Game(val fileName: String) {
  val fileDoesntExist = s"Specified File $fileName doesn't exist"
  val fileIsEmpty = s"Specified file $fileName is empty"
  val eof = s"Specified File $fileName has reached EOF"

  def initiateTheGame() =
    for {
      userInput <- Try(new UserInputFile(fileName)).toEither.left.map(_ => fileDoesntExist)
      board = createBoard
      _ = "Start of The Game"
      _ = renderBoard(board)
      move <- getMove(userInput, fileIsEmpty)
      res <- startGame(userInput, board, move)
    } yield res

  def startGame(
    input: UserInputFile,
    pieces: Map[Position, Piece],
    move: List[Int],
    turnColor: Color = White,
    isFirstMove: Boolean = true
  ): Either[String, Unit] = {
    val (from, to) = parseToPosition(move)
    for {
      sourcePiece <- pieces.get(Position(from.rank, from.file)).toRight("Empty piece on source move position")
      _ <- isBoundaryValid(move)
      _ = println(s"---+++---+++---Next Move---+++---+++---${parseToPosition(move)}\n ")
      _ <- preMoveValidation(isFirstMove, turnColor, sourcePiece, from, to, pieces)
      tempBoard = updateBoard(from, to, sourcePiece, pieces)
      isKingUnderCheck = checkIfOwnKingIsUnderCheck(turnColor, tempBoard)
      isOpponentUnderCheck = checkIfOpponentKingIsUnderCheck(turnColor, tempBoard)
      _ <-
        if (isKingUnderCheck) {
          println(s"-*-*-*  ${turnColor} King is in Check   -*-*-*")
          Left(s"Invalid Move $from->$to. It leaves your King under check")
        } else Right(())
      _ = renderBoard(tempBoard)
      _ = if (isOpponentUnderCheck)
        println(s"-*-*-*  ${getOpponentColor(turnColor)} King is under the Check   -*-*-*\n")
      nextMove <- getMove(input, eof)
      res <- startGame(input, tempBoard, nextMove, turnColor = getOpponentColor(turnColor), isFirstMove = false)
    } yield res
  }

  def parseToPosition(arr: List[Int]) = (Position(arr(0), arr(1)), Position(arr(2), arr(3)))

  def checkIfOwnKingIsUnderCheck(turnColor: Color, pieces: Map[Position, Piece]): Boolean = {
    val ownKing = findKing(turnColor, pieces)
    ownKing.exists { case (position, _) =>
      validateIfKingUnderCheck(turnColor, position, pieces)
    }
  }

  def checkIfOpponentKingIsUnderCheck(turnColor: Color, pieces: Map[Position, Piece]): Boolean = {
    val opponentKing = findKing(getOpponentColor(turnColor), pieces)
    opponentKing.exists { case (position, _) =>
      validateIfKingUnderCheck(getOpponentColor(turnColor), position, pieces)
    }
  }

  def validateIfKingUnderCheck(turnColor: Color, kingPosition: Position, pieces: Map[Position, Piece]) = {
    val opponentColor = getOpponentColor(turnColor)
    val isKingUnderCheck =
      pieces.filter { case (_, piece) => piece.color == opponentColor }.filter { case (position, piece) =>
        piece.validateMove(position, kingPosition, pieces)
      }
    isKingUnderCheck.nonEmpty
  }

  def getOpponentColor(color: Color) = color match {
    case Black => White
    case White => Black
  }

  def findKing(color: Color, pieces: Map[Position, Piece]) = {
    val kingPosition = pieces.find {
      case (Position(_, _), k @ King(_)) => k.color == color
      case _                             => false
    }
    kingPosition
  }

  def getMove(userInput: UserInputFile, errorMessage: String): Either[String, List[Int]] =
    Try(userInput.nextMove()).toEither.left.map(_ => errorMessage).flatMap { nextMove =>
      Option(nextMove).map(_.toList).toRight(errorMessage)
    }

}
