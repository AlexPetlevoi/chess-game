package com.chess.game

import cats.effect._
import com.chess.game.Board._
import com.whitehatgaming.UserInputFile
import model.pieces.{King, Piece}
import model.{Black, Color, Position, White}

import scala.io.StdIn
import scala.util.Try
import scala.util.matching.Regex

class Game(val fileName: String) {
  val fileDoesntExist = s"Specified File $fileName doesn't exist"
  val fileIsEmpty = s"Specified file $fileName is empty"
  val eof = s"Specified File $fileName has reached EOF"
  val regex_07 = "[0-7]{4}".r
  val regex_ah18 = "[a-h][1-8][a-h][1-8]".r

//  def readMove(player: String): (Position,Position) = {
  def readMove() = {
    println(s"Player  enter your move (e.g., 'e2 e4'): ")
    var board = createBoard
    renderBoard(board)
    var colorTurn: Color = White
    var isFirstMove = true
    var isGameOver = false

    while (!isGameOver) {
      val move = StdIn.readLine()
      val res = for {
        _ <- validateMoveBoundaries(regex_ah18, move)
        (from, to) = parseToPosition(move)
        res <- proceedWithMoves(List((from, to)), board, colorTurn, isFirstMove)
        _ = {
          board = updateBoard(from, to, board(from), board)
        }
      } yield res
      res match {
        case Right(_) => //updateBoard(from, to, sourcePiece, board)
        case Left(errorMessage) =>
          println(errorMessage)
          isGameOver = true
      }
      colorTurn = getOpponentColor(colorTurn)
      isFirstMove = false
    }
    println("Game is over")

  }

  def letTheGameBegin(): Either[String, Unit] =
    for {
      userInput <- Try(new UserInputFile(fileName)).toEither.left.map(_ => fileDoesntExist)
      allMoves <- readAllMoves(userInput)
      board = createBoard
      _ = "Start of The Game"
      _ = renderBoard(board)
      res <- proceedWithMoves(allMoves, board)
    } yield res

  def proceedWithMoves(
    moves: List[(Position, Position)],
    board: Map[Position, Piece],
    turnColor: Color = White,
    isFirstMove: Boolean = true
  ): Either[String, Unit] =
    moves match {
      case Nil => Right()
      case (from, to) :: remainingMoves =>
        for {

          sourcePiece <- board.get(Position(from.rank, from.file)).toRight("Empty piece on source move position")
          _ = println(s"---+++---+++---Next Move---+++---+++   $from->$to\n ")
          _ <- preMoveValidation(isFirstMove, turnColor, sourcePiece, from, to, board)
          updatedBoard = updateBoard(from, to, sourcePiece, board)
          isKingUnderCheck <- checkIfOwnKingIsUnderCheckWithSideEffect(
            turnColor,
            updatedBoard
          ) // side effect  -> return informative Left() with message if king under the check
          isOpponentUnderCheck = {
            val isOpponentKingUnderCheck = checkIfOpponentKingIsUnderCheck(
              turnColor,
              updatedBoard
            )
            if (isOpponentKingUnderCheck)
              println(s"-*-*-* Opponent ${getOpponentColor(turnColor)} King is in Check   -*-*-*")
          } //no side effect -> just informative message
          _ = renderBoard(updatedBoard)
          res <- proceedWithMoves(remainingMoves, updatedBoard, getOpponentColor(turnColor), isFirstMove = false)
        } yield res
    }

  def readAllMoves(userInput: UserInputFile): Either[String, List[(Position, Position)]] = {

    def auxFunction(acc: List[(Position, Position)]): Either[String, List[(Position, Position)]] =
      for {
        move <- Try(userInput.nextMove()).toEither.left.map(_.getMessage)

        result <- move match {
          case null => Right(acc)
          case _ =>
            for {
              _ <- validateMoveBoundaries(regex_07, move.mkString)
              res <- auxFunction(acc :+ parseToPosition(move))
            } yield res
        }
      } yield result
    auxFunction(List.empty)
  }

  def readLine = IO.blocking(StdIn.readLine())

  def validateMoveBoundaries(regex: Regex, move: String): Either[String, String] =
    move.trim match {
      case regex() => Right(move)
      case _       => Left(s"Invalid move format: $move")
    }

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
    val (from, to) = parseToPosition(move.toArray)
    for {
      sourcePiece <- pieces.get(Position(from.rank, from.file)).toRight("Empty piece on source move position")
      _ <- isBoundaryValid(move)
      _ = println(s"---+++---+++---Next Move---+++---+++---${parseToPosition(move.toArray)}\n ")
      _ <- preMoveValidation(isFirstMove, turnColor, sourcePiece, from, to, pieces)
      tempBoard = updateBoard(from, to, sourcePiece, pieces)
      isKingUnderCheck = checkIfOwnKingIsUnderCheck(turnColor, tempBoard) // side effect  -> return informative Left()
      isOpponentUnderCheck = checkIfOpponentKingIsUnderCheck(
        turnColor,
        tempBoard
      ) //no side effect -> just informative message
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

  def parseToPosition(arr: Array[Int]) = (Position(arr(0), arr(1)), Position(arr(2), arr(3)))

  def parseToPosition(move: String) =
    (Position(move.charAt(0) - 97, 56 - move.charAt(1)), Position(move.charAt(2) - 97, 56 - move.charAt(3)))

  def checkIfOwnKingIsUnderCheck(turnColor: Color, pieces: Map[Position, Piece]): Boolean = { //make it Either
    val ownKing = findKing(turnColor, pieces)
    ownKing.forall { case (position, _) =>
      validateIfKingUnderCheck(turnColor, position, pieces)
    }
  }

  def checkIfOwnKingIsUnderCheckWithSideEffect(
    turnColor: Color,
    pieces: Map[Position, Piece]
  ): Either[String, Unit] = //make it Either
    findKing(turnColor, pieces) match {
      case Some((position, king)) =>
        if (!validateIfKingUnderCheck(turnColor, position, pieces)) Right()
        else Left(s"Can't perform the move. $king in Check.")
      case None => Left("We lost the king")
    }

  def checkIfOpponentKingIsUnderCheck(turnColor: Color, pieces: Map[Position, Piece]): Boolean = { //make it Either
    val opponentKing = findKing(getOpponentColor(turnColor), pieces)
    opponentKing.exists { case (position, _) =>
      validateIfKingUnderCheck(getOpponentColor(turnColor), position, pieces)
    }
  }

  /*
  isKingUnderCheck.nonEmpty = no true result on method piece.validateMove(position, kingPosition, pieces) .King is not under check
  isKingUnderCheck.nonEmpty = !isKingUnderCheck.isEmpty
   */
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

  def findKing(color: Color, pieces: Map[Position, Piece]): Option[(Position, Piece)] = {
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
