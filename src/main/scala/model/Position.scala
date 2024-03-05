package model

case class Position(rank: Int, file: Int) {

  override def toString: String =
    s"${(rank + 97).toChar}${(56 - file).toChar}"
}
