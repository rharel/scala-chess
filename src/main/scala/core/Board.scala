package core

import CoordinateConversion.{intToRow, intToCol}

object Board {
  val Size = 8

  def squares: Iterator[Square] = squareList.iterator

  private val squareList: List[Square] = {
    var result = List.empty[Square]
    for (row <- 0 until Board.Size) {
      for (col <- 0 until Board.Size) {
        result = result :+ Square(row, col)
      }
    }
    result
  }
}
final class Board {
  def at(row: Row, col: Col): Option[Piece] = at(toGridIndex(row, col))
  def take(row: Row, col: Col): Option[Piece] = take(toGridIndex(row, col))
  def put(row: Row, col: Col, piece: Piece): Option[Piece] = put(toGridIndex(row, col), piece)

  def isFree(row: Row, col: Col): Boolean = at(row, col).isEmpty
  def isOccupied(row: Row, col: Col): Boolean = !isFree(row, col)

  def copy(source: Board): Unit = source.grid.copyToArray(this.grid)

  override def clone: Board = {
    val result = new Board
    result.copy(this)
    result
  }
  override def toString: String = {
    var result = ""
    for (Square(row, col) <- Board.squares) {
      result += (at(row, col) match {
        case Some(Piece(White, kind)) => kind.name(0).toUpper
        case Some(Piece(Black, kind)) => kind.name(0).toLower
        case None => "."
      })
      if (col.isLast) { result += "\n" }
    }
    result
  }

  private def toGridIndex(row: Row, col: Col): Int = Board.Size * row.index + col.index

  private def at(index: Int): Option[Piece] = grid(index)
  private def take(index: Int): Option[Piece] = {
    val contents = grid(index)
    grid(index) = None
    contents
  }
  private def put(index: Int, piece: Piece): Option[Piece] = {
    val previousContents = grid(index)
    grid(index) = Some(piece)
    previousContents
  }

  private val grid = Array.fill[Option[Piece]](Board.Size * Board.Size)(None)
}
