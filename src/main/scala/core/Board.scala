package core

final class Board {
  def at(row: Row, col: Col): Option[Piece] = throw new NotImplementedError()
  def take(row: Row, col: Col): Option[Piece] = throw new NotImplementedError()
  def put(row: Row, col: Col, piece: Piece) = throw new NotImplementedError()

  def isFree(row: Row, col: Col): Boolean = at(row, col).isEmpty
  def isOccupied(row: Row, col: Col): Boolean = !isFree(row, col)

  override def toString = throw new NotImplementedError()
}
