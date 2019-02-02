package core

final class Board {
  def at(row: Int, col: Int): Option[Piece] = throw new NotImplementedError()
  def take(row: Int, col: Int): Option[Piece] = throw new NotImplementedError()
  def put(row: Int, col: Int, piece: Piece) = throw new NotImplementedError()

  def isFree(row: Int, col: Int): Boolean = at(row, col).isEmpty
  def isOccupied(row: Int, col: Int): Boolean = !isFree(row, col)

  override def toString = throw new NotImplementedError()
}
