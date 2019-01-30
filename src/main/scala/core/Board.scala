package core

class Board extends Debuggable {
  def at(row: Int, col: Int): Option[Piece] = throw new NotImplementedError()
  def take(row: Int, col: Int): Option[Piece] = throw new NotImplementedError()
  def put(row: Int, col: Int, piece: Piece) = throw new NotImplementedError()

  def isFree(row: Int, col: Int): Boolean = at(row, col).isEmpty
  def isOccupied(row: Int, col: Int): Boolean = !isFree(row, col)

  def debug = throw new NotImplementedError()
}
