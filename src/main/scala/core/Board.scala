package core

class Board extends Debuggable {
  def at(row: Int, col: Int): Option[Piece] = throw new NotImplementedError()
  def take(row: Int, col: Int): Option[Piece] = throw new NotImplementedError()
  def put(row: Int, col: Int, piece: Piece) = throw new NotImplementedError()

  def isFree(row: Int, col: Int): Boolean = at(row, col).isEmpty
  def isOccupied(row: Int, col: Int): Boolean = !isFree(row, col)

  def findMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()
  def findPawnMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()
  def findKnightMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()
  def findBishopMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()
  def findRookMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()
  def findQueenMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()
  def findKingMovesFrom(row: Int, col: Int): List[PieceMove] = throw new NotImplementedError()

  def isCheck(color: PieceColor): Boolean = throw new NotImplementedError()
  def isMate(color: PieceColor): Boolean = throw new NotImplementedError()

  def debug() = throw new NotImplementedError()
}
