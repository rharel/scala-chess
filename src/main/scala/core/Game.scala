package core

final class Game {
  def moves: List[Move] = throw new NotImplementedError()
  def playerToMove: PieceColor = throw new NotImplementedError()

  def result: Option[GameResult] = throw new NotImplementedError()
  def isOver: Boolean = result.isDefined
  def isOngoing: Boolean = !isOver

  def isChecked(player: PieceColor): Boolean = throw new NotImplementedError()
  def isMated(player: PieceColor): Boolean = throw new NotImplementedError()

  def canCastleKingside(player: PieceColor): Boolean = throw new NotImplementedError()
  def canCastleQueenside(player: PieceColor): Boolean = throw new NotImplementedError()
  def canCaptureEnPassant(player: PieceColor, col: Col) = throw new NotImplementedError()

  def findMovesFor(player: PieceColor) = throw new NotImplementedError()
  def findMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()
  def findPawnMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()
  def findKnightMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()
  def findBishopMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()
  def findRookMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()
  def findQueenMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()
  def findKingMovesFrom(row: Row, col: Col): List[Move] = throw new NotImplementedError()

  override def toString = throw new NotImplementedError()
}
