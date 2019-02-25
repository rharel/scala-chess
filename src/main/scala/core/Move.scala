package core

sealed trait Move {
  def canPlayOn(board: Board): Boolean = throw new NotImplementedError()
  def playOn(board: Board): Boolean = throw new NotImplementedError()
}

final case class RegularMove(origin: Square, target: Square) extends Move {
  assert(origin != target)
  override def toString = s"$origin => $target"
}
object Promotion {
  def allFor(origin: Col, target: Col): Iterator[Promotion] = {
    Iterator[PromotionPiece](Knight, Bishop, Rook, Queen)
      .map(piece => Promotion(origin, target, piece))
  }
}
final case class Promotion(originCol: Col, targetCol: Col, piece: PromotionPiece) extends Move {
  assert(Math.abs(originCol.index - targetCol.index) <= 1)

  def originFor(player: Player) = Square(player.promotionEdgeRow, originCol)
  def targetFor(player: Player) = Square(player.promotionRow, targetCol)

  override def toString = s"$originCol => $targetCol promoted to $piece"
}
final case class Castle(side: BoardSide) extends Move {
  override def toString = s"$side castle"
}
