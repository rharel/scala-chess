package core

sealed trait Move

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
final case class Promotion(origin: Col, target: Col, piece: PromotionPiece) extends Move {
  assert(Math.abs(origin.index - target.index) <= 1)
  override def toString = s"$origin => $target promoted to $piece"
}
final case class Castle(side: BoardSide) extends Move {
  override def toString = s"$side castle"
}
