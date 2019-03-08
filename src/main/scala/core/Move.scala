package core

sealed trait Move

final case class Step(origin: Square, target: Square) extends Move {
  assert(origin != target)
  override def toString = s"$origin => $target"
}
object Promotion {
  def allFor(origin: Col, target: Col): Iterator[Promotion] =
    PromotionPieceKind.All.iterator
      .map(kind => Promotion(origin, target, kind))
}
final case class Promotion(
    originCol: Col,
    targetCol: Col,
    kind: PromotionPieceKind) extends Move {

  assert((originCol distanceTo targetCol) <= 1)

  def originFor(player: Player) = Square(player.promotionEdgeRow, originCol)
  def targetFor(player: Player) = Square(player.promotionRow, targetCol)

  override def toString = s"$originCol => $targetCol promoted to $kind"
}
final case class Castle(side: BoardSide) extends Move {
  override def toString = s"$side castle"
}
