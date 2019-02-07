package core

sealed trait Move

final case class RegularMove(origin: Square, target: Square) extends Move {
  override def toString = s"$origin => $target"
}
final case class Promotion(col: Col, target: PromotionTarget) extends Move {
  override def toString = s"promotion at $col to $target"
}
case object KingsideCastle extends Move {
  override def toString = "kingside castle"
}
case object QueensideCastle extends Move {
  override def toString = "queenside castle"
}
