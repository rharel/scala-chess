package core

sealed trait Move

final case class RegularMove(origin: Square, target: Square) extends Move {
  override def toString = s"$origin => $target"
}
final case class Promotion(origin: Col, target: Col, piece: PromotionPiece) extends Move {
  override def toString = s"$origin => $target promoted to $piece"
}
case object KingsideCastle extends Move {
  override def toString = "kingside castle"
}
case object QueensideCastle extends Move {
  override def toString = "queenside castle"
}
