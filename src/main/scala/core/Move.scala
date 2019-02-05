package core

sealed trait Move

final case class SimpleMove(origin: Square, target: Square) extends Move {
  override def toString = s"$origin => $target"
}
final case class EnPassant(col: Col) extends Move {
  override def toString = s"en passant at $col"
}
final case class Promotion(col: Col, kind: PieceKind) extends Move {
  override def toString = s"promotion at $col to $kind"
}
case object KingsideCastle extends Move {
  override def toString = "kingside castle"
}
case object QueensideCastle extends Move {
  override def toString = "queenside castle"
}
