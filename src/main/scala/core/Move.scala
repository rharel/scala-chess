package core

sealed trait Move

final case class SimpleMove(source: Square, target: Square) extends Move {
  override def toString = s"$source => $target"
}
final case class EnPassant(col: Int) extends Move {
  assert(0 <= col && col < 8)
  override def toString = s"en passant at ${('a' + col).toChar}"
}
final case class Promotion(col: Int, kind: PieceKind) extends Move {
  assert(0 <= col && col < 8)
  override def toString = s"promotion at ${('a' + col).toChar} to $kind"
}
case object KingsideCastle extends Move {
  override def toString = "kingside castle"
}
case object QueensideCastle extends Move {
  override def toString = "queenside castle"
}
