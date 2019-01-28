package core

sealed trait Move extends Debuggable

case class PieceMove(source: Square, target: Square) extends Move {
  def debug() = s"${source.debug()} => ${target.debug()}"
}
case class EnPassant(col: Int) extends Move {
  def debug() = s"en passant at $col"
}
case object KingsideCastle extends Move {
  def debug() = "kingside castle"
}
case object QueensideCastle extends Move {
  def debug() = "queenside castle"
}
