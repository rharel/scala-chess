package core

sealed trait Move extends Debuggable

case class PieceMove(source: Square, target: Square) extends Move {
  def debug = s"${source.debug()} => ${target.debug()}"
}
case class EnPassant(col: Int) extends Move {
  assert(0 <= col && col < 8)
  def debug = s"en passant at ${('a' + col).toChar}"
}
case class Promote(col: Int, kind: PieceKind) extends Move {
  assert(0 <= col && col < 8)
  def debug = s"promotion at ${('a' + col).toChar} to ${kind.debug()}"
}
case object KingsideCastle extends Move {
  def debug = "kingside castle"
}
case object QueensideCastle extends Move {
  def debug = "queenside castle"
}
