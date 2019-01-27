package core

sealed trait Move extends Debuggable
case class SimpleMove(source: Square, target: Square) extends Move {
  def debug() = s"${source.debug()} => ${target.debug()}"
}
case object CastleKingside extends Move {
  def debug() = "castle kingside"
}
case object CastleQueenside extends Move {
  def debug() = "castle queenside"
}
