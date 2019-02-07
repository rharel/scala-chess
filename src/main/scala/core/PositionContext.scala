package core

final class PositionContext {
  val playerToMove: Player = White
  val lastMove: Option[Move] = None
  val blackCastled: Boolean = false
  val whiteCastled: Boolean = false

  def didCastle(player: Player): Boolean = player match {
    case Black => blackCastled
    case White => whiteCastled
  }
}
