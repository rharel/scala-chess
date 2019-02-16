package core

object PositionContext {
  val Initial = PositionContext(
    playerToMove = Some(White),
    lastMove = None,
    blackForbiddenToCastle = false,
    whiteForbiddenToCastle = false
  )
}
final case class PositionContext(
    playerToMove: Option[Player],
    lastMove: Option[Move],
    blackForbiddenToCastle: Boolean,
    whiteForbiddenToCastle: Boolean) {

  def mayCastle(player: Player): Boolean = player match {
    case Black => blackForbiddenToCastle
    case White => whiteForbiddenToCastle
  }
}
