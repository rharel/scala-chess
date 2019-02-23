package core

import scala.collection.immutable.HashMap

object PositionContext {
  val Initial = PositionContext(
    playerToMove = Some(White),
    lastMove = None,
    forbiddenToCastle = HashMap((Black, false), (White, false))
  )
}
final case class PositionContext(
    playerToMove: Option[Player],
    lastMove: Option[Move],
    forbiddenToCastle: HashMap[Player, Boolean]) {
}
