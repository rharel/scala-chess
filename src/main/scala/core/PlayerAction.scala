package core

sealed trait PlayerAction

final case class Play(move: Move) extends PlayerAction {
  override def toString = move.toString
}
case object OfferDraw extends PlayerAction {
  override def toString = "offer draw"
}
case object AcceptDraw extends PlayerAction {
  override def toString = "accept draw"
}
case object Resign extends PlayerAction {
  override def toString = "resign"
}
