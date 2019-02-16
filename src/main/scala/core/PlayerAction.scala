package core

sealed trait PlayerAction
final case class Play(move: Move) extends PlayerAction {
  override def toString = move.toString
}
case object ProposeDraw extends PlayerAction {
  override def toString = "propose draw"
}
case object Resign extends PlayerAction {
  override def toString = "resign"
}
