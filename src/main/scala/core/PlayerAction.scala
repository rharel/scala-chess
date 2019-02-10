package core

sealed trait PlayerAction
final case class Play(move: Move) extends PlayerAction
case object ProposeDraw extends PlayerAction
case object Resign extends PlayerAction
