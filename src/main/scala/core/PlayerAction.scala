package core

sealed trait PlayerAction
final case class Play(move: Move) extends PlayerAction
case object DrawProposal extends PlayerAction
case object Resignation extends PlayerAction
