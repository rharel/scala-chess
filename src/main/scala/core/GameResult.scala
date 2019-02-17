package core

sealed trait GameResult

final case class Checkmate(winner: Player) extends GameResult {
  override def toString = s"$winner wins"
}
final case class Resignation(loser: Player) extends GameResult {
  override def toString = s"$loser resigns"
}
final case class Stalemate(playerToMove: Player) extends GameResult {
  override def toString = s"stalemate for $playerToMove"
}
final case class Draw(proposer: Player) extends GameResult {
  override def toString = s"draw proposed by $proposer"
}
case object ThreefoldRepetition extends GameResult {
  override def toString = "threefold repetition"
}
case object FiftyMoveRule extends GameResult {
  override def toString = "fifty move rule"
}
