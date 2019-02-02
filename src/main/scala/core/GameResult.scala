package core

sealed trait GameResult

final case class Checkmate(winner: PieceColor) extends GameResult {
  override def toString = s"$winner wins"
}
final case class Resign(loser: PieceColor) extends GameResult {
  override def toString = s"$loser resigns"
}
final case class Stalemate(playerToMove: PieceColor) extends GameResult {
  override def toString = s"stalemate for $playerToMove"
}
final case class DrawAgreement(proposer: PieceColor) extends GameResult {
  override def toString = s"draw proposed by $proposer"
}
case object ThreefoldRepetition extends GameResult {
  override def toString = "threefold repetition"
}
case object FiftyMoveRule extends GameResult {
  override def toString = "fifty move rule"
}
