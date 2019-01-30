package core

sealed trait GameResult extends Debuggable

case class Checkmate(winner: PieceColor) extends GameResult {
  def debug = s"${winner.debug} wins"
}
case class Resign(loser: PieceColor) extends GameResult {
  def debug = s"${loser.debug} resigns"
}
case class Stalemate(playerToMove: PieceColor) extends GameResult {
  def debug = s"stalemate for ${playerToMove.debug}"
}
case class AgreedDraw(proposer: PieceColor) extends GameResult {
  def debug = s"draw proposed by ${proposer.debug}"
}
case object ThreefoldRepetition extends GameResult {
  def debug = "threefold repetition"
}
case object FiftyMoveRule extends GameResult {
  def debug = "fifty move rule"
}
