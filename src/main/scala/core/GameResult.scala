package core

sealed trait GameResult

case object Checkmate extends GameResult {
  override def toString = "checkmate"
}
case object Resignation extends GameResult {
  override def toString = "resignation"
}
final case class Draw(reason: DrawReason) extends GameResult {
  override def toString = s"draw by $reason"
}

sealed trait DrawReason

case object DrawAgreement extends DrawReason {
  override def toString = "player agreement"
}
case object Stalemate extends DrawReason {
  override def toString = "stalemate"
}
case object ThreefoldRepetition extends GameResult {
  override def toString = "threefold repetition"
}
case object FiftyMoveRule extends DrawReason {
  override def toString = "fifty move rule"
}
case object SeventyFiveMoveRule extends DrawReason {
  override def toString = "seventy five move rule"
}
