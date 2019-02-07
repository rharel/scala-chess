package core

object Rules {
  def getPromotionRow(player: Player): Row = getPawnRow(Player.oppositeTo(player))
  def getPawnRow(player: Player): Row = player match {
    case Black => Row.fromIndex(Grid.Size - 2)
    case White => Row.fromIndex(1)
  }
  def getEnPassantRow(player: Player): Row = player match {
    case Black => Row.fromIndex(3)
    case White => Row.fromIndex(4)
  }
  def getPawnMarchDirection(player: Player): Int = player match {
    case Black => -1
    case White => +1
  }
}
