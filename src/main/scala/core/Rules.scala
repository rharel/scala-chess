package core

object Rules {
  def getPawnMarchDirection(player: Player): Int = player match {
    case Black => -1
    case White => +1
  }
  def getBaseRow(player: Player): Row = player match {
    case Black => Row.Last
    case White => Row.First
  }
  def getPawnRow(player: Player): Row =
    getBaseRow(player) + getPawnMarchDirection(player)
  def getEnPassantRow(player: Player): Row =
    getBaseRow(player) + getPawnMarchDirection(player) * 4
  def getPromotionEdgeRow(player: Player): Row =
    getPawnRow(Player.oppositeTo(player))
  def getPromotionRow(player: Player): Row =
    getBaseRow(Player.oppositeTo(player))
}
