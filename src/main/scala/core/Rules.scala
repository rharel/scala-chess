package core

object Rules {
  def getPawnRow(player: PieceColor): Row = player match {
    case White => Row.fromIndex(1)
    case Black => Row.fromIndex(Grid.Size - 2)
  }
  def getPawnMarchDirection(player: PieceColor): Int = player match {
    case White => +1
    case Black => -1
  }
}
