package core

import scala.collection.immutable.HashMap

sealed trait Player {
  val name: String
  val opponent: Player
  val marchDirection: Int
  val baseRow: Row
  val pawnRow: Row = baseRow + marchDirection
  val enPassantRow: Row = baseRow + 4 * marchDirection
  val promotionEdgeRow: Row = opponent.pawnRow
  val promotionRow: Row = opponent.baseRow
  val kingSquare = Square(baseRow, Col.fromIndex(4))
  val baseSquares: HashMap[BoardSide, Iterable[Square]] = HashMap(
    (Kingside, Grid.rayCast(kingSquare, Left).toIterable),
    (Queenside, Grid.rayCast(kingSquare, Right).toIterable)
  )
  override def toString: String = name
}
case object Black extends Player {
  val name = "black"
  val opponent: Player = White
  val marchDirection: Int = -1
  val baseRow: Row = Row.Last
}
case object White extends Player {
  val name = "white"
  val opponent: Player = Black
  val marchDirection: Int = +1
  val baseRow: Row = Row.First
}
