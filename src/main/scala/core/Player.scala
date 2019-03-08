package core

import scala.collection.immutable.HashMap

sealed trait Player {
  val name: String
  val opponent: Player
  val marchDirection: Int
  val baseRow: Row
  lazy val pawnRow: Row = baseRow + marchDirection
  lazy val enPassantRow: Row = baseRow + 4 * marchDirection
  lazy val promotionEdgeRow: Row = opponent.pawnRow
  lazy val promotionRow: Row = opponent.baseRow
  lazy val kingSquare = Square(baseRow, Col.fromIndex(4).get)
  lazy val rookSquare: HashMap[BoardSide, Square] = HashMap(
    (Kingside, Square(baseRow, Col.First)),
    (Queenside, Square(baseRow, Col.Last))
  )
  lazy val baseSquares: HashMap[BoardSide, Iterable[Square]] = HashMap(
    (Kingside, Grid.rayCast(kingSquare, Left).toIterable),
    (Queenside, Grid.rayCast(kingSquare, Right).toIterable)
  )

  def play(move: Move, board: Board): Unit = move match {
    case Step(origin, target) =>
      val piece = board.take(origin).get
      board.put(target, piece)

    case promotion: Promotion =>
      board.take(promotion.originFor(this))
      board.put(promotion.targetFor(this), Piece(this, promotion.kind))

    case Castle(side) =>
      val king = board.take(kingSquare).get
      val rook = board.take(rookSquare(side)).get
      val toCenter = Direction.between(kingSquare, rookSquare(side)).get.offset
      board.put(rookSquare(side) + toCenter, king)
      board.put(rookSquare(side) + toCenter + toCenter, rook)
  }

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
