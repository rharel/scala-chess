package core

import scala.collection.mutable.ListBuffer

final class Position(grid: Grid[Option[Piece]], context: PositionContext) {
  def apply(square: Square): Option[Piece] = grid(square)

  def isFree(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isFree(square)

  def isChecked(player: Player): Boolean = throw new NotImplementedError()
  def isMated(player: Player): Boolean = throw new NotImplementedError()

  def canKingsideCastle(player: Player): Boolean = throw new NotImplementedError()
  def canQueensideCastle(player: Player): Boolean = throw new NotImplementedError()

  def findMovesFor(player: Player): Iterator[Move] = {
    if (!context.playerToMove.contains(player)) { List.empty[Move].iterator }

    (if (canKingsideCastle(player)) Iterator(KingsideCastle) else List.empty[Move].iterator) ++
    (if (canQueensideCastle(player)) Iterator(QueensideCastle) else List.empty[Move].iterator) ++
    findPromotionsFor(player) ++
    Grid.Squares
      .flatMap(square => this(square) match {
        case Some(Piece(owner, kind)) if owner == player => kind match {
          case Pawn => findPawnMovesFrom(square, player)
          case Knight => findKnightMovesFrom(square, player)
          case Bishop => findBishopMovesFrom(square, player)
          case Rook => findRookMovesFrom(square, player)
          case Queen => findQueenMovesFrom(square, player)
          case King => findKingMovesFrom(square, player)
        }
        case None => Iterator.empty
      })
  }
  def findPromotionsFor(player: Player): Iterator[Promotion] = {
    def isFriendlyPawn(row: Row, col: Col): Boolean = {
      this(Square(row, col)).contains(Piece(player, Pawn))
    }
    val promotions = ListBuffer.empty[Promotion]
    val edgeRow = Rules.getPromotionEdgeRow(player)
    val promotionRow = Rules.getPromotionRow(player)
    for (col <- Col.All) {
      val target = Square(promotionRow, col)
      if (isFree(target) && isFriendlyPawn(edgeRow, col)) {
        promotions ++ Promotion.allFor(col, col)
      }
      else if (isHostile(target, player)) {
        promotions ++ Iterator[Int](-1, 1)
          .filter(dCol => col +? dCol && isFriendlyPawn(edgeRow, col + dCol))
          .flatMap(dCol => Promotion.allFor(col + dCol, target.col))
      }
    }
    promotions.iterator
  }
  def findPawnMovesFrom(origin: Square, player: Player): Iterator[RegularMove] ={
    if (origin.row == Rules.getPromotionEdgeRow(player)) {
      List.empty[Move].iterator
    }
    val moves = ListBuffer.empty[RegularMove]
    val dRow = Rules.getPawnMarchDirection(player)
    val oneStep = (dRow, 0)
    if (origin +? oneStep && isFree(origin + oneStep)) {
      moves += RegularMove(origin, origin + oneStep)
    }
    if (origin.row == Rules.getPawnRow(player)) {
      val twoStep = (2 * dRow, 0)
      if (origin +? twoStep && isFree(origin + twoStep)) {
        moves += RegularMove(origin, origin + twoStep)
      }
    }
    Iterator[(Int, Int)]((dRow, -1), (dRow, +1))
      .filter(captureStep => origin +? captureStep)
      .foreach(captureStep => {
        val target = origin + captureStep
        if (isHostile(target, player)) {
          moves += RegularMove(origin, target)  // Regular capture.
        }
        else if (context.lastMove.contains(RegularMove(_, target - (dRow, 0))) &&
                 this(target).contains(Piece(Player.oppositeTo(player), Pawn)) &&
                 isFree(target)) {
          moves += RegularMove(origin, target)  // Capture en passant.
        }
      })
    moves.toIterator
  }
  def findKnightMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
      .filter(offset => origin +? offset && notFriendly(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }
  def findBishopMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Iterator[(Int, Int)]((-1, -1), (-1, +1), (+1, -1), (+1, +1))
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => RegularMove(origin, target))
  }
  def findRookMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Iterator[(Int, Int)]((0, -1), (0, +1), (-1, 0), (+1, 0))
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => RegularMove(origin, target))
  }
  def findQueenMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    findBishopMovesFrom(origin, player) ++
    findRookMovesFrom(origin, player)
  }
  def findKingMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Iterator[(Int, Int)](
      (-1, -1), (-1,  0), (-1, +1),
      ( 0, -1),           ( 0, +1),
      (+1, -1), (+1,  0), (+1, +1))
      .filter(offset => origin +? offset && notFriendly(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }

  private def rayCast(
      player: Player,
      origin: Square,
      direction: (Int, Int)): Iterator[Square] = {

    val path = ListBuffer.empty[Square]
    var square = origin
    while (square +? direction && isFree(square + direction)) {
      square += direction
      path += square
    }
    if (square +? direction && isHostile(square + direction, player)) {
      path += square + direction
    }
    path.toIterator
  }

  private def isFriendly(square: Square, player: Player): Boolean =
    this(square) match {
      case Some(Piece(owner, _)) => player == owner
      case None => false
    }
  private def notFriendly(square: Square, player: Player): Boolean =
    !isFriendly(square, player)
  private def isHostile(square: Square, player: Player): Boolean =
    isOccupied(square) && notFriendly(square, player)
  private def notHostile(square: Square, player: Player): Boolean =
    !isHostile(square, player)
}
