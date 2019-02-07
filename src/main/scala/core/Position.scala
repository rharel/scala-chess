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

  def findMovesFor(player: Player): Iterator[Move] = throw new NotImplementedError()
  def findPromotionsFor(player: Player): Iterator[Promotion] = throw new NotImplementedError()

  def findPawnMovesFrom(origin: Square, player: Player): Iterator[RegularMove] ={
    val moves = ListBuffer.empty[RegularMove]
    val dRow = Rules.getPawnMarchDirection(player)
    val oneStep = (dRow, 0)
    if (origin.row != Rules.getPromotionRow(player) &&
        origin +? oneStep && isFree(origin + oneStep)) {
      moves += RegularMove(origin, origin + oneStep)
    }
    if (origin.row == Rules.getPawnRow(player)) {
      val twoStep = (2 * dRow, 0)
      if (origin +? twoStep && isFree(origin + twoStep)) {
        moves += RegularMove(origin, origin + twoStep)
      }
    }
    val captureSteps = Iterator[(Int, Int)]((dRow, -1), (dRow, +1))
    for (step <- captureSteps; if origin +? step) {
      val target = origin + step
      if (isHostile(target, player)) {
        moves += RegularMove(origin, target)  // Regular capture.
      }
      else if (context.lastMove.contains(RegularMove(_, target)) &&
               this(target).contains(Piece(Player.oppositeTo(player), Pawn)) &&
               isFree(target)) {
        moves += RegularMove(origin, target)  // Capture en passant.
      }
    }
    moves.toIterator
  }
  def findKnightMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    val offsets = Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
    offsets
      .filter(offset => origin +? offset &&
              notFriendly(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }
  def findBishopMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    val directions = Iterator[(Int, Int)]((-1, -1), (-1, +1), (+1, -1), (+1, +1))
    directions
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => RegularMove(origin, target))
  }
  def findRookMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    val directions = Iterator[(Int, Int)]((0, -1), (0, +1), (-1, 0), (+1, 0))
    directions
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => RegularMove(origin, target))
  }
  def findQueenMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    findBishopMovesFrom(origin, player) ++
    findRookMovesFrom(origin, player)
  }
  def findKingMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    val offsets = Iterator[(Int, Int)](
      (-1, -1), (-1,  0), (-1, +1),
      ( 0, -1),           ( 0, +1),
      (+1, -1), (+1,  0), (+1, +1))
    offsets
      .filter(offset => origin +? offset &&
              notFriendly(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }

  private def rayCast(
     player: Player,
     origin: Square,
     offset: (Int, Int)): Iterator[Square] = {
    val path = ListBuffer.empty[Square]
    var square = origin
    while (square +? offset && isFree(square + offset)) {
      square += offset
      path += square
    }
    if (square +? offset && isHostile(square + offset, player)) {
      path += square + offset
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
    this(square) match {
      case Some(Piece(owner, _)) => player != owner
      case None => false
    }
  private def notHostile(square: Square, player: Player): Boolean =
    !isHostile(square, player)
}
