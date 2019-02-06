package core

import scala.collection.mutable.ListBuffer

final class Position(grid: Grid[Option[Piece]]) {
  def apply(square: Square): Option[Piece] = grid(square)

  def isFree(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isFree(square)

  def findPawnMovesFrom(player: Player, origin: Square): Iterator[SimpleMove] ={
    val targets = ListBuffer.empty[Square]
    val dRow = Rules.getPawnMarchDirection(player)
    val oneStep = (1 * dRow, 0)
    if (origin +? oneStep && isFree(origin + oneStep)) {
      targets += origin + oneStep
    }
    if (origin.row == Rules.getPawnRow(player)) {
      val twoStep = (2 * dRow, 0)
      if (origin +? twoStep && isFree(origin + twoStep)) {
        targets += origin + twoStep
      }
    }
    val captureSteps = Iterator[(Int, Int)]((dRow, -1), (dRow, +1))
    for (step <- captureSteps) {
      if (origin +? step && isHostile(origin + step, player)) {
        targets += origin + step
      }
    }
    targets.toIterator.map(target => SimpleMove(origin, target))
  }
  def findKnightMovesFrom(player: Player, origin: Square): Iterator[SimpleMove] = {
    val offsets = Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
    offsets
      .filter(offset => origin +? offset &&
              notFriendly(origin + offset, player))
      .map(offset => SimpleMove(origin, origin + offset))
  }
  def findBishopMovesFrom(player: Player, origin: Square): Iterator[SimpleMove] = {
    val directions = Iterator[(Int, Int)]((-1, -1), (-1, +1), (+1, -1), (+1, +1))
    directions
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => SimpleMove(origin, target))
  }
  def findRookMovesFrom(player: Player, origin: Square): Iterator[SimpleMove] = {
    val directions = Iterator[(Int, Int)]((0, -1), (0, +1), (-1, 0), (+1, 0))
    directions
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => SimpleMove(origin, target))
  }
  def findQueenMovesFrom(player: Player, origin: Square): Iterator[SimpleMove] = {
    findBishopMovesFrom(player, origin) ++
    findRookMovesFrom(player, origin)
  }
  def findKingMovesFrom(player: Player, origin: Square): Iterator[SimpleMove] = {
    throw new NotImplementedError()
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
