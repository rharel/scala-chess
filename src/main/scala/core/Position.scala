package core

import scala.collection.mutable.ListBuffer

final class Position(grid: Grid[Option[Piece]]) {
  def apply(square: Square): Option[Piece] = grid(square)

  def isFree(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isFree(square)

  def findPawnMovesFrom(player: PieceColor, origin: Square): Iterator[SimpleMove] = {
    throw new NotImplementedError()
  }
  def findKnightMovesFrom(player: PieceColor, origin: Square): Iterator[SimpleMove] = {
    val offsets = Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
    offsets
      .filter(offset => origin +? offset &&
              notFriendly(origin + offset, player))
      .map(offset => SimpleMove(origin, origin + offset))
  }
  def findBishopMovesFrom(player: PieceColor, origin: Square): Iterator[SimpleMove] = {
    val directions = Iterator[(Int, Int)]((-1, -1), (-1, +1), (+1, -1), (+1, +1))
    directions
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => SimpleMove(origin, target))
  }
  def findRookMovesFrom(player: PieceColor, origin: Square): Iterator[SimpleMove] = {
    val directions = Iterator[(Int, Int)]((0, -1), (0, +1), (-1, 0), (+1, 0))
    directions
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => SimpleMove(origin, target))
  }
  def findQueenMovesFrom(player: PieceColor, origin: Square): Iterator[SimpleMove] = {
    findBishopMovesFrom(player, origin) ++
    findRookMovesFrom(player, origin)
  }
  def findKingMovesFrom(player: PieceColor, origin: Square): Iterator[SimpleMove] = {
    throw new NotImplementedError()
  }

  private def rayCast(
      player: PieceColor,
      origin: Square,
      offset: (Int, Int)): Iterator[Square] = {
    val path = ListBuffer.empty[Square]
    var square = origin
    while (square +? offset &&
           isFree(square + offset)) {
      square += offset
      path += square
    }
    if (square +? offset &&
        isHostile(square + offset, player)) {
      path += square + offset
    }
    path.toIterator
  }

  private def isFriendly(square: Square, player: PieceColor): Boolean =
    this(square) match {
      case Some(Piece(color, _)) => player == color
      case None => false
    }
  private def notFriendly(square: Square, player: PieceColor): Boolean =
    !isFriendly(square, player)

  private def isHostile(square: Square, player: PieceColor): Boolean =
    this(square) match {
      case Some(Piece(color, _)) => player != color
      case None => false
    }
  private def notHostile(square: Square, player: PieceColor): Boolean =
    !isHostile(square, player)
}
