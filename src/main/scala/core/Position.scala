package core

import scala.collection.mutable.ListBuffer

final class Position(grid: Grid[Option[Piece]], context: PositionContext) {
  def apply(square: Square): Option[Piece] = grid(square)

  def isEmpty(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isEmpty(square)

  def isChecked(player: Player): Boolean = throw new NotImplementedError()
  def isMated(player: Player): Boolean = throw new NotImplementedError()

  def canKingsideCastle(player: Player): Boolean = throw new NotImplementedError()
  def canQueensideCastle(player: Player): Boolean = throw new NotImplementedError()

  def isLegal(move: Move): Boolean = context.playerToMove.isDefined && (move match {
    case _: RegularMove |
         _: Promotion => throw new NotImplementedError()
    case Castle(side) => canCastle(context.playerToMove.get, side)
  })
  def isIllegal(move: Move): Boolean = !isLegal(move)

  def findMovesFor(player: Player): Iterator[Move] = {
    if (!context.playerToMove.contains(player)) { return Iterator.empty }
    if (isChecked(player)) { return findCheckBreakingMovesFor(player) }

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
      }) ++ (
        if (context.forbiddenToCastle(player))
          Iterator.empty
        else
          Iterator[Move](KingsideCastle, QueensideCastle)
      )
  }
  def findPromotionsFor(player: Player): Iterator[Promotion] = {
    def isFriendlyPawn(row: Row, col: Col): Boolean = {
      this(Square(row, col)).contains(Piece(player, Pawn))
    }
    val promotions = ListBuffer.empty[Promotion]
    for (col <- Col.All) {
      val target = Square(player.promotionRow, col)
      if (isEmpty(target) && isFriendlyPawn(player.promotionEdgeRow, col)) {
        promotions ++ Promotion.allFor(col, col)
      }
      else if (isHostile(target, player)) {
        promotions ++ Iterator[Int](-1, 1)
          .filter(dCol => col +? dCol && isFriendlyPawn(player.promotionEdgeRow, col + dCol))
          .flatMap(dCol => Promotion.allFor(col + dCol, target.col))
      }
    }
    promotions.iterator
  }
  def findCheckBreakingMovesFor(player: Player): Iterator[RegularMove] =
    throw new NotImplementedError()

  def findPawnMovesFrom(origin: Square, player: Player): Iterator[RegularMove] ={
    if (origin.row == player.promotionEdgeRow) {
      List.empty[Move].iterator
    }
    val moves = ListBuffer.empty[RegularMove]
    val dRow = player.marchDirection
    val oneStep = (dRow, 0)
    if (origin +? oneStep && isEmpty(origin + oneStep)) {
      moves += RegularMove(origin, origin + oneStep)
    }
    if (origin.row == player.pawnRow) {
      val twoStep = (2 * dRow, 0)
      if (origin +? twoStep && isEmpty(origin + twoStep)) {
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
                 this(target).contains(Piece(player.opponent, Pawn)) &&
                 isEmpty(target)) {
          moves += RegularMove(origin, target)  // Capture en passant.
        }
      })
    moves.toIterator
  }
  def findKnightMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
      .filter(offset => origin +? offset && isHostileOrEmpty(origin + offset, player))
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
      .filter(offset => origin +? offset && isHostileOrEmpty(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }

  private def rayCast(
      player: Player,
      origin: Square,
      direction: (Int, Int)): Iterator[Square] = {

    val path = ListBuffer.empty[Square]
    var square = origin
    while (square +? direction && isEmpty(square + direction)) {
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
  private def isHostile(square: Square, player: Player): Boolean =
    isOccupied(square) && isHostileOrEmpty(square, player)
  private def isHostileOrEmpty(square: Square, player: Player): Boolean =
    !isFriendly(square, player)
}
