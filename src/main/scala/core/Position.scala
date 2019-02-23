package core

import scala.collection.mutable.ListBuffer

final class Position(grid: Grid[Option[Piece]], context: PositionContext) {
  def apply(square: Square): Option[Piece] = grid(square)

  def isEmpty(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isEmpty(square)

  def isFriendlyTo(square: Square, player: Player): Boolean =
    this(square) match {
      case Some(Piece(owner, _)) => player == owner
      case None => false
    }
  def isHostileTo(square: Square, player: Player): Boolean =
    isOccupied(square) && !isFriendlyTo(square, player)

  def isThreatenedBy(square: Square, threat: Player): Boolean = throw new NotImplementedError()
  def isSafeFrom(square: Square, threat: Player): Boolean = !isThreatenedBy(square, threat)

  def findPiece(query: Piece): Option[Square] =
    Grid.Squares.find(square => this(square).contains(query))

  def isChecked(player: Player): Boolean =
    findPiece(Piece(player, King))
      .map(square => isThreatenedBy(square, player.opponent))
      .isDefined

  def isMated(player: Player): Boolean =
    isChecked(player) && findCheckBreakingMovesFor(player).isEmpty

  def canCastle(player: Player, side: BoardSide): Boolean =
    !context.forbiddenToCastle(player) &&
    !isChecked(player) &&
    player.baseSquares(side).forall(square => isSafeFrom(square, player.opponent))

  def isLegal(move: Move): Boolean = context.playerToMove.isDefined && (move match {
    case _: RegularMove |
         _: Promotion => throw new NotImplementedError()
    case Castle(side) => canCastle(context.playerToMove.get, side)
  })
  def isIllegal(move: Move): Boolean = !isLegal(move)

  def potentialMovesFor(player: Player): Iterator[Move] = {
    if (!context.playerToMove.contains(player)) { return Iterator.empty }
    if (isChecked(player)) { return findCheckBreakingMovesFor(player) }

    Grid.Squares.iterator
      .flatMap(square => this(square) match {
        case Some(Piece(owner, kind)) if owner == player => kind match {
          case Pawn => potentialPawnMovesFrom(square, player)
          case Knight => potentialKnightMovesFrom(square, player)
          case Bishop => potentialBishopMovesFrom(square, player)
          case Rook => potentialRookMovesFrom(square, player)
          case Queen => potentialQueenMovesFrom(square, player)
          case King => potentialKingMovesFrom(square, player)
        }
        case None => Iterator.empty
      }) ++
      Iterator[Move](Castle(Kingside), Castle(Queenside)) ++
      potentialPromotionsFor(player)
  }
  def potentialPromotionsFor(player: Player): Iterator[Promotion] = {
    def isFriendlyPawn(row: Row, col: Col): Boolean = {
      this(Square(row, col)).contains(Piece(player, Pawn))
    }
    val promotions = ListBuffer.empty[Promotion]
    for (col <- Col.All; if isFriendlyPawn(player.promotionEdgeRow, col)) {
      // Check if promotion is possible using a normal step forward.
      val forwardStep = Square(player.promotionRow, col)
      if (isEmpty(forwardStep)) {
        promotions ++ Promotion.allFor(col, col)
      }
      // Check if promotion is possible using a capture.
      promotions ++ Iterator[Int](-1, 1)
        .filter(dCol => col +? dCol)
        .map(dCol => Square(player.promotionRow, col + dCol))
        .filter(square => isHostileTo(square, player))
        .flatMap(captureSquare => Promotion.allFor(col, captureSquare.col))
    }
    promotions.iterator
  }
  def findCheckBreakingMovesFor(player: Player): Iterator[RegularMove] =
    throw new NotImplementedError()

  def potentialPawnMovesFrom(origin: Square, player: Player): Iterator[RegularMove] ={
    if (origin.row == player.promotionEdgeRow) {
      return Iterator.empty  // Promotions are generated in a separate method.
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
        if (isHostileTo(target, player)) {
          moves += RegularMove(origin, target)  // Regular capture.
        }
        else if (context.lastMove.contains(RegularMove(_, target - (dRow, 0))) &&
                 this(target - (dRow, 0)).contains(Piece(player.opponent, Pawn)) &&
                 isEmpty(target)) {
          moves += RegularMove(origin, target)  // Capture en passant.
        }
      })
    moves.iterator
  }
  def potentialKnightMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
      .filter(offset => origin +? offset && !isFriendlyTo(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }
  def potentialBishopMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Direction.Diagonal.iterator
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => RegularMove(origin, target))
  }
  def potentialRookMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Direction.Cross.iterator
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => RegularMove(origin, target))
  }
  def potentialQueenMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    potentialBishopMovesFrom(origin, player) ++
    potentialRookMovesFrom(origin, player)
  }
  def potentialKingMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    Direction.All.iterator
      .map(direction => direction.offset)
      .filter(offset => origin +? offset && !isFriendlyTo(origin + offset, player))
      .map(offset => RegularMove(origin, origin + offset))
  }

  private def rayCast(
      player: Player,
      origin: Square,
      direction: Direction): Iterator[Square] = {

    val offset = direction.offset
    val path = ListBuffer.empty[Square]
    var square = origin
    while (square +? offset && isEmpty(square + offset)) {
      square += offset
      path += square
    }
    if (square +? offset && isHostileTo(square + offset, player)) {
      path += square + offset
    }
    path.iterator
  }
}
