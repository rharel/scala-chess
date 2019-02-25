package core

import scala.collection.mutable.ListBuffer

final class Position(grid: Grid[Option[Piece]], context: PositionContext) {
  def apply(square: Square): Option[Piece] = grid(square)

  def isEmpty(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isEmpty(square)

  def isFriendlyTo(square: Square, player: Player): Boolean =
    this(square).exists(_.owner == player)
  def isHostileTo(square: Square, player: Player): Boolean =
    this(square).exists(_.owner == player.opponent)

  def isThreatenedBy(square: Square, player: Player): Boolean =
    throw new NotImplementedError()
  def isSafeFrom(square: Square, player: Player): Boolean =
    !isThreatenedBy(square, player)

  def isChecked(player: Player): Boolean =
    throw new NotImplementedError()
  def isMated(player: Player): Boolean =
    isChecked(player) && findCheckBreakersFor(player).isEmpty

  def canCastle(player: Player, side: BoardSide): Boolean =
    !context.forbiddenToCastle(player) &&
    !isChecked(player) &&
    player.baseSquares(side).forall(isSafeFrom(_, player.opponent))

  def isLegal(move: Move): Boolean = context.playerToMove.exists(player => {
    def isLegalFrom(origin: Square): Boolean = {
      isFriendlyTo(origin, player) &&
      potentialMovesFrom(origin).contains(move) &&
      !isSuicidal(move, player)
    }
    move match {
      case RegularMove(origin, _) => isLegalFrom(origin)
      case promotion: Promotion => isLegalFrom(promotion.originFor(player))
      case Castle(side) => canCastle(player, side)
    }
  })
  def isIllegal(move: Move): Boolean = !isLegal(move)

  def potentialMovesFor(player: Player): Iterator[Move] = {
    if (!context.playerToMove.contains(player)) { return Iterator.empty }
    if (isChecked(player)) { return findCheckBreakersFor(player) }

    Grid.Squares.iterator
      .filter(square => isFriendlyTo(square, player))
      .flatMap(square => potentialMovesFrom(square)) ++
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
        promotions ++= Promotion.allFor(col, col)
      }
      // Check if promotion is possible using a capture.
      promotions ++= Iterator[Int](-1, 1)
        .filter(dCol => col +? dCol)
        .map(dCol => Square(player.promotionRow, col + dCol))
        .filter(square => isHostileTo(square, player))
        .flatMap(captureSquare => Promotion.allFor(col, captureSquare.col))
    }
    promotions.iterator
  }

  def potentialMovesFrom(origin: Square): Iterator[RegularMove] =
    this(origin).map(piece =>
      piece.kind match {
        case Pawn => potentialPawnMovesFrom(origin, piece.owner)
        case Knight => potentialKnightMovesFrom(origin, piece.owner)
        case Bishop => potentialBishopMovesFrom(origin, piece.owner)
        case Rook => potentialRookMovesFrom(origin, piece.owner)
        case Queen => potentialQueenMovesFrom(origin, piece.owner)
        case King => potentialKingMovesFrom(origin, piece.owner)
      }
    ).getOrElse(Iterator.empty)

  def potentialPawnCapturesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
    val dRow = player.marchDirection
    Iterator[(Int, Int)]((dRow, -1), (dRow, +1))
      .filter(captureStep => origin +? captureStep)
      .map(captureStep => origin + captureStep)
      .filter(target => {
        isHostileTo(target, player) || {
          // Capture en passant.
          val opponentTwoStep = target - (dRow, 0)
          isEmpty(target) &&
          context.lastMove.contains(RegularMove(_, opponentTwoStep)) &&
          this(opponentTwoStep).contains(Piece(player.opponent, Pawn))
        }
      })
      .map(target => RegularMove(origin, target))
  }
  def potentialPawnMovesFrom(origin: Square, player: Player): Iterator[RegularMove] = {
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
    moves ++= potentialPawnCapturesFrom(origin, player)
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

  private def isSuicidal(move: Move, player: Player): Boolean = {
    val board = Board.fromGrid(grid)
    move.playOn(board)
    new Position(board.grid, context).isChecked(player)
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

  def findCheckBreakersFor(player: Player): Iterator[RegularMove] =
    throw new NotImplementedError()
}
