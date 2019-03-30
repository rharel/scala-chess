package core

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.mutable.ListBuffer

final class Position(
    val grid: Grid[Option[Piece]],
    val context: PositionContext) {

  def apply(square: Square): Option[Piece] = grid(square)

  def isEmpty(square: Square): Boolean = this(square).isEmpty
  def isOccupied(square: Square): Boolean = !isEmpty(square)

  def isFriendlyTo(square: Square, player: Player): Boolean =
    this(square).exists(_.owner == player)
  def isHostileTo(square: Square, player: Player): Boolean =
    this(square).exists(_.owner == player.opponent)

  def isThreatenedBy(square: Square, player: Player): Boolean =
    _threatsBy(player)(square).nonEmpty
  def isSafeFrom(square: Square, player: Player): Boolean =
    !isThreatenedBy(square, player)

  def isChecked(player: Player): Boolean =
    _kingSquare(player).exists(isThreatenedBy(_, player.opponent))
  def isMated(player: Player): Boolean =
    isChecked(player) && findCheckBreakersFor(player).isEmpty
  def isStale(player: Player): Boolean =
    potentialMovesFor(player).isEmpty

  def canCastle(player: Player, side: BoardSide): Boolean =
    context.castleRights((player, side)) &&
    !isChecked(player) &&
    player.baseSquares(side).forall(isSafeFrom(_, player.opponent))

  def isLegal(move: Move): Boolean = {
    val player = context.playerToMove

    def isLegalFrom(origin: Square): Boolean = {
      isFriendlyTo(origin, player) &&
      potentialMovesFrom(origin).contains(move) &&
      !isSuicidal(move, player)
    }
    move match {
      case Step(origin, _) => isLegalFrom(origin)
      case promotion: Promotion => isLegalFrom(promotion.originFor(player))
      case Castle(side) => canCastle(player, side)
    }
  }
  def isIllegal(move: Move): Boolean = !isLegal(move)

  def potentialMovesFor(player: Player): Iterator[Move] = {
    if (context.playerToMove != player) { return Iterator.empty }
    if (isChecked(player)) { return findCheckBreakersFor(player) }

    Grid.Squares.iterator
      .filter(square => isFriendlyTo(square, player))
      .flatMap(square => potentialMovesFrom(square)) ++
      Iterator[Move](Castle(Kingside), Castle(Queenside)) ++
      potentialPromotionsFor(player)
  }

  def potentialPromotionsFor(player: Player): Iterator[Promotion] = {
    def isFriendlyPawn(row: Row, col: Col) =
      this(Square(row, col)).contains(Piece(player, Pawn))

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

  def potentialMovesFrom(origin: Square): Iterator[Step] =
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

  def potentialPawnCapturesFrom(origin: Square, player: Player): Iterator[Step] = {
    val dRow = player.marchDirection
    Iterator[(Int, Int)]((dRow, -1), (dRow, +1))
      .filter(captureStep => origin +? captureStep)
      .map(captureStep => origin + captureStep)
      .filter(target => {
        isHostileTo(target, player) || {
          // Capture en passant.
          val opponent = player.opponent
          val epOrigin = Square(opponent.pawnRow, target.col)
          val epTarget = Square(opponent.pawnRow - 2 * dRow, target.col)
          isEmpty(target) &&
          context.lastMove.contains(Step(epOrigin, epTarget)) &&
          this(epTarget).contains(Piece(opponent, Pawn))
        }
      })
      .map(target => Step(origin, target))
  }
  def potentialPawnMovesFrom(origin: Square, player: Player): Iterator[Step] = {
    if (origin.row == player.promotionEdgeRow) {
      return Iterator.empty  // Promotions are generated in a separate method.
    }
    val moves = ListBuffer.empty[Step]
    val dRow = player.marchDirection
    val oneStep = (dRow, 0)
    if (origin +? oneStep && isEmpty(origin + oneStep)) {
      moves += Step(origin, origin + oneStep)
      if (origin.row == player.pawnRow) {
        val twoStep = (2 * dRow, 0)
        if (origin +? twoStep && isEmpty(origin + twoStep)) {
          moves += Step(origin, origin + twoStep)
        }
      }
    }
    moves ++= potentialPawnCapturesFrom(origin, player)
    moves.iterator
  }

  def potentialKnightMovesFrom(origin: Square, player: Player): Iterator[Step] = {
    Iterator[(Int, Int)](
      (-1, -2), (-1, +2), (+1, -2), (+1, +2),
      (-2, -1), (-2, +1), (+2, -1), (+2, +1))
      .filter(offset => origin +? offset && !isFriendlyTo(origin + offset, player))
      .map(offset => Step(origin, origin + offset))
  }

  def potentialBishopMovesFrom(origin: Square, player: Player): Iterator[Step] = {
    Direction.Diagonal.iterator
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => Step(origin, target))
  }

  def potentialRookMovesFrom(origin: Square, player: Player): Iterator[Step] = {
    Direction.Cross.iterator
      .flatMap(direction => rayCast(player, origin, direction))
      .map(target => Step(origin, target))
  }

  def potentialQueenMovesFrom(origin: Square, player: Player): Iterator[Step] = {
    potentialBishopMovesFrom(origin, player) ++
    potentialRookMovesFrom(origin, player)
  }

  def potentialKingMovesFrom(origin: Square, player: Player): Iterator[Step] = {
    Direction.All.iterator
      .map(direction => direction.offset)
      .filter(offset => origin +? offset && !isFriendlyTo(origin + offset, player))
      .map(offset => Step(origin, origin + offset))
  }

  def findCheckBreakersFor(player: Player): Iterator[Move] = {
    if (!isChecked(player)) { return Iterator.empty }

    var breakers = new HashSet[Move]
    val king = _kingSquare(player).get

    // Collect moves the king can make to save itself.
    breakers ++= potentialMovesFrom(king)
      .filter(move => isSafeFrom(move.target, player.opponent))

    // If there is more than one threat, there's nothing else to do.
    if (_threatsBy(player.opponent)(king).size > 1) { return breakers.iterator }

    // Otherwise, we have two options...
    val assassin = _threatsBy(player.opponent)(king).head

    // ...either we capture it...
    val guards = _threatsBy(player)(assassin)
    breakers ++= guards.map(guard => Step(guard, assassin))

    // ...or move a friendly piece to block its line of fire.
    this(assassin).get.kind match {
      case Bishop | Rook | Queen =>
        val lineOfFire = Grid.rayCast(
          assassin, Direction.between(assassin, king).get
        )
        breakers ++= lineOfFire.flatMap(square => {
          val blockers = _threatsBy(player)(square)
          blockers.map(Step(_, square))
        })
      case _ =>
    }
    breakers.iterator
  }

  private def isSuicidal(move: Move, player: Player): Boolean = {
    val board = Board.fromGrid(grid)
    player.play(move, board)
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

  private val _kingSquare: Map[Player, Option[Square]] = Map(
    Black -> Grid.Squares.find(this(_).contains(Piece(Black, King))),
    White -> Grid.Squares.find(this(_).contains(Piece(White, King)))
  )
  private val _threatsBy: Map[Player, ArrayGrid[ListBuffer[Square]]] = Map(
    Black -> ArrayGrid.populate(_ => new ListBuffer[Square]),
    White -> ArrayGrid.populate(_ => new ListBuffer[Square])
  )
  Grid.Squares.iterator
    .filter(square => isOccupied(square))
    .flatMap(square => this(square).get.kind match {
      case Pawn => potentialPawnCapturesFrom(square, this(square).get.owner)
      case _ => potentialMovesFrom(square)
    })
    .foreach(move => {
      val player = this(move.origin).get.owner
      _threatsBy(player)(move.target) += move.origin
    })
}
