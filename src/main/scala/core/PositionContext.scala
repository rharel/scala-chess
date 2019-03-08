package core

import scala.collection.immutable.HashMap
import scala.collection.mutable

object PositionContext {
  val Initial = PositionContext(
    currentMoveIndex = 0,
    lastMove = None,
    staleStreakSize = 0,
    castleRights = HashMap(
      ((Black, Kingside), true),
      ((Black, Queenside), true),
      ((White, Kingside), true),
      ((White, Queenside), true)
    )
  )
}
final case class PositionContext(
    currentMoveIndex: Int,
    lastMove: Option[Move],
    staleStreakSize: Int,
    castleRights: Map[(Player, BoardSide), Boolean]) {

  def playerToMove: Player =
    if (currentMoveIndex % 2 == 0) White
    else Black

  def after(move: Move, grid: Grid[Option[Piece]]): PositionContext =
    new PositionContext(
      currentMoveIndex + 1,
      Some(move),
      staleStreakSizeAfter(move, grid),
      castleRightsAfter(move, grid)
    )

  private def staleStreakSizeAfter(
      move: Move,
      grid: Grid[Option[Piece]]): Int = {

    val isPawnMove = move match {
      case RegularMove(origin, _) => grid(origin).exists(_.kind == Pawn)
      case _: Promotion => true
      case _ => false
    }
    val isCapture = (move match {
      case RegularMove(_, target) => Some(target)
      case promotion: Promotion => Some(promotion.targetFor(playerToMove))
      case _ => None
    }).exists(target => grid(target).nonEmpty)

    if (isPawnMove || isCapture) 0
    else staleStreakSize + 1
  }

  private def castleRightsAfter(
      move: Move,
      grid: Grid[Option[Piece]]): Map[(Player, BoardSide), Boolean] = {

    val player = playerToMove
    val newRights: mutable.HashMap[(Player, BoardSide), Boolean] = new mutable.HashMap
    newRights ++= castleRights
    move match {
      case RegularMove(origin, _)
        if grid(origin).contains(Piece(player, King)) =>
        newRights((player, Kingside)) = false
        newRights((player, Queenside)) = false

      case RegularMove(origin, _)
        if origin == player.rookSquare(Kingside) &&
           grid(origin).contains(Piece(player, Rook)) =>
        newRights((player, Kingside)) = false

      case RegularMove(origin, _)
        if origin == player.rookSquare(Queenside) &&
           grid(origin).contains(Piece(player, Rook)) =>
        newRights((player, Queenside)) = false

      case _: Castle =>
        newRights((player, Kingside)) = false
        newRights((player, Queenside)) = false

      case _ => Unit
    }
    newRights.toMap
  }
}
