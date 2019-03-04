package core

import scala.collection.immutable.HashMap
import scala.collection.mutable

object PositionContext {
  val Initial = PositionContext(
    playerToMove = Some(White),
    lastMove = None,
    castleRights = HashMap(
      ((Black, Kingside), true),
      ((Black, Queenside), true),
      ((White, Kingside), true),
      ((White, Queenside), true)
    )
  )
}
final case class PositionContext(
  playerToMove: Option[Player],
  lastMove: Option[Move],
  castleRights: Map[(Player, BoardSide), Boolean]) {

  def after(move: Move, grid: Grid[Option[Piece]]): Option[PositionContext] =
    playerToMove.map(player => {
      val newCastleRights: mutable.HashMap[(Player, BoardSide), Boolean] = new mutable.HashMap
      newCastleRights ++= castleRights
      move match {
        case RegularMove(origin, _)
        if grid(origin).contains(Piece(player, King)) =>
          newCastleRights((player, Kingside)) = false
          newCastleRights((player, Queenside)) = false

        case RegularMove(origin, _)
        if origin == player.kingsideRookSquare &&
           grid(origin).contains(Piece(player, Rook)) =>
          newCastleRights((player, Kingside)) = false

        case RegularMove(origin, _)
          if origin == player.queensideRookSquare &&
            grid(origin).contains(Piece(player, Rook)) =>
          newCastleRights((player, Queenside)) = false

        case _: Castle =>
          newCastleRights((player, Kingside)) = false
          newCastleRights((player, Queenside)) = false
      }
      new PositionContext(
        Some(player.opponent),
        Some(move),
        newCastleRights.toMap
      )
    }
  )
}
