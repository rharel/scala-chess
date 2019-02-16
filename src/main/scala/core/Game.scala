package core

import scala.collection.mutable.Stack

final class Game {
  def playerToMove: Option[Player] = _playerToMove

  def result: Option[GameResult] = _result
  def isOver: Boolean = result.isDefined
  def isOngoing: Boolean = !isOver

  def position: Position = new Position(_board.grid, _positionContext)

  def isLegal(action: PlayerAction): Boolean = throw new NotImplementedError()
  def submitAction(action: PlayerAction): Boolean = throw new NotImplementedError()
  def undoAction: Boolean = throw new NotImplementedError()

  override def toString = _history.map(action => action.toString).mkString(", ")

  private val _board: Board = Board.fromString(
    "RNBQKBNR" +
    "PPPPPPPP" +
    "........" +
    "........" +
    "........" +
    "........" +
    "pppppppp" +
    "rnbqkbnr"
  )
  private val _history: Stack[PlayerAction] = new Stack[PlayerAction]()

  private var _positionContext: PositionContext = new PositionContext()
  private var _playerToMove: Option[Player] = Some(White)
  private var _result: Option[GameResult] = None
}
