package core

import scala.collection.mutable.ListBuffer

final class Game {
  def activePlayer: Option[Player] = _activePlayer
  def history: Iterable[PlayerAction] = _history
  def lastAction: Option[PlayerAction] = _history.lastOption

  def result: Option[GameResult] = _result
  def isOver: Boolean = result.isDefined
  def inProgress: Boolean = !isOver
  def winner: Option[Player] = throw new NotImplementedError()
  def loser: Option[Player] = winner.map(_.opponent)
  def isDrawn: Boolean = isOver && winner.isEmpty && loser.isEmpty

  def position: Position = new Position(_board.grid, _positionContext)

  def isLegal(action: PlayerAction): Boolean = inProgress && (action match {
    case Play(move) => position.isLegal(move)
    case OfferDraw | Resign => lastAction.contains(Play(_))
    case AcceptDraw => lastAction.contains(OfferDraw)
  })
  def isIllegal(action: PlayerAction): Boolean = !isLegal(action)

  def submitAction(action: PlayerAction): Boolean = {
    if (isIllegal(action)) { return false }
    action match {
      case Play(_) => throw new NotImplementedError()
      case OfferDraw =>
      case AcceptDraw => endWith(Draw(DrawAgreement))
      case Resign => endWith(Resignation)
    }
    _history.append(action)
    _activePlayer = _activePlayer.map(_.opponent)
    true
  }
  def undoAction: Boolean = throw new NotImplementedError()

  override def toString: String = _history.mkString(", ")

  private def endWith(result: GameResult): Unit = {
    assert(inProgress)
    _result = Some(result)
    _activePlayer = None
    _positionContext = PositionContext(
      playerToMove = None,
      _positionContext.lastMove,
      _positionContext.forbiddenToCastle
    )
  }

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
  private val _history: ListBuffer[PlayerAction] = new ListBuffer

  private var _positionContext: PositionContext = PositionContext.Initial
  private var _activePlayer: Option[Player] = Some(White)
  private var _result: Option[GameResult] = None
}
