package core

import scala.collection.mutable.ListBuffer

final class Game {
  def activePlayer: Option[Player] = _activePlayer

  def actionHistory: Iterable[PlayerAction] = _actionHistory
  def lastAction: Option[PlayerAction] = _actionHistory.lastOption
  def lastToAct: Option[Player] =
    if (_actionHistory.isEmpty) None
    else if (_actionHistory.size % 2 == 0) Some(White)
    else Some(Black)

  def moveHistory: Iterable[Move] = _moveHistory
  def lastMove: Option[Move] = _moveHistory.lastOption
  def lastToMove: Option[Player] =
    if (_moveHistory.isEmpty) None
    else if (_moveHistory.size % 2 == 0) Some(White)
    else Some(Black)

  def result: Option[GameResult] = _result
  def isOver: Boolean = result.isDefined
  def inProgress: Boolean = !isOver
  def winner: Option[Player] =
    if (result.contains(Checkmate)) lastToMove
    else if (result.contains(Resignation)) lastToAct
    else None

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
    _actionHistory.append(action)
    _activePlayer = _activePlayer.map(_.opponent)
    true
  }
  def undoAction: Boolean = throw new NotImplementedError()

  override def toString: String = _actionHistory.mkString(", ")

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
  private val _actionHistory: ListBuffer[PlayerAction] = new ListBuffer
  private val _moveHistory: ListBuffer[Move] = new ListBuffer
  private var _positionContext: PositionContext = PositionContext.Initial
  private var _activePlayer: Option[Player] = Some(White)
  private var _result: Option[GameResult] = None
}
