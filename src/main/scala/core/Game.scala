package core

import scala.collection.mutable.ListBuffer

final class Game {
  def position: Position = _position
  def activePlayer: Option[Player] =
    if (isOver) None
    else if (actionHistory.size % 2 == 0) Some(White)
    else Some(Black)

  def actionHistory: Iterable[PlayerAction] = _actionHistory
  def lastAction: Option[PlayerAction] = actionHistory.lastOption
  def lastToAct: Option[Player] = activePlayer.map(_.opponent)

  def moveHistory: Iterable[Move] = _moveHistory
  def lastMove: Option[Move] = moveHistory.lastOption
  def lastToMove: Option[Player] =
    if (moveHistory.isEmpty) None
    else if (moveHistory.size % 2 == 0) Some(White)
    else Some(Black)

  def result: Option[GameResult] = _result
  def isOver: Boolean = result.isDefined
  def inProgress: Boolean = !isOver
  def isDrawn: Boolean = isOver && winner.isEmpty && loser.isEmpty

  def loser: Option[Player] = winner.map(_.opponent)
  def winner: Option[Player] =
    if (result.contains(Checkmate)) lastToMove
    else if (result.contains(Resignation)) lastToAct
    else None

  def isLegal(action: PlayerAction): Boolean = inProgress && (action match {
    case Play(move) => position.isLegal(move)
    case OfferDraw | Resign => lastAction.contains(Play(_))
    case AcceptDraw => lastAction.contains(OfferDraw)
  })
  def isIllegal(action: PlayerAction): Boolean = !isLegal(action)

  def submitAction(action: PlayerAction): Boolean = {
    if (isIllegal(action)) { return false }

    _result = action match {
      case Play(move) => play(move)
      case OfferDraw => None
      case AcceptDraw => Some(Draw(DrawAgreement))
      case Resign => Some(Resignation)
    }
    _actionHistory += action

    true
  }
  def undoAction: Boolean = throw new NotImplementedError

  override def toString: String = _actionHistory.mkString(", ")

  private def play(move: Move): Option[GameResult] = {
    assert(move.canPlayOn(_board))

    _moveHistory += move
    _positionContext = position.context.after(move, position.grid).get
    _position = new Position(_board.grid, _positionContext)

    move.playOn(_board)

    val playerToMove = position.context.playerToMove.get
    if (position.isMated(playerToMove)) Some(Checkmate)
    else if (position.isStale(playerToMove)) Some(Draw(Stalemate))
    else None
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
  private var _position = new Position(_board.grid, _positionContext)
  private var _result: Option[GameResult] = None
}
