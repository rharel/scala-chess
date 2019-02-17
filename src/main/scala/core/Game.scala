package core

import scala.collection.mutable.ListBuffer

final class Game {
  def activePlayer: Option[Player] = _activePlayer
  def history: Iterable[PlayerAction] = _history
  def lastAction: Option[PlayerAction] = _history.lastOption

  def result: Option[GameResult] = _result
  def isOver: Boolean = result.isDefined
  def inProgress: Boolean = !isOver

  def position: Position = new Position(_board.grid, _positionContext)

  def isLegalAction(action: PlayerAction): Boolean = inProgress && (action match {
    case Play(move) => position.isLegalMove(move)
    case OfferDraw => true
    case Resign => lastAction.contains(Play(_))
  })
  def submitAction(action: PlayerAction): Boolean = {
    if (!isLegalAction(action)) { return false }
    action match {
      case Play(_) => {
        throw new NotImplementedError()
      }
      case OfferDraw => {
        if (lastAction.contains(OfferDraw)) {
          val proposer = activePlayer.get.opponent
          endWith(DrawAgreement(proposer))
        }
      }
      case Resign => endWith(Resignation(activePlayer.get))
    }
    _history.append(action)
    _activePlayer = _activePlayer.map(player => player.opponent)
    true
  }
  def undoAction: Boolean = throw new NotImplementedError()

  override def toString: String = _history.map(action => action.toString).mkString(", ")

  private def endWith(result: GameResult): Unit = {
    _result = Some(result)
    _activePlayer = None
    _positionContext = PositionContext(
      playerToMove = None,
      _positionContext.lastMove,
      _positionContext.blackForbiddenToCastle,
      _positionContext.whiteForbiddenToCastle
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
  private val _history: ListBuffer[PlayerAction] = new ListBuffer[PlayerAction]

  private var _positionContext: PositionContext = PositionContext.Initial
  private var _activePlayer: Option[Player] = Some(White)
  private var _result: Option[GameResult] = None
}
