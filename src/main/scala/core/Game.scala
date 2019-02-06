package core

final class Game {
  def moves: List[Move] = throw new NotImplementedError()
  def playerToMove: Player = throw new NotImplementedError()

  def result: Option[GameResult] = throw new NotImplementedError()
  def isOver: Boolean = result.isDefined
  def isOngoing: Boolean = !isOver

  override def toString = throw new NotImplementedError()
}
