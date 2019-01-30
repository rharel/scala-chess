package core

case class Piece(color: PieceColor, kind: PieceKind) extends Debuggable {
  def debug = s"${color.debug()} ${kind.debug()}"
}
