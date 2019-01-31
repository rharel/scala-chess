package core

case class Piece(color: PieceColor, kind: PieceKind) {
  override def toString = s"$color $kind"
}
