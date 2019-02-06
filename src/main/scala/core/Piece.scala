package core

final case class Piece(owner: Player, kind: PieceKind) {
  override def toString = s"$owner $kind"
}
