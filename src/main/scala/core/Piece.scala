package core

object Piece {
  def fromChar(source: Char): Option[Piece] =
    PieceKind.fromChar(source)
      .map(kind => Piece(if (source.isLower) Black else White, kind))
}
final case class Piece(owner: Player, kind: PieceKind) {
  def toChar: Char = {
    val result = kind match {
      case Pawn => 'p'
      case Knight => 'n'
      case Bishop => 'b'
      case Rook => 'r'
      case Queen => 'q'
      case King => 'k'
    }
    if (owner == Black) result.toLower else result.toUpper
  }
  override def toString = s"$owner $kind"
}
