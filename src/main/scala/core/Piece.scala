package core

object Piece {
  def fromChar(source: Char): Option[Piece] =
    (source.toLower match {
      case 'p' => Some(Pawn)
      case 'n' => Some(Knight)
      case 'b' => Some(Bishop)
      case 'r' => Some(Rook)
      case 'q' => Some(Queen)
      case 'k' => Some(King)
      case  _  => None
    }).map(kind => Piece(if (source.isLower) Black else White, kind))
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
