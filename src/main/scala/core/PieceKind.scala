package core

object PieceKind {
  def fromChar(source: Char): Option[PieceKind] = source.toLower match {
    case 'p' => Some(Pawn)
    case 'n' => Some(Knight)
    case 'b' => Some(Bishop)
    case 'r' => Some(Rook)
    case 'q' => Some(Queen)
    case 'k' => Some(King)
    case  _  => None
  }
}
sealed trait PieceKind {
  val name: String
  override def toString: String = name
}

object PromotionPieceKind {
  val All: Iterable[PromotionPieceKind] =
    List(Knight, Bishop, Rook, Queen)
}
sealed trait PromotionPieceKind extends PieceKind

case object Pawn extends PieceKind { val name = "pawn" }
case object Knight extends PromotionPieceKind { val name = "knight" }
case object Bishop extends PromotionPieceKind { val name = "bishop" }
case object Rook extends PromotionPieceKind { val name = "rook" }
case object Queen extends PromotionPieceKind { val name = "queen" }
case object King extends PieceKind { val name = "king" }
