package core

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
