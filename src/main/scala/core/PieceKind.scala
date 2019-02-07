package core

sealed trait PieceKind {
  val name: String
  override def toString: String = name
}
sealed trait PromotionTarget extends PieceKind

case object Pawn extends PieceKind { val name = "pawn" }
case object Knight extends PromotionTarget { val name = "knight" }
case object Bishop extends PromotionTarget { val name = "bishop" }
case object Rook extends PromotionTarget { val name = "rook" }
case object Queen extends PromotionTarget { val name = "queen" }
case object King extends PieceKind { val name = "king" }
