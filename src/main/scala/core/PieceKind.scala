package core

sealed trait PieceKind {
  val name: String
  override def toString: String = name
}
sealed trait PromotionTarget

case object Pawn extends PieceKind { val name = "pawn" }
case object Knight extends PieceKind with PromotionTarget { val name = "knight" }
case object Bishop extends PieceKind with PromotionTarget { val name = "bishop" }
case object Rook extends PieceKind with PromotionTarget { val name = "rook" }
case object Queen extends PieceKind with PromotionTarget { val name = "queen" }
case object King extends PieceKind { val name = "king" }
