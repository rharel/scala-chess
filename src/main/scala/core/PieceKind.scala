package core

sealed trait PieceKind {
  val name: String
  override def toString: String = name
}
sealed trait PromotionPiece extends PieceKind

case object Pawn extends PieceKind { val name = "pawn" }
case object Knight extends PromotionPiece { val name = "knight" }
case object Bishop extends PromotionPiece { val name = "bishop" }
case object Rook extends PromotionPiece { val name = "rook" }
case object Queen extends PromotionPiece { val name = "queen" }
case object King extends PieceKind { val name = "king" }
