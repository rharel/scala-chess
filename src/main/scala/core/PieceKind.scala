package core

sealed trait PieceKind {
  def name: String
  override def toString: String = name
}
case object Pawn extends PieceKind { val name = "pawn" }
case object Knight extends PieceKind { val name = "knight" }
case object Bishop extends PieceKind { val name = "bishop" }
case object Rook extends PieceKind { val name = "rook" }
case object Queen extends PieceKind { val name = "queen" }
case object King extends PieceKind { val name = "king" }
