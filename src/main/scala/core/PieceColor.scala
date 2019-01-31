package core

sealed trait PieceColor {
  def name: String
  override def toString: String = name
}
case object Black extends PieceColor { val name = "black" }
case object White extends PieceColor { val name = "white" }
