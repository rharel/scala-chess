package core

sealed trait PieceColor extends Debuggable {
  def name: String
  def debug(): String = name
}
case object Black extends PieceColor { val name = "black" }
case object White extends PieceColor { val name = "white" }
