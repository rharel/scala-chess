package core

sealed trait PieceColor {
  def name: String
  def goesFirst(): Boolean
  def goesLast(): Boolean = !goesFirst()
}
case object Black extends PieceColor {
  val name = "black"
  def goesFirst() = false
}
case object White extends PieceColor {
  val name = "white"
  def goesFirst() = true
}
