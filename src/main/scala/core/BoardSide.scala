package core

sealed trait BoardSide {
  val name: String
  override def toString: String = s"$name side"
}
case object Kingside extends BoardSide { val name = "king" }
case object Queenside extends BoardSide { val name = "queen" }
