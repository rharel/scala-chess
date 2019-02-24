package core

sealed trait BoardSide {
  val name: String
  override def toString: String = name
}
case object Kingside extends BoardSide { val name = "kingside" }
case object Queenside extends BoardSide { val name = "queenside" }
