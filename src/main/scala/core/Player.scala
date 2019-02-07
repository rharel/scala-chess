package core

object Player {
  def oppositeTo(player: Player): Player = player match {
    case Black => White
    case White => Black
  }
}
sealed trait Player {
  val name: String
  override def toString: String = name
}
case object Black extends Player { val name = "black" }
case object White extends Player { val name = "white" }
