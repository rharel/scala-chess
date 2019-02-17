package core

sealed trait Player {
  val name: String
  val opponent: Player
  override def toString: String = name
}
case object Black extends Player { val name = "black"; val opponent: Player = White }
case object White extends Player { val name = "white"; val opponent: Player = Black }
