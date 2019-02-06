package core

sealed trait Player {
  val name: String
  override def toString: String = name
}
case object Black extends Player { val name = "black" }
case object White extends Player { val name = "white" }
