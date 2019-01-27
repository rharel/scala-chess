package core

case class Square(row: Coordinate, col: Coordinate) extends Debuggable {
  def debug() = s"${('A' + row.index).toChar}${col.index}"
}
