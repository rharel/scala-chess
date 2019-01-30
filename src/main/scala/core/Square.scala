package core

case class Square(row: Int, col: Int) extends Debuggable {
  assert(0 <= row && row < 8)
  assert(0 <= col && col < 8)
  
  def debug = s"${('a' + col).toChar}$row"
}
