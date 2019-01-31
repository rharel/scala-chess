package core

case class Square(row: Int, col: Int) {
  assert(0 <= row && row < 8)
  assert(0 <= col && col < 8)
  
  override def toString = s"${('a' + col).toChar}$row"
}
