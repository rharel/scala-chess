package core

object Square {
  def isValid(row: Row, col: Col): Boolean = row.isValid && col.isValid
}
final case class Square(row: Row, col: Col) {
  assert(isValid)

  def isValid: Boolean = Square.isValid(row, col)
  override def toString = s"$col$row"
}
