package core

final case class Square(row: Row, col: Col) {
  override def toString = s"$col$row"
}
