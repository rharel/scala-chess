package core

final case class Square(row: Row, col: Col) {
  def +(offset: (Int, Int)): Square = Square(row + offset._1, col + offset._2)
  def -(offset: (Int, Int)): Square = this + (-offset._1, -offset._2)

  def +?(offset: (Int, Int)): Boolean = row +? offset._1 && col +? offset._2
  def -?(offset: (Int, Int)): Boolean = this +? (-offset._1, -offset._2)

  def ==(other: Square): Boolean = this.row == other.row && this.col == other.col
  def !=(other: Square): Boolean = !(this == other)

  override def toString = s"$col$row"
}
