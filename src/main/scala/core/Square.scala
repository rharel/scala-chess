package core

final case class Square(row: Row, col: Col) {
  def +(offset: (Int, Int)): Square = Square(row + offset._1, col + offset._2)
  def -(offset: (Int, Int)): Square = this + (-offset._1, -offset._2)

  def +?(offset: (Int, Int)): Boolean = row +? offset._1 && col +? offset._2
  def -?(offset: (Int, Int)): Boolean = this +? (-offset._1, -offset._2)

  def offset(other: Square): (Int, Int) = (
    this.row offset other.row,
    this.col offset other.col
  )

  override def toString = s"$col$row"
}
