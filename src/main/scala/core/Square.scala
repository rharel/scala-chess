package core

object Square {
  def fromString(source: String): Option[Square] =
    if (source.length != 2) None
    else {
      val row = Row.fromChar(source(1))
      val col = Col.fromChar(source(0))

      if (row.isDefined && col.isDefined)
        Some(Square(row.get, col.get))
      else
        None
    }
}
final case class Square(row: Row, col: Col) {
  def +(offset: (Int, Int)): Square = Square(row + offset._1, col + offset._2)
  def -(offset: (Int, Int)): Square = this + (-offset._1, -offset._2)

  def +?(offset: (Int, Int)): Boolean = row +? offset._1 && col +? offset._2
  def -?(offset: (Int, Int)): Boolean = this +? (-offset._1, -offset._2)

  def offsetFrom(other: Square): (Int, Int) = (
    this.row offsetFrom other.row,
    this.col offsetFrom other.col
  )

  override def toString = s"$col$row"
}
