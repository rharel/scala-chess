package core

object Grid {
  val Size = 8
  val SquareCount: Int = Size * Size

  def squares: Iterator[Square] = {
    import CoordinateConversion.{intToRow, intToCol}
    for {
      row <- (0 until Grid.Size).iterator
      col <- (0 until Grid.Size).iterator
    } yield Square(row, col)
  }
}
trait Grid[A] extends Iterable[(Square, A)] {
  def apply(square: Square): A = this(square.row, square.col)
  def apply(row: Row, col: Col): A

  override def iterator: Iterator[(Square, A)] =
    for { square <- Grid.squares } yield (square, this(square))

  override def toString: String = {
    var result = ""
    for (Square(row, col) <- Grid.squares) {
      result += s"${this(row, col)}"
      if (col.isLast) { result += "\n" }
    }
    result
  }
}
