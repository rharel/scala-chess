package core

object Grid {
  val Size = 8
  val SquareCount: Int = Size * Size
  val Squares: Iterable[Square] = {
    for {
      row <- Row.All
      col <- Col.All
    } yield Square(row, col)
  }
}
trait Grid[A] extends Iterable[(Square, A)] {
  def apply(square: Square): A

  override def iterator: Iterator[(Square, A)] =
    for { square <- Grid.Squares.iterator } yield (square, this(square))

  override def toString: String = {
    var result = ""
    for (square <- Grid.Squares) {
      result += s"${this(square)}"
      if (square.col.isLast) { result += "\n" }
    }
    result
  }
}
