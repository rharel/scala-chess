package core

object Grid {
  val Size = 8
  val SquareCount: Int = Size * Size
  val Squares: Iterable[Square] =
    for (row <- Row.All; col <- Col.All) yield Square(row, col)

  def rayCast(origin: Square, direction: Direction): Iterator[Square] = new Iterator[Square]{
    def hasNext: Boolean = _current +? direction.offset
    def next: Square = {
      _current += direction.offset
      _current
    }
    private var _current: Square = origin
  }
}
trait Grid[A] extends Iterable[A] {
  def apply(square: Square): A

  override def iterator: Iterator[A] =
    Grid.Squares.iterator.map(square => this(square))

  override def toString: String =
    Grid.Squares.iterator
      .map(square => s"$square: ${this(square)}")
      .mkString(", ")
}
