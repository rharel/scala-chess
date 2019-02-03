package core

object Board {
  val Size = 8
  val SquareCount: Int = Size * Size

  def squares: Iterator[Square] = {
    import CoordinateConversion.{intToRow, intToCol}
    for {
      row <- (0 until Board.Size).iterator
      col <- (0 until Board.Size).iterator
    } yield Square(row, col)
  }

  def toGridIndex(row: Row, col: Col): Int =
    Board.Size * row.index + col.index
}
final class Board {
  def at(row: Row, col: Col): Option[Piece] =
    grid(Board.toGridIndex(row, col))

  def take(row: Row, col: Col): Option[Piece] =
    take(Board.toGridIndex(row, col))

  def put(row: Row, col: Col, piece: Piece): Option[Piece] =
    put(Board.toGridIndex(row, col), piece)

  def isFree(row: Row, col: Col): Boolean = at(row, col).isEmpty
  def isOccupied(row: Row, col: Col): Boolean = !isFree(row, col)

  def copy(source: Board): Unit =
    source.grid.copyToArray(this.grid)

  override def clone: Board = {
    val result = new Board
    result.copy(this)
    result
  }
  override def toString: String = {
    var result = ""
    for (Square(row, col) <- Board.squares) {
      result += (at(row, col) match {
        case Some(Piece(White, kind)) => kind.name(0).toUpper
        case Some(Piece(Black, kind)) => kind.name(0).toLower
        case None => "."
      })
      if (col.isLast) { result += "\n" }
    }
    result
  }

  private def take(index: Int): Option[Piece] = {
    val contents = grid(index)
    grid(index) = None
    contents
  }
  private def put(index: Int, piece: Piece): Option[Piece] = {
    val previousContents = grid(index)
    grid(index) = Some(piece)
    previousContents
  }

  private val grid =
    Array.fill[Option[Piece]](Board.SquareCount)(None)
}
