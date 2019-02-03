package core

final class Board {
  def apply(square: Square): Option[Piece] = this(square.row, square.col)
  def apply(row: Row, col: Col): Option[Piece] = grid(row, col)

  def update(square: Square, value: Option[Piece]): Unit =
    this(square.row, square.col) = value
  def update(row: Row, col: Col, value: Option[Piece]): Unit =
    grid(row, col) = value

  def take(square: Square): Option[Piece] = take(square.row, square.col)
  def take(row: Row, col: Col): Option[Piece] = {
    val contents = this(row, col)
    this(row, col) = None
    contents
  }

  def put(square: Square, piece: Piece): Unit = put(square.row, square.col, piece)
  def put(row: Row, col: Col, piece: Piece): Unit = this(row, col) = Some(piece)

  def copy(source: Board): Unit = this.grid.copy(source.grid)

  override def clone: Board = {
    val result = new Board
    result.copy(this)
    result
  }

  override def toString: String = {
    var result = ""
    for (Square(row, col) <- Grid.squares) {
      result += (this(row, col) match {
        case Some(Piece(White, kind)) => kind.name(0).toUpper
        case Some(Piece(Black, kind)) => kind.name(0).toLower
        case None => "."
      })
      if (col.isLast) { result += "\n" }
    }
    result
  }

  val grid = new ArrayGrid[Option[Piece]](None)
}
