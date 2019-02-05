package core

final class Board {
  def apply(square: Square): Option[Piece] = grid(square)
  def update(square: Square, value: Option[Piece]): Unit = grid(square) = value

  def take(square: Square): Option[Piece] = {
    val contents = this(square)
    this(square) = None
    contents
  }
  def put(square: Square, piece: Piece): Unit = this(square) = Some(piece)

  def copy(source: Board): Unit = this.grid.copy(source.grid)

  override def clone: Board = {
    val result = new Board
    result.copy(this)
    result
  }

  override def toString: String = {
    var result = ""
    for (square <- Grid.squares) {
      result += (this(square) match {
        case Some(Piece(White, kind)) => kind.name(0).toUpper
        case Some(Piece(Black, kind)) => kind.name(0).toLower
        case None => "."
      })
      if (square.col.isLast) { result += "\n" }
    }
    result
  }

  val grid = new ArrayGrid[Option[Piece]](None)
}
