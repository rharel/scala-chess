package core

object Board{
  def fromGrid(source: Grid[Option[Piece]]): Board =
    source.iterator
      .flatMap(deployment => deployment match {
        case (square, Some(piece)) => Some(square, piece)
        case (_, None) => None
      })
      .foldLeft(new Board)((board, deployment) => {
        val (square, piece) = deployment
        board.put(square, piece)
        board
      })

  def fromString(source: String): Board =
    source.iterator
      .flatMap(Piece.fromChar)
      .zip(Grid.Squares.iterator)
      .take(Grid.SquareCount)
      .foldLeft(new Board)((board, deployment) => {
        val (piece, square) = deployment
        board.put(square, piece)
        board
      })
}
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
    for (square <- Grid.Squares) {
      result += (this(square) match {
        case Some(piece) => piece.toChar
        case None => "."
      })
      if (square.col.isLast) { result += "\n" }
    }
    result
  }

  val grid = new ArrayGrid[Option[Piece]](None)
}
