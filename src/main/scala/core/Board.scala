package core

object Board{
  def fromGrid(source: Grid[Option[Piece]]): Board =
    source.iterator
      .zip(Grid.Squares.iterator)
      .flatMap(deployment => deployment match {
        case (Some(piece), square) => Some(piece, square)
        case (None, _) => None
      })
      .foldLeft(new Board)((board, deployment) => {
        val (piece, square) = deployment
        board.put(square, piece)
        board
      })

  def fromString(source: String): Board =
    source.iterator
      .map(Piece.fromChar)
      .zip(Grid.Squares.iterator)
      .take(Grid.SquareCount)
      .foldLeft(new Board)((board, deployment) => {
        val (value, square) = deployment
        board(square) = value
        board
      })
}
final class Board {
  def grid: Grid[Option[Piece]] = _grid

  def apply(square: Square): Option[Piece] = grid(square)
  def update(square: Square, value: Option[Piece]): Unit = _grid(square) = value

  def isEmpty: Boolean = pieceSquares.isEmpty
  def pieceCount: Int = pieceSquares.size
  def pieceSquares: Iterator[Square] =
    Grid.Squares.iterator.filter(square => this(square).isDefined)

  def take(square: Square): Option[Piece] = {
    val contents = this(square)
    this(square) = None
    contents
  }
  def put(square: Square, piece: Piece): Unit = this(square) = Some(piece)

  def copy(source: Board): Unit = this._grid.copy(source._grid)

  override def clone: Board = {
    val result = new Board
    result.copy(this)
    result
  }

  override def toString: String = {
    var result = ""
    for (square <- Grid.Squares) {
      result += (grid(square) match {
        case Some(piece) => piece.toChar
        case None => "."
      })
      if (square.col.isLast) { result += "\n" }
    }
    result
  }

  val _grid = new ArrayGrid[Option[Piece]](None)
}
