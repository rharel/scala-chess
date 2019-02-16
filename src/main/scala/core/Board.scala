package core

object Board{
  def fromString(source: String): Board = {
    val result = new Board()
    source.iterator
      .map(char => Piece.fromChar(char))
      .collect{case option: Option[Piece] if option.isDefined => option.get}
      .take(Grid.SquareCount)
      .zip(Grid.Squares.iterator)
      .foreach[Unit](deployment => {
        val (piece, square) = deployment
        result.put(square, piece)
      })
    result
  }
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
