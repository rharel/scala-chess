package core_units

import core._
import core.CoordinateConversion.{intToCol, intToRow}
import org.scalatest._

class BoardSpec extends FlatSpec with Matchers {
  "Board" should "be initially empty" in {
    val board = new Board
    board.isEmpty should be (true)
    board.grid.forall(_.isEmpty) should be (true)
    board.pieceCount should be (0)
    board.pieceSquares.isEmpty should be (true)
  }
  it should "be able to be constructed from a string" in {
    val board = Board.fromString(
      "pP nN bB" +
      "........" +
      "rR qQ kK"
    )
    board.pieceCount should be (12)
    board(Square(0, 0)).contains(Piece(Black, Pawn)) should be (true)
    board(Square(0, 1)).contains(Piece(White, Pawn)) should be (true)
    board(Square(0, 3)).contains(Piece(Black, Knight)) should be (true)
    board(Square(0, 4)).contains(Piece(White, Knight)) should be (true)
    board(Square(0, 6)).contains(Piece(Black, Bishop)) should be (true)
    board(Square(0, 7)).contains(Piece(White, Bishop)) should be (true)
    board(Square(2, 0)).contains(Piece(Black, Rook)) should be (true)
    board(Square(2, 1)).contains(Piece(White, Rook)) should be (true)
    board(Square(2, 3)).contains(Piece(Black, Queen)) should be (true)
    board(Square(2, 4)).contains(Piece(White, Queen)) should be (true)
    board(Square(2, 6)).contains(Piece(Black, King)) should be (true)
    board(Square(2, 7)).contains(Piece(White, King)) should be (true)
  }
}
