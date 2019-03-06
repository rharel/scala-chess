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
      "rR qQ kK" +
      "........" +
      "........" +
      "........" +
      "........" +
      "P......p"
    )
    board.pieceCount should be (14)
    board(Square(7, 0)).contains(Piece(Black, Pawn)) should be (true)
    board(Square(7, 1)).contains(Piece(White, Pawn)) should be (true)
    board(Square(7, 3)).contains(Piece(Black, Knight)) should be (true)
    board(Square(7, 4)).contains(Piece(White, Knight)) should be (true)
    board(Square(7, 6)).contains(Piece(Black, Bishop)) should be (true)
    board(Square(7, 7)).contains(Piece(White, Bishop)) should be (true)
    board(Square(5, 0)).contains(Piece(Black, Rook)) should be (true)
    board(Square(5, 1)).contains(Piece(White, Rook)) should be (true)
    board(Square(5, 3)).contains(Piece(Black, Queen)) should be (true)
    board(Square(5, 4)).contains(Piece(White, Queen)) should be (true)
    board(Square(5, 6)).contains(Piece(Black, King)) should be (true)
    board(Square(5, 7)).contains(Piece(White, King)) should be (true)
    board(Square(0, 0)).contains(Piece(White, Pawn)) should be (true)
    board(Square(0, 7)).contains(Piece(Black, Pawn)) should be (true)
  }
}
