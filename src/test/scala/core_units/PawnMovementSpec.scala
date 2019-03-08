package core_units

import core.CoordinateConversion._
import core._
import org.scalatest._

class PawnMovementSpec extends FlatSpec with Matchers {
  "A pawn" should "be able to move 1 or 2 steps forward from base row" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    var moves = position.potentialPawnMovesFrom(Square(1, 0), White).toList
    moves should have size 2
    moves should contain (RegularMove(Square(1, 0), Square(2, 0)))
    moves should contain (RegularMove(Square(1, 0), Square(3, 0)))

    moves = position.potentialPawnMovesFrom(Square(6, 0), Black).toList
    moves should have size 2
    moves should contain (RegularMove(Square(6, 0), Square(5, 0)))
    moves should contain (RegularMove(Square(6, 0), Square(4, 0)))
  }
  it should "be able to move 1 step forward from non-base row" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    var moves = position.potentialPawnMovesFrom(Square(2, 0), White).toList
    moves should have size 1
    moves should contain (RegularMove(Square(2, 0), Square(3, 0)))

    moves = position.potentialPawnMovesFrom(Square(5, 0), Black).toList
    moves should have size 1
    moves should contain (RegularMove(Square(5, 0), Square(4, 0)))
  }
  it should "be blocked from stepping forward by any piece" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "P.p....." +
      "........" +
      "........" +
      "P.p....." +
      "........" +
      "........"
    )
    val position = new Position(board.grid, PositionContext.Initial)
    
    position.potentialPawnMovesFrom(Square(1, 0), White) shouldBe empty
    position.potentialPawnMovesFrom(Square(1, 2), White) shouldBe empty
    position.potentialPawnMovesFrom(Square(6, 0), Black) shouldBe empty
    position.potentialPawnMovesFrom(Square(6, 2), Black) shouldBe empty
  }
  it should "be able to capture diagonally forward both left and right" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "ppp....." +
      "........" +
      "........" +
      "PPP....." +
      "........" +
      "........"
    )
    val position = new Position(board.grid, PositionContext.Initial)

    var moves = position.potentialPawnMovesFrom(Square(4, 1), White).toList
    moves should have size 2
    moves should contain (RegularMove(Square(4, 1), Square(5, 0)))
    moves should contain (RegularMove(Square(4, 1), Square(5, 2)))

    moves = position.potentialPawnMovesFrom(Square(3, 1), Black).toList
    moves should have size 2
    moves should contain (RegularMove(Square(3, 1), Square(2, 0)))
    moves should contain (RegularMove(Square(3, 1), Square(2, 2)))
  }
  it should "be able to capture en passant as white" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "p.p....." +
      ".p......" +
      "........" +
      "........" +
      "........" +
      "........"
    )
    val position = new Position(
      board.grid,
      PositionContext.Initial.after(
        RegularMove(Square(6, 1), Square(4, 1)),
        board.grid
      )
    )

    var moves = position.potentialPawnMovesFrom(Square(4, 0), White).toList
    moves should have size 1
    moves should contain (RegularMove(Square(4, 0), Square(5, 1)))

    moves = position.potentialPawnMovesFrom(Square(4, 2), White).toList
    moves should have size 1
    moves should contain (RegularMove(Square(4, 2), Square(5, 1)))
  }
  it should "be able to capture en passant as black" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "........" +
      ".P......" +
      "P.P....." +
      "........" +
      "........"
    )
    val position = new Position(
      board.grid,
      PositionContext.Initial.after(
        RegularMove(Square(1, 1), Square(3, 1)),
        board.grid
      )
    )

    var moves = position.potentialPawnMovesFrom(Square(3, 0), Black).toList
    moves should have size 1
    moves should contain (RegularMove(Square(3, 0), Square(2, 1)))

    moves = position.potentialPawnMovesFrom(Square(3, 2), Black).toList
    moves should have size 1
    moves should contain (RegularMove(Square(3, 2), Square(2, 1)))
  }
}
