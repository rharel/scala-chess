package core_units

import core.CoordinateConversion._
import core._
import org.scalatest._

class KnightMovementSpec extends FlatSpec with Matchers {
  "A knight" should "be able to move at 3-square radius" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    val moves = position.findKnightMovesFrom(Square(3, 3), White).toSet
    moves.size should be (8)
    moves contains RegularMove(Square(3, 3), Square(1, 2)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(1, 4)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(2, 1)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(2, 5)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(5, 2)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(5, 4)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(4, 1)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(4, 5)) should be (true)
  }
  it should "not be able to move beyond the edge of the board" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    var moves = position.findKnightMovesFrom(Square(0, 0), White).toSet
    moves.size should be (2)
    moves contains RegularMove(Square(0, 0), Square(1, 2)) should be (true)
    moves contains RegularMove(Square(0, 0), Square(2, 1)) should be (true)

    moves = position.findKnightMovesFrom(Square(7, 7), White).toSet
    moves.size should be (2)
    moves contains RegularMove(Square(7, 7), Square(6, 5)) should be (true)
    moves contains RegularMove(Square(7, 7), Square(5, 6)) should be (true)
  }
  it should "be able to capture hostile pieces" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(2, 5)) = Some(Piece(Black, Pawn))

    val moves = position.findKnightMovesFrom(Square(3, 3), White).toSet
    moves.size should be (8)
    moves contains RegularMove(Square(3, 3), Square(2, 5)) should be (true)
  }
  it should "not be able to move onto friendly pieces" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(2, 5)) = Some(Piece(White, Pawn))

    val moves = position.findKnightMovesFrom(Square(3, 3), White).toSet
    moves.size should be (7)
    moves contains RegularMove(Square(3, 3), Square(2, 5)) should be (false)
  }
}
