package core_units

import core.CoordinateConversion._
import core._
import org.scalatest._

class RookMovementSpec extends FlatSpec with Matchers {
  "A rook" should "be able to move at a cross in each direction when free" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    val moves = position.potentialRookMovesFrom(Square(3, 3), White).toSet
    moves.size should be (14)
    moves contains RegularMove(Square(3, 3), Square(3, 0)) should be (true)  // First edge
    moves contains RegularMove(Square(3, 3), Square(3, 1)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(3, 2)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(3, 4)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(3, 5)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(3, 6)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(3, 7)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(0, 3)) should be (true)  // Second edge
    moves contains RegularMove(Square(3, 3), Square(1, 3)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(2, 3)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(4, 3)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(5, 3)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(6, 3)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(7, 3)) should be (true)
  }
  it should "be able to capture hostile pieces but not move beyond them" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(3, 5)) = Some(Piece(Black, Pawn))

    val moves = position.potentialRookMovesFrom(Square(3, 3), White).toSet
    moves.size should be (12)
    moves contains RegularMove(Square(3, 3), Square(3, 5)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(3, 6)) should be (false)
    moves contains RegularMove(Square(3, 3), Square(3, 7)) should be (false)
  }
  it should "not be able to move onto friendly pieces or beyond them" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(3, 5)) = Some(Piece(White, Pawn))

    val moves = position.potentialRookMovesFrom(Square(3, 3), White).toSet
    moves.size should be (11)
    moves contains RegularMove(Square(3, 3), Square(3, 5)) should be (false)
    moves contains RegularMove(Square(3, 3), Square(3, 6)) should be (false)
    moves contains RegularMove(Square(3, 3), Square(3, 7)) should be (false)
  }
}
