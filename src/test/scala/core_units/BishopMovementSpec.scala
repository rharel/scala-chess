package core_units

import core.CoordinateConversion._
import core._
import org.scalatest._

class BishopMovementSpec extends FlatSpec with Matchers {
  "A bishop" should "be able to move diagonally in each direction when free" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    val moves = position.findBishopMovesFrom(Square(3, 3), White).toSet
    moves.size should be (13)
    moves contains RegularMove(Square(3, 3), Square(0, 0)) should be (true)  // First diagonal
    moves contains RegularMove(Square(3, 3), Square(1, 1)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(2, 2)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(4, 4)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(5, 5)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(6, 6)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(7, 7)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(0, 6)) should be (true)  // Second diagonal
    moves contains RegularMove(Square(3, 3), Square(1, 5)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(2, 4)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(4, 2)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(5, 1)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(6, 0)) should be (true)
  }
  it should "be able to capture hostile pieces but not move beyond them" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(5, 5)) = Some(Piece(Black, Pawn))

    val moves = position.findBishopMovesFrom(Square(3, 3), White).toSet
    moves.size should be (11)
    moves contains RegularMove(Square(3, 3), Square(5, 5)) should be (true)
    moves contains RegularMove(Square(3, 3), Square(6, 6)) should be (false)
    moves contains RegularMove(Square(3, 3), Square(7, 7)) should be (false)
  }
  it should "not be able to move onto friendly pieces or beyond them" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(5, 5)) = Some(Piece(White, Pawn))

    val moves = position.findBishopMovesFrom(Square(3, 3), White).toSet
    moves.size should be (10)
    moves contains RegularMove(Square(3, 3), Square(5, 5)) should be (false)
    moves contains RegularMove(Square(3, 3), Square(6, 6)) should be (false)
    moves contains RegularMove(Square(3, 3), Square(7, 7)) should be (false)
  }
}
