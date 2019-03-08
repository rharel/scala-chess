package core_units

import core._
import core.CoordinateConversion._
import org.scalatest._

class KingMovementSpec extends FlatSpec with Matchers {
  "A king" should "be able to move one square in each direction when free" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    val moves = position.potentialKingMovesFrom(Square(3, 3), White).toSet
    moves.size should be (8)
    moves contains Step(Square(3, 3), Square(2, 2)) should be (true)
    moves contains Step(Square(3, 3), Square(2, 3)) should be (true)
    moves contains Step(Square(3, 3), Square(2, 4)) should be (true)
    moves contains Step(Square(3, 3), Square(3, 2)) should be (true)
    moves contains Step(Square(3, 3), Square(3, 4)) should be (true)
    moves contains Step(Square(3, 3), Square(4, 2)) should be (true)
    moves contains Step(Square(3, 3), Square(4, 3)) should be (true)
    moves contains Step(Square(3, 3), Square(4, 4)) should be (true)
  }
  it should "not be able to move beyond the edge of the board" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    var moves = position.potentialKingMovesFrom(Square(0, 0), White).toSet
    moves.size should be (3)
    moves contains Step(Square(0, 0), Square(0, 1)) should be (true)
    moves contains Step(Square(0, 0), Square(1, 0)) should be (true)
    moves contains Step(Square(0, 0), Square(1, 1)) should be (true)

    moves = position.potentialKingMovesFrom(Square(7, 7), White).toSet
    moves.size should be (3)
    moves contains Step(Square(7, 7), Square(6, 6)) should be (true)
    moves contains Step(Square(7, 7), Square(6, 7)) should be (true)
    moves contains Step(Square(7, 7), Square(7, 6)) should be (true)
  }
  it should "be able to capture hostile pieces" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(4, 4)) = Some(Piece(Black, Pawn))

    val moves = position.potentialKingMovesFrom(Square(3, 3), White).toSet
    moves.size should be (8)
    moves contains Step(Square(3, 3), Square(4, 4)) should be (true)
  }
  it should "not be able to move onto friendly pieces" in {
    val grid = ArrayGrid.fill[Option[Piece]](None)
    val position = new Position(grid, PositionContext.Initial)

    grid(Square(4, 4)) = Some(Piece(White, Pawn))

    val moves = position.potentialKingMovesFrom(Square(3, 3), White).toSet
    moves.size should be (7)
    moves contains Step(Square(3, 3), Square(4, 4)) should be (false)
  }
}
