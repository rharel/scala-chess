package core_units

import core.CoordinateConversion._
import core._
import org.scalatest._

class QueenMovementSpec extends FlatSpec with Matchers {
  "A queen" should "be able to move both as a rook and a bishop" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, new PositionContext())

    val moves = position.findQueenMovesFrom(Square(3, 3), White).toSet

    moves should be ((
      position.findBishopMovesFrom(Square(3, 3), White) ++
      position.findRookMovesFrom(Square(3, 3), White)).toSet)
  }
  it should "be able to capture hostile pieces but not move beyond them" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, new PositionContext())

    grid(Square(3, 5)) = Some(Piece(Black, Pawn))

    val moves = position.findQueenMovesFrom(Square(3, 3), White).toSet
    moves should be ((
      position.findBishopMovesFrom(Square(3, 3), White) ++
      position.findRookMovesFrom(Square(3, 3), White)).toSet)
  }
  it should "not be able to move onto friendly pieces or beyond them" in {
    val grid = new ArrayGrid[Option[Piece]](None)
    val position = new Position(grid, new PositionContext())

    grid(Square(3, 5)) = Some(Piece(White, Pawn))

    val moves = position.findQueenMovesFrom(Square(3, 3), White).toSet
    moves should be ((
      position.findBishopMovesFrom(Square(3, 3), White) ++
      position.findRookMovesFrom(Square(3, 3), White)).toSet)
  }
}