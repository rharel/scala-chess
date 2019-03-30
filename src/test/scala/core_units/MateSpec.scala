package core_units

import core._
import core.CoordinateConversion.{intToCol, intToRow}
import org.scalatest._

class MateSpec extends FlatSpec with Matchers {
  "Mate" should "be avoidable by moving the king to a safe square" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "..PPP..." +
      "..PKP..." +
      "..P.Pn.." +
      "........" +
      "........"
    )

    val position = new Position(board.grid, PositionContext.Initial)
    val breakers = position.findCheckBreakersFor(White).toList

    position.isChecked(White) should be (true)
    position.isMated(White) should be (false)
    breakers should have size 1
    breakers should contain (Step(Square(3, 3), Square(2, 3)))
  }

  it should "be avoidable by capturing an only threat" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "..PPP..." +
      "..PKP..." +
      "..PPPn.." +
      "........" +
      ".....R.."
    )

    val position = new Position(board.grid, PositionContext.Initial)
    val breakers = position.findCheckBreakersFor(White).toList

    position.isChecked(White) should be (true)
    position.isMated(White) should be (false)
    breakers should have size 1
    breakers should contain (Step(Square(0, 5), Square(2, 5)))
  }

  it should "be avoidable by blocking an only threat" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "..PPP..." +
      "..PKP..." +
      "..P.P..." +
      "R......." +
      "...r...."
    )

    val position = new Position(board.grid, PositionContext.Initial)
    val breakers = position.findCheckBreakersFor(White).toList

    position.isChecked(White) should be (true)
    position.isMated(White) should be (false)
    breakers should have size 1
    breakers should contain (Step(Square(1, 0), Square(1, 3)))
  }

  it should "not be avoidable when the king has no escape" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "..PPP..." +
      "..PKP..." +
      "..PPPn.." +
      "........" +
      "........"
    )

    val position = new Position(board.grid, PositionContext.Initial)
    val breakers = position.findCheckBreakersFor(White).toList

    position.isChecked(White) should be (true)
    position.isMated(White) should be (true)
    breakers shouldBe empty
  }

  it should "not be avoidable by capturing one threat out of many" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "..PPP..." +
      "..PKP..." +
      ".nPPPn.." +
      "........" +
      ".....R.."
    )

    val position = new Position(board.grid, PositionContext.Initial)
    val breakers = position.findCheckBreakersFor(White).toList

    position.isChecked(White) should be (true)
    position.isMated(White) should be (true)
    breakers shouldBe empty
  }

  it should "not be avoidable by blocking one threat out of many" in {
    val board = Board.fromString(
      "........" +
      "........" +
      "........" +
      "..PPP..." +
      "..PK...r" +
      "..P.P..." +
      "R......." +
      "...r...."
    )

    val position = new Position(board.grid, PositionContext.Initial)
    val breakers = position.findCheckBreakersFor(White).toList

    position.isChecked(White) should be (true)
    position.isMated(White) should be (true)
    breakers shouldBe empty
  }
}
