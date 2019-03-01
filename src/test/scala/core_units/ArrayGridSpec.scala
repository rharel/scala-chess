package core_units

import core.{ArrayGrid, Grid}
import org.scalatest._

class ArrayGridSpec extends FlatSpec with Matchers {
  "ArrayGrid" should "be filled with an initial value" in {
    val grid = new ArrayGrid[Int](-1)
    for (value <- grid) {
      value should be (-1)
    }
  }
  it should "have mutable value per square" in {
    val grid = new ArrayGrid[Int](-1)
    var value = 0
    for (square <- Grid.Squares) {
      grid(square) = value
      grid(square) should be (value)
      value += 1
    }
  }
}
