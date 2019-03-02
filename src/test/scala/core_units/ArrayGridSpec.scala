package core_units

import core.{ArrayGrid, Grid}
import org.scalatest._

class ArrayGridSpec extends FlatSpec with Matchers {
  "ArrayGrid" should "be able to be constructed with a fill value" in {
    val grid = ArrayGrid.fill[Int](-1)
    grid.foreach(_ should be (-1))
  }
  it should "be able to be constructed with a fill function" in {
    val grid = ArrayGrid.populate[Int](
      square => square.row.index * 10 + square.col.index
    )
    grid.iterator
      .zip(Grid.Squares.iterator)
      .foreach(deployment => {
        val (value, square) = deployment
        value should be (square.row.index * 10 + square.col.index)
      })
  }
  it should "have mutable value per square" in {
    val grid = ArrayGrid.fill[Int](-1)
    var value = 0
    for (square <- Grid.Squares) {
      grid(square) = value
      grid(square) should be (value)
      value += 1
    }
  }
}
