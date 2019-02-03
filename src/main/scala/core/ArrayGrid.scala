package core

import scala.reflect.ClassTag

object ArrayGrid {
  private def toIndex(row: Row, col: Col): Int =
    Grid.Size * row.index + col.index
}
final class ArrayGrid[A:ClassTag](fillValue: A) extends Grid[A] {
  def apply(row: Row, col: Col): A =
    data(ArrayGrid.toIndex(row, col))

  def update(square: Square, value: A): Unit =
    this(square.row, square.col) = value
  def update(row: Row, col: Col, value: A): Unit =
    data(ArrayGrid.toIndex(row, col)) = value

  def copy(source: ArrayGrid[A]): Unit =
    source.data.copyToArray(this.data)

  override def clone: ArrayGrid[A] = {
    val result = new ArrayGrid[A](fillValue)
    result.copy(this)
    result
  }

  val data: Array[A] =
    Array.fill[A](Grid.SquareCount)(fillValue)
}
