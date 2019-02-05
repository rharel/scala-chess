package core

import scala.reflect.ClassTag

object ArrayGrid {
  private def toIndex(square: Square): Int =
    Grid.Size * square.row.index + square.col.index
}
final class ArrayGrid[A:ClassTag](fillValue: A) extends Grid[A] {
  def apply(square: Square): A =
    data(ArrayGrid.toIndex(square))
  def update(square: Square, value: A): Unit =
    data(ArrayGrid.toIndex(square)) = value

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
