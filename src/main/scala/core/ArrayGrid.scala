package core

import scala.reflect.ClassTag

object ArrayGrid {
  def fill[A:ClassTag](value: A): ArrayGrid[A] = populate(_ => value)
  def populate[A:ClassTag](getValue: Square => A): ArrayGrid[A] =
    Grid.Squares.iterator
      .foldLeft(new ArrayGrid[A])((grid, square) => {
        grid(square) = getValue(square)
        grid
      })

  private def toIndex(square: Square): Int =
    Grid.Size * square.row.index + square.col.index
}
final class ArrayGrid[A:ClassTag] private extends Grid[A] {
  def apply(square: Square): A =
    _data(ArrayGrid.toIndex(square))
  def update(square: Square, value: A): Unit =
    _data(ArrayGrid.toIndex(square)) = value

  def copy(source: ArrayGrid[A]): Unit =
    source._data.copyToArray(this._data)

  override def clone: ArrayGrid[A] = {
    val result = new ArrayGrid[A]
    result.copy(this)
    result
  }

  private val _data: Array[A] = new Array(Grid.SquareCount)
}
