package core

object Coordinate {
  val First: Int = 0
  val Last: Int = Grid.Size - 1

  def isValid(value: Int): Boolean = First <= value && value <= Last
  def isFirst(value: Int): Boolean = value == First
  def isLast(value: Int): Boolean = value == Last
}

object Row {
  val First: Row = Row.fromIndex(Coordinate.First)
  val Last: Row = Row.fromIndex(Coordinate.Last)
  val All: Iterable[Row] =
    (Coordinate.First to Coordinate.Last)
      .map(value => Row.fromIndex(value))
      .toList

  def fromIndex(index: Int): Row = {
    assert(Coordinate.isValid(index))
    Row(index)
  }
}
final case class Row private (index: Int) extends AnyVal {
  def isValid: Boolean = Coordinate.isValid(index)
  def isFirst: Boolean = Coordinate.isFirst(index)
  def isLast: Boolean = Coordinate.isLast(index)

  def +(offset: Int): Row = Row.fromIndex(index + offset)
  def -(offset: Int): Row = this + (-offset)

  def +?(offset: Int): Boolean = Coordinate.isValid(index + offset)
  def -?(offset: Int): Boolean = this +? (-offset)

  def ==(other: Row): Boolean = this.index == other.index
  def !=(other: Row): Boolean = !(this == other)

  def <(other: Row): Boolean = this.index < other.index
  def <=(other: Row): Boolean = !(other < this)
  def >(other: Row): Boolean = other < this
  def >=(other: Row): Boolean = !(this < other)

  override def toString = s"${index + 1}"
}

object Col {
  val First: Col = Col.fromIndex(Coordinate.First)
  val Last: Col = Col.fromIndex(Coordinate.Last)
  val All: Iterable[Col] =
    (Coordinate.First to Coordinate.Last)
      .map(value => Col.fromIndex(value))
      .toList

  def fromIndex(index: Int): Col = {
    assert(Coordinate.isValid(index))
    Col(index)
  }
}
final case class Col private (index: Int) extends AnyVal {
  def isValid: Boolean = Coordinate.isValid(index)
  def isFirst: Boolean = Coordinate.isFirst(index)
  def isLast: Boolean = Coordinate.isLast(index)

  def +(offset: Int): Col = Col.fromIndex(index + offset)
  def -(offset: Int): Col = this + (-offset)

  def +?(offset: Int): Boolean = Coordinate.isValid(index + offset)
  def -?(offset: Int): Boolean = this +? (-offset)

  def ==(other: Col): Boolean = this.index == other.index
  def !=(other: Col): Boolean = !(this == other)

  def <(other: Col): Boolean = this.index < other.index
  def <=(other: Col): Boolean = !(other < this)
  def >(other: Col): Boolean = other < this
  def >=(other: Col): Boolean = !(this < other)
  
  override def toString = s"${('a' + index).toChar}"
}

object CoordinateConversion {
  import scala.language.implicitConversions

  implicit def intToRow(index: Int): Row = Row.fromIndex(index)
  implicit def intToCol(index: Int): Col = Col.fromIndex(index)
}
