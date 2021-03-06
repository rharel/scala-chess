package core

object Coordinate {
  val First: Int = 0
  val Last: Int = 7

  def isValid(value: Int): Boolean = First <= value && value <= Last
  def isFirst(value: Int): Boolean = value == First
  def isLast(value: Int): Boolean = value == Last
}

object Row {
  val First: Row = Row.fromIndex(Coordinate.First).get
  val Last: Row = Row.fromIndex(Coordinate.Last).get

  val All: Iterable[Row] =
    (Coordinate.First to Coordinate.Last).map(Row(_))
  val AllReversed: Iterable[Row] =
    (Coordinate.Last to Coordinate.First by -1).map(Row(_))

  def fromIndex(index: Int): Option[Row] =
    if (Coordinate.isValid(index)) Some(Row(index))
    else None

  def fromChar(source: Char): Option[Row] =
    Row.fromIndex(source - '1')
}
final case class Row private (index: Int) extends AnyVal {
  def isValid: Boolean = Coordinate.isValid(index)
  def isFirst: Boolean = Coordinate.isFirst(index)
  def isLast: Boolean = Coordinate.isLast(index)

  def +(offset: Int): Row = Row.fromIndex(index + offset).get
  def -(offset: Int): Row = this + (-offset)

  def +?(offset: Int): Boolean = Coordinate.isValid(index + offset)
  def -?(offset: Int): Boolean = this +? (-offset)

  def <(other: Row): Boolean = this.index < other.index
  def <=(other: Row): Boolean = !(other < this)
  def >(other: Row): Boolean = other < this
  def >=(other: Row): Boolean = !(this < other)

  def distanceTo(other: Row): Int = Math.abs(this offsetFrom other)
  def offsetFrom(other: Row): Int = this.index - other.index

  override def toString = s"${index + 1}"
}

object Col {
  val First: Col = Col.fromIndex(Coordinate.First).get
  val Last: Col = Col.fromIndex(Coordinate.Last).get

  val All: Iterable[Col] =
    (Coordinate.First to Coordinate.Last).map(Col(_))
  val AllReversed: Iterable[Col] =
    (Coordinate.Last to Coordinate.First by -1).map(Col(_))

  def fromIndex(index: Int): Option[Col] =
    if (Coordinate.isValid(index)) Some(Col(index))
    else None

  def fromChar(source: Char): Option[Col] =
    Col.fromIndex(source - 'a')
}
final case class Col private (index: Int) extends AnyVal {
  def isValid: Boolean = Coordinate.isValid(index)
  def isFirst: Boolean = Coordinate.isFirst(index)
  def isLast: Boolean = Coordinate.isLast(index)

  def +(offset: Int): Col = Col.fromIndex(index + offset).get
  def -(offset: Int): Col = this + (-offset)

  def +?(offset: Int): Boolean = Coordinate.isValid(index + offset)
  def -?(offset: Int): Boolean = this +? (-offset)

  def <(other: Col): Boolean = this.index < other.index
  def <=(other: Col): Boolean = !(other < this)
  def >(other: Col): Boolean = other < this
  def >=(other: Col): Boolean = !(this < other)

  def distanceTo(other: Col): Int = Math.abs(this offsetFrom other)
  def offsetFrom(other: Col): Int = this.index - other.index

  override def toString = s"${('a' + index).toChar}"
}

object CoordinateConversion {
  import scala.language.implicitConversions

  implicit def intToRow(index: Int): Row = Row.fromIndex(index).get
  implicit def intToCol(index: Int): Col = Col.fromIndex(index).get
}
