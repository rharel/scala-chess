package core

object Direction {
  val All: Iterable[Direction] = List(
    Left, Right, Bottom, Top,
    BottomLeft, BottomRight, TopLeft, TopRight
  )
  val Cross: Iterable[Direction] = List(Left, Right, Bottom, Top)
  val Diagonal: Iterable[Direction] = List(BottomLeft, BottomRight, TopLeft, TopRight)
}
sealed trait Direction {
  val offset: (Int, Int)
  override def toString = s"(${offset._1}, ${offset._2})"
}
case object Left extends Direction { val offset: (Int, Int) = (0, -1) }
case object Right extends Direction { val offset: (Int, Int) = (0, +1) }
case object Bottom extends Direction { val offset: (Int, Int) = (-1, 0) }
case object BottomLeft extends Direction { val offset: (Int, Int) = (-1, -1) }
case object BottomRight extends Direction { val offset: (Int, Int) = (-1, +1) }
case object Top extends Direction { val offset: (Int, Int) = (+1, 0) }
case object TopLeft extends Direction { val offset: (Int, Int) = (+1, -1) }
case object TopRight extends Direction { val offset: (Int, Int) = (+1, +1) }