package core

sealed trait Coordinate { def index: Int; }

case object Coordinate0 extends Coordinate { val index = 0; }
case object Coordinate1 extends Coordinate { val index = 1; }
case object Coordinate2 extends Coordinate { val index = 2; }
case object Coordinate3 extends Coordinate { val index = 3; }
case object Coordinate4 extends Coordinate { val index = 4; }
case object Coordinate5 extends Coordinate { val index = 5; }
case object Coordinate6 extends Coordinate { val index = 6; }
case object Coordinate7 extends Coordinate { val index = 7; }
