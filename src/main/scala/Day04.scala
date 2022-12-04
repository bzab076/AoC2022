object Day04 extends CommonPuzzle (4) {

  override def partOne: Any = inputLines.map { case s"$x1-$y1,$x2-$y2" => (x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt) }
    .count { case (r1, r2) => rangeContained(Range.inclusive(r1._1, r1._2), Range.inclusive(r2._1, r2._2)) }

  override def partTwo: Any = inputLines.map { case s"$x1-$y1,$x2-$y2" => (x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt) }
    .count { case (r1, r2) => Range.inclusive(r1._1, r1._2).intersect(Range.inclusive(r2._1, r2._2)).nonEmpty }


  def rangeContained(r1 : Range, r2 : Range) : Boolean = r1 == r1.intersect(r2) || r2 == r1.intersect(r2)
}
