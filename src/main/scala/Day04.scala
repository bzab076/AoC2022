object Day04 extends CommonPuzzle (4) {

  val input: List[(Range, Range)] = inputLines.map { case s"$x1-$y1,$x2-$y2" =>
    Range.inclusive(x1.toInt, y1.toInt) -> Range.inclusive(x2.toInt, y2.toInt) }

  override def partOne: Any = input.count { case (r1, r2) => r1 == r1.intersect(r2) || r2 == r1.intersect(r2) }

  override def partTwo: Any = input.count { case (r1, r2) => r1.intersect(r2).nonEmpty }

}
