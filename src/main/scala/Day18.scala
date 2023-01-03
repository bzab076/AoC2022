object Day18 extends CommonPuzzle (18) {

  val grid: List[(Int, Int, Int)] = inputLines.map{ case s"$x,$y,$z" => (x.toInt, y.toInt, z.toInt)}
  val maxX: Int = grid.map { case (x, _, _) => x }.max
  val minX: Int = grid.map { case (x, _, _) => x }.min
  val maxY: Int = grid.map { case (_, y, _) => y }.max
  val minY: Int = grid.map { case (_, y, _) => y }.min
  val maxZ: Int = grid.map { case (_, _, z) => z }.max
  val minZ: Int = grid.map { case (_, _, z) => z }.min

  val adjacent = List((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))

  override def partOne: Any = surfaceArea(grid)

  override def partTwo: Any = {
    val airPockets = findAirPockets(grid)
    surfaceArea(grid.appendedAll(airPockets))
  }

  def surfaceArea(someGrid: List[(Int, Int, Int)]): Int = someGrid.map(c => 6 - adjacentCubes(c, someGrid)).sum

  def adjacentCubes(cube: (Int, Int, Int), someGrid: List[(Int, Int, Int)]): Int =
    adjacent.map{ case(dx,dy,dz) => if(someGrid.contains((cube._1+dx,cube._2+dy,cube._3+dz))) 1 else 0}.sum

  def findAirPockets(someGrid: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
    val airPockets = (for {
      x <- minX+1 to maxX-1
      y <- minY+1 to maxY-1
      z <- minZ+1 to maxZ-1
      if !someGrid.contains((x, y, z))
      if someGrid.filter { case (a, b, c) => a > x && b == y && c == z }.nonEmpty && someGrid.filter { case (a, b, c) => a < x && b == y && c == z }.nonEmpty
      if someGrid.filter { case (a, b, c) => a == x && b > y && c == z }.nonEmpty && someGrid.filter { case (a, b, c) => a == x && b < y && c == z }.nonEmpty
      if someGrid.filter { case (a, b, c) => a == x && b == y && c > z }.nonEmpty && someGrid.filter { case (a, b, c) => a == x && b == y && c < z }.nonEmpty
    } yield (x,y,z)).toList

    // remove outliers
    airPockets.filter(cube => adjacentCubes(cube,someGrid) + adjacentCubes(cube,airPockets) == 6)
  }

}
