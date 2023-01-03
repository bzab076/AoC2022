object Day15 extends CommonPuzzle (15) {

  val inputData: List[((Int, Int), (Int, Int), Int)] = parseInput()
  val sensors: List[((Int, Int), Int)] = inputData.map{case (s, _,d) => (s,d)}
  val beacons: List[(Int, Int)] = inputData.map{case (_,b, _) => b}.distinct

  override def partOne: Any = {

    val y = 2000000
    val minX = inputData.map{case (s, _, _) => s._1}.min
    val maxX = inputData.map{case (s, _, _) => s._1}.max
    val maxDistance = inputData.map{case (_, _,d) => d}.max
    var count = 0
    for(x <- minX - maxDistance to maxX + maxDistance) {
       if(sensors.map{case (s,d) => Utils.manhattanDist((x,y),s)<=d}.foldLeft(false){(acc,c) => acc || c} && !beacons.contains((x,y)))  count += 1
    }
    count
  }

  override def partTwo: Any = {

    val maxVal = 4000000
    for( (sensor, dist) <- sensors) {
      val startPoint = (sensor._1 - dist -1, sensor._2)  // left edge of diamond
      var currentPoint = (startPoint._1 +1, startPoint._2 + 1)
      var xIncrement = 1
      var yIncrement = 1
      while(currentPoint != startPoint) {
        if(currentPoint._1 >= 0 && currentPoint._1 <= maxVal && currentPoint._2 >= 0 && currentPoint._2 <= maxVal)
          if(sensors.map { case (s, d) => Utils.manhattanDist(currentPoint, s) > d }.forall(c => c)) {
            val freq = currentPoint._1.toLong*maxVal + currentPoint._2
            return freq
          }
        currentPoint = (currentPoint._1 + xIncrement, currentPoint._2 + yIncrement)
        if(currentPoint._2 == sensor._2) xIncrement *= -1
        if(currentPoint._1 == sensor._1) yIncrement *= -1
      }
    }

    0  // default value if we do not find result
  }

  def parseInput() : List[((Int,Int),(Int,Int),Int)] = {

    val pairs = inputLines.map(line => line.substring(10).split(": closest beacon is at ").toList).map(l => (l.head,l.tail.head))
      .map{case (s,b) => (parseCoordinates(s), parseCoordinates(b))}

    pairs.map{case (s,b) => (s,b,Utils.manhattanDist(s,b))}
  }

  def parseCoordinates(str : String) : (Int,Int) = {
    val arr = str.split(", ").map(p => p.substring(2).toInt)
    (arr(0), arr(1))
  }

}
