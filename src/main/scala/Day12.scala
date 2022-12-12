object Day12 extends CommonPuzzle (12) {

  var start : (Int,Int) = (0,0)
  var end : (Int,Int) = (0,0)
  val map: Array[Array[Char]] = inputAsArrayOfChar
  private val maxY: Int = map.length
  private val maxX: Int = map(0).length

  override def partOne: Any = GraphUtils.findCheapestPath(start,end, getNeighbours , getCost)

  override def partTwo: Any =  {

    val coordinates = for (
         x <- 0 until maxX;
         y <- 0 until maxY)
       yield (x, y)

    coordinates.filter{case (x,y) => value(x,y) == 'a'}.map(s => GraphUtils.findCheapestPath(s,end, getNeighbours , getCost)).filter(_ > 0).min
  }

  def inputAsArrayOfChar: Array[Array[Char]] = {

    val xSize: Int = inputLines.head.length
    val ySize: Int = inputLines.length
    val result = Array.ofDim[Char](ySize, xSize)

    var y = 0
    for(line <- inputLines) {
      var x = 0
      for(char <- line.toList) {
        result(y)(x) = char
        if(char=='E') end = (x,y)
        if(char=='S') start = (x,y)
        x += 1
      }
      y += 1
    }

    result
  }

  def value(x : Int,y : Int) : Int = {
    if(x==start._1 && y==start._2) 'a'.toInt
    else if(x==end._1 && y==end._2) 'z'.toInt
    else map(y)(x).toInt
  }

  def getNeighbours(x: Int, y: Int) : List[(Int,Int)] =
    List( (x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)).filter { case (xx, yy) => xx >= 0 && yy >= 0 && xx< maxX && yy < maxY }
      .filter{case (xx, yy) => (value(xx,yy) - value(x,y)) <= 1}

  def getCost(p1 : (Int, Int), p2: (Int, Int)) : Int = 1
}
