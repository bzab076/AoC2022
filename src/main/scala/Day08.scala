object Day08 extends CommonPuzzle (8) {

  val grid: Array[Array[Int]] = inputAsArrayOfDigits
  val maxY: Int = grid.length
  val maxX: Int = grid(0).length

  override def partOne: Any = {

    var visibleCount = 0
    for(x <- 0 until maxX) {
      for(y <- 0 until maxY) {
        if(x==0 || x==maxX-1 || y==0 || y==maxY-1) visibleCount += 1 // edges are always visible
        else if(isTopVisible(x,y) || isRightVisible(x,y) || isLeftVisible(x,y) || isBottomVisible(x,y)) visibleCount += 1
      }
    }

    visibleCount
  }

  def isLeftVisible(x : Int, y : Int) : Boolean = {

    var visible = true
    for(i <- 0 until x) if(grid(y)(i) >= grid(y)(x)) visible = false
    visible

  }

  def isRightVisible(x : Int, y : Int) : Boolean = {

    var visible = true
    for(i <- x + 1 until maxX) if(grid(y)(i) >= grid(y)(x)) visible = false
    visible

  }

  def isTopVisible(x : Int, y : Int) : Boolean = {

    var visible = true
    for(j <- 0 until y) if(grid(j)(x) >= grid(y)(x)) visible = false
    visible

  }

  def isBottomVisible(x : Int, y : Int) : Boolean = {

    var visible = true
    for(j <- y + 1 until maxY) if(grid(j)(x) >= grid(y)(x)) visible = false
    visible

  }

  def scenicScore(x : Int,y : Int) : Int = {

    var leftScore = 0
    var i = x-1
    while(i >= 0 && grid(y)(i) < grid(y)(x)) {
      leftScore+=1
      i-=1
    }
    if(i>=0) leftScore += 1

    var rightScore = 0
    i = x+1
    while(i<maxX && grid(y)(i) < grid(y)(x)) {
      rightScore+=1
      i+=1
    }
    if(i<maxX) rightScore += 1

    var topScore = 0
    var j = y-1
    while(j >= 0 && grid(j)(x) < grid(y)(x)) {
      topScore+=1
      j-=1
    }
    if(j>=0) topScore += 1

    var bottomScore = 0
    j = y+1
    while(j<maxY && grid(j)(x) < grid(y)(x)) {
      bottomScore+=1
      j+=1
    }
    if(j<maxY) bottomScore += 1

    rightScore*topScore*bottomScore*leftScore

  }

  override def partTwo: Any = (for {x <- Range(1, maxX - 2)
                                    y <- Range(1, maxY - 2)
                               } yield scenicScore(x,y)).max

}
