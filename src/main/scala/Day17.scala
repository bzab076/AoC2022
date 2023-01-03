import scala.collection.mutable

object Day17 extends CommonPuzzle (17) {

  val rock1 = List((0,0),(1,0),(2,0),(3,0))
  val rock2 = List((1,0),(0,1),(1,1),(2,1),(1,2))
  val rock3 = List((0,0),(1,0),(2,0),(2,1),(2,2))
  val rock4 = List((0,0),(0,1),(0,2),(0,3))
  val rock5 = List((0,0),(1,0),(0,1),(1,1))
  val rocks: Array[List[(Int, Int)]] = Array(rock1,rock2,rock3,rock4,rock5)

  val leftWall = 0
  val rightWall = 8
  val jetStream: String = inputString

  override def partOne: Any = simulateRockFalling(2022)

  override def partTwo: Any = {

    val targetStep = 1000000000000L
    val firstRepeatingStep = 866  // obtained using debug code and spreadsheet
    val repeatAfterStep = 1705   // obtained using debug code and spreadsheet
    val firstResidual = simulateRockFalling(firstRepeatingStep)
    val repeatingValue = simulateRockFalling(firstRepeatingStep + repeatAfterStep) - firstResidual
    val repeatsRequired = (targetStep - firstRepeatingStep) / repeatAfterStep
    val lastResidualSteps = (targetStep - firstRepeatingStep) % repeatAfterStep
    val lastResidual = simulateRockFalling(firstRepeatingStep + lastResidualSteps.toInt) - firstResidual
    val result = repeatsRequired * repeatingValue + firstResidual + lastResidual
    result
  }

  def moveRock(direction : (Int,Int), rock : List[(Int,Int)]) : List[(Int,Int)] = rock.map{case (x,y) => (x+direction._1, y+direction._2)}

  def simulateRockFalling(steps : Int): Int = {

    val rockPile: mutable.Set[(Int, Int)] = mutable.Set.empty

    def canMoveRock(direction : (Int,Int), rock : List[(Int,Int)]) : Boolean = {
      val potentialMove =  moveRock(direction,rock)
      if(potentialMove.map{case (_,y) => y}.min <= 0) return false
      if(potentialMove.map{case (x, _) => x}.min <= leftWall) return false
      if(potentialMove.map{case (x, _) => x}.max >= rightWall) return false
      potentialMove.forall(p => !rockPile.contains(p))
    }

    var currentBottom = 0
    var rockIndex = 0
    var jetIndex = 0

    for(step <- 1 to steps)  {

      var currentRock = moveRock((3,currentBottom + 4), rocks(rockIndex))
      var canDrop = true

      while(canDrop) {

        // jet push
        val push = if(jetStream.charAt(jetIndex) == '>') (1,0) else (-1,0)
        if(canMoveRock(push,currentRock)) currentRock = moveRock(push,currentRock)

        // fall down
        val down = (0,-1)
        canDrop = canMoveRock(down,currentRock)
        if(canDrop) currentRock = moveRock(down,currentRock)
        else {
          // rock comes to a stop
          rockPile.addAll(currentRock)
          val newBottom = Math.max(currentBottom,currentRock.map{case (_,y) => y}.max)
          currentBottom = newBottom
        }

        jetIndex += 1
        jetIndex %= jetStream.length
      }

      rockIndex += 1
      rockIndex %= rocks.length
      //if((1 to 7).forall(x => rockPile.contains((x,currentBottom)))) println(s"********** $step    $currentBottom") // use this to find out where rock pile starts repeating
    }
    currentBottom

  }
}
