import scala.collection.mutable

object Day23 extends CommonPuzzle (23) {

  var elvesPositions : List[(Int,Int)] = parseInput()
  val adjacent : List[(Int,Int)] = List((-1,0),(1,0),(-1,1),(1,1),(0,-1),(0,1),(1,-1),(-1,-1))
  val adjacentN : List[(Int,Int)] = List((0,-1),(1,-1),(-1,-1))
  val adjacentS : List[(Int,Int)] = List((-1,1),(1,1),(0,1))
  val adjacentW : List[(Int,Int)] = List((-1,0),(-1,1),(-1,-1))
  val adjacentE : List[(Int,Int)] = List((1,0),(1,1),(1,-1))


  override def partOne: Any = {

    for(round <- 0 until 10) simulateMove(round)

    val maxX = elvesPositions.map{case(x,_) => x}.max
    val minX = elvesPositions.map{case(x,_) => x}.min
    val maxY = elvesPositions.map{case(_,y) => y}.max
    val minY = elvesPositions.map{case(_,y) => y}.min

    (maxX-minX+1) * (maxY-minY+1) - elvesPositions.size
  }

  override def partTwo: Any = {

    // reset elves positions to original positions
    elvesPositions = parseInput()
    var moved = true
    var round = 1
    while(moved) {
      moved = simulateMove(round-1)
      round += 1
    }
    round
  }

  // returns whether or not elves have moved
  def simulateMove(round : Int): Boolean = {
    val proposedMoves = elvesPositions.map(pt => (pt, proposedMove(pt,elvesPositions,round)))
    val actualMoves = proposedMoves.map{case (p1,p2) => if(isDuplicatedMove(p2,proposedMoves)) p1 else p2}
    elvesPositions = actualMoves
    !elvesPositions.map(pos => (!existsAdjacent(pos,elvesPositions,adjacent))).forall(con => con)
  }

  def isDuplicatedMove(pos :(Int,Int), list: List[((Int,Int),(Int,Int))]) : Boolean =
    list.map { case (_, m) => m }.count(p => p == pos) > 1

  def proposedMove (pos :(Int,Int), list: List[(Int,Int)], direction : Int) : (Int,Int) = {

    if(!existsAdjacent(pos,list,adjacent)) return pos

    for(dirDelta <- 0 to 4) {
      val newDir = (direction + dirDelta) % 4
      newDir match {
        case 0 => if(!existsAdjacent(pos,list,adjacentN)) return (pos._1,pos._2 - 1)
        case 1 => if(!existsAdjacent(pos,list,adjacentS)) return (pos._1,pos._2 + 1)
        case 2 => if(!existsAdjacent(pos,list,adjacentW)) return (pos._1 - 1,pos._2)
        case 3 => if(!existsAdjacent(pos,list,adjacentE)) return (pos._1 + 1,pos._2)
      }
    }
    pos

  }

  def existsAdjacent(pos :(Int,Int), list: List[(Int,Int)], adjList : List[(Int,Int)] ) : Boolean =
    adjList.map{case (x,y) => (x+pos._1,y+pos._2)}.map(pt => list.contains(pt)).foldLeft(false)((acc,con) => acc || con)


  def parseInput(): List[(Int, Int)] = {

    val inputList: mutable.ArrayBuffer[(Int, Int)] = mutable.ArrayBuffer.empty
    var y = 0
    for(line <- inputLines) {
      for(x <- 0 until line.length)
        if(line.charAt(x)=='#') inputList.addOne((x,y))
      y += 1
    }
    inputList.toList
  }

  def printPositions() : Unit = {

    val maxX = elvesPositions.map{case(x,_) => x}.max
    val minX = elvesPositions.map{case(x,_) => x}.min
    val maxY = elvesPositions.map{case(_,y) => y}.max
    val minY = elvesPositions.map{case(_,y) => y}.min

    for(y <- minY to maxY){
      for (x <- minX to maxX)
        if(elvesPositions.contains((x,y))) print("#") else print(".")
      println()
    }
    println()
  }
}
