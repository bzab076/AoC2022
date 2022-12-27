import scala.annotation.tailrec
import scala.collection.mutable

object Day24 extends CommonPuzzle (24) {

  val maxX: Int = inputLines.head.length - 1
  val minX: Int = 0
  val maxY: Int = inputLines.length - 1
  val minY: Int = 0
  var blizzards : List[(Int,Int,Char)] = parseBlizzards()
  val moves : List[(Int,Int)] = List((-1,0),(1,0),(0,1),(0,-1))
  val start: (Int, Int) = (1,0)
  val end: (Int, Int) = (maxX-1,maxY)

  override def partOne: Any = moveTroughValley(1, Set(start), end)

  override def partTwo: Any = {

    // reset blizzards to initial condition
    blizzards = parseBlizzards()
    val trip1 = moveTroughValley(1, Set(start), end)
    val trip2 = moveTroughValley(1, Set(end), start)
    val trip3 = moveTroughValley(1, Set(start), end)
    trip1 + trip2 + trip3
  }

  @tailrec
  def moveTroughValley(time : Int, positions : Set[(Int,Int)], goal : (Int,Int)) : Int = {

    if(positions.contains(goal)) return time - 1

    // move blizzards to find out where they will be next minute
    blizzards = blizzards.map(b => moveBlizzard(b))
    val blizzardPositions = blizzards.map(b => (b._1,b._2))

    val newPositions: mutable.Set[(Int, Int)] = mutable.Set.empty
    for(pos <- positions) {
      val legalMoves = moves.map{case (dx,dy) => (pos._1+dx,pos._2+dy)}.filter(p => !blizzardPositions.contains(p))
        .filter(p => (p._1>minX && p._1<maxX && p._2>minY && p._2<maxY) || p==goal)
      newPositions.addAll(legalMoves)
    }

    // consider staying in position
    for(pos <- positions) {
      if(!blizzardPositions.contains(pos)) newPositions.addOne(pos)
    }

    if(newPositions.contains(goal)) return time
    moveTroughValley(time+1,newPositions.toSet, goal)
  }

  def moveBlizzard(blizz : (Int, Int, Char)) : (Int, Int, Char) = {

    var x = blizz._1
    var y = blizz._2

    blizz._3 match {
      case '<' => x -= 1
      case '>' => x += 1
      case '^' => y -= 1
      case 'v' => y += 1
    }

    if(x == minX) x = maxX - 1
    if(x == maxX) x = minX + 1
    if(y == minY) y = maxY - 1
    if(y == maxY) y = minY + 1

    (x,y,blizz._3)
  }

  def parseBlizzards() : List[(Int,Int,Char)] = {

    val blizz: Set[Char] = Set('<','>','^','v')
    val blizzList : mutable.ArrayBuffer[(Int,Int,Char)] = mutable.ArrayBuffer.empty
    var y=0
    for(line <- inputLines) {
      for(x <- 0 until line.length) {
        if(blizz.contains(line.charAt(x)))  blizzList.addOne((x,y, line.charAt(x)))
      }
      y += 1
    }
    blizzList.toList
  }
}
