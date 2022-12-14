import scala.collection.mutable

object Day14 extends CommonPuzzle (14) {

  var rock : mutable.Set[(Int,Int)] = mutable.Set.empty
  var sand : mutable.Set[(Int,Int)] = mutable.Set.empty
  val start: (Int, Int) = (500,0)

  override def partOne: Any = {
    parseInput()
    simulateSandFalling(false)
    sand.size
  }

  override def partTwo: Any = {

    // initialize
    rock = mutable.Set.empty
    sand = mutable.Set.empty
    parseInput()

    simulateSandFalling(true)
    sand.size
  }

  def parseInput() : Unit = {

    for(line <- inputLines) {
      val coords = line.split(" -> ").map(it => it.split(",")).map(it => (it.head.toInt, it.tail.head.toInt))
      val pairs = coords.sliding(2).map(it => (it.head, it.tail.head))
      for(pair <- pairs) {
        if(pair._1._1 == pair._2._1) {
          val x = pair._1._1
          val from = Math.min(pair._1._2,pair._2._2)
          val to = Math.max(pair._1._2,pair._2._2)
          for(y <- from to to) rock.addOne((x,y))
        }
        else {
          val y = pair._1._2
          val from = Math.min(pair._1._1,pair._2._1)
          val to = Math.max(pair._1._1,pair._2._1)
          for(x <- from to to) rock.addOne((x,y))
        }
      }

    }
  }

  def simulateSandFalling(part2 : Boolean) : Unit = {

    var infinity = false
    val maxY = rock.map{case (_,y) => y}.max
    val floor = maxY + 2
    var currentPosition = start
    var added = false

    while(!infinity && !sand.contains(start)) {

      var newPosition = (currentPosition._1, currentPosition._2 + 1)
      if (!rock.contains(newPosition) && !sand.contains(newPosition) && (!part2 || newPosition._2<floor)) {
        currentPosition = newPosition
      }
      else {
        newPosition = (currentPosition._1 - 1, currentPosition._2 + 1)
        if (!rock.contains(newPosition) && !sand.contains(newPosition) && (!part2 || newPosition._2<floor)) {
          currentPosition = newPosition
        }
        else {
          newPosition = (currentPosition._1 + 1, currentPosition._2 + 1)
          if (!rock.contains(newPosition) && !sand.contains(newPosition) && (!part2 || newPosition._2<floor)) {
            currentPosition = newPosition
          }
          else { // cannot move
            sand.addOne(currentPosition)
            currentPosition = start
            added = true
          }
        }

      }

      if(!added) {
        currentPosition = newPosition
        if(!part2) infinity = newPosition._2 > maxY
      }
      else added = false

    }
  }

}
