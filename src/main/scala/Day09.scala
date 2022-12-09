import scala.collection.mutable.Set

object Day09 extends CommonPuzzle (9) {

  var headPos : (Int,Int) = (0,0)
  var tailPos : (Int,Int) = (0,0)
  var knots : Array[(Int,Int)] = Array((0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0))
  val tailPositions1 : Set[(Int,Int)] = Set((0,0))
  val tailPositions2 : Set[(Int,Int)] = Set((0,0))

  val moves: Seq[(Char, Int)] = inputLines.map(line => line.split(" ")).map(a => (a(0).charAt(0),a(1).toInt))

  override def partOne: Any = {

    for(move <- moves) {
      for(_ <- 1 to move._2) {
        simulateMove1(move._1)
      }
    }
    tailPositions1.size
  }

  override def partTwo: Any = {

    for(move <- moves) {

      for(_ <- 1 to move._2) {
        for(i <- 1 to 9)
        simulateMove2(move._1,i)
      }

    }
    tailPositions2.size
  }



  def simulateMove1(direction : Char ) : Unit = {

    var newHead : (Int,Int) = headPos
    direction match {
      case 'U' => newHead = (headPos._1, headPos._2 + 1)
      case 'D' => newHead = (headPos._1, headPos._2 - 1)
      case 'L' => newHead = (headPos._1 - 1, headPos._2)
      case 'R' => newHead = (headPos._1 + 1, headPos._2)
    }

    headPos = newHead
    if(headPos._2 == tailPos._2) {  // move in x dimension if required
      if (headPos._1 - tailPos._1 > 1) tailPos = (tailPos._1 + 1, tailPos._2)
      else if (headPos._1 - tailPos._1 < -1) tailPos = (tailPos._1 - 1, tailPos._2)
    }
    else if(headPos._1 == tailPos._1) { // move in y dimension if required
      if (headPos._2 - tailPos._2 > 1) tailPos = (tailPos._1, tailPos._2 + 1)
      else if (headPos._2 - tailPos._2 < -1) tailPos = (tailPos._1, tailPos._2 -1)
    }
    else { //move diagonally if required
      if(Math.abs(headPos._1-tailPos._1) > 1 || Math.abs(headPos._2 - tailPos._2) > 1) {

        val newTail1 = (tailPos._1 + 1, tailPos._2 + 1)
        val newTail2 = (tailPos._1 + 1, tailPos._2 - 1)
        val newTail3 = (tailPos._1 - 1, tailPos._2 + 1)
        val newTail4 = (tailPos._1 - 1, tailPos._2 - 1)

        if(Math.abs(headPos._1-newTail1._1) <= 1 && Math.abs(headPos._2 - newTail1._2) <= 1) tailPos = newTail1
        else if(Math.abs(headPos._1-newTail2._1) <= 1 && Math.abs(headPos._2 - newTail2._2) <= 1) tailPos = newTail2
        else if(Math.abs(headPos._1-newTail3._1) <= 1 && Math.abs(headPos._2 - newTail3._2) <= 1) tailPos = newTail3
        else if(Math.abs(headPos._1-newTail4._1) <= 1 && Math.abs(headPos._2 - newTail4._2) <= 1) tailPos = newTail4

      }
    }
    tailPositions1.addOne(tailPos)
  }


  def simulateMove2(direction : Char, knotIndex : Int) : Unit = {

    if(knotIndex<1 || knotIndex > 9) return // illegal move

    var newHead : (Int,Int) = (0,0)
    val oldHead = knots(knotIndex-1)

    if(knotIndex==1) {
      direction match {
        case 'U' => newHead = (oldHead._1, oldHead._2 + 1)
        case 'D' => newHead = (oldHead._1, oldHead._2 - 1)
        case 'L' => newHead = (oldHead._1 - 1, oldHead._2)
        case 'R' => newHead = (oldHead._1 + 1, oldHead._2)
      }
      knots(knotIndex-1) = newHead
    }
    else newHead = oldHead

    val oldTail = knots(knotIndex)
    var newTail = oldTail

    if(newHead._2 == oldTail._2) { // move in x dimension if required
      if (newHead._1 - oldTail._1 > 1) newTail = (oldTail._1 + 1, oldTail._2)
      else if (newHead._1 - oldTail._1 < -1) newTail = (oldTail._1 - 1, oldTail._2)
    }
    else if(newHead._1 == oldTail._1) { // move in y dimension if required
      if (newHead._2 - oldTail._2 > 1) newTail = (oldTail._1, oldTail._2 + 1)
      else if (newHead._2 - oldTail._2 < -1) newTail = (oldTail._1, oldTail._2 -1)
    }
    else { //move diagonally if required
      if(Math.abs(newHead._1-oldTail._1) > 1 || Math.abs(newHead._2 - oldTail._2) > 1) {

        // try all 4 directions
        val newTail1 = (oldTail._1 + 1, oldTail._2 + 1)
        val newTail2 = (oldTail._1 + 1, oldTail._2 - 1)
        val newTail3 = (oldTail._1 - 1, oldTail._2 + 1)
        val newTail4 = (oldTail._1 - 1, oldTail._2 - 1)

        if(Math.abs(newHead._1-newTail1._1) <= 1 && Math.abs(newHead._2 - newTail1._2) <= 1) newTail = newTail1
        else if(Math.abs(newHead._1-newTail2._1) <= 1 && Math.abs(newHead._2 - newTail2._2) <= 1) newTail = newTail2
        else if(Math.abs(newHead._1-newTail3._1) <= 1 && Math.abs(newHead._2 - newTail3._2) <= 1) newTail = newTail3
        else if(Math.abs(newHead._1-newTail4._1) <= 1 && Math.abs(newHead._2 - newTail4._2) <= 1) newTail = newTail4
      }
    }

    knots(knotIndex) = newTail
    if(knotIndex==9) tailPositions2.addOne(newTail)
  }
}
