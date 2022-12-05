import scala.collection.mutable

object Day05 extends CommonPuzzle (5) {

  val stacks = new Array[mutable.Stack[Char]](9)  // initial stacks
  var moves: List[(Int, Int, Int)] = List.empty  // list of moves

  override def partOne: Any = {
    parseInput()
    for (move <- moves) {
      for (_ <- 1 to move._1) {
        val elem = stacks(move._2 - 1).pop()
        stacks(move._3 - 1).push(elem)
      }
    }
    getTopOfStacks
  }

  override def partTwo: Any = {
    parseInput()
    for (move <- moves) {
      val tempStack = new mutable.Stack[Char]()
      for (_ <- 1 to move._1) {
        val elem = stacks(move._2 - 1).pop()
        tempStack.push(elem)
      }
      for(_ <- 1 to move._1) {
        stacks(move._3 - 1).push(tempStack.pop())
      }
    }
    getTopOfStacks
  }

  def parseInput(): Unit = {

    for (i <- 0 to 8) {
      stacks(i) = new mutable.Stack[Char]
    }

    // load stacks
    for (line <- inputLines.take(8).reverse) {
      for (i <- 0 to 8) {
        if (line.charAt(i * 4 + 1).isUpper) stacks(i).push(line.charAt(i * 4 + 1))
      }
    }

    // parse moves
    moves = inputLines.drop(10).map { case s"move $n from $o to $d" => (n.toInt, o.toInt, d.toInt)}
  }

  // prints top of each stack as string
  def getTopOfStacks: String = {
    val sb = new StringBuilder("")
    for (i <- 0 to 8) {
      sb += stacks(i).pop()
    }
    sb.toString()
  }

}
