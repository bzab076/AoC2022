object Day10 extends CommonPuzzle (10) {

  override def partOne: Any = executeInstructions

  def executeInstructions: Int = {

    var register = 1
    var cycle = 0
    var result = 0
    var cycleCheck = 19
    var pixelPosition = 0

    def drawPixel(): Unit = {
      pixelPosition = cycle  % 40
      if (Math.abs(pixelPosition - register) < 2) print("#") else print(".")
      if (pixelPosition % 40 == 39) println()
    }

    def updateResult(): Unit = {
      if (cycleCheck == cycle) {
        result += (cycleCheck + 1) * register
        if (cycleCheck < 220) cycleCheck += 40
      }
    }

    for(line <- inputLines) {

      updateResult()
      drawPixel()

      if(line.startsWith("noop")) {
        cycle += 1
      }
      else {
        val increment :Int = line.split(" ")(1).toInt
        cycle += 1
        updateResult()
        drawPixel()
        cycle += 1
        register += increment
      }
    }

    println()
    result
  }

  override def partTwo: Any = "see drawing in part one"
}
