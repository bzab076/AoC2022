import scala.io.Source

abstract class CommonPuzzle(day : Int) {

  private val resourceName = f"input$day%02d.txt"

  def partOne : Any
  def partTwo : Any

  def inputLines: List[String] =
    Source.fromResource(resourceName).getLines().toList

  def inputNumbers: List[Int] = inputLines.map(it => it.toInt)

  def inputString: String = inputLines.head

  def inputAsArrayOfDigits: Array[Array[Int]] = {

    val xSize: Int = inputLines.head.length
    val ySize: Int = inputLines.length
    val result = Array.ofDim[Int](ySize, xSize)

    var y = 0
    for(line <- inputLines) {
      var x = 0
      for(char <- line.toList) {
        result(y)(x) = char - 48
        x += 1
      }
      y += 1
    }

    result
  }

}