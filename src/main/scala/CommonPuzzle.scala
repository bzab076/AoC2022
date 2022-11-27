import scala.io.Source

abstract class CommonPuzzle(day : Int) {

  private val resourceName = f"input$day%02d.txt"

  def partOne : Any
  def partTwo : Any

  def inputLines =
    Source.fromResource(resourceName).getLines().toList

  def inputNumbers = inputLines.map(it => it.toInt)

  def inputString: String = inputLines.head

}