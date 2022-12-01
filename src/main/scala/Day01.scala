import scala.collection.mutable.ArrayBuffer

object Day01 extends  CommonPuzzle (1) {

  val calorieList : List[Int] = getCalories

  override def partOne: Any = calorieList.max

  override def partTwo: Any = calorieList.sorted.reverse.take(3).sum

  def getCalories : List[Int] = {
    val cals: ArrayBuffer[Int] = ArrayBuffer[Int]()
    var sum = 0
    for (line <- inputLines) {
      if (line.trim.isEmpty) {
        cals.addOne(sum)
        sum = 0
      }
      else sum += line.toInt
    }
    cals.addOne(sum)
    cals.toList
  }


}
