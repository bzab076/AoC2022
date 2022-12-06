object Day06 extends CommonPuzzle (6) {

  override def partOne: Any = getDistinctSubstringIndex(inputString,4)

  override def partTwo: Any = getDistinctSubstringIndex(inputString,14)

  def getDistinctSubstringIndex(str : String, chunkSize : Int) : Int = {

    var i = 0
    while(i < str.length - chunkSize) {
      val substr = inputString.substring(i, i + chunkSize)
      if(substr.toCharArray.toList.distinct.size == chunkSize) return i+chunkSize
      i += 1
    }
    0 // default result if no distinct substring is found

  }
}
