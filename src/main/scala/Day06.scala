object Day06 extends CommonPuzzle (6) {

  override def partOne: Any = distinctSubstringIndex(4)

  override def partTwo: Any = distinctSubstringIndex(14)

  def distinctSubstringIndex(chunkSize : Int) : Int = inputString.toList.sliding(chunkSize).zipWithIndex.toList.filter{ case (l,i) => l.distinct.length == chunkSize}.head._2 + chunkSize

}
