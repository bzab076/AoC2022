object Day20 extends CommonPuzzle (20) {

  override def partOne: Any = {
    val file1: List[(Long, Int)] = inputNumbers.map(n => n.toLong).zipWithIndex
    getResult(shuffle(file1))
  }

  override def partTwo: Any = {
    val file2: List[(Long, Int)] = inputNumbers.map(n => n*811589153L).zipWithIndex
    var currentFile = file2
    for(_ <- 1 to 10)
      currentFile = shuffle(currentFile)

    getResult(currentFile)
  }

  private def getResult(file2: List[(Long, Int)]) = {
    val zeroIndex = file2.filter{case(n,_) => n==0}.head._2
    val n1 = file2.filter{case (_,i) => i == ((1000 + zeroIndex) % file2.size)}.head._1
    val n2 = file2.filter{case (_,i) => i == ((2000 + zeroIndex) % file2.size)}.head._1
    val n3 = file2.filter{case (_,i) => i == ((3000 + zeroIndex) % file2.size)}.head._1
    n1 + n2 + n3
  }

  def shuffle(originalFile : List[(Long, Int)]) : List[(Long, Int)] = {

    var shuffled : List[(Long, Int, Int)] = originalFile.map{case(n,i) => (n,i,i)} // we keep two indices, original and new, because numbers in list are not unique
    for((num,idx) <- originalFile) {
      val oldIdx = shuffled.filter { case (n, oi, _) => n == num && oi == idx}.head._3
      var newIdx = (oldIdx+num) % (originalFile.size - 1) //else oldIdx+num-1 % (originalFile.size -1)  .. if(num>=0)
      if (newIdx <= 0) {
        newIdx += originalFile.size - 1
      }

      if(oldIdx < newIdx)
        shuffled = shuffled.map{case (n,o,i) => if(i>oldIdx && i<=newIdx) (n,o,i-1) else (n,o,i)}
      else if (oldIdx > newIdx)
        shuffled = shuffled.map{case (n,o,i) => if(i>=newIdx && i<oldIdx) (n,o,i+1) else (n,o,i)}

      shuffled = shuffled.map{case(n,o,i) => if(n==num && o==idx) (n,o,newIdx.toInt) else (n,o,i)}
    }

    shuffled.map{case (n,_,i) => (n,i)}
  }

}
