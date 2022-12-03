object Day03 extends CommonPuzzle (3) {

  override def partOne: Any = inputLines.map(line => priority(commonChar2(line.substring(0,line.length / 2), line.substring(line.length / 2)))).sum

  override def partTwo: Any =  inputLines.sliding(3,3).toList.map(it => priority(commonChar3(it.head,it.tail.head,it.last))).sum


  def commonChar2(str1: String, str2: String): Char = str1.intersect(str2).toCharArray()(0)

  def commonChar3(str1: String, str2: String, str3 : String): Char = str1.intersect(str2).intersect(str3).toCharArray()(0)

  def priority(chr : Char) : Int =  if(chr.isLower) chr.toInt - 96 else chr.toInt - 38

}
