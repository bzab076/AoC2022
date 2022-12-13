object Day13 extends CommonPuzzle (13) {

  override def partOne: Any = inputLines.sliding(2,3).toList
    .map(block => compareSignals(parseLine(trimBrackets(block.head)),parseLine(trimBrackets(block.tail.head))))
    .zipWithIndex.filter{case (c,_) => c == -1}
    .map{ case (_,i) => i+1}
    .sum


  override def partTwo: Any = {

    val allPackets = inputLines.filter(line => line.trim.nonEmpty).map(line => parseLine(trimBrackets(line)))
    val allPacketsWithOrder = allPackets.map(pck => (allPackets.filter(p => p != pck).map(p2 => compareSignals(pck, p2)).count(v => v == -1),pck))
    val sortedPackets = allPacketsWithOrder.sortWith(_._1 > _._1).map{case (_,b) => b}
    val div1 = parseLine(trimBrackets("[[2]]"))
    val div2 = parseLine(trimBrackets("[[6]]"))

    var index : Int = 1
    var index1 : Int = 0
    var index2 : Int  = 0
    for(pck <- sortedPackets) {
      if(compareSignals(div1,pck) == -1 && index1==0) index1 = index
      if(compareSignals(div2,pck) == -1 && index2==0) index2 = index + 1
      index += 1
    }

    index1*index2
  }


  def compareSignals(list1 : List[Element], list2 : List[Element]) : Int ={

    var result = 0
    if(list1.isEmpty && list2.isEmpty) return 0
    else if(list1.isEmpty)  return -1
    else if(list2.isEmpty) return 1

    val el1 : Element = list1.head
    val el2 : Element = list2.head

    if(el1.value<Int.MaxValue && el2.value<Int.MaxValue) {   // both integers
      if(el1.value < el2.value) result =  -1  // -1 first is smaller
      else if(el1.value > el2.value) result =  1 // 1 first is bigger
      else  result =  0  // 0 equal
    }
    else if(el1.value==Int.MaxValue && el2.value==Int.MaxValue) result = compareSignals(el1.list, el2.list)  // list to list
    else if(el1.value<Int.MaxValue && el2.value==Int.MaxValue) result = compareSignals(List(el1),el2.list)  // integer to list
    else if(el2.value<Int.MaxValue && el1.value==Int.MaxValue) result = compareSignals(el1.list,List(el2))  // list to integer
    else result=0 //must not happen

    if(result !=0 ) result
    else compareSignals(list1.tail,list2.tail)
  }

  def parseLine(line : String) : List[Element] = {

    if(line.isEmpty) return List.empty

    if(!line.contains('[') && !line.contains(']') && !line.contains(',')) return List(Element(line.trim.toInt,List.empty))    // value

    if(!line.startsWith("[") ) { // head is an integer
      val v = line.substring(0,line.indexOf(',')).trim.toInt
      val head = Element(v, List.empty)
      val tail = parseLine(line.substring(line.indexOf(',')+1))
      tail.prepended(head)
    }
    else if(line.startsWith("[") && !line.endsWith("]")) { // tail is an integer
      val head = parseLine(line.substring(0,line.lastIndexOf(',')))
      val v =  line.substring(line.lastIndexOf(',')+1).trim.toInt
      val tail = Element(v, List.empty)
      head.appended(tail)
    }
    else if (line.startsWith("[") && line.endsWith("]")) { // head is a list
      val fe = firstEnclosing(line)
      if(fe._2.isEmpty)
        List(Element(Int.MaxValue,parseLine(trimBrackets(line))))
      else {
        val head = Element(Int.MaxValue,parseLine(trimBrackets(fe._1)))
        val tail = parseLine(fe._2)
        tail.prepended(head)
      }
    }
    else List.empty    // should never happen
  }

  // gets first element in line closed with brackets
  // returns (element , remainder)
  def firstEnclosing(str : String) : (String,String) = {
    if(!str.startsWith("[")) return ("","")
    var bracketCount = 1
    for(i <- 1 until str.length) {
      if(str.charAt(i)=='[') bracketCount += 1
      if(str.charAt(i)==']') bracketCount -= 1
      if(bracketCount == 0)
        return (str.substring(0,i+1),str.substring(math.min(i+2,str.length)))
    }
    (str,"")
  }

  def trimBrackets(line : String) : String = line.substring(1,line.length-1)

  case class Element(value : Int, list: List[Element])

}
