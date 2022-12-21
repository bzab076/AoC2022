import scala.collection.mutable

object Day21 extends CommonPuzzle (21) {

  var allMonkeys : mutable.Map[String, Monkey] = parseInput()

  override def partOne: Any = yellNumber("root")._1

  override def partTwo: Any = findNumber("root", 0L)


  def parseInput() : mutable.Map[String, Monkey] = {

    val monkeyMap: mutable.Map[String, Monkey] = mutable.Map.empty

    for(line <- inputLines) {
      var monkey : Monkey = null
      val id = line.split(": ")(0)
      val tail = line.split(": ")(1).trim
      if(tail.trim.charAt(0).isLower) {
        val tails = tail.split(" ")
        monkey = Monkey(id,Long.MaxValue,tails(1)(0), Array(tails(0),tails(2)))
      }
      else {
        monkey = Monkey(id,tail.toLong,'.', Array.empty)
      }
      monkeyMap.addOne(monkey.id, monkey)
    }

    monkeyMap
  }

  def yellNumber(id : String) : (Long,Boolean) = {

    val monkey : Monkey = allMonkeys.getOrElse(id,null)
    if(monkey!=null && monkey.operands.isEmpty) return (monkey.number,id=="humn")

    val (op1,h1) = yellNumber(monkey.operands(0))
    val (op2,h2) = yellNumber(monkey.operands(1))

    val result : Long = monkey.operation match {
      case '+' => op1 + op2
      case '-' => op1 - op2
      case '*' => op1 * op2
      case '/' => op1 / op2
    }

    (result,h1 || h2)
  }


  def findNumber(id : String, target : Long) : Long = {

    if(id=="humn") return target

    val monkey : Monkey = allMonkeys.getOrElse(id,null)
    if(monkey!=null && monkey.operands.isEmpty)  return monkey.number

    val (op1,human1) = yellNumber(monkey.operands(0))
    val (op2,human2) = yellNumber(monkey.operands(1))

    if(id=="root") return if(human1) findNumber(monkey.operands(0),op2) else findNumber(monkey.operands(1),op1)

    if(human1 || human2) {
      var targetId = monkey.operands(0)
      var opr = op2
      if(human2) {
        targetId = monkey.operands(1)
        opr = op1
      }

      monkey.operation match {
        case '+' => findNumber(targetId,target - opr)
        case '-' => if(human2) findNumber(targetId,opr - target) else findNumber(targetId,opr + target)
        case '*' => findNumber(targetId,target / opr)
        case '/' => if(human2) findNumber(targetId,opr / target ) else findNumber(targetId,target * opr)
      }

    }
    else 0  // should not happen
  }


  case class Monkey(id : String, number : Long, operation : Char, operands : Array[String])
}
