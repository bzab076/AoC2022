import scala.collection.mutable

object Day11 extends CommonPuzzle (11) {

  var monkeys : mutable.ArraySeq[Monkey] = mutable.ArraySeq()  // array of monkeys
  var divisor : Int = Int.MaxValue // to keep worry levels manageable

  override def partOne: Any = {
    parseInput()
    for(round <-  1 to 20)
      for(monkey <- monkeys)
        monkey.turn(true)

    monkeys.toList.map(m => m.activity).sorted.reverse.take(2).product
  }

  override def partTwo: Any = {
    monkeys = mutable.ArraySeq()  // initialize monkeys
    parseInput()
    for(round <-  1 to 10000)
      for (monkey <- monkeys)
        monkey.turn(false)

    monkeys.toList.map(m => m.activity.toLong).sorted.reverse.take(2).product
  }

  def parseInput() : Unit = {

    inputLines.sliding(6,7).toList.foreach { m =>
      val mindex = m.take(1).head.substring(7, 8).toInt  // not needed
      val q = mutable.Queue[Long]()
      m.slice(1, 2).head.trim.split("Starting items: ")(1).split(", ").toList.map(it => q.enqueue(it.toInt))
      val op1 = m.slice(2, 3).head.trim.substring(21, 22).charAt(0)
      val op2 = if(m.slice(2, 3).head.trim.substring(23) == "old") 0 else m.slice(2, 3).head.substring(25).toInt
      val tst = m.slice(3, 4).head.trim.substring(19).toInt
      val t = m.slice(4, 5).head.trim.split(" throw to monkey ")(1).toInt
      val f = m.slice(5, 6).head.trim.split(" throw to monkey ")(1).toInt
      val monkey : Monkey = new Monkey((op1,op2),q,tst,(t,f))
      monkeys = monkeys.appended(monkey)
    }

    divisor = monkeys.toList.map { mk => mk.getTestValue }.product
  }

  class Monkey(operation : (Char,Int), items : mutable.Queue[Long], test : Int, throwTo : (Int,Int), var activity : Int = 0) {

    def receiveItem(item : Long): Unit = items.enqueue(item)

    def turn(part1 : Boolean): Unit = {
      while(items.nonEmpty) {
        var item = items.dequeue()
        val operand = if(operation._2 == 0) item else operation._2
        operation._1 match {
          case '*' => item *= operand
          case '+' => item += operand
        }
        if(part1) item /= 3 else item = item % divisor
        if(item % test == 0) monkeys(throwTo._1).receiveItem(item) else monkeys(throwTo._2).receiveItem(item)
        activity += 1
      }
    }

    def getTestValue : Int = test
  }
}
