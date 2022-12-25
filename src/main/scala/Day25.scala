object Day25 extends CommonPuzzle (25) {

  override def partOne: Any = inputLines.foldLeft("0")((acc, num) => snafuAdd(acc,num))

  override def partTwo: Any = "Elf provided the star fruit number 50."

  def snafu2digit(snafuDigit : Char) : Int = if(snafuDigit.isDigit) snafuDigit.toInt - 48 else if(snafuDigit=='-') -1 else -2
  def digit2snafu(digit : Int) : String = if(digit == -2) "=" else if (digit == -1 ) "-" else digit.toString
  def addLeadingZeroes(number : String, len : Int) : String = if(number.length >= len) number else addLeadingZeroes("0" + number, len)

  // adds two SNAFU numbers
  def snafuAdd (operand1 : String, operand2 : String) : String = {

    val len = Math.max(operand1.length,operand2.length) + 1
    val op1 : Array[Char] = addLeadingZeroes(operand1,len).toCharArray.reverse
    val op2 : Array[Char] = addLeadingZeroes(operand2,len).toCharArray.reverse

    val sb : StringBuilder = new StringBuilder("")
    var overflow : Int = 0
    for(i <- 0 until len-1) {
      val sum : Int = snafu2digit(op1(i)) + snafu2digit(op2(i)) + overflow
      var digit = 0
      if(sum>2) {
        overflow = 1
        digit = sum - 5
      }
      else if(sum < -2) {
        overflow = -1
        digit = sum + 5
      }
      else {
        overflow = 0
        digit = sum
      }
      sb.addAll(digit2snafu(digit))
    }
    sb.addAll(digit2snafu(overflow))
    val result = sb.toString().reverse
    if(result.startsWith("0")) result.tail else result
  }

}
