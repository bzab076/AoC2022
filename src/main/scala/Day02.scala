object Day02 extends CommonPuzzle (2) {


  override def partOne: Any = inputLines.map(it => it.split(" ")).map(it => score1(it(0),it(1))).sum

  override def partTwo: Any = inputLines.map(it => it.split(" ")).map(it => score2(it(0),it(1))).sum

  def score1(move1 : String, move2 : String) : Int =

     move1 match {  // rock
      case "A" => move2 match {
        case "X"  => 4 // rock
        case "Y" => 8  // paper
        case "Z" => 3  // scissors
      }
      case "B" => move2 match {  // paper
        case "X"  => 1  // rock
        case "Y" => 5  // paper
        case "Z" => 9  // scissors
      }
      case "C" => move2 match { // scissors
        case "X"  => 7  // rock
        case "Y" => 2   // paper
        case "Z" => 6   // scissors
      }
      case _  => 0
    }


  def score2 (move1 : String, winner : String) : Int =

    move1 match {  // rock
      case "A" => winner match {
        case "X"  => 3  // lose
        case "Y" => 4   // draw
        case "Z" => 8   // win
      }
      case "B" => winner match { // paper
        case "X"  => 1 // lose
        case "Y" => 5  // draw
        case "Z" => 9
      }
      case "C" => winner match { // scissors
        case "X"  => 2 // lose
        case "Y" => 6  // draw
        case "Z" => 7  // win
      }
      case _  => 0
    }

}
