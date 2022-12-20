import scala.annotation.tailrec
import scala.collection.mutable


object Day19 extends CommonPuzzle (19) {

  // Warning: This is very inefficient solution. It need a lot of optimization and improvement.

  val blueprints: List[Blueprint] = inputLines.map{ case s"Blueprint $id: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $obs1 ore and $obs2 clay. Each geode robot costs $geode1 ore and $geode2 obsidian."
      => Blueprint(id.toInt, ore.toInt, clay.toInt, (obs1.toInt, obs2.toInt), (geode1.toInt, geode2.toInt))}

  var stateCache : mutable.Map[(Int,State), mutable.Set[State]] = mutable.Map.empty

  override def partOne: Any = blueprints.map(bp => bp.id*qualityLevel(24,bp, mutable.Set(State(0,0,0,0,1,0,0,0)))).sum

  override def partTwo: Any = blueprints.take(3).map(bp => qualityLevel(32, bp, mutable.Set(State(0, 0, 0, 0, 1, 0, 0, 0)))).product

  @tailrec
  def qualityLevel (minute : Int, blueprint: Blueprint, states : mutable.Set[State]) : Int = {

    if(minute==0) {
      val result =  states.map(s => s.geode).max
      println(s"${blueprint.id}  ->  $result")
      return result
    }

    if(minute%3 ==0) println(s"$minute  -  size " + states.size)

    var newStates : mutable.Set[State] = mutable.Set.empty
    newStates = states.foldLeft(newStates)((acc, st) => acc.union(forkStates(blueprint,st)))

    // TODO - find a better solution to keep number of states low
    if(newStates.size > 20000) newStates = mutable.Set() ++ newStates.toList.sortWith(orderStates2).take(20000)

    qualityLevel(minute-1, blueprint, newStates)
  }


  def forkStates(blueprint: Blueprint, oldState : State) : mutable.Set[State] = {

    var result = stateCache.getOrElse((blueprint.id,oldState),mutable.Set.empty)
    if(result.isEmpty) {
      result = collectMaterial(oldState, buildRobots(blueprint,oldState))
      stateCache.addOne((blueprint.id,oldState),result)
    }
    result
  }

  def collectMaterial (oldState : State, newStates : mutable.Set[State]) : mutable.Set[State] =
    newStates.map(s => State(s.ore + oldState.oreRobot,s.clay + oldState.clayRobot,s.obsidian + oldState.obsidianRobot,s.geode + oldState.geodeRobot,s.oreRobot,s.clayRobot,s.obsidianRobot,s.geodeRobot ))

  def buildRobots(blueprint: Blueprint, initialState : State) : mutable.Set[State] = {

    val newStates : mutable.Set[State] = mutable.Set.empty
    var canBoth : Boolean = false
    var canClay : Boolean = false
    var canOre : Boolean = false

    if(initialState.obsidian >= blueprint.geode._2 && initialState.ore >= blueprint.geode._1) {
      val newState = State(initialState.ore - blueprint.geode._1, initialState.clay, initialState.obsidian - blueprint.geode._2,initialState.geode,initialState.oreRobot,initialState.clayRobot,initialState.obsidianRobot, initialState.geodeRobot+1)
      newStates.addOne(newState)
    }

    else if (initialState.clay >= blueprint.obsidian._2 && initialState.ore >= blueprint.obsidian._1) {
      val newState = State(initialState.ore - blueprint.obsidian._1, initialState.clay - blueprint.obsidian._2, initialState.obsidian, initialState.geode, initialState.oreRobot, initialState.clayRobot, initialState.obsidianRobot + 1, initialState.geodeRobot)
        newStates.addOne(newState)

      /*
      if(tempState.ore >= blueprint.clay) {
        // add clay robot
        newState = State(tempState.ore - blueprint.clay, tempState.clay, tempState.obsidian, tempState.geode, initialState.oreRobot, initialState.clayRobot + 1, initialState.obsidianRobot, initialState.geodeRobot)
        newStates.addOne(newState)
      }
      else if(tempState.ore >= blueprint.ore && tempState.oreRobot < 5) {
        // add ore robot
         newState = State(tempState.ore - blueprint.ore, tempState.clay, tempState.obsidian, tempState.geode, initialState.oreRobot + 1, initialState.clayRobot, initialState.obsidianRobot, initialState.geodeRobot)
         newStates.addOne(newState)
       }

       */

       newStates.addOne(initialState)
      }

    else {
       canBoth = initialState.ore >= blueprint.clay + blueprint.ore && initialState.oreRobot < 5
       canClay = initialState.ore >= blueprint.clay
       canOre =  initialState.ore >= blueprint.ore && initialState.oreRobot < 5
       newStates.addOne(initialState)
    }

    /*
    **  Never create more than one robot in a single turn
    if(canBoth) {
      // add both
      var newState = State(initialState.ore - blueprint.clay - blueprint.ore, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot + 1, initialState.clayRobot + 1, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
      // add clay robot
      newState = State(initialState.ore - blueprint.clay, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot, initialState.clayRobot + 1, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
      // add ore robot
      newState = State(initialState.ore - blueprint.ore, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot + 1, initialState.clayRobot, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
    }
    else
     */
    if(canClay && !canOre) {
      // add clay robot
      val newState = State(initialState.ore - blueprint.clay, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot, initialState.clayRobot + 1, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
    }
    else if(!canClay && canOre) {
      // add ore robot
      val newState = State(initialState.ore - blueprint.ore, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot + 1, initialState.clayRobot, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
    }
    else if(canClay && canOre) {
      // add clay robot
      var newState = State(initialState.ore - blueprint.clay, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot, initialState.clayRobot + 1, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
      // add ore robot
      newState = State(initialState.ore - blueprint.ore, initialState.clay, initialState.obsidian, initialState.geode, initialState.oreRobot + 1, initialState.clayRobot, initialState.obsidianRobot, initialState.geodeRobot)
      newStates.addOne(newState)
    }

    newStates
  }

  case class Blueprint(id : Int, ore : Int, clay : Int, obsidian : (Int, Int), geode : (Int, Int))

  case class State(ore : Int, clay : Int, obsidian : Int, geode : Int, oreRobot : Int, clayRobot : Int, obsidianRobot : Int, geodeRobot : Int) {

    def value : Int = 100*geodeRobot + 10*obsidianRobot + clayRobot
    def material : Int = ore + clay + obsidian + geode
  }

  def orderStates(state1 : State, state2 : State) : Boolean = state1.value > state2.value

  def orderStates2(state1 : State, state2 : State) : Boolean =
    if((state1.geodeRobot >0 || state2.geodeRobot>0) && state1.geodeRobot!=state2.geodeRobot) state1.geodeRobot > state2.geodeRobot
    else if((state1.obsidianRobot >0 || state2.obsidianRobot>0) && state1.obsidianRobot!=state2.obsidianRobot) state1.obsidianRobot > state2.obsidianRobot
    //else if((state1.clayRobot >0 || state2.clayRobot>0) && state1.clayRobot!=state2.clayRobot) state1.clayRobot > state2.clayRobot
    else state1.material > state2.material
}
