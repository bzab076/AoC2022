object Day19 extends CommonPuzzle (19) {

  val blueprints: List[Blueprint] = inputLines.map{ case s"Blueprint $id: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $obs1 ore and $obs2 clay. Each geode robot costs $geode1 ore and $geode2 obsidian."
      => Blueprint(id.toInt, ore.toInt, clay.toInt, (obs1.toInt, obs2.toInt), (geode1.toInt, geode2.toInt))}


  override def partOne: Any = blueprints.map(bp => bp.id*getMaxGeode(24,bp,State(0,0,0,0,1,0,0,0)) ).sum

  override def partTwo: Any = blueprints.take(3).map(bp => getMaxGeode(32, bp, State(0, 0, 0, 0, 1, 0, 0, 0))).product


  def getMaxGeode(time : Int, blueprint: Blueprint, state : State) : Int = {

    if(time == 0) return state.geode
    var maxOutcome = state.geode + time*state.geodeRobot

    //build geode robot
    if(state.obsidianRobot > 0) {
      val timeToBuild = Math.max(Math.max(Math.ceil((blueprint.geode._1 - state.ore) / state.oreRobot.toDouble).toInt,Math.ceil((blueprint.geode._2 - state.obsidian) / state.obsidianRobot.toDouble).toInt),0) +1
      if(timeToBuild < time) {
        val newState = State(state.ore - blueprint.geode._1 + timeToBuild*state.oreRobot, state.clay + timeToBuild*state.clayRobot, state.obsidian - blueprint.geode._2 + timeToBuild*state.obsidianRobot,state.geode + timeToBuild*state.geodeRobot,state.oreRobot,state.clayRobot,state.obsidianRobot,state.geodeRobot + 1)
        val outcome = getMaxGeode(time-timeToBuild,blueprint,newState)
        if(outcome > maxOutcome) {
          maxOutcome = outcome
        }
      }
    }

    //build obsidian robot
    if(state.obsidianRobot < blueprint.maxObsidianConsumption() && state.clayRobot > 0) {
      val timeToBuild = Math.max(Math.max(Math.ceil((blueprint.obsidian._1 - state.ore) / state.oreRobot.toDouble).toInt,Math.ceil((blueprint.obsidian._2 - state.clay) / state.clayRobot.toDouble).toInt),0) + 1
      if(timeToBuild < time) {
        val newState = State(state.ore - blueprint.obsidian._1 + timeToBuild*state.oreRobot, state.clay - blueprint.obsidian._2 + timeToBuild*state.clayRobot, state.obsidian + timeToBuild*state.obsidianRobot,state.geode + timeToBuild*state.geodeRobot,state.oreRobot,state.clayRobot,state.obsidianRobot + 1,state.geodeRobot)
        val outcome = getMaxGeode(time-timeToBuild,blueprint, newState)
        if(outcome > maxOutcome) {
          maxOutcome = outcome
        }
      }
    }

    //build clay robot
    if(state.clayRobot < blueprint.maxClayConsumption() && state.oreRobot > 0) {
      val timeToBuild = Math.max(Math.ceil((blueprint.clay - state.ore) / state.oreRobot.toDouble).toInt,0) + 1
      if(timeToBuild < time) {
        val newState = State(state.ore - blueprint.clay + timeToBuild*state.oreRobot, state.clay + timeToBuild*state.clayRobot, state.obsidian + timeToBuild*state.obsidianRobot,state.geode + timeToBuild*state.geodeRobot,state.oreRobot,state.clayRobot + 1,state.obsidianRobot,state.geodeRobot)
        val outcome = getMaxGeode(time-timeToBuild,blueprint, newState)
        if(outcome > maxOutcome) {
          maxOutcome = outcome
        }
      }
    }

    //build ore robot
    if(state.oreRobot < blueprint.maxOreConsumption() && state.oreRobot > 0) {
      val timeToBuild = Math.max(Math.ceil((blueprint.ore - state.ore) / state.oreRobot.toDouble).toInt,0) + 1
      if(timeToBuild < time) {
        val newState = State(state.ore - blueprint.ore + timeToBuild*state.oreRobot, state.clay + timeToBuild*state.clayRobot, state.obsidian + timeToBuild*state.obsidianRobot,state.geode + timeToBuild*state.geodeRobot,state.oreRobot + 1,state.clayRobot,state.obsidianRobot,state.geodeRobot)
        val outcome = getMaxGeode(time-timeToBuild,blueprint, newState)
        if(outcome > maxOutcome) {
          maxOutcome = outcome
        }
      }
    }

    maxOutcome
  }

  case class Blueprint(id : Int, ore : Int, clay : Int, obsidian : (Int, Int), geode : (Int, Int)) {
    def maxOreConsumption() : Int = List(ore,clay,obsidian._1,geode._1).max
    def maxClayConsumption() : Int = obsidian._2
    def maxObsidianConsumption() : Int = geode._2
  }

  case class State(ore : Int, clay : Int, obsidian : Int, geode : Int, oreRobot : Int, clayRobot : Int, obsidianRobot : Int, geodeRobot : Int)

}
