import scala.annotation.tailrec
import scala.collection.mutable

object Day16 extends CommonPuzzle (16) {

  val valves : Map[String, (Int, List[String])] = parseInput()
  val valveIndices: List[(String, Int)] = valves.keys.toList.distinct.zipWithIndex
  val valvesWithIndices : Map[Int, (Int, List[Int])] = valves.map{ case (k,v) => (getValveIndex(k),(v._1,v._2.map(id => getValveIndex(id))))}
  val valvesWithDists : Map[Int, Map[Int, Int]] = valvesWithIndices.map{ case (k, _) =>
    (k, (0 until valves.size).filter(idx => idx!=k).map(id => (id, GraphUtils.findCheapestPath((k,0), (id,0), getNeighbours, getCost))).toMap)} // shortest distance for each pair of valves
  val goodValves: Iterable[String] = valves.filter{case (_,v) => v._1 > 0}.keys  // remove valves with 0 flow rate
  var maxMinutes = 30

  override def partOne: Any = releasePressure(0, Map(0 -> Map(State("AA", Set.empty) -> 0))).values.max

  override def partTwo: Any = {

    maxMinutes = 26
    val valvesPressure = releasePressure(0, Map(0 -> Map(State("AA", Set.empty) -> 0)))
    val splitWork = (
      for {
        (valves1, pressure1) <- valvesPressure.iterator // my valves and pressures
        otherValves = goodValves.filter(v => !valves1.contains(v))
        valves2 <- otherValves.toSet.subsets() // get all subsets of remaining valves for the elephant
        pressure2 <- valvesPressure.get(valves2)  // elephant pressures
        if(valves2.nonEmpty && pressure2>0)
    } yield ((valves1,pressure1), (valves2, pressure2)) ).toList

    splitWork.map{case (me, elephant) => me._2 + elephant._2}.max
  }


  def parseInput(): Map[String, (Int, List[String])] = {

    val map: mutable.Map[String, (Int, List[String])] = mutable.Map.empty

    for(line <- inputLines) {
      val valveName = line.substring(6,8)
      val substr = line.substring(23)
      var pressure : Int = 0
      var toValves : List[String] = List.empty
      if(substr.contains("tunnels")) {
        pressure = line.substring(23).split("; tunnels lead to valves ")(0).toInt
        toValves = line.substring(23).split("; tunnels lead to valves ")(1).split(", ").toList
      }
      else {
        pressure = line.substring(23).split("; tunnel leads to valve ")(0).toInt
        toValves = line.substring(23).split("; tunnel leads to valve ")(1).split(", ").toList
      }

      map.addOne(valveName,(pressure,toValves))
    }
    map.toMap

  }

  // *********************************************************************

  // For Dijkstra algorithm

  def getValveIndex(valve : String) : Int = valveIndices.filter{case (v, _) => v==valve}.head._2

  def getNeighbours(valveId: Int, other: Int) : List[(Int,Int)] = valvesWithIndices.getOrElse(valveId,(0,List.empty))._2.map(i => (i,0))

  def getCost(p2 : (Int, Int), p1: (Int, Int)) : Int = 1

  // **************************************************************************************


  // main function to get released pressure for every state
  @tailrec
  def releasePressure(minute: Int, states: Map[Int, Map[State, Int]]): Map[Set[String], Int] = {
    if (minute < maxMinutes) {
      val minuteToStates = for {
        (State(valve, openValves), pressure) <- states.getOrElse(minute, Map.empty).iterator
        newValve <- goodValves // only consider good valves
        if !openValves.contains(newValve)
          dist = valvesWithDists(getValveIndex(valve))(getValveIndex(newValve))
          flowRate = valves(newValve)._1
          openMinute = minute + dist + 1
          if openMinute < maxMinutes // avoid move, which won't finish in time
            extraPressure = flowRate * (maxMinutes - openMinute)
      } yield openMinute -> (State(newValve, openValves + newValve) -> (pressure + extraPressure))

      val minuteToStatesLst = minuteToStates.toList

      val newStates = minuteToStatesLst.foldLeft(states)({ case (acc, (minute, (state, pressure))) =>
        val minuteStates = acc.getOrElse(minute, Map.empty)
        val newPressure = minuteStates.getOrElse(state, 0) max pressure
        val newMinuteStates = minuteStates.updated(state, newPressure)
        acc.updated(minute, newMinuteStates)
      })

      releasePressure(minute + 1, newStates)
    }
    else {
      (for {
        minuteStates <- states.valuesIterator
        (state, pressure) <- minuteStates.iterator
      } yield (state.openValves, pressure)).toList.groupMapReduce(_._1)(_._2)(_ max _)
    }
  }


  case class State(valve: String, openValves: Set[String])

}
