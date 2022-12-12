import scala.collection.mutable
import scala.collection.mutable.{Map, Set}

object GraphUtils {


  def CostOrdering: Ordering[(Int, (Int, Int))] = (t1: (Int, (Int, Int)), t2: (Int, (Int, Int))) => t2._1.compare(t1._1)

  def findCheapestPath(startNode : (Int,Int), endNode : (Int,Int), getNeighbours:(Int, Int) => List[(Int,Int)], cost:((Int, Int),(Int, Int)) => Int ) : Int = {

    val costMap: Map[(Int, Int), Int] = Map(startNode -> 0)
    val pq = mutable.PriorityQueue[(Int,(Int,Int))]()(CostOrdering)
    pq.addOne((0,startNode))
    val visited : Set[(Int,Int)] = Set.empty

    while (pq.nonEmpty && !visited.contains(endNode)) {

      val current = pq.dequeue()
      val currentVertex : (Int,Int) = current._2
      visited.add(currentVertex)
      val currentCost : Int = current._1
      val newVertices = getNeighbours(currentVertex._1, currentVertex._2).map(v => (v,cost(v,currentVertex) + currentCost))
        .filter{ case (v,_) => !visited.contains(v)}
      newVertices.foreach {
        case (v, c) =>
          val oldCost = costMap.getOrElse(v, Int.MaxValue)
          costMap.addOne(v, Math.min(oldCost, c))
          if (c < oldCost && !visited.contains(v)) {
            pq.dropWhile(b => b==(oldCost,v))
            pq.enqueue((c, v))
          }
      }
    }

    costMap.getOrElse(endNode,Int.MaxValue)
  }

  def findAllCheapestPaths(startNode : (Int,Int), getNeighbours:(Int, Int) => List[(Int,Int)], cost:((Int, Int),(Int, Int)) => Int ) : Map[(Int, Int), (Int,List[(Int,Int)])] = {

    val costMap: Map[(Int, Int), (Int,List[(Int,Int)])] = Map(startNode -> (0,List.empty))
    val pq = mutable.PriorityQueue[(Int,(Int,Int))]()(CostOrdering)
    pq.addOne((0,startNode))
    val visited : Set[(Int,Int)] = Set.empty

    while (pq.nonEmpty) {

      val current = pq.dequeue()
      val currentVertex : (Int,Int) = current._2
      visited.add(currentVertex)
      val currentCost : Int = current._1
      val option = costMap.filter{case (k,_) => !visited.contains(k)}.toSeq.sortBy(_._2._1).headOption
      val currentList : List[(Int,Int)] = option.getOrElse(default = ((0, 0), (0,List.empty)))._2._2
      val newVertices = getNeighbours(currentVertex._1, currentVertex._2).map(v => (v,cost(v,currentVertex) + currentCost))
        .filter{ case (v,_) => !visited.contains(v)}
      newVertices.foreach {
        case (v, c) =>
          val oldCost = costMap.getOrElse(v,(Int.MaxValue,List.empty))._1
          //costMap.addOne(v, Math.min(oldCost, c))
          if (c < oldCost && !visited.contains(v)) {
            val newList = currentList.appended(currentVertex)
            costMap.addOne(v,(c,newList))
            pq.dropWhile(b => b==(oldCost,v))
            pq.enqueue((c, v))
          }
      }
    }

    //val path = costMap.getOrElse(endNode,(0,List.empty))._2.appended(endNode)
    //println(path.toString())
    costMap
  }

}
