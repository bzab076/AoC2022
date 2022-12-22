object Utils {

  def manhattanDist(p1 : (Int,Int), p2 : (Int, Int)) : Int = Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)

  def rotate ( matrix: ((Int, Int), (Int,Int)), vector : (Int, Int)) : (Int, Int) = (matrix._1._1 * vector._1 + matrix._1._2 * vector._2, matrix._2._1*vector._1 + matrix._2._2*vector._2)

  def getInverseRotation (fromVec : (Int,Int) , toVec : (Int,Int)) : ((Int, Int), (Int,Int)) =
       rotationsMap.values.filter(matr => rotate(matr,fromVec) == toVec).head

  def getInverseRotationKey (fromVec : (Int,Int) , toVec : (Int,Int)) : String =
    rotationsMap.filter{case (_,matr) => rotate(matr,fromVec) == toVec}.head._1


  val rotationsMap : Map[String, ((Int, Int), (Int,Int))] = Map(
    "0" -> ((1, 0), (0,1)),
    "90" -> ((0, -1), (1,0)),
    "180" -> ((-1, 0), (0,-1)),
    "270" -> ((0, 1), (-1,0))
  )



}
