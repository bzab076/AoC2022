import scala.collection.mutable

object Day22 extends CommonPuzzle (22) {

  val map: Array[Array[Char]] = inputAsArrayOfChar
  val maxX: Int = map(0).length
  val maxY: Int = map.length
  val instructions: Seq[String] = parseInstructions()

  val faceSize = 50
  val coordinatesMap : mutable.Map[(Int,(Int,Int)), (Int,Int)] = mutable.Map.empty
  val faces : Array[Array[Array[Char]]] = determineFaces() // get faces from the part1 map

  // this map has to be populated manually for each input
  // TODO - populate map automatically from input file
  val faceTransitions : Map[(Int,(Int,Int)), (Int,(Int,Int))] = Map(
    (0,(1,0)) -> (1,(1,0)),
    (0,(0,1)) -> (2,(0,1)),
    (0,(-1,0)) -> (3,(1,0)),
    (0,(0,-1)) -> (5,(1,0)),

    (1,(1,0)) -> (4,(-1,0)),
    (1,(0,1)) -> (2,(-1,0)),
    (1,(-1,0)) -> (0,(-1,0)),
    (1,(0,-1)) -> (5,(0,-1)),

    (2,(1,0)) -> (1,(0,-1)),
    (2,(0,1)) -> (4,(0,1)),
    (2,(-1,0)) -> (3,(0,1)),
    (2,(0,-1)) -> (0,(0,-1)),

    (3,(1,0)) -> (4,(1,0)),
    (3,(0,1)) -> (5,(0,1)),
    (3,(-1,0)) -> (0,(1,0)),
    (3,(0,-1)) -> (2,(1,0)),

    (4,(1,0)) -> (1,(-1,0)),
    (4,(0,1)) -> (5,(-1,0)),
    (4,(-1,0)) -> (3,(-1,0)),
    (4,(0,-1)) -> (2,(0,-1)),

    (5,(1,0)) -> (4,(0,-1)),
    (5,(0,1)) -> (1,(0,1)),
    (5,(-1,0)) -> (0,(0,1)),
    (5,(0,-1)) -> (3,(0,-1))
  )


  override def partOne: Any = {
    val (x,y,d) : (Int, Int, (Int,Int)) = moveAroundMap()
    generatePassword(x, y, d)
  }

  override def partTwo: Any = {
    val (f,x,y,d) = moveAroundCube()
    val (x0,y0) = coordinatesMap.getOrElse((f,(x,y)), (0,0))
    generatePassword(x0, y0, d)
  }

  // Moves around the map according to instructions and returns final position
  def moveAroundMap() : (Int, Int, (Int,Int)) = {

    var yPos = 0
    var xPos = map(0).mkString.indexOf('.')
    var direction : (Int,Int) = (1,0)
    var counter = 1
    for(ins <- instructions) {
      counter += 1
      if(ins.length==1 && ins.charAt(0).isUpper) {
        direction = if(ins.charAt(0) == 'R')  Utils.rotateCounterClockwise(direction) else Utils.rotateClockwise(direction)
      }
      else {
        // move
        var canMove : Boolean = true
        for(_ <- 1 to ins.toInt) {
          if(canMove){
            var xTemp = xPos + direction._1
            var yTemp = yPos + direction._2
            if(xTemp == maxX ) xTemp = 0
            if(yTemp == maxY ) yTemp = 0
            if(xTemp == -1 ) xTemp = maxX - 1
            if(yTemp == -1 ) yTemp = maxY - 1
            canMove = map(yTemp)(xTemp) != '#'
            while(map(yTemp)(xTemp) == ' ' && canMove) {
              xTemp = xTemp + direction._1
              yTemp = yTemp + direction._2
              if(xTemp == maxX ) xTemp = 0
              if(yTemp == maxY ) yTemp = 0
              if(xTemp == -1 ) xTemp = maxX - 1
              if(yTemp == -1 ) yTemp = maxY - 1
              canMove = map(yTemp)(xTemp) != '#'
            }
            if(canMove) {
              xPos = xTemp
              yPos = yTemp
            }
          }

        }
      }

    }

    (xPos,yPos,direction)
  }

  // Moves around the cube according to instructions and returns final position
  def moveAroundCube() : (Int, Int, Int, (Int,Int)) = {

    var yPos = 0
    var xPos = 0
    var direction : (Int,Int) = (1,0)
    var currentFace = 0
    for(ins <- instructions) {
      if(ins.length==1 && ins.charAt(0).isUpper) {
        direction = if(ins.charAt(0) == 'R')  Utils.rotateCounterClockwise(direction) else Utils.rotateClockwise(direction)
      }
      else {
        // move
        var canMove : Boolean = true
        for(_ <- 1 to ins.toInt) {
          if(canMove){
            var xTemp = xPos + direction._1
            var yTemp = yPos + direction._2
            var faceTemp = currentFace
            var dirTemp = direction
            if(xTemp<0 || xTemp>=faceSize || yTemp<0 || yTemp>=faceSize) {
              // go over the edge
              val (newFace,newDir) = faceTransitions.getOrElse((currentFace,direction), (0,(0,0)))
              if(xTemp<0) xTemp += faceSize
              if(xTemp>=faceSize) xTemp %= faceSize
              if(yTemp<0) yTemp += faceSize
              if(yTemp>=faceSize) yTemp %= faceSize
              val rotationKey = Utils.getInverseRotationKey(direction,newDir)
              var (xTemp2,yTemp2) = (xTemp,yTemp)
              rotationKey match {
                case "90" => yTemp2 = xTemp
                             xTemp2 = faceSize - 1 - yTemp
                case "270" => xTemp2 = yTemp
                              yTemp2 = faceSize - 1 - xTemp
                case "180" => xTemp2 = faceSize - 1 - xTemp
                              yTemp2 = faceSize - 1 - yTemp
                case _ => xTemp2 = xTemp
                          yTemp2 = yTemp
              }

              xTemp = xTemp2
              yTemp = yTemp2
              faceTemp = newFace
              dirTemp = newDir
            }

            canMove = faces(faceTemp)(yTemp)(xTemp) != '#'
            if(canMove) {
              xPos = xTemp
              yPos = yTemp
              currentFace = faceTemp
              direction = dirTemp
            }
          }

        }
      }

    }

    (currentFace,xPos,yPos,direction)
  }

  def parseInstructions(): Seq[String] = {

    var result : Seq[String] = Seq.empty
    val insStr = inputLines.reverse.head

    var i = 0
    var startIndex = 0
    var endIndex = 0
    while(i<insStr.length) {
      if(insStr.charAt(i).isUpper) {
        result = result.appended(insStr.substring(startIndex,endIndex+1))
        result = result.appended(insStr.charAt(i).toString)
        startIndex = i + 1
        endIndex = i + 1
      }
      else {
        endIndex = i
      }
      i += 1
    }
    result = result.appended(insStr.substring(startIndex,endIndex+1))
    result

  }

  def inputAsArrayOfChar: Array[Array[Char]] = {

    val inputMap = inputLines.dropRight(2)
    val xSize: Int = inputMap.map(l => l.length).max
    val ySize: Int = inputMap.length
    val result = Array.ofDim[Char](ySize, xSize)

    var y = 0
    for(line <- inputMap) {
      var x = 0
      for(char <- line.toList) {
        result(y)(x) = char
        x += 1
      }
      if(x < xSize - 1) {
        for(x1 <- x until xSize)
          result(y)(x1) = ' '
      }
      y += 1
    }

    result
  }

  def facing(dir : (Int,Int)) : Int = {

    val x = dir._1
    val y = dir._2
    var result = 100
    if(x==1 && y==0) result = 0 // right
    if(x==0 && y==1) result = 1 // down
    if(x== -1 && y==0) result = 2 // left
    if(x==0 && y== -1) result = 3 // up
    result
  }

  private def generatePassword(x: Int, y: Int, direction: (Int, Int)) = {
    1000 * (y + 1) + 4 * (x + 1) + facing(direction)
  }

  private def determineFaces(): Array[Array[Array[Char]]] = {

    val facesGrid = Array.ofDim[Char](6,faceSize, faceSize)
    var faceId = -1
    var faceX = 0
    var faceY = 0
    for(y <- 0 until maxY) {

      if(y % faceSize == 0) faceId += 1 + (faceX-1) / faceSize
      faceX = 0

      for(x <- 0 until maxX) {
        if(map(y)(x) != ' ') {
          facesGrid(faceId + faceX / faceSize)(faceY)(faceX % faceSize) = map(y)(x)
          coordinatesMap.addOne((faceId + faceX / faceSize,(faceX % faceSize,faceY)), (x,y))
          faceX+=1
        }
      }
      faceY += 1
      faceY %= faceSize
    }

    facesGrid
  }
}
