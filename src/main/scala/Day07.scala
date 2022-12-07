import scala.collection.mutable.ArrayBuffer

object Day07 extends CommonPuzzle (7) {

  val root : AoCFile = AoCFile("root", 0, isDir = true, ArrayBuffer.empty, null)

  override def partOne: Any = {
    parseInput()
    root.sizeList.filter(s => s<=100000).sum
  }

  override def partTwo: Any = {
    val requiredSpace = 30000000- 70000000 + root.totalSize
    root.sizeList.filter(s => s>=requiredSpace).min
  }

  def parseInput() : Unit = {

    var currentDir = root

    for(line <- inputLines) {

      if(line.startsWith("$")) {
        val command = line.split(" ")(1)
        val param = if(command=="ls") "" else line.split(" ")(2)
        if(command=="cd") {
          if(param=="..") currentDir = currentDir.parent
          else if(param!="/") currentDir = currentDir.subdirs.toList.filter(f => f.name==param).head
        }
        // command is ls go to next line
      }
      else {
        // list dirs
        val arg1 = line.split(" ")(0)
        val arg2 = line.split(" ")(1)
        if(arg1=="dir") currentDir.addFile(AoCFile(arg2,0,isDir = true,ArrayBuffer.empty, currentDir))
          else  currentDir.addFile(AoCFile(arg2,arg1.toInt,isDir = false,ArrayBuffer.empty, currentDir))
      }
    }

  }


 case  class AoCFile(name : String, size : Int, isDir : Boolean, subdirs : ArrayBuffer[AoCFile], parent: AoCFile) {

    def totalSize : Int = if(!isDir) size else subdirs.toList.map(file => file.totalSize).sum

    def addFile(file : AoCFile) : Unit = subdirs.addOne(file)

    def sizeList : List[Int] = if(isDir) subdirs.toList.foldLeft(List(totalSize)){(acc, file) => acc ++ file.sizeList} else List.empty

  }

}


