import sun.util.locale.LocaleUtils.isEmpty

object PuzzleRunner {

  def main(args: Array[String]): Unit = {

    if(args.length >= 1) {

      val day : Int = args(0).toInt
      val objectName = f"Day$day%02d"
      val o = puzzleReference(objectName)
      println(s"Results for day $day:")
      printResults(o)

    }

    else {

      for(day <- 1 to 25) {
        try {
          val objectName = f"Day$day%02d"
          val o = puzzleReference(objectName)
          println(s"Results for day $day:")
          printResults(o)
        }
        catch {
          case _: Throwable => // ignore
        }

      }

    }
  }

  private def printResults(o: CommonPuzzle): Unit = {

    try {
      var result = o.partOne.toString
      if (!isEmpty(result))
        time {
          print("part 1: " + result)
          print("    >   ")
        }

      result = o.partTwo.toString
      if (!isEmpty(result))
        time {
          print("part 2: " + result)
          print("    >   ")
        }
    } catch {
      case _: NotImplementedError => // ignore
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + " ns")
    result
  }


  def puzzleReference(objName : String) : CommonPuzzle =
    try {
      import scala.reflect.runtime.{universe => ru}
      val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
      val staticModule = runtimeMirror.staticModule(objName)
      val reflectModule = runtimeMirror.reflectModule(staticModule)
      reflectModule.instance.asInstanceOf[CommonPuzzle]
    }


}

