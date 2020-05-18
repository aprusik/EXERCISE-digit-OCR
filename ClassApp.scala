import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object ClassApp {
  var numAttr: Int = -1
  var numVal: Int = -1
  var numClass: Int = -1

  def knn(k: Int = 4): Unit = {
    //////////// TRAINING ////////////
    var points: List[(Int, Array[Int])] = Nil

    var line: Array[String] = StdIn.readLine().split( " ")
    while (line(0) != "--") {
      points = (line.head.toInt, line.tail.map(a => a.toInt)) +: points
      line = StdIn.readLine().split( " ")
    }

    //////////// TESTING ////////////
    line = StdIn.readLine().split(" ")
    while (!line.isEmpty) {
      val test = line.map(a => a.toInt)

      val mins = RopesLib.minDist( points, k, test )

      val classCount: ArrayBuffer[Int] = ArrayBuffer()
      1.to(numClass).foreach(_ => classCount += 0)

      mins.foreach(m =>
        classCount(m._1) = classCount(m._1) + 1
      )

      val maxClass = classCount.max
      val maxClassInd = classCount.indexOf(maxClass)
      val conf = maxClass.toFloat / k.toFloat
      println(s"$maxClassInd $conf")

      val nextLine = StdIn.readLine()
      if (nextLine == null) line = Array()
      else line = nextLine.split( " " )
    }

  }

  def linear(): Unit = {
    /////////// TRAINING /////////////
    var alphaCount = 1
    def alpha: Float = 0.0000002.toFloat
    val classifiers: Array[ArrayBuffer[Float]] =
      1.to(numClass).map(_ => {
        val thetas: ArrayBuffer[Float] = ArrayBuffer()
        1.to(numAttr + 1).map(_ => thetas += 0)
        thetas
      }).toArray

    // update all thetas at classifier ti
    def updateOne(ti: Int, train: Array[String], y: Int): Unit = {
      val thetas = classifiers(ti)
      val yHat: Float = calculate(thetas, train)
      thetas.zipWithIndex.foreach(i =>
        thetas(i._2) = i._1.toFloat - alpha * (yHat - y) * {
          if (i._2 == 0) 1
          else train(i._2).toInt
        }
      )
    }

    // calculate prediction on set of thetas and test line
    def calculate(thetas: ArrayBuffer[Float], test: Array[String]): Float = {
      var count = 0
      val e = thetas.reduceLeft((a, b) => {
        val r = a + b * test(count).toFloat
        count += 1
        r
      })
//      var s = 0.toFloat
//      thetas.zipWithIndex.foreach(t => {
//        if (t._2 == 0) s += t._1
//        else s += t._1 * test(t._2 - 1).toInt
//      })
//      s
//      println(e)
      e
    }

    var line: Array[String] = StdIn.readLine().split(" ")
    while (line(0) != "--") {
      alphaCount += 1
      // for each of the classifiers, update their thetas.
      classifiers.zipWithIndex.foreach(c => {
        // is classifier being updated the one being trained?
        if (c._2 == line(0).toInt) updateOne(line(0).toInt, line, 1)
        else updateOne(line(0).toInt, line, 0)
      })

//      println("")
//      classifiers.zipWithIndex.foreach(foo => println("" + foo._2 + " " + foo._1))

      line = StdIn.readLine().split(" ")
    }

    ///////////// TESTING //////////////
    line = StdIn.readLine().split(" ")
    while (!line.isEmpty) {
      val predict = classifiers.map(c => calculate(c, line))

      val conf = predict.max
      val best = predict.indexOf(conf)

      println(s"$best $conf")

      val nextLine = StdIn.readLine()
      if (nextLine == null) line = Array()
      else line = nextLine.split(" ")
    }
  }

  def usage(): Unit = {
    println("Usage: ./run.sh [algorithm]")
    println("Valid algorithms: knn, linear")
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      usage()
    else {
      val header = StdIn.readf3("{0} attributes, {1} values, {2} classes")
      numAttr = header._1.toString.toInt
      numVal = header._2.toString.toInt
      numClass = header._3.toString.toInt

      StdIn.readLine()

      args(0) match {
        case "knn" => knn()
        case "linear" => linear()
        case _ => usage()
      }
    }
  }
}
