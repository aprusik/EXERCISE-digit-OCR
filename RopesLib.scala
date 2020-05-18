import scala.collection.mutable.ArrayBuffer

object RopesLib {
  def sum(i: Range, f: Int => Float): Float = {
    var s: Float = 0
    i.foreach(x => s += f(x))
    s
  }

  def min(a: List[(Int, Float)], x: Int): List[(Int, Float)] = {
    val mins: ArrayBuffer[(Int, Float)] = ArrayBuffer()
    1.to(x).foreach(_ => (-1, 700.toFloat) +=: mins)

    a.foreach(f => {
      if (mins.maxBy(_._2)._2 > f._2)
        mins(mins.indexOf(mins.maxBy(_._2))) = f
    })

    mins.toList
  }

  def minDist(a: List[(Int, Array[Int])], x: Int, test: Array[Int]): List[(Int, Float)] = {
    val mins: ArrayBuffer[(Int, Float)] = ArrayBuffer()
    1.to(x).foreach(_ => (-1, 700.toFloat) +=: mins)

    a.foreach(f => {
      val d = dist(f._2, test)
      if (mins.maxBy(_._2)._2 > d)
        mins(mins.indexOf(mins.maxBy(_._2))) = (f._1, d)
    })

    mins.toList
  }

  def dist(point: Array[Int], test: Array[Int]): Float = {
    Math.sqrt(RopesLib.sum(
      point.indices,
      i => math.pow(point(i) - test(i), 2).toFloat
    )).toFloat
  }

//  def main(args: Array[String]): Unit = {
//    val foo = List(
//      (1, 87.2.toFloat),
//      (2, 123.54.toFloat),
//      (3, 0.123.toFloat),
//      (4, 15.0.toFloat),
//      (5, 1.0.toFloat),
//      (6, 23.123.toFloat),
//      (7, 77.77.toFloat)
//    )
//    println(min(foo, 5))
//  }
}
