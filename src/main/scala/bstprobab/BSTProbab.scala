package bstprobab

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object BSTProbab extends App {
  /**
    *
    * - INPUT DATA:
    * - PROBLEM:
    * - HINT:
    * - SOLUTION:
    * - COMPLEXITY:
    *
    */

  val filename = "src/main/resources/bstprobab.txt"
  val iter = Source.fromFile(filename).getLines()
  val nr_nodes =iter.next().toInt
  val probab = new ArrayBuffer[Int](nr_nodes)
  for (line <- iter) {
    probab.addOne(line.toInt)
  }

  val arr = new ArrayBuffer[ArrayBuffer[Int]](nr_nodes)
  for (i <- 0 to nr_nodes-1) {
    val arr_row = new ArrayBuffer[Int](nr_nodes)
    for (j <- 0 to nr_nodes-1) {
      arr_row.addOne(0)
    }
    arr.addOne(arr_row)
  }

  for (s <- 0 to nr_nodes-1) {
    for (i <- 0 to nr_nodes-s-1) {
      var min = Int.MaxValue
      var pk = 0
      for (k <- i to i+s) { pk += probab(k) }
      for (r <- i to i+s) {
        val left = if (i > r-1) 0 else arr(i)(r-1)
        val right = if (r+1 > i+s) 0 else arr(r+1)(i+s)
        min = Math.min(min, pk + left + right)
      }
      arr(i)(i+s) = min
    }
  }

  println(s"Result is: ${arr(0)(nr_nodes-1)}")
}
