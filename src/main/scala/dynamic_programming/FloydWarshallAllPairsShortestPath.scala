package dynamic_programming

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object FloydWarshallAllPairsShortestPath extends App {

  val INFINITY = Int.MaxValue
  //val filename = "src/main/resources/floydwarshall_all_pairs_shortest_path.txt"
  val filename = "src/main/resources/large.txt"
  val iter = Source.fromFile(filename).getLines()
  val nrVertex =iter.next().toInt

  var arrCurr = new ArrayBuffer[ArrayBuffer[Int]](nrVertex)
  var arrPrev = new ArrayBuffer[ArrayBuffer[Int]](nrVertex)
  var kk = 0;
  for (_ <- 0 to nrVertex-1) {
    val arr0 = new ArrayBuffer[Int](nrVertex)
    val arr1 = new ArrayBuffer[Int](nrVertex)
    kk = kk + 1
    if (kk % 10 == 0) println("We are here - kk: ", kk)

    for (_ <- 0 to nrVertex-1) {
      arr0.addOne(INFINITY)
      arr1.addOne(INFINITY)
    }
    arrCurr.addOne(arr0)
    arrPrev.addOne(arr1)
  }
  for (i <- 0 to nrVertex-1) arrCurr(i)(i) = 0

  for (line <- iter) {
    val items = line.split(" ")
    val v1 = items(0).toInt - 1
    val v2 = items(1).toInt - 1
    val w = items(2).toInt
    arrCurr(v1)(v2) = w
  }

  for (k <- 0 to nrVertex-1) {
    val t = arrCurr
    arrCurr = arrPrev
    arrPrev = t

    if (k % 10 == 0) println("We are here: ", k)

    for (i <- 0 to nrVertex-1) {
      for (j <- 0 to nrVertex-1) {
        var minVal = arrPrev(i)(j)
        if ((arrPrev(i)(k) != INFINITY) && (arrPrev(k)(j) != INFINITY))
          minVal = Math.min(minVal, arrPrev(i)(k) + arrPrev(k)(j))
        arrCurr(i)(j) = minVal
      }
    }
  }

  println("Negative Cycles: ", checkNegativeCycles(arrCurr))
  println("Shortest Shortest Path: ", shortestShortestPath(arrCurr))
  //printResults(arrCurr)

  def printResults(arr: ArrayBuffer[ArrayBuffer[Int]]): Unit = {
    for (line <- arr) {
      for (elem <- line) {
        if (elem == INFINITY) print("  INF")
        else print(f"$elem%5d")
      }
      println()
    }
  }

  def checkNegativeCycles(arr: ArrayBuffer[ArrayBuffer[Int]]): Boolean = {
    for (i <- 0 to arr.size-1) {
      if (arr(i)(i) < 0) return true
    }
    return false
  }

  def shortestShortestPath(arr: ArrayBuffer[ArrayBuffer[Int]]): Int = {
    var ssp = INFINITY
    for (i <- 0 to arr.size-1) {
      val line = arr(i)
      for (j <- 0 to line.size-1) {
        if (i != j) ssp = Math.min(ssp, arr(i)(j))
      }
    }
    return ssp;
  }
}
