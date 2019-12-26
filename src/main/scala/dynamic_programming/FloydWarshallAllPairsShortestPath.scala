package dynamic_programming

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object FloydWarshallAllPairsShortestPath {

  final val INFINITY = Int.MaxValue

  def run(adjMatrix: ArrayBuffer[ArrayBuffer[Int]]): Option[ArrayBuffer[ArrayBuffer[Int]]] = {
    var arrCurr = adjMatrix
    val nrVertex = adjMatrix.size
    var arrPrev = new ArrayBuffer[ArrayBuffer[Int]](nrVertex)
    for (_ <- 0 to nrVertex - 1) {
      val arr0 = new ArrayBuffer[Int](nrVertex)
      for (_ <- 0 to nrVertex - 1) {
        arr0.addOne(INFINITY)
      }
      arrPrev.addOne(arr0)
    }

    for (k <- 0 to nrVertex - 1) {
      val t = arrCurr
      arrCurr = arrPrev
      arrPrev = t

      for (i <- 0 to nrVertex - 1) {
        for (j <- 0 to nrVertex - 1) {
          var minVal = arrPrev(i)(j)
          if ((arrPrev(i)(k) != INFINITY) && (arrPrev(k)(j) != INFINITY))
            minVal = Math.min(minVal, arrPrev(i)(k) + arrPrev(k)(j))
          arrCurr(i)(j) = minVal
        }
      }
    }

    // If we have identified negative cycles, we return "None"
    if (checkNegativeCycles(arrCurr)) return None
    Some(arrCurr)
  }

  def readGraphFromFile(filename: String): ArrayBuffer[ArrayBuffer[Int]] = {
    val iter = Source.fromFile(filename).getLines()
    val nrVertex = iter.next().toInt
    val adjMatrix = new ArrayBuffer[ArrayBuffer[Int]](nrVertex)

    for (_ <- 0 to nrVertex - 1) {
      val arr0 = new ArrayBuffer[Int](nrVertex)
      for (_ <- 0 to nrVertex - 1) {
        arr0.addOne(INFINITY)
      }
      adjMatrix.addOne(arr0)
    }
    for (i <- 0 to nrVertex - 1) adjMatrix(i)(i) = 0

    for (line <- iter) {
      val items = line.split(" ")
      val v1 = items(0).toInt - 1
      val v2 = items(1).toInt - 1
      val w = items(2).toInt
      adjMatrix(v1)(v2) = w
    }

    adjMatrix
  }

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
