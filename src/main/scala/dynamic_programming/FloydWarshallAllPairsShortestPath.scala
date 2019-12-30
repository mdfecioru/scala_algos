package dynamic_programming

import scala.collection.mutable.ArrayBuffer

import util.GraphTypes._

object FloydWarshallAllPairsShortestPath {

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


  private def checkNegativeCycles(arr: ArrayBuffer[ArrayBuffer[Int]]): Boolean = {
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
