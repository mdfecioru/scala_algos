package npcomplete

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object TravelingSalesmanProblem {
  final val INFINITY = Int.MaxValue

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

    for (line <- iter) {
      val items = line.split(" ")
      val v1 = items(0).toInt - 1
      val v2 = items(1).toInt - 1
      val w = items(2).toInt
      adjMatrix(v1)(v2) = w
      adjMatrix(v2)(v1) = w
    }

    adjMatrix
  }

  def run(adjMatrix: ArrayBuffer[ArrayBuffer[Int]]): Int = {
    val nrVertex = adjMatrix.size
    var curr = new ArrayBuffer[ArrayBuffer[Int]]()
    var hmCurr = new mutable.HashMap[String, mutable.HashMap[Int, Int]]()
    for (i <- 1 to nrVertex - 1) {
      val arr0 = new ArrayBuffer[Int]()
      arr0.addOne(i)
      curr.addOne(arr0)
      val hmv = new mutable.HashMap[Int, Int]()
      hmCurr.addOne(i.toString(), hmv)
      hmv.addOne(i, adjMatrix(0)(i))
    }

    for (i <- 2 to nrVertex - 1) {
      curr = generateCombination(curr, nrVertex - 1)
      val hmPrev = hmCurr
      hmCurr = new mutable.HashMap[String, mutable.HashMap[Int, Int]]()
      for (combination <- curr) {
        val hmv = new mutable.HashMap[Int, Int]()
        hmCurr.addOne(generateHashKey(combination), hmv)
        for (j <- combination) {
          val c = new ArrayBuffer[Int]()
          c.addAll(combination)
          c -= j
          var minVal = INFINITY
          for (k <- c) {
            minVal = Math.min(minVal, hmPrev(generateHashKey(c))(k) + adjMatrix(k)(j))
          }
          hmv.addOne(j, minVal)
        }
      }
    }

    var minV = INFINITY
    for ((_, hmv) <- hmCurr) {
      hmv foreach (x => minV = Math.min(minV, x._2 + adjMatrix(0)(x._1)))
    }

    minV
  }

  def printArr(arr: ArrayBuffer[ArrayBuffer[Int]]) = {
    for (line <- arr) {
      var strToPrint = "("
      for (elem <- line) {
        strToPrint = strToPrint + f"$elem%3d,"
      }
      strToPrint = strToPrint.dropRight(1)
      strToPrint = strToPrint + ") "
      print(strToPrint)
    }
    println()
  }

  def generateCombination(currentStep: ArrayBuffer[ArrayBuffer[Int]], size: Int): ArrayBuffer[ArrayBuffer[Int]] = {
    val next = new ArrayBuffer[ArrayBuffer[Int]]()
    for (c <- currentStep) {
      val maxElem = c(c.size-1)
      for (elem <- maxElem+1 to size) {
        val arr0 = new ArrayBuffer[Int]()
        arr0.addAll(c)
        arr0.addOne(elem)
        next.addOne(arr0)
      }
    }
    next
  }

  def generateHashKey(arr: ArrayBuffer[Int]): String = {
    var s = ""
    for (elem <- arr) {
      s = s + elem.toString() + "_"
    }
    s.dropRight(1)
  }

}
