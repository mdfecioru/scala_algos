package util

import util.GraphTypes._
import util.PointType.Point

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object GraphIO {

  def directedNoWeightsAsList(filename: String): ArrayBuffer[Vertex] = {

    val adjList = new ArrayBuffer[Vertex]()
    val iter = Source.fromFile(filename).getLines()
    val nrVertex = iter.next().toInt

    for (_ <- 0 to nrVertex-1) {
      adjList.addOne(Vertex(new ArrayBuffer[Edge](), new ArrayBuffer[Edge]()))
    }

    for (line <- iter) {
      val items = line.split(" ")
      val v1 = items(0).toInt - 1
      val v2 = items(1).toInt - 1
      adjList(v2).inEdges.addOne(Edge(v1, v2))
      adjList(v1).outEdges.addOne(Edge(v1, v2))
    }

    adjList
  }

  def directedWithWeightsAsList(filename: String): ArrayBuffer[Vertex] = {
    val adjList = new ArrayBuffer[Vertex]()
    val iter = Source.fromFile(filename).getLines()
    val nrVertex =iter.next().toInt

    for (_ <- 1 to nrVertex) {
      adjList.addOne(Vertex(new ArrayBuffer[Edge](), new ArrayBuffer[Edge]()))
    }

    for (line <- iter) {
      val items = line.split(" ")
      val v1 = items(0).toInt - 1
      val v2 = items(1).toInt - 1
      val w = items(2).toInt
      adjList(v2).inEdges.addOne(Edge(v1, v2, w))
      adjList(v1).outEdges.addOne(Edge(v1, v2, w))
    }

    adjList
  }

  def directedWithWeightsAsMatrix(filename: String): ArrayBuffer[ArrayBuffer[Int]] = {
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


  def undirectedWithWeightsAsMatrix(filename: String): ArrayBuffer[ArrayBuffer[Int]] = {
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


  def directedWithWeightsAsPointList(filename: String): ArrayBuffer[Point] = {

    val iter = Source.fromFile(filename).getLines()
    val nrVertex = iter.next().toInt
    val vertexList = new ArrayBuffer[Point](nrVertex)

    for (line <- iter) {
      val items = line.split(" ")
      val vertexID = items(0).toInt - 1
      val x = items(1).toDouble
      val y = items(2).toDouble
      vertexList.addOne(Point(vertexID, x, y))
    }

    vertexList
  }


  def printMatrix(arr: ArrayBuffer[ArrayBuffer[Int]]) = {
    for (line <- arr) {
      for (elem <- line) {
        if (elem == INFINITY) print("  INF")
        else print(f"$elem%5d")
      }
      println()
    }
  }

}
