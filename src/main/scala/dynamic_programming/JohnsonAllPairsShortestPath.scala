package dynamic_programming

import greedy.DijkstraShortestPath
import util.GraphTypes.{Edge, Vertex}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object JohnsonAllPairsShortestPath {

  final val INFINITY = Int.MaxValue

  def run(adjList: ArrayBuffer[Vertex]): Option[ArrayBuffer[ArrayBuffer[Int]]] = {
    val graphWithVertexWeightZero = addVertexWeightZero(adjList)
    val weights = BellmanFordShortestPath.run(graphWithVertexWeightZero, graphWithVertexWeightZero.size-1)

    // If the BellmanFord run identified a negative cycle, return as the original graph has a negative cycle.
    if (!weights.isDefined) return None

    val reweightedGraph = reweightGraph(adjList, weights.get)
    val result = computeAllShortestPaths(reweightedGraph, weights.get)

    result
  }

  def addVertexWeightZero(adjList: ArrayBuffer[Vertex]): ArrayBuffer[Vertex] = {
    val adjListNew = new ArrayBuffer[Vertex]()
    val nrVertex = adjList.size

    val vertexNew = Vertex(new ArrayBuffer[Edge](), new ArrayBuffer[Edge]())
    for (i <- 0 to nrVertex-1) {
      val vertex = adjList(i)
      val vertex0 = Vertex(new ArrayBuffer[Edge](), new ArrayBuffer[Edge]())
      for (e <- vertex.inEdges) vertex0.inEdges.addOne(e)
      for (e <- vertex.outEdges) vertex0.outEdges.addOne(e)

      vertexNew.outEdges.addOne(Edge(nrVertex, i, 0))
      vertex0.inEdges.addOne(Edge(nrVertex, i, 0))
      adjListNew.addOne(vertex0)
    }
    adjListNew.addOne(vertexNew)

    adjListNew
  }

  def reweightGraph(adjList: ArrayBuffer[Vertex], weights: ArrayBuffer[Int]): ArrayBuffer[Vertex] = {
    val adjListReweighted = new ArrayBuffer[Vertex]()

    for (vertex <- adjList) {
      val vertex0 = Vertex(new ArrayBuffer[Edge](), new ArrayBuffer[Edge]())
      for (e <- vertex.inEdges)
        vertex0.inEdges.addOne(Edge(e.tail, e.head, e.weight + weights(e.tail) - weights(e.head)))
      for (e <- vertex.outEdges)
        vertex0.outEdges.addOne(Edge(e.tail, e.head, e.weight + weights(e.tail) - weights(e.head)))

      adjListReweighted.addOne(vertex0)
    }

    adjListReweighted
  }

  def computeAllShortestPaths(adjList: ArrayBuffer[Vertex], weights: ArrayBuffer[Int]):
                                                              Option[ArrayBuffer[ArrayBuffer[Int]]] = {
    val nrVertex = adjList.size
    val allPathMatrix = new ArrayBuffer[ArrayBuffer[Int]](nrVertex)

    for (i <- 0 to nrVertex-1) {
      val cheapestPaths = DijkstraShortestPath.run(adjList, i)
      val arr0 = new ArrayBuffer[Int]()
      for (j <- 0 to cheapestPaths.size-1) {
        if (cheapestPaths(j).cheapestPathWeight == INFINITY)
          arr0.addOne(cheapestPaths(j).cheapestPathWeight)
        else arr0.addOne(cheapestPaths(j).cheapestPathWeight - weights(i) + weights(j))
      }
      allPathMatrix.addOne(arr0)
    }

    Some(allPathMatrix)
  }

  def readGraphFromFile(filename: String): ArrayBuffer[Vertex] = {
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

  def printResults(arr: ArrayBuffer[ArrayBuffer[Int]]): Unit = {
    for (line <- arr) {
      for (elem <- line) {
        if (elem > INFINITY / 2) print("  INF")
        else print(f"$elem%5d")
      }
      println()
    }
  }

}
