package dynamic_programming

import greedy.DijkstraShortestPath
import util.GraphTypes._

import scala.collection.mutable.ArrayBuffer

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

  private def addVertexWeightZero(adjList: ArrayBuffer[Vertex]): ArrayBuffer[Vertex] = {
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

  private def reweightGraph(adjList: ArrayBuffer[Vertex], weights: ArrayBuffer[Int]): ArrayBuffer[Vertex] = {
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

  private def computeAllShortestPaths(adjList: ArrayBuffer[Vertex], weights: ArrayBuffer[Int]):
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
}
