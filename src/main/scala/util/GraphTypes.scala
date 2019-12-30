package util

import scala.collection.mutable.ArrayBuffer

object GraphTypes {

  final val INFINITY = Int.MaxValue

  case class Edge(tail: Int, head: Int,  weight: Int = 0)
  case class Vertex(outEdges: ArrayBuffer[Edge], inEdges: ArrayBuffer[Edge],
                    visited: Boolean = false, leader: Int = INFINITY)
  case class CheapestPath(targetVertex: Int, cheapestPathWeight: Int, cheapestPath: ArrayBuffer[Edge])

  def markAsVisited(vertex: Vertex): Vertex = {
    Vertex(vertex.outEdges, vertex.inEdges, true, vertex.leader)
  }

  def setLeader(vertex: Vertex, leader: Int): Vertex = {
    Vertex(vertex.outEdges, vertex.inEdges, vertex.visited, leader)
  }

  def reverseEdges(adjList: ArrayBuffer[Vertex]): ArrayBuffer[Vertex] = {
    val reverseAdjList = new ArrayBuffer[Vertex]()

    for (vertex <- adjList) {
      val reversedOutEdges = new ArrayBuffer[Edge]()
      val reversedInEdges = new ArrayBuffer[Edge]()
      for (outEdge <- vertex.outEdges) reversedOutEdges.addOne(reverseEdge(outEdge))
      for (inEdge <- vertex.inEdges) reversedOutEdges.addOne(reverseEdge(inEdge))
      reverseAdjList.addOne(Vertex(reversedOutEdges, reversedInEdges))
    }

    reverseAdjList
  }

  def reverseEdge(edge: Edge): Edge = {
    Edge(edge.head, edge.tail, edge.weight)
  }

}
