package graph

import util.GraphTypes._

import scala.collection.mutable.ArrayBuffer

object DepthFirstSearch {

  def run(adjList: ArrayBuffer[Vertex], order: Option[ArrayBuffer[Int]] = None): ArrayBuffer[Int] = {
    val nrVertex = adjList.size
    var dfsOrder = new ArrayBuffer[Int]()

    for (i <- nrVertex-1 to 0 by -1) {
      val vertexIndex = if (order.isDefined) order.get(i) else i
      if (!adjList(vertexIndex).visited)
        dfsOrder = dfs(adjList, vertexIndex, vertexIndex, dfsOrder)
    }

    dfsOrder
  }

  private def dfs(adjList: ArrayBuffer[Vertex], vertexIndex: Int, leaderIndex: Int, dfsOrder: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    adjList(vertexIndex) = markAsVisited(adjList(vertexIndex))
    adjList(vertexIndex) = setLeader(adjList(vertexIndex), leaderIndex)

    for (outEdge <- adjList(vertexIndex).outEdges) {
      if (!adjList(outEdge.head).visited)
        dfs(adjList, outEdge.head, leaderIndex, dfsOrder)
    }

    dfsOrder.addOne(vertexIndex)
    dfsOrder
  }
}
