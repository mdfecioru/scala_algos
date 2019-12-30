package graph

import scala.collection.mutable.ArrayBuffer

import util.GraphTypes._

object KosarajuStrongConnectedComponents {

  def run(adjList: ArrayBuffer[Vertex]): ArrayBuffer[Int] = {
    val reversedAdjList = reverseEdges(adjList)
    val dfsOrder = DepthFirstSearch.run(adjList)
    DepthFirstSearch.run(reversedAdjList, Some(dfsOrder))

    // Extract the connected component IDs and return it as an array
    val scc = new ArrayBuffer[Int]()
    for (vertex <- reversedAdjList) scc.addOne(vertex.leader)
    scc
  }

}
