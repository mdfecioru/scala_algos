package greedy

import java.util.PriorityQueue

import util.GraphTypes.{CheapestPath, Edge, Vertex}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object DijkstraShortestPath {
  /**
    *  Dijkstra's Single Source Shortest Path
    * - INPUT DATA:
    *   - We have a connected directed graph with edges having ONLY positive weights
    * - PROBLEM:
    *   - Giving a source vertex (S) we need to find the shortest path from this source vertex (S) to
    *     any other vertex in the graph
    * - HINT:
    *   - We start from the source and in each step we find the next vertex we can compute the shortest path to.
    *   - The next vertex will be selected from all the vertexes that can be reached from the set of vertexes we
    *     have already computed the shortest path - we will select the vertex that has the smallest path from S
    *   - We will use a MinHeap to optimize the process of selection the next vertex.
    * - SOLUTION:
    *   - The list of all vertexes is V
    *   - The list of vertexes that have the shortest path computed is X (X will be initialized with {S})
    *   - The map of distances from vertexes to source S is D (D will be initialized with D[S]=0)
    *   - minHeap contains only vertexes in V-X and the key for each vertex (the one used for sorting the vertex
    *     in the heap) is the weight of the cheapest edge with the head in X set and with the tail the vertex itself.
    *   - main loop: while X != V
    *     - extract the vertex (nextVertex) from the top of the minHeap
    *     - use the edge that put nextVertex on top of minHeap to identify the length of the path from source S to
    *       nextVertex. Let's assume that the head of this edge is headOfEdge. Then the path from source S to
    *       nextVertex is: D[nextVertex] = D[headOfEdge] + edgeWeigtht
    *     - add nextVertex to X
    *     - for each edge that has nextVertex as head and the tail in V-X (or is in minHeap). Let's call it tailOfEdge
    *       - extract tailOfEdge from minHeap
    *       - heapKey[tailOfEdge] = min (heapKey[tailOfEdge], D[nextVertex] + edgeWeight)
    *       - add back tailOfEdge in minHeap
    * - COMPLEXITY:
    *   - O(m * log(n)) where m is the number of edges and n tye number of vertices
    * - COMMENTS:
    *   - The algorithm always works ONLY for positive weights.
    *   - The algorithm is not suited to be run on a distributed infrastructure
    *   - The Bellman-Ford algorithm is a solution for the 2 issues above.
    *     - More specific, it will allow edges with negative weights as long as there is no cycle of a negative value.
    *       In the case of a cycle with negative value, the algorithm will fail and will identify one such cycle.
    */

  final val INFINITY = Int.MaxValue

  def readGraphFromFile(filename: String): ArrayBuffer[Vertex] = {

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
      val w = items(2).toInt
      adjList(v2).inEdges.addOne(Edge(v1, v2, w))
      adjList(v1).outEdges.addOne(Edge(v1, v2, w))
    }

    adjList
  }

  def run(adjList: ArrayBuffer[Vertex], startVertex: Int = 0): ArrayBuffer[CheapestPath] = {
    val nrVertex = adjList.size
    val heapNodeList = new ArrayBuffer[CheapestPath]()
    val minHeap = new PriorityQueue[CheapestPath]((o1, o2) => o1.cheapestPathWeight compare o2.cheapestPathWeight)

    for (i <- 0 to nrVertex-1) {
      heapNodeList.addOne(CheapestPath(i, INFINITY, null))
    }
    heapNodeList(startVertex) = CheapestPath(startVertex, 0, new ArrayBuffer[Edge]())

    for (e <- adjList(startVertex).outEdges) {
      heapNodeList(e.head) = CheapestPath(e.head, e.weight, new ArrayBuffer[Edge]().addOne(e))
    }

    for (i <- 0 to nrVertex - 1) {
      if (i != startVertex) minHeap.add(heapNodeList(i))
    }

    while (minHeap.size() > 0) {
      val n = minHeap.poll()

      // If this test is true, then all the nodes remaining in the minHeap are nor reachable from startVertex
      if (n.cheapestPathWeight == INFINITY) return heapNodeList

      val prevNode = heapNodeList(n.cheapestPath(0).tail)
      // Save the final result for node n in heapNodeList
      heapNodeList(n.targetVertex) = CheapestPath(n.targetVertex, n.cheapestPathWeight, prevNode.cheapestPath ++ n.cheapestPath)

      for (e <- adjList(n.targetVertex).outEdges) {
        val neighborIndex = e.head
        val neighborHeapNode = heapNodeList(neighborIndex)
        if ((minHeap.contains(neighborHeapNode)) && (neighborHeapNode.cheapestPathWeight > n.cheapestPathWeight + e.weight)) {
          minHeap.remove(neighborHeapNode)
          heapNodeList(neighborIndex) = CheapestPath(neighborIndex, n.cheapestPathWeight + e.weight, new ArrayBuffer[Edge]().addOne(e))
          minHeap.add(heapNodeList(neighborIndex))
        }
      }
    }

    heapNodeList
  }
}
