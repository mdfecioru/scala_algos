package greedy

import java.util.PriorityQueue

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object DijkstraShortestPath extends App {
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
    *     - O(m * log(n)) where m is the number of edges and n tye number of vertices
    */

  case class Edge(tail: Int, head: Int,  weight: Int)
  case class Vertex(edges: ArrayBuffer[Edge])
  case class HeapNode(nodeIndex: Int, cheapestPathWeight: Int, cheapestPath: ArrayBuffer[Edge])

  val INFINITY = Int.MaxValue
  val adjList = new ArrayBuffer[Vertex]()
  val heapNodeList = new ArrayBuffer[HeapNode]()
  val minHeap = new PriorityQueue[HeapNode]((o1, o2) => o1.cheapestPathWeight compare o2.cheapestPathWeight)

  val filename = "src/main/resources/dijkstra_shortest_path.txt"
  val iter = Source.fromFile(filename).getLines()
  val nr_vertex =iter.next().toInt

  adjList.addOne(Vertex(new ArrayBuffer[Edge]()))
  heapNodeList.addOne(HeapNode(0, 0, new ArrayBuffer[Edge]()))
  for (i <- 2 to nr_vertex) {
    adjList.addOne(Vertex(new ArrayBuffer[Edge]()))
    heapNodeList.addOne(HeapNode(i-1, INFINITY, null))
  }

  for (line <- iter) {
    val items = line.split(" ")
    val v1 = items(0).toInt - 1
    val v2 = items(1).toInt - 1
    val w = items(2).toInt
    adjList(v1).edges.addOne(Edge(v1, v2, w))
  }

  for (e <- adjList(0).edges) {
    heapNodeList(e.head) = HeapNode(e.head, e.weight, new ArrayBuffer[Edge]().addOne(e))
  }

  for (i <- 1 to nr_vertex-1) {
    minHeap.add(heapNodeList(i))
  }

  while (minHeap.size() > 0) {
    val n = minHeap.poll()
    val prevNode = heapNodeList(n.cheapestPath(0).tail)
    // Save the final result for node n in heapNodeList
    heapNodeList(n.nodeIndex) = HeapNode(n.nodeIndex, n.cheapestPathWeight, prevNode.cheapestPath++n.cheapestPath)

    for (e <- adjList(n.nodeIndex).edges) {
      val neighborIndex = e.head
      val neighborHeapNode = heapNodeList(neighborIndex)
      if ((minHeap.contains(neighborHeapNode)) && (neighborHeapNode.cheapestPathWeight > n.cheapestPathWeight + e.weight)) {
        minHeap.remove(neighborHeapNode)
        heapNodeList(neighborIndex) = HeapNode(neighborIndex, n.cheapestPathWeight + e.weight, new ArrayBuffer[Edge]().addOne(e))
        minHeap.add(heapNodeList(neighborIndex))
      }
    }
  }

  println("Shortest path results: " + heapNodeList)

}
