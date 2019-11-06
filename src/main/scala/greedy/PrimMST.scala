package greedy

import java.util.PriorityQueue

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object PrimMST extends App {
  /**
    * Prim's Minimum Spanning Tree (MST) algorithm
    *   - INPUT DATA:
    *     - We have a edge-weighted undirected graph
    *       - V is the list of vertexes
    *       - E is the list of edges (each edge is undirected and has a weight)
    *   - PROBLEM:
    *     - Need to find the tree that contains all the graph nodes (spans the entire graph) and has the
    *       following property: the sum of all the edges in this tree is the minimum of all the trees that
    *       span the graph (MST)
    *   - HINT:
    *     - We evolve the tree by adding edges to the partially-built tree in the order of the edge weight
    *       (we will also select the edge that has the smaller weight and that will not create cycles in the tree).
    *       We'll use a min-heap that will keep not-processed vertexes in the order of their cheapest edge that
    *       links the vertex to the MST in construction.
    *   - SOLUTION:
    *     - X = {s} -> this is the list of vertexes we have visited so far. It contains only one vertex - the
    *       start vertex.
    *     - MST = {} -> this will be the list of edges that will compose the MST
    *     - MIN_HEAP = {all the vertexes except for vertex s} -> this will be a min-heap that will contain all vertexes
    *       that have not yet been processed (V - X). For each vertex we will keep the value of the cheapest edge
    *       that connect the vertex with any node from the set X (so edges that cross the X / V - X cut). The
    *       top of the heap will be the vertex that has the smallest cheapest edge that crosses the X / V - X cut.
    *       Thus this will be initialized with all vertexes but vertex s, all vertexes that have a direct edge to s
    *       will have as value the cheapest edge to vertex s, the rest will have as value MAX_INT.
    *     - while X != V do
    *         - extract the next vertex to add to X from MIN_HEAP. Let's call this nextVertex
    *         - add nextVertex to X set
    *         - add nextVertex's cheapest edge to MST
    *         - identify all nextVertex neighbours that are in the (V - X) set and check if the value of the
    *           cheapest edge that crosses the X / V - X cut needs to be updated now that nextVertex is part of X
    *           Basically, for each nextVertex neighbour that is in (V - X)
    *             - if (weight of the nextVertexNeighbour to nextVertex edge < new nextVertexNeighbour cheapest edge)
    *               - extract nextVertexNeighbour from MIN_HEAP
    *               - update nextVertexNeighbour cheapest edge to weight of the nextVertexNeighbour to nextVertex edge
    *               - add nextVertexNeighbour back to MIN_HEAP
    *   - COMPLEXITY:
    *     - O(m * log(n)) where m is the number of edges and n tye number of vertices
    *
    */

  case class Edge(tail: Int, head: Int, weight: Int)
  case class Vertex(edges: ArrayBuffer[Edge])
  case class HeapNode(nodeIndex: Int, cheapestEdge: Edge)

  val INFINITY = Int.MaxValue
  val adjList = new ArrayBuffer[Vertex]()
  val heapNodeList = new ArrayBuffer[HeapNode]()
  val minHeap = new PriorityQueue[HeapNode]((o1, o2) => o1.cheapestEdge.weight compare o2.cheapestEdge.weight)
  val MST = new ListBuffer[Edge]()
  var mstValue = 0

  val filename = "src/main/resources/prim_mst.txt"
  val iter = Source.fromFile(filename).getLines()
  val nr_vertex =iter.next().toInt
  for (i <- 1 to nr_vertex) {
    adjList.addOne(Vertex(new ArrayBuffer[Edge]()))
    heapNodeList.addOne(HeapNode(i-1, Edge(i-1, i-1, INFINITY)))
  }

  for (line <- iter) {
    val items = line.split(" ")
    val v1 = items(0).toInt - 1
    val v2 = items(1).toInt - 1
    val w = items(2).toInt
    adjList(v1).edges.addOne(Edge(v1, v2, w))
    adjList(v2).edges.addOne(Edge(v2, v1, w))
  }

  for (e <- adjList(0).edges) {
    heapNodeList(e.head) = HeapNode(e.head, e)
  }

  for (i <- 1 to nr_vertex-1) {
    minHeap.add(heapNodeList(i))
  }

  while (minHeap.size() > 0) {
    val n = minHeap.poll()
    mstValue += n.cheapestEdge.weight
    MST.addOne(n.cheapestEdge)

    for (e <- adjList(n.nodeIndex).edges) {
      val neighbor = e.head
      val neighborHeapNode = heapNodeList(neighbor)
      if ((minHeap.contains(neighborHeapNode)) && (neighborHeapNode.cheapestEdge.weight > e.weight)) {
        minHeap.remove(neighborHeapNode)
        heapNodeList(neighbor) = HeapNode(neighbor, e)
        minHeap.add(heapNodeList(neighbor))
      }
    }
  }

  println("MST value is: " + mstValue)
  println("MST is: " + MST)
}
