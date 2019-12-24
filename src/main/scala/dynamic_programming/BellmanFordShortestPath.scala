package dynamic_programming

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object BellmanFordShortestPath extends App {
  /**
    *  Bellman-Ford Single Source Shortest Path
    * - INPUT DATA:
    *   - We have a connected directed graph with edges that may have negative edges weights
    * - PROBLEM:
    *   - Giving a source vertex (S) we need to find the shortest path from this source vertex (S) to
    *     any other vertex in the graph IF there is no negative cycle in the graph
    *   - If the graph has a negative cycle, the algorithm will fail and will identify that negative cycle.
    * - HINT:
    *   - The shortest-path between 2 vertexes (S and V) with at most "i" edges is either (the minimum between):
    *     - the shortest-path between the 2 vertexes (S and V) with at most "i-1" edges
    *     - for each node  Wj that has an EDGE starting in Wj and ending in V, compute the minimum of (shortest path
    *       between S and Wj with at most "i-1" edges + [Wj, V] EDGE weight)
    *   - For a graph that does not have negative cycles, the shortest path between 2 vertexes (S and V) cannot have
    *     more than N-1 EDGEs where N is the number of vertexes of the graph.
    * - SOLUTION:
    * - COMPLEXITY:
    * - COMMENTS:
    */

  case class Edge(tail: Int, head: Int,  weight: Int)
  case class Vertex(inEdges: ArrayBuffer[Edge])

  val INFINITY = Int.MaxValue
  val adjList = new ArrayBuffer[Vertex]()

  val filename = "src/main/resources/bellmanford_shortest_path.txt"
  val iter = Source.fromFile(filename).getLines()
  val nrVertex =iter.next().toInt

  for (_ <- 1 to nrVertex) {
    adjList.addOne(Vertex(new ArrayBuffer[Edge]()))
  }

  for (line <- iter) {
    val items = line.split(" ")
    val v1 = items(0).toInt - 1
    val v2 = items(1).toInt - 1
    val w = items(2).toInt
    adjList(v2).inEdges.addOne(Edge(v1, v2, w))
  }

  var arr = new ArrayBuffer[ArrayBuffer[Int]]()
  var lineArrSource = new ArrayBuffer[Int]()
  lineArrSource.addOne(0)
  for (j <- 2 to nrVertex) lineArrSource.addOne(INFINITY)
  arr.addOne(lineArrSource)

  for (i <- 1 to nrVertex-1) {
    val lineArr = new ArrayBuffer[Int]()
    for (j <- 0 to nrVertex-1) {
      val v = adjList(j)
      var minVal = arr(i-1)(j)
      for (inEdge <- v.inEdges) {
        if (arr(i-1)(inEdge.tail) != INFINITY)
          minVal = Math.min(minVal, arr(i-1)(inEdge.tail) + inEdge.weight)
      }
      lineArr.addOne(minVal)
    }
    arr.addOne(lineArr)
  }

  println(arr(nrVertex-1))

}
