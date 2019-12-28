package dynamic_programming

import util.GraphTypes.{Edge, Vertex}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object BellmanFordShortestPath {
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

  final val INFINITY = Int.MaxValue

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

  def run(adjList: ArrayBuffer[Vertex], startVertex: Int = 0): Option[ArrayBuffer[Int]] = {
    val nrVertex = adjList.size
    val arr = new ArrayBuffer[ArrayBuffer[Int]]()
    val lineArrSource = new ArrayBuffer[Int]()
    for (_ <- 0 to nrVertex-1) {
      lineArrSource.addOne(INFINITY)
    }
    lineArrSource(startVertex) = 0
    arr.addOne(lineArrSource)

    for (i <- 0 to nrVertex-1) {
      val lineArr = new ArrayBuffer[Int]()
      for (j <- 0 to nrVertex - 1) {
        val v = adjList(j)
        var minVal = arr(i)(j)
        for (inEdge <- v.inEdges) {
          if (arr(i)(inEdge.tail) != INFINITY)
            minVal = Math.min(minVal, arr(i)(inEdge.tail) + inEdge.weight)
        }
        lineArr.addOne(minVal)
      }
      arr.addOne(lineArr)
    }

    // We test if a negative cycle exists by checking if the last 2 runs of the loop generated the same values
    if (arr(nrVertex - 1).sameElements(arr(nrVertex))) return Some(arr(nrVertex - 1))
    None // If a negative cycle was detected we announce that by returning None
  }
}
