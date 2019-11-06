package greedy

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object KruskalMST extends App {
  /**
    * Kruskal's Minimum Spanning Tree (MST) algorithm
    *   - INPUT DATA:
    *     - We have a edge-weighted undirected graph
    *       - V is the list of vertexes
    *       - E is the list of edges (each edge is undirected and has a weight)
    *   - PROBLEM:
    *     - Need to find the tree that contains all the graph nodes (spans the entire graph) and has the
    *       following property: the sum of all the edges in this tree is the minimum of all the trees that
    *       span the graph (MST)
    *   - HINT:
    *     - We order the edges in ascending order of their weights
    *     - We pick edges from the ordered list (starting with the cheapest) and we will add the edge to MST only
    *       if the edge will not create a cycle in the MST.
    *   - SOLUTION:
    *     - We will apply the strategy described in the "HINT" section and we will use the Union-Find algorithm
    *       to identify if adding an edge will create a cycle in MST
    *   - COMPLEXITY:
    *     - O(m * log(n)) where m is the number of edges and n tye number of vertices
    */

  case class Edge(tail: Int, head: Int, weight: Int)

  val filename = "src/main/resources/kruskal_mst.txt"
  val iter = Source.fromFile(filename).getLines()
  val nr_vertex =iter.next().toInt
  val edgeArr = new ArrayBuffer[Edge]()
  val MST = new ArrayBuffer[Edge]()
  val uf = UnionFind(nr_vertex)

  for (line <- iter) {
    val items = line.split(" ")
    val v1 = items(0).toInt - 1
    val v2 = items(1).toInt - 1
    val w = items(2).toInt
    edgeArr.addOne(Edge(v1, v2, w))
  }

  implicit object EdgeWeightOrdering extends Ordering[Edge] {
    override def compare(x: Edge, y: Edge): Int = x.weight compare y.weight
  }

  val orderedEdgeArr = edgeArr.sorted.reverse
  var mstValue = 0

  while (orderedEdgeArr.size != 0) {
    // Extract the cheapest edge
    val e = orderedEdgeArr.remove(orderedEdgeArr.size - 1)

    if (uf.Find(e.head, e.tail)) {
      uf.Union(e.head, e.tail)
      MST.addOne(e)
      mstValue += e.weight
    }
  }

  println("MST value is: " + mstValue)
  println("MST is: " + MST)
}

case class UnionFind(nr_vertex: Int) {

  case class Vertex(index: Int, leader: Int, followers: List[Int])
  val vertexArr = new ArrayBuffer[Vertex](nr_vertex)

  for (i <- 0 to nr_vertex-1) {
    vertexArr.addOne(Vertex(i, i, List[Int](i)))
  }

  def Union (v1: Int, v2: Int) = {
    val v1LeaderVertex = vertexArr(vertexArr(v1).leader)
    val v2LeaderVertex = vertexArr(vertexArr(v2).leader)

    // The vertex that has the most folloers will become the new "leader"; the other vertex is the "follower"
    val (leader, follower) = if (v1LeaderVertex.followers.length >= v2LeaderVertex.followers.length)
      (v1LeaderVertex, v2LeaderVertex)
    else (v2LeaderVertex, v1LeaderVertex)

    // The followers list for "leader" is the merge of the followers of "follower" and "leader"
    val leaderVertex = Vertex(leader.index, leader.index, leader.followers ++ follower.followers)
    // The followers list for "follower" is an empty list
    val followerVertex = Vertex(follower.index, leader.index, List())

    // For all the vertexes that were following "follower" set the leader "leader"
    for (followerIndex <- follower.followers)
      vertexArr(followerIndex) = Vertex(followerIndex, leader.index, List())

    vertexArr(leader.index) = leaderVertex
    vertexArr(follower.index) = followerVertex
  }

  // Returns true if the 2 vertexes do not belong to the same cluster
  def Find (v1: Int, v2: Int): Boolean = {
    vertexArr(v1).leader != vertexArr(v2).leader
  }
}
