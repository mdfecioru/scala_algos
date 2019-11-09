package dynamic_programming

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object BSTProbab extends App {
  /**
    * Binary Search Tree (BST) with probabilities on the likelihood for a node to be searched
    * - INPUT DATA:
    *   - We have a set of N nodes with integer values associated that we want to store in a BST to allow
    *     fast search based on the integer value.
    *   - For each node, we also have a probability (pi) for that node to be needed / searched for
    * - PROBLEM:
    *   - What should be the BST that will optimize the overall search costs across all values?
    *     - Overall Search Cost = SUM(ci * pi)
    *       - ci = the cost of finding element "i" in the BST (the length of the path to "i")
    *       - p1 = the probability for element "i" to be needed (searched for)
    * - HINT:
    *   - Let's assume the nodes 1, 2, 3, ... N are sorted according to their values
    *   - We will start by solving smaller scale problems: groups of 1 element, groups of 2 elements etc
    *   - The sub-problem for the group of "s" elements (i, i+1, ..., j-1, j) will have the solution:
    *     - for every "r" in the (i, j) interval compute the overall search cost of a BST that has:
    *       - "r" as the root
    *       - (i, r-1) as the sub-tree in the left - this sub-problem we have already solved
    *       - (r+1, j) as the sub-tree in the right - this sub-problem we have already solved
    *     - Use the following formula:
    *       - OSC(i, j) = SUM(pi..pj) + OSC(i, r-1) + OSC(r+1, j)
    *         - SUM(pi..pj) is the sum of probabilities of nodes in the (i, j) interval
    *         - OSC(i, j) = Optimal Search Cost for the (i, j) problem
    *     - the SBT with the minimum overall cost will be the solution for the (i, j) interval
    * - SOLUTION:
    *   - The nodes 1, 2, ... N are already sorted after their value. We only get the probabilities (we have no
    *     interest in the values)
    *   - for all problem sizes "s" from 1 to N
    *     - for all groups (i, i+s) of size "s"
    *       - for each r in the (i, i+s) interval
    *         - compute the cost of the BST of (i, i+s) nodes with "r" as root
    *           - CostBST(i, i+s)(r) = SUM(pi..pi+s) + OSC(i, r-1) + OSC(r+1, i+s)
    *       - the minimum across all CostBST(i, i+s)(r) is the solution for the (i, i+s) sub-problem
    *   - the solution for the last sub-problem (the sub-problem of size N) is the solution of our problem
    * - COMPLEXITY:
    *   - O(n * n * n)
    */

  val filename = "src/main/resources/bstprobab.txt"
  val iter = Source.fromFile(filename).getLines()
  val nr_nodes =iter.next().toInt
  val probab = new ArrayBuffer[Int](nr_nodes)
  for (line <- iter) {
    probab.addOne(line.toInt)
  }

  val arr = new ArrayBuffer[ArrayBuffer[Int]](nr_nodes)
  for (i <- 0 to nr_nodes-1) {
    val arr_row = new ArrayBuffer[Int](nr_nodes)
    for (j <- 0 to nr_nodes-1) {
      arr_row.addOne(0)
    }
    arr.addOne(arr_row)
  }

  for (s <- 0 to nr_nodes-1) {
    for (i <- 0 to nr_nodes-s-1) {
      var min = Int.MaxValue
      var pk = 0
      for (k <- i to i+s) { pk += probab(k) }
      for (r <- i to i+s) {
        val left = if (i > r-1) 0 else arr(i)(r-1)
        val right = if (r+1 > i+s) 0 else arr(r+1)(i+s)
        min = Math.min(min, pk + left + right)
      }
      arr(i)(i+s) = min
    }
  }

  println(s"Result is: ${arr(0)(nr_nodes-1)}")
}
