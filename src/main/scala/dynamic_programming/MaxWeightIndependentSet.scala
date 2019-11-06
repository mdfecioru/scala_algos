package dynamic_programming

import scala.collection.mutable.ListBuffer

import scala.io.Source

object MaxWeightIndependentSet  extends App {
  /**
    *
    * - INPUT DATA:
    *   - We have a path of vertexes and each vertex has a weight assigned
    * - PROBLEM:
    *   - Identify the set of non-consecutive vertexes (thus vertexes that are in the same path but no pair of
    *     selected vertex are consecutive in the original path) that have the sum of weight maximum
    * - HINT:
    *   - Compute the solutions for all sub-problems by assuming that the solutions of the problems of lower cardinality
    *     were already solved.
    * - SOLUTION:
    *   - arr is the array where we will keep the results of partial solutions
    *   - Initialization:
    *     - arr(0)  0
    *     - arr(1) = the weight of the first vertex
    *   - for the rest of the vertexes, we really have 2 options:
    *     - the vertex was added in the path up to it: in this case the previous vertex is not in the path, thus we
    *       will take into account the partial solution of the problem prior to the previous vertex
    *     - the vertex was NOT added in the path: in this case, the solution for the problem containing the current
    *       vertex is the same as the one for the previous vertex.
    *   - In code, the previous statements translate to the following:
    *     - arr(index) = Math.max(arr(index-1), arr(index-2) + weights(index))
    *   - Once we have filled in the solution for all the sub-problems (the "arr" array) we can walk back through the
    *     "arr" array and identify the vertexes that took part at building the path for the solution of the final
    *     problem: in each step we test if the solution of the current problem was
    *       - simply the same as the previous problem (in this case we do not add the current vertex in the path)
    *       - the result of adding the current vertex weight to the solution prior to the previous problem (in this
    *         case we add the current vertex in the path)
    * - COMPLEXITY:
    *   - O(n)
    */

  var weights = new ListBuffer[Long]()
  val filename = "src/main/resources/mwis.txt"
  // Add a fake element at the beginning of the list to force the indexing to start from 1 (instead of 0)
  weights += 0
  for (line <- Source.fromFile(filename).getLines) {
    weights += line.toInt
  }

  // "arr" will contain the solutions for all the sub-problems
  var arr = new ListBuffer[Long]()
  arr += weights(0)
  arr += weights(1)
  for (index <- 2 to weights.size-1) {
    arr += Math.max(arr(index-1), arr(index-2) + weights(index))
  }

  // In "path" we re-construct the path that generated the final solution
  var path = new ListBuffer[Long]()
  var index = arr.size-1
  while (index >= 2) {
    if (arr(index-1) >= arr(index-2) + weights(index)) {
      index -= 1
    }
    else {
      path.insert(0, index)
      index -= 2
    }
  }
  // If the last element added to the path has index 3, then we add index 1 too.
  if (path(0) == 3) path.insert(0, 1)

  println(arr)
  println(path)

}
