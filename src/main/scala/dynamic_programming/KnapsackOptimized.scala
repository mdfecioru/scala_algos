package dynamic_programming

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object KnapsackOptimized extends App {

  /**
    * The knapsack problem
    * - INPUT DATA:
    *   - We have N items, each of them having a value (v1, v2 ... vN) and a weight (w1, w2 ... wN)
    *   - We also have a knapsack of capacity W
    * - PROBLEM:
    *   - We want to identify the set of items we can put in the knapsack that will satisfy 2 conditions:
    *     - The sum of their weights is less than the knapsack capacity W
    *     - The sum of their values is the maximum across all other possible set of items
    * - HINT:
    *   - We compute solutions for all the sub-problems of
    *     - knapsack capacities from 0 to W
    *     - selecting 0 to n items
    * - SOLUTION:
    *   - we will construct a matrix with elements on the X-axe and knapsack capacities on the Y-axe. The value
    *     of cell (i, j) is the optimal solution of the following problem: if I have access to only the first "i"
    *     items and I have a knapsack of capacity j, what is that maximum value I can put in the knapsack?
    *       - thus, the cell of index (N, W) has the solution for the final problem
    *   - for "i" from 1..N (take subgroups of items)
    *     - for "j" 1 to W (take knapsacks of different sizes)
    *       - matrix(i, j) = MAX( matrix(i-1, j), matrix (i-1, j-wi) + vi )
    *         - the 2 options from the above MAX expression are:
    *           - either we do not pick the item "i", thus we will inherit the solution for the sub-problem with
    *             the same backpack size but using only items from 1..i-1
    *           - or we pick the item "i" and in this case the solution for the (i, j) sub-problem is the sum of the
    *             value of item "i" and the solution of the sub-problem using only items 0..i-1 and with a backpack
    *             size of j-wi (because wi space is used by item "i" which we decided we will pick in the backpack)
    *   - the cell of index (N, W) has the solution for the final problem
    *   - THE OPTIMIZATION here (compared with the non-optimal solution) is a memory size one (not a complexity one).
    *     We observe that the algorithm only uses the current and the previous column, thus, instead of keeping the
    *     entire matrix, we only keep 2 columns.
    * - COMPLEXITY:
    *   - O(N*W)
    */

  case class Item(value: Int, weight: Int)

  val filename = "src/main/resources/knapsack.txt"
  val iter = Source.fromFile(filename).getLines()
  val header = iter.next().split(" ")
  val weight = header(0).toInt
  val nr_items = header(1).toInt
  val items = new ArrayBuffer[Item](nr_items)
  for (line <- iter) {
    val item = line.split(" ")
    items.addOne(Item(item(0).toInt, item(1).toInt))
  }

  var arr = new ArrayBuffer[ArrayBuffer[Long]](2)
  var line_arr0 = new ArrayBuffer[Long](weight)
  var line_arr1 = new ArrayBuffer[Long](weight)
  for (j <- 0 to weight) line_arr0.addOne(0)
  for (j <- 0 to weight) line_arr1.addOne(0)
  arr.addOne(line_arr0)
  arr.addOne(line_arr1)

  var index = 1
  for (i <- 1 to nr_items) {
    val line_arr_curr = arr(index)
    val line_arr_last = arr(index ^ 1)
    for (j <- 0 to weight) {
      val item = items(i - 1)
      if (item.weight > j) line_arr_curr(j) = line_arr_last(j)
      else line_arr_curr(j) = Math.max(line_arr_last(j), line_arr_last(j - item.weight) + item.value.toLong)
    }
    index ^= 1
  }

  val result = arr(index ^ 1)(weight)
  println(s"Result is: ${result}")
}
