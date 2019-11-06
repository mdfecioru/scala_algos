package knapsack

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object KnapsackOptimized extends App {

  /**
    *
    * - INPUT DATA:
    * - PROBLEM:
    * - HINT:
    * - SOLUTION:
    * - COMPLEXITY:
    *
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
    if (i % 10 == 0) println(s"Iteration ${i} out of ${nr_items}")
    index ^= 1
  }

  val result = arr(index ^ 1)(weight)
  println(s"Result is: ${result}")
}
