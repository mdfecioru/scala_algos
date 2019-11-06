package knapsack

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Knapsack extends App {
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
  val items = new ListBuffer[Item]()
  for (line <- iter) {
    val item = line.split(" ")
    items.addOne(Item(item(0).toInt, item(1).toInt))
  }

  var arr = new ListBuffer[ListBuffer[Int]]()
  var line_arr0 = new ListBuffer[Int]()
  for (j <- 0 to weight) line_arr0.addOne(0)
  arr.addOne(line_arr0)

  for (i <- 1 to nr_items) {
    val line_arr = new ListBuffer[Int]()
    for (j <- 0 to weight) {
      val item = items(i-1)
      if (item.weight > j) line_arr.addOne(arr(i-1)(j))
      else line_arr.addOne(Math.max(arr(i-1)(j), arr(i-1)(j-item.weight) + item.value))
    }
    arr.addOne(line_arr)
  }


  val result = arr(nr_items)(weight)
  println(s"Result is: ${result}")
}
