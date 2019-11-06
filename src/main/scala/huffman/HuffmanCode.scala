
import scala.io.Source

object HuffmanCode extends App {
  /**
    *
    * - INPUT DATA:
    * - PROBLEM:
    * - HINT:
    * - SOLUTION:
    * - COMPLEXITY:
    *
    */

  sealed trait Tree
  case class Node(left: Tree, right: Tree, value: Int, min: Int, max: Int) extends Tree
  case object EmptyNode extends Tree

  object MinOrder extends Ordering[Node] {
    override def compare(x: Node, y: Node): Int = y.value compare x.value
  }

  val minHeap = scala.collection.mutable.PriorityQueue.empty(MinOrder)

  val filename = "src/main/resources/huffman.txt"
  for (line <- Source.fromFile(filename).getLines) {
    minHeap.addOne(Node(EmptyNode, EmptyNode, line.toInt, 0, 0))
  }

  var node = minHeap.dequeue()
  while (!minHeap.isEmpty) {
    val secondNode = minHeap.dequeue()
    minHeap.addOne(Node(node, secondNode, node.value + secondNode.value,
      Math.min(node.min, secondNode.min) + 1, Math.max(node.max, secondNode.max) + 1))
    node = minHeap.dequeue()
  }

  println(s"MAX : ${node.max}  ||  MIN: ${node.min}")
}
