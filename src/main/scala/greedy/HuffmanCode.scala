package greedy

import scala.io.Source

object HuffmanCode extends App {
  /**
    * Huffman prefix-free binary code
    * - INPUT DATA:
    *   - We have a text document, thus we have a set of characters that comprise the document
    * - PROBLEM:
    *   - We need to compress the document as much as possible. This means that we need to find the
    *     best encoding for each character that would optimize the final size of the encoded document.
    *   - What is requested here is the size (in bits) of the smallest encoding and the largest encoding
    *     used for characters
    * - HINT:
    *   - Use the character frequency in the document to assign smaller binary codes to characters
    *     that have a higher frequency.
    *   - Use binary prefix-free encoding scheme to encode characters (for each pair of characters the
    *     corresponding encodings will have the property that neither one is a prefix of the other) to
    *     avoid any ambiguity during the decoding process (otherwise we will not know whether we have just
    *     decoded a character or we are just dealing with the prefix of another character.
    *   - to generate the prefix-free binary codes one may create a binary tree where characters will always
    *     be leafs and a path to a certain character is not a sub-path for another character. Left child is
    *     encoded with bit "0" and right child is encoded with bit "1"
    * - SOLUTION:
    *   - Insert all the characters in a minHeap based on the character frequency in the document
    *   - While the heap is not empty
    *     - extract the 2 nodes with the smallest frequency
    *     - create a new node as a parent of the 2 nodes, with:
    *       - frequency = the sum of frequencies of the 2 children
    *       - the maximum encoding of a character belonging to the tree that has as root this new node is
    *         1 + MAX(maximum encoding of a character belonging to the tree that has as root each of the children)
    *       - the minimum encoding of a character belonging to the tree that has as root this new node is
    *         1 + MIN(minimum encoding of a character belonging to the tree that has as root each of the children)
    *   - At the end of the loop, we will have a single node which is the root of the tree that contains all the
    *     characters and the min/max encodings of all characters.
    * - COMPLEXITY:
    *   - O(n * log(n)) - where n is the number of characters in the document.
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
