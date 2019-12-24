package util

import scala.collection.mutable.ArrayBuffer

object GraphTypes {

  case class Edge(tail: Int, head: Int,  weight: Int)
  case class Vertex(outEdges: ArrayBuffer[Edge], inEdges: ArrayBuffer[Edge])

}
