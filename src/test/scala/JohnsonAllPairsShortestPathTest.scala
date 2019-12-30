import dynamic_programming.JohnsonAllPairsShortestPath
import org.scalatest.funspec.AnyFunSpec
import util.GraphIO

import scala.collection.mutable.ArrayBuffer

class JohnsonAllPairsShortestPathTest  extends AnyFunSpec {
  describe("Running using Johnson shortest path algo") {
    it("should find all shortest paths for floydwarshall_all_pairs_shortest_path.txt") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/floydwarshall_all_pairs_shortest_path.txt")
      val result = JohnsonAllPairsShortestPath.run(adjMatrix)

      assert(result.isDefined)
      assert(result.get.sameElements(Array(ArrayBuffer(0, -1, -2, 0),
        ArrayBuffer(4, 0, 2, 4),
        ArrayBuffer(5, 1, 0, 2),
        ArrayBuffer(3, -1, 1, 0))))
    }
    it("should find all shortest paths for floydwarshall_all_pairs_shortest_path2.txt") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/floydwarshall_all_pairs_shortest_path2.txt")
      val result = JohnsonAllPairsShortestPath.run(adjMatrix)

      assert(result.isDefined)
      assert(result.get.sameElements(Array(ArrayBuffer(0, 3, 8, 2, -4),
        ArrayBuffer(3, 0, 11, 1, -1),
        ArrayBuffer(-3, 0, 0, -5, -7),
        ArrayBuffer(2, 5, 10, 0, -2),
        ArrayBuffer(8, 11, 16, 6, 0))))
    }
    it("should FAIL finding all shortest paths for floydwarshall_all_pairs_shortest_path3.txt due to a negative cycle detected.") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/floydwarshall_all_pairs_shortest_path3.txt")
      val result = JohnsonAllPairsShortestPath.run(adjMatrix)
      assert(!result.isDefined)
    }
    it("should find all shortest paths for johnson_all_pairs_shortest_path.txt") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/johnson_all_pairs_shortest_path.txt")
      val result = JohnsonAllPairsShortestPath.run(adjMatrix)

      assert(result.isDefined)
      assert(result.get.sameElements(Array(ArrayBuffer(0, -2, -3, -1, -6, 2147483647),
        ArrayBuffer(3, 0, -1, 1, -4, 2147483647),
        ArrayBuffer(4, 2, 0, 2, -3, 2147483647),
        ArrayBuffer(2147483647, 2147483647, 2147483647, 0, 2147483647, 2147483647),
        ArrayBuffer(2147483647, 2147483647, 2147483647, 2147483647, 0, 2147483647),
        ArrayBuffer(2147483647, 2147483647, 2147483647, 1, -4, 0))))
    }
  }
}
