import dynamic_programming.BellmanFordShortestPath
import org.scalatest.funspec.AnyFunSpec
import util.GraphIO

class BellmanFordShortestPathTest extends AnyFunSpec {
  describe("Running using Bellman-Ford shortest path algo") {
    it("should find the following shortest path for bellmanford_shortest_path.txt input") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/bellmanford_shortest_path.txt")
      val result = BellmanFordShortestPath.run(adjMatrix)
      assert(result.isDefined)
      assert(result.get.sameElements(Array(0, 2, 3, 4, 6)))
    }
    it("should find the following shortest path for bellmanford_shortest_path2.txt input") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/bellmanford_shortest_path2.txt")
      val result = BellmanFordShortestPath.run(adjMatrix)
      assert(result.isDefined)
      assert(result.get.sameElements(Array(0, -1, 2, -2, 1)))
    }
    it("should find the following shortest path for bellmanford_shortest_path2.txt input with inputVertex 3") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/bellmanford_shortest_path2.txt")
      val result = BellmanFordShortestPath.run(adjMatrix, 3)
      assert(result.isDefined)
      assert(result.get.sameElements(Array(2147483647, 1, 4, 0, 3)))
    }
    it("should FAIL finding shortest path for bellmanford_shortest_path3.txt input because of a negative cycle") {
      val adjMatrix = GraphIO.
        directedWithWeightsAsList("src/main/resources/bellmanford_shortest_path3.txt")
      val result = BellmanFordShortestPath.run(adjMatrix)
      assert(!result.isDefined)
    }
  }
}

