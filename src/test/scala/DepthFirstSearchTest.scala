import graph.DepthFirstSearch
import org.scalatest.funspec.AnyFunSpec
import util.GraphIO

class DepthFirstSearchTest extends AnyFunSpec {
  describe("Running using DepthFirstSearch shortest path algo") {
    it("should generate the following ordered list of vertex indexes for dfs.txt") {
      val adjList = GraphIO.directedNoWeightsAsList("src/main/resources/dfs.txt")
      val dfsOrder = DepthFirstSearch.run(adjList)
      assert(dfsOrder.sameElements(Array(5, 4, 6, 9, 8, 7, 10, 3, 1, 0, 2)))
    }
    it("should generate the following graph printout for dfs2.txt") {
      val adjList = GraphIO.directedNoWeightsAsList("src/main/resources/dfs2.txt")
      val dfsOrder = DepthFirstSearch.run(adjList)
      assert(dfsOrder.sameElements(Array(2, 4, 1, 7, 5, 8, 0, 3, 6)))
    }
  }
}
