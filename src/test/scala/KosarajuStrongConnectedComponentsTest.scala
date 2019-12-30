import graph.KosarajuStrongConnectedComponents
import org.scalatest.funspec.AnyFunSpec
import util.GraphIO

class KosarajuStrongConnectedComponentsTest extends AnyFunSpec {
  describe("Running Kosaraju's strong connected components algo") {
    it("should generate the following strong connected components for dfs.txt") {
      val adjList = GraphIO.directedNoWeightsAsList("src/main/resources/dfs.txt")
      val scc = KosarajuStrongConnectedComponents.run(adjList)
      assert(scc.sameElements(Array(2, 2, 2, 3, 6, 6, 6, 10, 10, 10, 10)))
    }
    it("should generate the following strong connected components for dfs2.txt") {
      val adjList = GraphIO.directedNoWeightsAsList("src/main/resources/dfs2.txt")
      val scc = KosarajuStrongConnectedComponents.run(adjList)
      assert(scc.sameElements(Array(6, 7, 8, 6, 7, 8, 6, 7, 8)))
    }
  }
}
