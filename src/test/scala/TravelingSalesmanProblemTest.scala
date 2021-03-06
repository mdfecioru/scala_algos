import npcomplete.TravelingSalesmanProblem
import org.scalatest.funspec.AnyFunSpec
import util.GraphIO

class TravelingSalesmanProblemTest extends AnyFunSpec {
  describe("Running the Travel Salesman algo") {
    it("should find the following solution for tsp_small.txt") {
      val adjMatrix = GraphIO.undirectedWithWeightsAsMatrix("src/main/resources/tsp_small.txt")
      val result = TravelingSalesmanProblem.run(adjMatrix)

      assert(result == 46)
    }
    it("should find the following solution for tsp_small2.txt") {
      val adjMatrix = GraphIO.undirectedWithWeightsAsMatrix("src/main/resources/tsp_small2.txt")
      val result = TravelingSalesmanProblem.run(adjMatrix)

      assert(result == 32)
    }
  }
}
