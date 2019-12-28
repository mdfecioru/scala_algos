import npcomplete.TravelingSalesmanProblemGreedy
import org.scalatest.funspec.AnyFunSpec

class TravelingSalesmanProblemGreedyTest extends AnyFunSpec {
  describe("Running the greedy version of Travel Salesman algo") {
    it("should find the following solution for tsp_greedy.txt") {
      val vertexList = TravelingSalesmanProblemGreedy.
        readGraphFromFile("src/main/resources/tsp_greedy.txt")
      val result = TravelingSalesmanProblemGreedy.run(vertexList)

      assert(result.distance.toInt == 26)
      assert(result.path.sameElements(Array(0, 1, 2, 3, 4, 5, 0)))
    }
  }
}
