import greedy.DijkstraShortestPath
import org.scalatest.funspec.AnyFunSpec
import util.GraphTypes.{CheapestPath, Edge}

import scala.collection.mutable.ArrayBuffer

class DijkstraShortestPathTest extends AnyFunSpec {
  describe("Running using Bellman-Ford shortest path algo") {
    it("should find the following shortest path for dijkstra_shortest_path.txt input and startVertex 0") {
      val adjMatrix = DijkstraShortestPath.
        readGraphFromFile("src/main/resources/dijkstra_shortest_path.txt")
      val result = DijkstraShortestPath.run(adjMatrix, 0)
      assert(result.sameElements(Array(CheapestPath(0,0,ArrayBuffer()),
              CheapestPath(1,2,ArrayBuffer(Edge(0,1,2))),
              CheapestPath(2,5,ArrayBuffer(Edge(0,1,2), Edge(1,2,3))),
              CheapestPath(3,6,ArrayBuffer(Edge(0,1,2), Edge(1,2,3), Edge(2,3,1))),
              CheapestPath(4,7,ArrayBuffer(Edge(0,1,2), Edge(1,4,5))))))
    }
    it("should find the following shortest path for dijkstra_shortest_path.txt input and startVertex 3") {
      val adjMatrix = DijkstraShortestPath.
        readGraphFromFile("src/main/resources/dijkstra_shortest_path.txt")
      val result = DijkstraShortestPath.run(adjMatrix, 3)
      assert(result.sameElements(Array(CheapestPath(0,2147483647,null),
        CheapestPath(1,2147483647,null),
        CheapestPath(2,2147483647,null),
        CheapestPath(3,0,ArrayBuffer()),
        CheapestPath(4,4,ArrayBuffer(Edge(3,4,4))))))
    }
    it("should find the following shortest path for dijkstra_shortest_path2.txt input and startVertex 3") {
      val adjMatrix = DijkstraShortestPath.
        readGraphFromFile("src/main/resources/dijkstra_shortest_path2.txt")
      val result = DijkstraShortestPath.run(adjMatrix, 3)
      assert(result.sameElements(Array(CheapestPath(0,19,ArrayBuffer(Edge(3,2,7), Edge(2,1,8), Edge(1,0,4))),
        CheapestPath(1,15,ArrayBuffer(Edge(3,2,7), Edge(2,1,8))),
        CheapestPath(2,7,ArrayBuffer(Edge(3,2,7))),
        CheapestPath(3,0,ArrayBuffer()),
        CheapestPath(4,9,ArrayBuffer(Edge(3,4,9))),
        CheapestPath(5,11,ArrayBuffer(Edge(3,2,7), Edge(2,5,4))),
        CheapestPath(6,13,ArrayBuffer(Edge(3,2,7), Edge(2,5,4), Edge(5,6,2))),
        CheapestPath(7,14,ArrayBuffer(Edge(3,2,7), Edge(2,5,4), Edge(5,6,2), Edge(6,7,1))),
        CheapestPath(8,9,ArrayBuffer(Edge(3,2,7), Edge(2,8,2))))))
    }
  }
}
