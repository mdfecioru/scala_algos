import greedy.DijkstraShortestPath

object MainAlgoRunner extends App {

  /* BellmanFordShortestPath algorithm */
  val adjMatrix = DijkstraShortestPath.
                    readGraphFromFile("src/main/resources/dijkstra_shortest_path.txt")
  val result = DijkstraShortestPath.run(adjMatrix, 3)

  for (i <- result)
    println(i)

}
