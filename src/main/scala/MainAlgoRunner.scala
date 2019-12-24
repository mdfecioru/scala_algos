import dynamic_programming.BellmanFordShortestPath

object MainAlgoRunner extends App {

/*
  /* BellmanFordShortestPath algorithm */
  val adjMatrix = BellmanFordShortestPath.
                    readGraphFromFile("src/main/resources/bellmanford_shortest_path3.txt")
  val result = BellmanFordShortestPath.run(adjMatrix)
  if (result.isDefined) println(result)
  else println("NEGATIVE CYCLE DETECTED!")
*/

}
