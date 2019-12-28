package npcomplete

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object TravelingSalesmanProblemGreedy {

  case class Point(vertexIndex: Int, x: Double, y: Double)
  case class TSP(distance: Double, path: ArrayBuffer[Int])

  def readGraphFromFile(filename: String): ArrayBuffer[Point] = {

    val iter = Source.fromFile(filename).getLines()
    val nrVertex = iter.next().toInt
    val vertexList = new ArrayBuffer[Point](nrVertex)

    for (line <- iter) {
      val items = line.split(" ")
      val vertexID = items(0).toInt - 1
      val x = items(1).toDouble
      val y = items(2).toDouble
      vertexList.addOne(Point(vertexID, x, y))
    }

    vertexList
  }

  def run(vertexList: ArrayBuffer[Point], startIndex: Int = 0): TSP = {
    val path = new ArrayBuffer[Int]()
    val startPoint = vertexList(startIndex)
    var sourcePoint = startPoint
    var pathDistance: Double = 0.0

    path.addOne(startIndex)
    vertexList.remove(startIndex)
    while (vertexList.nonEmpty) {
      val nextPointIndex = getClosestPoint(sourcePoint, vertexList)
      val nextPoint = vertexList(nextPointIndex)
      vertexList.remove(nextPointIndex)
      path.addOne(nextPoint.vertexIndex)
      pathDistance = pathDistance +  Math.sqrt(distPointsNoSqrt(sourcePoint, nextPoint))
      sourcePoint = nextPoint
    }

    pathDistance = pathDistance + Math.sqrt(distPointsNoSqrt(sourcePoint, startPoint))
    path.addOne(startIndex)

    TSP(pathDistance, path)
  }

  def distPointsNoSqrt(p1: Point, p2: Point): Double = {
    (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)
  }

  def getClosestPoint(source: Point, listPoints: ArrayBuffer[Point]): Int = {
    var minDist = distPointsNoSqrt(source, listPoints(0))
    var minIndex = 0

    for (i <- 1 to listPoints.size-1) {
      val dist = distPointsNoSqrt(source, listPoints(i))
      if (dist < minDist) {
        minDist = dist
        minIndex = i
      }
    }

    minIndex
  }
}
