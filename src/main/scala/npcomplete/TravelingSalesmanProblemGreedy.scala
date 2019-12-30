package npcomplete

import util.PointType._

import scala.collection.mutable.ArrayBuffer

object TravelingSalesmanProblemGreedy {

  case class TSP(distance: Double, path: ArrayBuffer[Int])

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
}
