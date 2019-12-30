package util

import scala.collection.mutable.ArrayBuffer

object PointType {

  case class Point(vertexIndex: Int, x: Double, y: Double)

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
