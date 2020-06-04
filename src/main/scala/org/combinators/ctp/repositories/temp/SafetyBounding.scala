package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.toplevel.PolygonScene
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, Point}

object SafetyBounding {
  def apply(robot : List[List[Float]], polyScene: PolygonScene) : PolygonScene = {

    val robotPoints: List[Point] = robot.map(vertex => {
      new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1)))
    })

    def robotDiameters(coords: List[Point]): List[Double] = {
      if (coords.isEmpty) return Nil
      val p1 = coords.head
      val rest = coords.tail
      val distances = rest.map(p => p.distance(p1))
      distances ++ robotDiameters(rest)
    }

    val boundingWidth: Double = robotDiameters(robotPoints).max

    val obstacleVertexLists: List[List[List[Float]]] = polyScene.obstacles.map(i => i.map(polyScene.vertices))
    val hulls: List[Geometry] = obstacleVertexLists.map { actualCoords =>
      // generate convex hull for current batch of points
      val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      hullGeometry
    }

    val bufferedHulls: List[Geometry] = hulls.map(hull =>{
      hull.buffer(boundingWidth).getEnvelope
    })

    val bufferedVertexList: List[List[List[Float]]] = bufferedHulls.map(hull => {
      val coords = hull.getCoordinates.toList
      coords.map(c => List(c.x.toFloat, c.y.toFloat))
    })

    val newVertices = bufferedVertexList.flatten.distinct
    val newObstacles = bufferedVertexList.map(vertexList =>
      vertexList.map(vertex => newVertices.indexOf(vertex)))

    PolygonScene(newVertices,newObstacles,polyScene.boundaries)
  }
}
