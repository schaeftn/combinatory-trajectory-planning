package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.toplevel.{PolySceneLineSegmentation, PolygonScene}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineSegment}

object Vcd2D {

  def apply(polyScene: PolygonScene): PolySceneLineSegmentation = {

    val topBoundary: Float = polyScene.boundaries(1) / 2
    val bottomBoundary: Float = -topBoundary

    val obstacleVertexLists: Seq[List[List[Float]]] = polyScene.obstacles.map(i => i.map(polyScene.vertices))
    val hulls: Seq[Geometry] = obstacleVertexLists.map { actualCoords =>
      // generate convex hull for current batch of points
      val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      hullGeometry
    }

    val upExtVertices: Seq[List[Float]] = upwardExtendableVertices(obstacleVertexLists)
    val downExtVertices: Seq[List[Float]] = downwardExtendableVertices(obstacleVertexLists)

    val vertexLinesUP: Seq[List[List[Float]]] = computeLines(upExtVertices, topBoundary, hulls, (d1, d2) => d1 <= d2)

    val vertexLinesDown: Seq[List[List[Float]]] = computeLines(downExtVertices, bottomBoundary, hulls, (d1, d2) => d1 >= d2)

    val vertexLineList = vertexLinesUP ++ vertexLinesDown

    val newVertices: Seq[List[Float]] = vertexLineList.flatMap(vertexLine =>
      vertexLine.filter(vertex => !polyScene.vertices.contains(vertex)))

    val polySceneSegVertices: Seq[List[Float]] = polyScene.vertices ++ newVertices

    val topVertices = newVertices.filter(vertex => vertex(1) == topBoundary)
      .map(vertex => polySceneSegVertices.indexOf(vertex))

    val bottomVertices = newVertices.filter(vertex => vertex(1) == bottomBoundary)
      .map(vertex => polySceneSegVertices.indexOf(vertex))

    val newObstacles = obstacleVertexLists.map { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val newVerticesInHull = newVertices.filter(vertex => hullGeometry.contains(new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1)))))
      obstacle ++ newVerticesInHull
    }

    val polySceneSegObstacles = newObstacles.map(vertexList =>
      vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

    val lineList: Seq[List[Int]] = vertexLineList.map(vertexList =>
      vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

    PolySceneLineSegmentation(polySceneSegVertices.toList,
      polySceneSegObstacles.toList,
      polyScene.boundaries,
      topVertices.toList,
      bottomVertices.toList,
      lineList.toList)
  }

  def upwardExtendableVertices(obstacleVertexLists: Seq[List[List[Float]]]): Seq[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndUpPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1) + 0.01))))
      verticesAndUpPoints.filter(pair => !(hullGeometry.distance(pair._2) == 0)).map(pair => pair._1)
    }
  }

  def downwardExtendableVertices(obstacleVertexLists: Seq[List[List[Float]]]): Seq[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndDownPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1) - 0.01))))
      verticesAndDownPoints.filter(pair => !(hullGeometry.distance(pair._2) == 0)).map(pair => pair._1)
    }
  }

  def computeLines(extVertices: Seq[List[Float]], boundary: Float, hulls: Seq[Geometry], partialOrder: (Double, Double) => Boolean): Seq[List[List[Float]]] = extVertices.map(vertex => {
    val vertexCoord = new Coordinate(vertex.head, vertex(1))
    val boundaryCoord = new Coordinate(vertex.head, boundary)
    val line = new LineSegment(vertexCoord, boundaryCoord)

    assert(line.isVertical, "Segmentation lines should always be vertical!")

    val intersectionGeometry: Seq[Geometry] = hulls.map(hull =>
      hull.intersection(line.toGeometry(new GeometryFactory))).filter(intersect => !intersect.isEmpty)
    val intersectionCoordinate = intersectionGeometry.map(intersect => {
      intersect.getCoordinates.sortWith((c1, c2) => partialOrder(c1.y, c2.y))
    })

    assert(intersectionCoordinate.nonEmpty, "intersectionCoordinate has to have at least one element")
    val sortedIntersection = intersectionCoordinate.sortWith((ls1, ls2) => partialOrder(ls1.head.y, ls2.head.y))
    val newTopCoord = if (sortedIntersection.length > 1) {
      sortedIntersection(1).head
    } else {
      boundaryCoord
    }

    assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")

    List(vertex, List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
  })

}
