package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.toplevel.{PolySceneLineSegmentation, PolygonScene}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineSegment}

object vcd2D {

  def apply(polyScene : PolygonScene) : PolySceneLineSegmentation = {

    val topBoundary: Float = polyScene.boundaries(1) / 2
    val bottomBoundary: Float = - topBoundary

    val obstacleVertexLists: Seq[List[List[Float]]] = polyScene.obstacles.map(i => i.map(polyScene.vertices))
    val hulls: Seq[Geometry] = obstacleVertexLists.map { actualCoords =>
      // generate convex hull for current batch of points
      val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      hullGeometry
    }

    val preUpExtVertices: Seq[List[Float]] = upwardExtendableVertices(obstacleVertexLists)
    val preDownExtVertices: Seq[List[Float]] = downwardExtendableVertices(obstacleVertexLists)
    val upExtVertices: Seq[List[Float]] = preUpExtVertices
    val downExtVertices: Seq[List[Float]] = preDownExtVertices

    val vertexLinesUP : Seq[List[List[Float]]] = upExtVertices.map(vertex => {
      val vertexCoord = new Coordinate(vertex.head, vertex(1))
      val topCoord = new Coordinate(vertex.head, topBoundary)
      val line = new LineSegment(vertexCoord,topCoord)
      assert(line.isVertical, "Segmentation lines should always be vertical!")
      val intersectionGeometry: Seq[Geometry] = hulls.map(hull =>
        hull.intersection(line.toGeometry(new GeometryFactory))).filter(intersect => !intersect.isEmpty)
      val intersectionCoordinate = intersectionGeometry.map(intersect =>{
        intersect.getCoordinates.sortWith((c1, c2) => c1.y <= c2.y)
      })

      if(intersectionCoordinate.nonEmpty){
        val sortedIntersection = intersectionCoordinate.sortWith((ls1,ls2) => ls1.head.y <= ls2.head.y)
        val newTopCoord = if(sortedIntersection.length > 1){
          sortedIntersection(1).head
        } else {
          topCoord
        }
        assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")
        List(vertex, List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
      } else {
        //This case should not be reachable
        List(vertex,List(vertex.head,topBoundary))
      }
    })

    val vertexLinesDown : Seq[List[List[Float]]] = downExtVertices.map(vertex => {
      val vertexCoord = new Coordinate(vertex.head, vertex(1))
      val bottomCoord = new Coordinate(vertex.head, bottomBoundary)
      val line = new LineSegment(vertexCoord,bottomCoord)
      assert(line.isVertical, "Segmentation lines should always be vertical!")
      val intersectionGeometry = hulls.map(hull =>
        hull.intersection(line.toGeometry(new GeometryFactory))).filter(intersect => !intersect.isEmpty)
      val intersectionCoordinate = intersectionGeometry.map(intersect =>{
        intersect.getCoordinates.sortWith((c1, c2) => c1.y >= c2.y)
      })

      if(intersectionCoordinate.nonEmpty){
        val sortedIntersection = intersectionCoordinate.sortWith((ls1,ls2) => ls1.head.y >= ls2.head.y)
        val newTopCoord = if(sortedIntersection.length > 1){
          sortedIntersection(1).head
        } else {
          bottomCoord
        }
        assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")
        List(vertex, List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
      } else {
        //This case should not be reachable
        List(vertex, List(vertex.head,bottomBoundary))
      }
    })

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
      val newVerticesInHull = newVertices.filter(vertex => hullGeometry.contains(new GeometryFactory().createPoint(new Coordinate(vertex.head,vertex(1)))))
      obstacle ++ newVerticesInHull
    }

    val polySceneSegObstacles = newObstacles.map(vertexList =>
      vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

    val lineList: Seq[List[Int]] = vertexLineList.map(vertexList =>
      vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

    //TODO compute cells from lines

    PolySceneLineSegmentation(polySceneSegVertices.toList,
      polySceneSegObstacles.toList,
      polyScene.boundaries,
      topVertices.toList,
      bottomVertices.toList,
      lineList.toList)
    //PolySceneCellSegmentation(polySceneCellSegVertices.toList,polyScene.obstacles, polyScene.boundaries,Nil)

  }

  def upwardExtendableVertices(obstacleVertexLists: Seq[List[List[Float]]]) : Seq[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndUpPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head,vertex(1) + 0.01))))
      verticesAndUpPoints.filter(pair => !hullGeometry.contains(pair._2)).map(pair => pair._1)
    }
  }

  def downwardExtendableVertices(obstacleVertexLists: Seq[List[List[Float]]]) : Seq[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndDownPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head,vertex(1) - 0.01))))
      verticesAndDownPoints.filter(pair => !hullGeometry.contains(pair._2)).map(pair => pair._1)
    }
  }

 }
