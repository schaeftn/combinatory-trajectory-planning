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

    //println("obstacles: " + polyScene.obstacles.toString())
    //println("vertexObstacles: " + obstacleVertexLists.toString())

    val preUpExtVertices: Seq[List[Float]] = upwardExtendableVertices(obstacleVertexLists)
    val preDownExtVertices: Seq[List[Float]] = downwardExtendableVertices(obstacleVertexLists)
    //val upAndDownExtVertices: Seq[List[Float]] = preUpExtVertices.intersect(preDownExtVertices)
    val upExtVertices: Seq[List[Float]] = preUpExtVertices//.diff(upAndDownExtVertices)
    val downExtVertices: Seq[List[Float]] = preDownExtVertices//.diff(upAndDownExtVertices)

    //println("preUp: " + preUpExtVertices.toString())
    //println("up: "+ upExtVertices.toString())
    //println("preDown: " + preDownExtVertices.toString())
    //println("down: "+ downExtVertices.toString())
    //println("UpAndDown: " + upAndDownExtVertices.toString())

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
      //println("vertex: "+vertex.toString())
      //println("intersectionGeometry: " + intersectionGeometry.toString())
      //println("vertexLinesUp - intersectionCoordinate: "+ intersectionCoordinate.map(array => array.map(c => c.toString).toList.toString).toString())
      //assert(vertex.length>=3, "A vertex should have at least 3 components?!")
      if(intersectionCoordinate.nonEmpty){
        val sortedIntersection = intersectionCoordinate.sortWith((ls1,ls2) => ls1.head.y <= ls2.head.y)
        //println("sorted Intersection: " + sortedIntersection.map(array => array.toList.toString()).toString())
        val newTopCoord = if(sortedIntersection.length > 1){
          sortedIntersection(1).head
        } else {
          //println("intersectionGeometry just the same?: " + intersectionGeometry.toString())
          //intersectionCoordinate.sortWith((ls1,ls2) => ls1.head.y <= ls2.head.y).head.head
          topCoord
        }
        //println("newTop: " + newTopCoord.toString)
        assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")
        List(vertex, List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
      } else {
        //assert(vertex.length>=3, "A vertex should have at least 3 components?!")
        List(vertex,List(vertex.head,topBoundary))
      }
    })

    //println("vertexLinesUp: "+ vertexLinesUP.toString)

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
      //println("vertex: "+vertex.toString())
      //println("intersectionGeometry: " + intersectionGeometry.toString())
      //println("vertexLinesUp - intersectionCoordinate: "+ intersectionCoordinate.map(array => array.map(c => c.toString).toList.toString).toString())
      //assert(vertex.length>=3, "A vertex should have at least 3 components?!")
      if(intersectionCoordinate.nonEmpty){
        val sortedIntersection = intersectionCoordinate.sortWith((ls1,ls2) => ls1.head.y >= ls2.head.y)
        //println("sorted Intersection: " + sortedIntersection.map(array => array.toList.toString()).toString())
        val newTopCoord = if(sortedIntersection.length > 1){
          sortedIntersection(1).head
        } else {
          bottomCoord
          //intersectionCoordinate.sortWith((ls1,ls2) => ls1.head.y <= ls2.head.y).head.head
        }
        //println("newTop: " + newTopCoord.toString)
        assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")
        List(vertex, List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
      } else {
        //println("why am i here?")
        //assert(vertex.length>=3, "A vertex should have at least 3 components?!")
        List(vertex, List(vertex.head,bottomBoundary))
      }
    })

    //println("vertexLinesDown: " + vertexLinesDown.toString)

    /*
    val vertexLinesUPandDOWN : Seq[List[List[Float]]] = upAndDownExtVertices.map(vertex => {
      val vertexCoord = new Coordinate(vertex.head, vertex(1))
      val topCoord = new Coordinate(vertex.head, topBoundary)
      val bottomCoord = new Coordinate(vertex.head, bottomBoundary)
      val lineUP = new LineSegment(vertexCoord,topCoord)
      val lineDOWN = new LineSegment(vertexCoord,bottomCoord)

      assert(lineUP.isVertical, "Segmentation lines should always be vertical!")
      assert(lineDOWN.isVertical, "Segmentation lines should always be vertical!")

      val intersectionGeometry = hulls.map(hull =>
        hull.intersection(lineUP.toGeometry(new GeometryFactory))).filter(intersect => !intersect.isEmpty)
      val intersectionCoordinateUP = intersectionGeometry.map(intersect =>{
        intersect.getCoordinates().sortWith((c1,c2) => c1.y <= c2.y)
      })
      val intersectionCoordinateDown = intersectionGeometry.map(intersect =>{
        intersect.getCoordinates().sortWith((c1,c2) => c1.y >= c2.y)
      })

      //assert(vertex.length>=3, "A vertex should have at least 3 components?!")

      val vertexLineUp = if(!intersectionCoordinateUP.isEmpty){
        val sortedIntersections = intersectionCoordinateUP.sortWith((ls1,ls2) => ls1.head.y <= ls2.head.y)
        val newTopCoord = if(sortedIntersections.length > 1){
          sortedIntersections(1).head
        } else {
          topCoord
          //sortedIntersections.head.head
        }
        assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")
        List(List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
      } else {
        //assert(vertex.length>=3, "A vertex should have at least 3 components?!")
        List(List(vertex.head,topBoundary))
      }

      val vertexLineDown = if(!intersectionCoordinateDown.isEmpty){
        val sortedIntersections = intersectionCoordinateDown.sortWith((ls1,ls2) => ls1.head.y >= ls2.head.y)
        val newTopCoord = if(sortedIntersections.length > 1){
          sortedIntersections(1).head
        } else {
          bottomCoord
          //sortedIntersections.head.head
        }
        assert(vertex.head == newTopCoord.x, "There should be no shift along x-axis")
        List(List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
      } else {
        //assert(vertex.length>=3, "A vertex should have at least 3 components?!")
        List(List(vertex.head,bottomBoundary))
      }

      vertexLineUp ++ vertexLineDown
    })

     */

    //println("vertexLinesUpAndDown: " + vertexLinesUPandDOWN)

    val vertexLineList = vertexLinesUP ++ vertexLinesDown //++ vertexLinesUPandDOWN

    //println("vertexLineList: " + vertexLineList.toString)

    val newVertices: Seq[List[Float]] = vertexLineList.flatMap(vertexLine =>
      vertexLine.filter(vertex => !polyScene.vertices.contains(vertex)))

    //println("vertices: " + polyScene.vertices.toString)
    //println("new vertices: " + newVertices.toString())
    //println("intersect both: " + polyScene.vertices.intersect(newVertices.toList).toString())

    val polySceneSegVertices: Seq[List[Float]] = polyScene.vertices ++ newVertices

    //println("segmentated vertices: " + polySceneSegVertices.toString)

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

    //println("vertexLineList: " + vertexLineList.toString)
    //println("segmentated vertices: " + polySceneSegVertices.toString)
    //println("lineList: " + lineList.toString())
    //val head = polySceneSegVertices.head
    //println("head of segmentated vertices: " + head.toString())
    //println("index of this head :" + polySceneSegVertices.indexOf(head))

    //TODO compute cells from lines

    PolySceneLineSegmentation(polySceneSegVertices.toList,
      polySceneSegObstacles.toList,
      polyScene.boundaries,
      topVertices.toList,
      bottomVertices.toList,
      lineList.toList)
    //PolySceneCellSegmentation(polySceneCellSegVertices.toList,polyScene.obstacles, polyScene.boundaries,Nil)

  }

  //def checkIntersection(extandableVertices: Seq[List[Float]], hulls: Seq[Geometry], boundaries : Seq[Float])

  def upwardExtendableVertices(obstacleVertexLists: Seq[List[List[Float]]]) : Seq[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndUpPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head,vertex(1) + 0.01))))
      //println("verticesAndUpPoints: " + verticesAndUpPoints.toString())
      val filtered = verticesAndUpPoints.filter(pair => !hullGeometry.contains(pair._2)).map(pair => pair._1)
      //println("filtered: " + filtered.toString())
      filtered
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
