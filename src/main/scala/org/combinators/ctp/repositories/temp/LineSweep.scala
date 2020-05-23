package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.toplevel.{PolySceneCellSegmentation, PolygonScene}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineSegment}

object LineSweep {
  def apply(polyScene: PolygonScene): PolySceneCellSegmentation = {
    val topBoundary: Float = polyScene.boundaries(1) / 2
    val rightBoundary: Float = polyScene.boundaries.head / 2

    val obstacleVertexLists: List[List[List[Float]]] = polyScene.obstacles.map(i => i.map(polyScene.vertices))
    val hulls: List[Geometry] = obstacleVertexLists.map { actualCoords =>
      // generate convex hull for current batch of points
      val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      hullGeometry
    }

    println("obstacleVertexList with length " +obstacleVertexLists.flatten.length + ":" )
    println(obstacleVertexLists.toString())

    println("vertex list with length " + polyScene.vertices.length + ":")
    println(polyScene.vertices)

    println("difference between both:")
    println(polyScene.vertices.diff(obstacleVertexLists.flatten))

    val sortedVertices = polyScene.vertices.sortWith((v1, v2) => v1.head <= v2.head)/*.filter(
      vertex => {
        vertex.head <= rightBoundary && vertex.head >= -rightBoundary &&
        vertex(1) <= topBoundary && vertex(1) >= - topBoundary
      }).sortWith((v1, v2) => v1.head <= v2.head)
      */

    println("Sorted Vertices: " + sortedVertices.toString())

    val upExtVertices: List[List[Float]] = upwardExtendableVertices(obstacleVertexLists)
    val downExtVertices: List[List[Float]] = downwardExtendableVertices(obstacleVertexLists)

    println("difference of vertices and ups: ")
    println(polyScene.vertices.diff(upExtVertices))

    println("difference of vertices and downs: ")
    println(polyScene.vertices.diff(downExtVertices))

    println("difference of vertices and ups and downs: ")
    println(polyScene.vertices.diff(upExtVertices.union(downExtVertices)))

    val pairedVertices = sortedVertices.map(vertex => {
      (upExtVertices.contains(vertex), downExtVertices.contains(vertex)) match {
        case (false, false) => (vertex, direction.None())
        case (false, true) => (vertex, direction.Down())
        case (true, false) => (vertex, direction.Up())
        case (true, true) => (vertex, direction.UpAndDown())
      }
    })

    println("paired vertices: " )//+ pairedVertices.toString())
    pairedVertices.foreach(p => {
      println(p.toString())
      println(hulls.map(hull => hull.distance(new GeometryFactory().createPoint(new Coordinate(p._1.head, p._1(1)))) == 0))
    })

    val initialLeft : Float = sortedVertices.head.head - 1

    //val initialTop : Float = (polyScene.vertices.sortWith((v1, v2) => v1(1) >= v2(1)).head(1) + 1).max(-polyScene.vertices.sortWith((v1, v2) => v1(1) <= v2(1)).head(1) + 1)

    //println("initialTop: " + initialTop.toString)
    //println("initialLeft: " + initialLeft.toString)

    val initialOpenCells = List(List(List(initialLeft, -topBoundary), List(initialLeft, topBoundary)))

    val freeCells: List[List[List[Float]]] = sweepLine(pairedVertices,
      initialOpenCells,
      Nil,
      hulls,
      rightBoundary,
      topBoundary)

    val newVertices: List[List[Float]] = freeCells.flatMap(cell =>
      cell.filter(vertex => !polyScene.vertices.contains(vertex)))

    val polySceneSegVertices: List[List[Float]] = polyScene.vertices ++ newVertices

    val newObstacles: List[List[List[Float]]] = obstacleVertexLists.map { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val newVerticesInHull = newVertices.filter(vertex => hullGeometry.contains(new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1)))))
      obstacle ++ newVerticesInHull
    }

    val polySceneSegObstacles: List[List[Int]] = newObstacles.map(vertexList =>
      vertexList.map(vertex => polySceneSegVertices.indexOf(vertex)))

    val freeCellsIndexed = freeCells.map(cell => cell.map(vertex => polySceneSegVertices.indexOf(vertex)))

    PolySceneCellSegmentation(polySceneSegVertices,
      polySceneSegObstacles,
      polyScene.boundaries,
      freeCellsIndexed)
  }

  //hulls, boundaryX, boundaryY are explicit parameters for the sake of pure functionality.
  //They could be accessed through global values, if one prefers an impure style
  @scala.annotation.tailrec
  def sweepLine(sortedVertices : List[(List[Float], direction.Extendable)],
                openCells : List[List[List[Float]]],
                closedCells : List[List[List[Float]]],
                hulls : Seq[Geometry],
                boundaryX : Float,
                boundaryY : Float) : List[List[List[Float]]] = {
    assert(openCells.nonEmpty, "There has to be at least one open Cell")
    if(sortedVertices.isEmpty){
      assert(openCells.length == 1, "When recursion stops, there should be exactly one open cell!")
      println("end recursion")
      return (List(boundaryX, boundaryY) :: List(boundaryX, - boundaryY) :: openCells.head) :: closedCells
    }
    assert(sortedVertices.nonEmpty, "This case should be handled above")
    val first = sortedVertices.head
    val second = sortedVertices(1)
    if(first._1.head == second._1.head){
      println("vertical face case")
      val restVertices = sortedVertices.tail.tail
      (first, second) match {
        case ((vertexUp, direction.Up()), (vertexDown, direction.Down())) =>
          val top: List[Float] = computeExtendedVertex(vertexUp, boundaryY, hulls, (d1, d2) => d1 <= d2)
          val openCellUp: List[List[Float]] = List(top, vertexUp)
          val closeCellUp: List[List[Float]] = findOpenCell(top, boundaryY, hulls, openCells)
          val bottom: List[Float] = computeExtendedVertex(vertexDown, -boundaryY, hulls, (d1, d2) => d1 >= d2)
          val openCellDown: List[List[Float]] = List(bottom, vertexDown)
          val closeCellDown: List[List[Float]] = findOpenCell(bottom, -boundaryY, hulls, openCells)
          if (closeCellUp.equals(closeCellDown)) {
            val cellsToOpen: List[List[List[Float]]] = List(openCellUp, openCellDown)
            val closedCell: List[List[Float]] = List(top, bottom) ++ closeCellUp
            val newOpenCells = cellsToOpen ++ openCells.diff(List(closeCellUp))
            sweepLine(restVertices, newOpenCells, closedCell :: closedCells, hulls, boundaryX, boundaryY)
          } else {
            val newOpenCell: List[List[Float]] = List(bottom, top)
            val newClosedCells: List[List[List[Float]]] = List(openCellUp ++ closeCellUp, openCellDown ++ closeCellDown)
            val newOpenCells = newOpenCell :: openCells.diff(List(closeCellUp)).diff(List(closeCellDown))
            sweepLine(restVertices, newOpenCells, newClosedCells ++ closedCells, hulls, boundaryX, boundaryY)
          }
        case ((vertexDown, direction.Down()), (vertexUp, direction.Up())) =>
          val top: List[Float] = computeExtendedVertex(vertexUp, boundaryY, hulls, (d1, d2) => d1 <= d2)
          val openCellUp: List[List[Float]] = List(top, vertexUp)
          val closeCellUp: List[List[Float]] = findOpenCell(top, boundaryY, hulls, openCells)
          val bottom: List[Float] = computeExtendedVertex(vertexDown, -boundaryY, hulls, (d1, d2) => d1 >= d2)
          val openCellDown: List[List[Float]] = List(bottom, vertexDown)
          val closeCellDown: List[List[Float]] = findOpenCell(bottom, -boundaryY, hulls, openCells)
          if (closeCellUp.equals(closeCellDown)) {
            val cellsToOpen: List[List[List[Float]]] = List(openCellUp, openCellDown)
            val closedCell: List[List[Float]] = List(top, bottom) ++ closeCellUp
            val newOpenCells = cellsToOpen ++ openCells.diff(List(closeCellUp))
            sweepLine(restVertices, newOpenCells, closedCell :: closedCells, hulls, boundaryX, boundaryY)
          } else {
            val newOpenCell: List[List[Float]] = List(bottom, top)
            val newClosedCells: List[List[List[Float]]] = List(openCellUp ++ closeCellUp, openCellDown ++ closeCellDown)
            val newOpenCells = newOpenCell :: openCells.diff(List(closeCellUp)).diff(List(closeCellDown))
            sweepLine(restVertices, newOpenCells, newClosedCells ++ closedCells, hulls, boundaryX, boundaryY)
          }
        case ((vertex1, direction.UpAndDown()), (vertex2, direction.UpAndDown())) =>
          val hulls1 = hulls.map(hull => hull.contains(new GeometryFactory().createPoint(new Coordinate(vertex1.head, vertex1(1)))))
          val hulls2 = hulls.map(hull => hull.contains(new GeometryFactory().createPoint(new Coordinate(vertex2.head, vertex2(1)))))
          println(vertex1)
          println(hulls1)
          println(vertex2)
          println(hulls2)

          closedCells
        case _ =>
          println("this case should never be reached!")
          sweepLine(restVertices, openCells, closedCells, hulls, boundaryX, boundaryY)
      }
    } else{
      println("vertex case")
      val restVertices = sortedVertices.tail
      first match {
        case (vertex, direction.Up()) =>
          val top : List[Float] = computeExtendedVertex(vertex, boundaryY, hulls, (d1, d2) => d1 <= d2)
          val newOpenCell : List[List[Float]] = List(top, vertex)
          val cellToClose : List[List[Float]] = findOpenCell(top, boundaryY, hulls, openCells)
          val closedCell : List[List[Float]] = newOpenCell ++ cellToClose
          val newOpenCells = newOpenCell :: openCells.diff(List(cellToClose))
          sweepLine(restVertices, newOpenCells, closedCell :: closedCells, hulls, boundaryX, boundaryY)
        case (vertex, direction.Down()) =>
          val bottom : List[Float] = computeExtendedVertex(vertex, -boundaryY, hulls, (d1, d2) => d1 >= d2)
          val newOpenCell : List[List[Float]] = List(bottom, vertex)
          val cellToClose : List[List[Float]] = findOpenCell(bottom, -boundaryY, hulls, openCells)
          val closedCell : List[List[Float]] = newOpenCell ++ cellToClose
          val newOpenCells = newOpenCell :: openCells.diff(List(cellToClose))
          sweepLine(restVertices, newOpenCells, closedCell :: closedCells, hulls, boundaryX, boundaryY)
        case (vertex, direction.UpAndDown()) =>
          val top : List[Float] = computeExtendedVertex(vertex, boundaryY, hulls, (d1, d2) => d1 <= d2)
          val openCellUp : List[List[Float]] = List(top, vertex)
          val closeCellUp : List[List[Float]] = findOpenCell(top, boundaryY, hulls, openCells)
          val bottom : List[Float] = computeExtendedVertex(vertex, -boundaryY, hulls, (d1, d2) => d1 >= d2)
          val openCellDown : List[List[Float]] = List(bottom, vertex)
          val closeCellDown : List[List[Float]] = findOpenCell(bottom, -boundaryY, hulls, openCells)
          if(closeCellUp.equals(closeCellDown)){
            val cellsToOpen : List[List[List[Float]]] = List(openCellUp, openCellDown)
            val closedCell : List[List[Float]] = List(top, bottom) ++ closeCellUp
            val newOpenCells = cellsToOpen ++ openCells.diff(List(closeCellUp))
            sweepLine(restVertices, newOpenCells, closedCell :: closedCells, hulls, boundaryX, boundaryY)
          } else{
            val newOpenCell : List[List[Float]] = List(bottom, top)
            val newClosedCells : List[List[List[Float]]] = List(openCellUp ++ closeCellUp, openCellDown ++ closeCellDown)
            val newOpenCells = newOpenCell :: openCells.diff(List(closeCellUp)).diff(List(closeCellDown))
            sweepLine(restVertices, newOpenCells, newClosedCells ++ closedCells, hulls, boundaryX, boundaryY)
          }
        case (vertex, direction.None()) =>
          sweepLine(restVertices, List(vertex) :: openCells, closedCells, hulls, boundaryX, boundaryY)
      }
    }
  }

  def upwardExtendableVertices(obstacleVertexLists: List[List[List[Float]]]): List[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndUpPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1) + 0.01))))
      verticesAndUpPoints.filter(pair => !(hullGeometry.distance(pair._2) == 0)).map(pair => pair._1)
    }
  }

  def downwardExtendableVertices(obstacleVertexLists: List[List[List[Float]]]): List[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndDownPoints = obstacle.map(vertex => (vertex, new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1) - 0.01))))
      verticesAndDownPoints.filter(pair => !(hullGeometry.distance(pair._2) == 0)).map(pair => pair._1)
    }
  }

  def findOpenCell(vertex : List[Float],
                   boundary: Float,
                   hulls: Seq[Geometry],
                   openCells : List[List[List[Float]]]) : List[List[Float]] = {
    val filteredHulls = hulls.filter(hull => hull.distance(new GeometryFactory().createPoint(new Coordinate(vertex.head, vertex(1))))==0)
    assert(filteredHulls.length <= 1, "A Line extended from a vertex intersects with maximal one hull.")
    if(filteredHulls.isEmpty){
      val filteredOpenCells = openCells.filter(cell => cell.map(vert => vert(1) == boundary).fold(false)((a,b) => a || b))
      /*
      assert(filteredOpenCells.length == 1, "There should be exactly one open cell to find, but there are " +
        filteredOpenCells.length.toString + "\nwith vertex = " + vertex.toString + ", boundary = " + boundary.toString +
        " and opencells = \n" + openCells.toString()
      )
       */
      if(filteredOpenCells.isEmpty){
        Nil
      }else {
        filteredOpenCells.head
      }
    }else{
      val filteredOpenCells = openCells.filter(cell => cell.map(vert => filteredHulls.head.distance(new GeometryFactory().createPoint(new Coordinate(vert.head, vert(1))))==0).fold(false)((a,b) => a || b))
      /*
      assert(filteredOpenCells.length == 1, "There should be exactly one open cell to find, but there are " +
        filteredOpenCells.length.toString + "\nwith vertex = " + vertex.toString + ", boundary = " + boundary.toString +
        " and opencells = \n" + openCells.toString()
      )
       */
      if(filteredOpenCells.isEmpty){
        openCells.head
      }else {
        filteredOpenCells.head
      }
    }
  }

  def computeExtendedVertex(vertex : List[Float],
                            boundary: Float,
                            hulls: Seq[Geometry],
                            partialOrder: (Double, Double) => Boolean): List[Float] = {
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

    List(newTopCoord.x.toFloat, newTopCoord.y.toFloat)
  }

  object direction {
    sealed trait Extendable
    case class Up() extends Extendable
    case class Down() extends Extendable
    case class UpAndDown() extends Extendable
    case class None() extends Extendable
  }
}
