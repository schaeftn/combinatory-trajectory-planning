package org.combinators.ctp.repositories.cmp

import com.typesafe.scalalogging.LazyLogging
import org.combinators.ctp.repositories.toplevel.{PolySceneCellSegmentation, PolySceneLineSegmentation}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineSegment, Point}
import io.circe.generic.auto._
import io.circe.syntax._

trait VcdUtils extends LazyLogging {
  object VcdLinesToCells{
    @scala.annotation.tailrec
    def sweepStep(l: PolySceneLineSegmentation, currentX: Float, openedCells: List[List[Int]],
                  closedCells: List[List[Int]]): (List[List[Int]], List[List[Int]]) = {
      println(f"OpenedCells: $openedCells")
      println(f"ClosedCells: $closedCells")
      val lines = l.lines

      def findBoundaryLines(fl: Float): List[List[Int]] =
        List(List(l.topVertices.filter(i => l.vertices(i).head == fl).head,
          l.bottomVertices.filter(i => l.vertices(i).head == fl).head))

      def findXLines(x: Float): List[List[Int]] = {
        val fromLines = lines.filter(a => l.vertices(a.head).head == x)
        if(fromLines.isEmpty) {
          val blines = findBoundaryLines(x)
          println(s"blines x: $blines")
          blines
        } else {
          fromLines
        }
      }

      def findNextX(x: Float):Option[Float] = {
        lines.map(line => l.vertices(line.head).head).distinct.sorted.find(a => x < a)
      }

      def findVerticalObstacleLines(x: Float, opening: Boolean): List[List[Int]] = {
        val xVertexIndices = l.vertices.indices.filter(a => l.vertices(a).head == x)
        val xObstacles: List[List[Int]] = l.obstacles.filter(obstacle => (obstacle intersect xVertexIndices).nonEmpty)
        val relevantObstacles:List[List[Int]] = if (opening) {
          xObstacles.filter(currentObs => currentObs.forall(l.vertices(_).head <= x))
        } else {
          xObstacles.filter(currentObs => currentObs.forall(l.vertices(_).head >= x))
        }
        relevantObstacles.map(rObstacle => rObstacle intersect xVertexIndices)
      }

      def getObstacleForVertex(vertexId: Int): List[Int] = {
        println(s"getObstacleForVertex: v: ${l.vertices(vertexId)}")
        println(s"${l.obstacles.zipWithIndex.filter(i => i._1.contains(vertexId)).map(_._2)}")
        l.obstacles.zipWithIndex.filter(i => i._1.contains(vertexId)).map(_._2)
      }

      def getBoundaryObstacleIdsForLine(openedCell: List[Int]): List[Int] = {
        val sortedVertices = openedCell.zipWithIndex.map(cMem => (l.vertices(cMem._1), cMem._2)).sortBy(_._1.last)
        getObstacleForVertex(openedCell(sortedVertices.head._2)) ++ getObstacleForVertex(openedCell(sortedVertices.last._2))
      }

      def closeCellsT(x: Float, openCells: List[List[Int]]): (List[List[Int]], List[List[Int]]) = {
        println(s"Closing cells at pos x: $x")
        @scala.annotation.tailrec
        def getClosingLines(remainingLines: List[List[Int]], collectedLines: List[List[Int]]): List[List[Int]]= {
          remainingLines match {
            case h::tail =>
              val (mergeLines, otherCollectedLines): (List[List[Int]], List[List[Int]]) =
                collectedLines.partition(line => line.contains(h.head) || line.contains(h.last))
              val newLine: List[List[Int]] =
                if (mergeLines.nonEmpty && !splitTestClosingVertex(h))
                  List((mergeLines.reduce(_ ++ _) ++ h).distinct)
                else if (h.size == 1)
                  mergeLines
                else
                  h +: mergeLines

              getClosingLines(tail, newLine ++ otherCollectedLines )
            case Nil => collectedLines
          }
        }
        /**
         * Returns true if vertices are rightbound on obstacles. ie no merging
         *
         */
        def splitTestClosingVertex(v: List[Int]): Boolean = {
          val obsList = l.obstacles.filter(l => l.contains(v.head))
          obsList.exists(o => o.forall(l.vertices(_).head <= currentX))
        }

        @scala.annotation.tailrec
        def closeCells(closingLines: List[List[Int]],
                       closedCells:List[List[Int]],
                       updatedOpenCells: List[List[Int]]): (List[List[Int]], List[List[Int]]) ={
          println(s"ClosingLines: $closingLines")
          closingLines match {
            case h :: tail =>
              println("inside h::tail")
              val boundaryObstacles = getBoundaryObstacleIdsForLine(h)
              if (boundaryObstacles == List(7, 2)) {
                println("fpopo")
              }
              println(s"Closing: boundary obstacles: $boundaryObstacles")
              val obstaclePails = updatedOpenCells.map(cell => getBoundaryObstacleIdsForLine(cell))
              println(s"Closing: boundary obstacles for openCells: $obstaclePails")
              println(s"boundaryObstacles intersect obstaclePails: ${boundaryObstacles intersect obstaclePails}")

              val (newlyClosedCells, newUpdatedOpenCells) = updatedOpenCells.partition(
                cell => getBoundaryObstacleIdsForLine(cell).intersect(boundaryObstacles).size >= 2)
              if (newlyClosedCells.size > 1) {
                println(s"Closing: Single Line closes more than 1 cell at x=$x")
              }
              closeCells(tail, closedCells ++ newlyClosedCells.map(i => i ++ h), newUpdatedOpenCells)
            case Nil => (closedCells, updatedOpenCells)
          }}

        val currentXLines = findXLines(currentX)
        val splittingObstacles = findVerticalObstacleLines(currentX, opening = false)
        println(s"Closing at x: $currentX: closing splittingObs: $splittingObstacles")
        val closingLines = getClosingLines(currentXLines ++ splittingObstacles, List.empty[List[Int]])

        val (t, result) = closeCells(closingLines, List.empty[List[Int]], openCells)
        println(s"Closing done at pos x: $x. Closed cells: $t, cells still open: $result")
        (t, result)
      }


      def openNewCells(currentX: Float): Option[List[List[Int]]] = {
        println(s"Opening cells at pos x: $currentX")
        val currentXLines = findXLines(currentX)
        val splittingObstacles = findVerticalObstacleLines(currentX, opening = true)
        Some(getOpeningLines(currentXLines ++ splittingObstacles))
      }

      def splitTestOpeningVertex(v: List[Int]): Boolean = {
        val obsList = l.obstacles.filter(l => l.contains(v.head))
        obsList.exists(o => o.forall(l.vertices(_).head >= currentX))
      }

      def getOpeningLines(list: List[List[Int]]):List[List[Int]] = {
        @scala.annotation.tailrec
        def openingLines(remainingLines: List[List[Int]], collectedLines: List[List[Int]]): List[List[Int]] =
          remainingLines match {
            case h::tail =>
              val (mergeLines, otherCollectedLines): (List[List[Int]], List[List[Int]]) =
                collectedLines.partition(line => line.contains(h.head) || line.contains(h.last))
              val newLine: List[List[Int]] =
                if (mergeLines.nonEmpty && !splitTestOpeningVertex(h))
                  List((mergeLines.reduce(_ ++ _) ++ h).distinct)
                else if (h.size == 1)
                  mergeLines
                else
                  h +: mergeLines
              openingLines(tail, newLine.distinct ++ otherCollectedLines)
            case Nil => collectedLines: List[List[Int]]
          }
        println(s"opening lines: $list")
        openingLines(list, List.empty[List[Int]])
      }

      val (newlyClosedCells, updatedOpenedCells) = closeCellsT(currentX, openedCells)
      val newOpenedCells = openNewCells(currentX)

      findNextX(currentX) match {
        case None => (updatedOpenedCells ++ newOpenedCells.getOrElse(List.empty[List[Int]]),
          closedCells ++ newlyClosedCells)
        case Some(x) => sweepStep(l, x, updatedOpenedCells ++ newOpenedCells.getOrElse(List.empty[List[Int]]),
          closedCells ++ newlyClosedCells)
      }
    }

    def apply: PolySceneLineSegmentation => PolySceneCellSegmentation = { ls =>
      println(ls.asJson.toString())
      val (stillOpen, cells) = sweepStep(ls, -ls.boundaries.head / 2, List.empty[List[Int]], List.empty[List[Int]])
      PolySceneCellSegmentation(ls.vertices, ls.obstacles, ls.boundaries, cells.distinct)
    }
  }

  def getExtendableVertices(obstacleVertexLists: List[List[List[Float]]]): (List[List[Float]],List[List[Float]]) = {
    (getUpDownExtendableVertices(obstacleVertexLists, getUpExVerts = true),
      getUpDownExtendableVertices(obstacleVertexLists, getUpExVerts = false))
  }

  //shifts point on yAxis, selects upward extendable vertices based on distance to obstacles
  def getUpDownExtendableVertices(obstacleVertexLists: List[List[List[Float]]],
                                  getUpExVerts: Boolean): List[List[Float]] = {
    obstacleVertexLists.flatMap { obstacle =>
      val gf = new GeometryFactory()
      val coords = obstacle.map(vertex => new Coordinate(vertex.head, vertex(1)))
      val convexHull = new ConvexHull(coords.toArray, gf)
      val hullGeometry = convexHull.getConvexHull
      val verticesAndDownPoints: List[(List[Float], Point)] = obstacle.map(vertex => {
        if (getUpExVerts)
          (vertex, gf.createPoint(new Coordinate(vertex.head, vertex(1) + 0.01)))
        else
          (vertex, gf.createPoint(new Coordinate(vertex.head, vertex(1) - 0.01)))
      })
      verticesAndDownPoints.filter(pair => !(hullGeometry.distance(pair._2) == 0)).map(pair => pair._1)
    }
  }

  def toPoint(v: List[Float]): Point = {
    new GeometryFactory().createPoint(
      new Coordinate(v.head, v(1)))
  }

  object VcdExtensionDirection extends Enumeration {
    type VcdExtensionDirection = Value
    val Up, Down, UpAndDown, None = Value
  }

  def indexToVertex(i: Int, ls: PolySceneLineSegmentation): List[Float] = ls.vertices(i)
  def vertexIdToObjectId(i: Int, ls: PolySceneLineSegmentation): Int = ls.obstacles.indexWhere(_.contains(i))

  //adds line (v,v) for all obstacle vertices on top, bottom boundaries
  def addUtilityLines(ls: PolySceneLineSegmentation): PolySceneLineSegmentation = {
    val i_lines = ls.topVertices.concat(ls.bottomVertices).intersect(ls.obstacles.flatten).map(i => List(i, i))
    val newLines = (ls.lines ++ i_lines).map { i => i.zip(i.map(a => indexToVertex(a, ls))) }.map(
      o => if (o.head._2(1) > o(1)._2(1)) o.map(_._1) else o.map(_._1).reverse)
    PolySceneLineSegmentation(ls.vertices, ls.obstacles, ls.boundaries, ls.topVertices, ls.bottomVertices, newLines)
  }

  def addBoundaryLines(l: PolySceneLineSegmentation): PolySceneLineSegmentation = {
    val ls = addUtilityLines(l)
    val boundaryLineLeft:List[List[Float]] = if (l.vertices.exists(v => v.head == -ls.boundaries.head / 2)) {
      List.empty[List[Float]]
    }
    else {
      List(List(-ls.boundaries.head / 2, ls.boundaries(1) / 2),
        List(-ls.boundaries.head / 2, -ls.boundaries(1) / 2))
    }

    val boundaryLineRight:List[List[Float]] =
      if (l.vertices.exists(v => v.head == ls.boundaries.head / 2))
        List.empty[List[Float]]
      else
        List(List(ls.boundaries.head / 2, ls.boundaries(1) / 2), List(ls.boundaries.head / 2, -ls.boundaries(1) / 2))

    val lineVerts: List[List[Float]] = boundaryLineLeft ++ boundaryLineRight

    val newVerts = ls.vertices ++ lineVerts.filter { a => ls.vertices.indexOf(a) == -1 }
    val vList1 = boundaryLineLeft.map(v => newVerts.indexOf(v))
    val vList2 = boundaryLineRight.map(v => newVerts.indexOf(v))

    val newLines = List(vList1, vList2).filter(l => l.nonEmpty)

    val topObstacle: List[Int] = ls.topVertices ++ newLines.map(i => i.head)
    val bottomObstacle: List[Int] = ls.bottomVertices ++ newLines.map(i => i.last)
    PolySceneLineSegmentation(newVerts, ls.obstacles :+ topObstacle :+ bottomObstacle,
      ls.boundaries, ls.topVertices, ls.bottomVertices, ls.lines ++ newLines)
  }

  def computeLines(extVertices: Seq[List[Float]], boundary: Float, hulls: Seq[Geometry],
                   partialOrder: (Double, Double) => Boolean): Seq[List[List[Float]]] = extVertices.map(vertex => {

    //po for up: (d1, d2) => d1 <= d2

    val gf = new GeometryFactory()
    val vertexCoord = new Coordinate(vertex.head, vertex(1))
    val boundaryCoord = new Coordinate(vertex.head, boundary)
    val line = new LineSegment(vertexCoord, boundaryCoord)
    val lineGeometry = line.toGeometry(gf)

    if (!line.isVertical)
      logger.warn(s"Segmentation lines should always be vertical. Line: $line")

    val intersectionGeometry: Seq[Geometry] = hulls.map(hull =>
      hull.intersection(lineGeometry)).filter(intersect => !intersect.isEmpty)
    val intersectionCoordinate = intersectionGeometry.map(intersect => {
      intersect.getCoordinates.sortWith((c1, c2) => partialOrder(c1.y, c2.y))
    })

    if (intersectionGeometry.isEmpty) {
      logger.warn(s"intersectionCoordinate is empty but should have at least one element. " +
        s"Intersecting geometries: $intersectionGeometry, " +
        s"Line: ${line.toString}")
    }

    val sortedIntersection = intersectionCoordinate.sortWith((ls1, ls2) => partialOrder(ls1.head.y, ls2.head.y))
    val newTopCoord = if (sortedIntersection.length > 1) {
      sortedIntersection(1).head
    } else {
      boundaryCoord
    }

    if (vertex.head != newTopCoord.x)
      logger.warn(s"There should be no shift along x-axis: $vertex, ${newTopCoord.toString}")

    List(vertex, List(newTopCoord.x.toFloat, newTopCoord.y.toFloat))
  })
}
