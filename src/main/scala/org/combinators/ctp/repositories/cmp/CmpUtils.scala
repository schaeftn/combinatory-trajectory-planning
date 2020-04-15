package org.combinators.ctp.repositories.cmp

import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene.{CellSegmentation, PolySceneCellSegmentation, PolySceneCellSegmentationCentroids, PolySceneLineSegmentation, PolySceneSegmentationRoadmap, PolygonScene}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, Point}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import io.circe.generic.auto._
import io.circe.syntax._

import io.circe.parser.decode
import org.combinators.ctp.repositories.toplevel.EncodeImplicits

trait CmpUtils extends GeometryUtils with EncodeImplicits{



  //Adds lines if obstacle has a vertex on bottom or top boundary

  object vcdLtC2 {
    //Adds lines if obstacle has a vertex on bottom or top boundary
    def addUtilityLines(ls: PolySceneLineSegmentation): PolySceneLineSegmentation = {
      val i_lines = ls.topVertices.union(ls.bottomVertices).intersect(ls.obstacles.flatten).map(i => List(i, i))
      val newLines = (ls.lines ++ i_lines).map { i => i.zip(i.map(a => indexToVertex(a, ls))) }.map(o => if (o.head._2(1) > o(1)._2(1)) o.map(_._1) else o.map(_._1).reverse)
      PolySceneLineSegmentation(ls.vertices, ls.obstacles, ls.boundaries, ls.topVertices, ls.bottomVertices, newLines)
    }

    def addBoundaryLines(l: PolySceneLineSegmentation): PolySceneLineSegmentation = {
      val ls = addUtilityLines(l)
      val boundaryLineLeft = List(List(-ls.boundaries.head / 2, ls.boundaries(1) / 2), List(-ls.boundaries.head / 2, -ls.boundaries(1) / 2))
      val boundaryLineRight = List(List(ls.boundaries.head / 2, ls.boundaries(1) / 2), List(ls.boundaries.head / 2, -ls.boundaries(1) / 2))
      val lineVerts = List(boundaryLineLeft.head, boundaryLineLeft.last, boundaryLineRight.head, boundaryLineRight.last)

      val newVerts = ls.vertices ++ lineVerts.filter { a => ls.vertices.indexOf(a) == -1 }
      val newLines = List(List(newVerts.indexOf(lineVerts.head), newVerts.indexOf(lineVerts(1))),
        List(newVerts.indexOf(lineVerts(2)), newVerts.indexOf(lineVerts(3))))
      val topObstacle: List[Int] = (ls.topVertices :+ newLines.head.head) :+ newLines.last.head
      val bottomObstacle: List[Int] = (ls.bottomVertices :+ newLines.head.last) :+ newLines.last.last
      PolySceneLineSegmentation(newVerts, ls.obstacles :+ topObstacle :+ bottomObstacle,
        ls.boundaries, ls.topVertices, ls.bottomVertices, ls.lines ++ newLines)
    }

    def indexToVertex(i: Int, ls: PolySceneLineSegmentation): List[Float] = ls.vertices(i)

    def vertexIdToObjectId(i: Int, ls: PolySceneLineSegmentation): Int = ls.obstacles.indexWhere(_.contains(i))

    @scala.annotation.tailrec
    def findRightCell(currentLineId: Int, l: PolySceneLineSegmentation, currentX: Float): (List[Int], List[Int]) = {
      val lines = l.lines

      @scala.annotation.tailrec
      def extendUp(currentLine: List[Int], l: List[List[Int]]): List[Int] = {
        l.find(_.last == currentLine.head) match {
          case None => currentLine
          case Some(a) => extendUp(a.head +: currentLine, l.filter(_ != a))
        }
      }

      @scala.annotation.tailrec
      def extendDown(currentLine: List[Int], l: List[List[Int]]): List[Int] = {
        l.find(_.head == currentLine.last) match {
          case None => currentLine
          case Some(a) => extendDown(currentLine :+ a.last, l.filter(_ != a))
        }
      }

      def allCompositions(cl: List[Int]): List[List[Int]] = {
        val upExt = extendUp(cl, lines.filter(_ != cl)).dropRight(2)
        val downExt = extendDown(cl, lines.filter(_ != cl)).drop(2)
        val up_comb = List.empty[Int] +: (for (i <- upExt.indices) yield upExt.take(i + 1).reverse).toList
        val down_comb = List.empty[Int] +: (for (i <- downExt.indices) yield downExt.take(i + 1)).toList

        (for (i <- up_comb;
              j <- down_comb) yield i ++ cl ++ j).distinct
      }

      def findXLines(x: Float): List[List[Int]] = {
        lines.filter(a => l.vertices(a.head).head == x)
      }

      def findNextX(x: Float):Option[Float] = {
        lines.map(line => l.vertices(line.head).head).distinct.sorted.find(a => x < a)
      }

      def cellFaceForSegLine(segmentationLine: List[List[Int]]): List[(List[Int], Int, Int)] = {
        for (i <- segmentationLine;
             o1 <- l.obstacles.filter(_.contains(i.head));
             o2 <- l.obstacles.filter(_.contains(i.last)))
          yield (i, l.obstacles.indexOf(o1), l.obstacles.indexOf(o2))
      }

      val currentLine = lines(currentLineId)
      val currentCompositions = allCompositions(currentLine)
      val objects_current = cellFaceForSegLine(currentCompositions)

      val nextXLines = findNextX(currentX) match {
        case None => List.empty
        case Some(a) => findXLines(a)
      }
      val nextCompositions = nextXLines.flatMap(allCompositions).distinct
      val nextObjects = cellFaceForSegLine(nextCompositions) // Tuple (Line, ObjectId, ObjectId)

      val commonObjectsCurrentNext = nextObjects.map { i => (i._2, i._3) }.intersect(objects_current.map { i => (i._2, i._3) }) //List of object tuples that are contained in both collections
      if (commonObjectsCurrentNext.nonEmpty) {
        (objects_current.filter(i => i._2 == commonObjectsCurrentNext.head._1 && i._3 == commonObjectsCurrentNext.head._2).head._1,
          nextObjects.filter(i => i._2 == commonObjectsCurrentNext.head._1 && i._3 == commonObjectsCurrentNext.head._2).head._1)
      } else if (nextXLines.isEmpty)
        (currentLine, List.empty)
      else
        findRightCell(currentLineId, l, findNextX(currentX).get)
    }

    def apply: PolySceneLineSegmentation =>  PolySceneCellSegmentation = { ls =>
      val ls_var = addBoundaryLines(ls)
      val cells = for (i <- ls_var.lines.indices;
                       cellLines = findRightCell(i, ls_var, ls_var.vertices(ls_var.lines(i).head).head)
                       if cellLines._2.nonEmpty) yield {
        cellLines._1 ++ cellLines._2
      }

      PolySceneCellSegmentation(ls_var.vertices, ls_var.obstacles, ls_var.boundaries, cells.distinct.toList)
    }
  }
  object VcdLinesToCells{
    def addUtilityLines(ls: PolySceneLineSegmentation): PolySceneLineSegmentation = {
      val i_lines = ls.topVertices.union(ls.bottomVertices).intersect(ls.obstacles.flatten).map(i => List(i, i))
      val newLines = (ls.lines ++ i_lines).map { i => i.zip(i.map(a => indexToVertex(a, ls))) }.map(o => if (o.head._2(1) > o(1)._2(1)) o.map(_._1) else o.map(_._1).reverse)
      PolySceneLineSegmentation(ls.vertices, ls.obstacles, ls.boundaries, ls.topVertices, ls.bottomVertices, newLines)
    }

    def addBoundaryLines(l: PolySceneLineSegmentation): PolySceneLineSegmentation = {
      val ls = addUtilityLines(l)
      val boundaryLineLeft = List(List(-ls.boundaries.head / 2, ls.boundaries(1) / 2), List(-ls.boundaries.head / 2, -ls.boundaries(1) / 2))
      val boundaryLineRight = List(List(ls.boundaries.head / 2, ls.boundaries(1) / 2), List(ls.boundaries.head / 2, -ls.boundaries(1) / 2))
      val lineVerts = List(boundaryLineLeft.head, boundaryLineLeft.last, boundaryLineRight.head, boundaryLineRight.last)

      val newVerts = ls.vertices ++ lineVerts.filter { a => ls.vertices.indexOf(a) == -1 }
      val newLines = List(List(newVerts.indexOf(lineVerts.head), newVerts.indexOf(lineVerts(1))),
        List(newVerts.indexOf(lineVerts(2)), newVerts.indexOf(lineVerts(3))))
      val topObstacle: List[Int] = (ls.topVertices :+ newLines.head.head) :+ newLines.last.head
      val bottomObstacle: List[Int] = (ls.bottomVertices :+ newLines.head.last) :+ newLines.last.last
      PolySceneLineSegmentation(newVerts, ls.obstacles :+ topObstacle :+ bottomObstacle,
        ls.boundaries, ls.topVertices, ls.bottomVertices, ls.lines ++ newLines)
    }

    def indexToVertex(i: Int, ls: PolySceneLineSegmentation): List[Float] = ls.vertices(i)

    def vertexIdToObjectId(i: Int, ls: PolySceneLineSegmentation): Int = ls.obstacles.indexWhere(_.contains(i))

    @scala.annotation.tailrec
    def findRightCell(currentLineId: Int, l: PolySceneLineSegmentation, currentX: Float): (List[Int], List[Int]) = {
      val lines = l.lines

      @scala.annotation.tailrec
      def extendUp(currentLine: List[Int], l: List[List[Int]]): List[Int] = {
        l.find(_.last == currentLine.head) match {
          case None => currentLine
          case Some(a) => extendUp(a.head +: currentLine, l.filter(_ != a))
        }
      }

      @scala.annotation.tailrec
      def extendDown(currentLine: List[Int], l: List[List[Int]]): List[Int] = {
        l.find(_.head == currentLine.last) match {
          case None => currentLine
          case Some(a) => extendDown(currentLine :+ a.last, l.filter(_ != a))
        }
      }

      def allCompositions(cl: List[Int]): List[List[Int]] = {
        val upExt = extendUp(cl, lines.filter(_ != cl)).dropRight(2)
        val downExt = extendDown(cl, lines.filter(_ != cl)).drop(2)
        val up_comb = List.empty[Int] +: (for (i <- upExt.indices) yield upExt.take(i + 1).reverse).toList
        val down_comb = List.empty[Int] +: (for (i <- downExt.indices) yield downExt.take(i + 1)).toList

        (for (i <- up_comb;
              j <- down_comb) yield i ++ cl ++ j).distinct
      }

      def findXLines(x: Float): List[List[Int]] = {
        lines.filter(a => l.vertices(a.head).head == x)
      }

      def findNextX(x: Float):Option[Float] = {
        lines.map(line => l.vertices(line.head).head).distinct.sorted.find(a => x < a)
      }

      def cellFaceForSegLine(segmentationLine: List[List[Int]]): List[(List[Int], Int, Int)] = {
        for (i <- segmentationLine;
             o1 <- l.obstacles.filter(_.contains(i.head));
             o2 <- l.obstacles.filter(_.contains(i.last)))
          yield (i, l.obstacles.indexOf(o1), l.obstacles.indexOf(o2))
      }

      val currentLine = lines(currentLineId)
      val currentCompositions = allCompositions(currentLine)
      val objects_current = cellFaceForSegLine(currentCompositions)

      val nextXLines = findNextX(currentX) match {
        case None => List.empty
        case Some(a) => findXLines(a)
      }
      val nextCompositions = nextXLines.flatMap(allCompositions).distinct
      val nextObjects = cellFaceForSegLine(nextCompositions) // Tuple (Line, ObjectId, ObjectId)

      val commonObjectsCurrentNext = nextObjects.map { i => (i._2, i._3) }.intersect(objects_current.map { i => (i._2, i._3) }) //List of object tuples that are contained in both collections
      if (commonObjectsCurrentNext.nonEmpty) {
        (objects_current.filter(i => i._2 == commonObjectsCurrentNext.head._1 && i._3 == commonObjectsCurrentNext.head._2).head._1,
          nextObjects.filter(i => i._2 == commonObjectsCurrentNext.head._1 && i._3 == commonObjectsCurrentNext.head._2).head._1)
      } else if (nextXLines.isEmpty)
        (currentLine, List.empty)
      else
        findRightCell(currentLineId, l, findNextX(currentX).get)
    }

    def apply: PolySceneLineSegmentation =>  PolySceneCellSegmentation = { ls =>
      val ls_var = addBoundaryLines(ls)
      val cells = for (i <- ls_var.lines.indices;
                       cellLines = findRightCell(i, ls_var, ls_var.vertices(ls_var.lines(i).head).head)
                       if cellLines._2.nonEmpty) yield {
        cellLines._1 ++ cellLines._2
      }

      PolySceneCellSegmentation(ls_var.vertices, ls_var.obstacles, ls_var.boundaries, cells.distinct.toList)
    }
  }

  def nearestNodesForStartGoal: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => (Int, Int) = {
    case (g, mpTask) =>
      val startPoint = mpTask.startPosition
      val endPoint = mpTask.endPosition
      val n = g.nodes.toOuter.toIndexedSeq
      val startDistances = n.map(distance(_, startPoint))
      val endDistances = n.map(distance(_, endPoint))
      val startIndex = n.indices.minBy(startDistances)
      val endIndex = n.indices.minBy(endDistances)
      (startIndex, endIndex)
  }

  def findFreeCell2D: (PolySceneCellSegmentation, MpTaskStartGoal) => (Int, Int) = {
    case (sSeg, mpTask) => val geoFactory = new GeometryFactory()
      val startPoint: Point = geoFactory.createPoint(new Coordinate(mpTask.startPosition.head, mpTask.startPosition(1)))
      val endPoint: Point = geoFactory.createPoint(new Coordinate(mpTask.endPosition.head, mpTask.endPosition(1)))

      val polygons: List[Geometry] = sSeg.freeCells.
        map(i => i.map(pid => sSeg.vertices(pid)).
          map(p => new Coordinate(p.head, p(1)))).
        map { cl =>
          val ch = new ConvexHull(cl.toArray, geoFactory)
          ch.getConvexHull
        }

      val startIndex = polygons.indices.find(g => polygons(g).contains(startPoint)).get
      val endIndex = polygons.indices.find(g => polygons(g).contains(endPoint)).get
      (startIndex, endIndex)
  }




  //combinator adds connects all freeCell points to start/endpoint
  def addNodesStartEndToRoadmapAllVertices: (PolySceneSegmentationRoadmap, MpTaskStartGoal) => PolySceneSegmentationRoadmap = {
    (sSeg, mpTask) =>
      val g = sSeg.roadmap
      val (startNodeCellIndex, endNodeCellIndex) = (0,0) // TODO Remove findFreeCell2D(sSeg, mpTask)
      val startCell = sSeg.freeCells(startNodeCellIndex)
      val endCell = sSeg.freeCells(endNodeCellIndex)
      val startCellValues = startCell.map(i => sSeg.vertices(i))
      val endCellValues = endCell.map(i => sSeg.vertices(i))
      val newNodes = g.nodes.toOuter ++ List(mpTask.startPosition, mpTask.endPosition)

      val newEdges = g.edges.toOuter ++
        startCellValues.map{ i => WUnDiEdge(i, mpTask.startPosition)(distance(i, mpTask.startPosition)) } ++
        endCellValues.map{ i => WUnDiEdge(i, mpTask.endPosition)(distance(i, mpTask.endPosition)) }

      //sSeg.withRoadmap(Graph.from(newNodes, newEdges))
      sSeg.withRoadmap(Graph.from(newNodes, newEdges))
  }

  /*
  Adds new nodes for start and goal positions to the graph and connects them to centroids of corresponding freeCell
  //combinator adds centroid connection for start endpoint
  TODO Remove
   */

  def addStartGoalCentroidsOnly: (PolySceneSegmentationRoadmap, MpTaskStartGoal) => PolySceneSegmentationRoadmap = {
    case (sSeg, mpTask) =>
//      val (startIndex, endIndex) = findFreeCell2D(sSeg,mpTask)
//      val startCentroid = sSeg.centroids(startIndex)
//      val endCentroid = sSeg.centroids(endIndex)
//
//      val newNodes = sSeg.roadmap.nodes.toOuter.toList :+ mpTask.startPosition :+ mpTask.endPosition
//      val newEdges = sSeg.roadmap.edges.toOuter.toList ++ List(
//        WUnDiEdge(mpTask.startPosition, startCentroid)(distance(mpTask.startPosition, startCentroid)),
//        WUnDiEdge(mpTask.endPosition, endCentroid)(distance(mpTask.endPosition, endCentroid))
//      )
//
//      sSeg.withRoadmap(Graph.from(newNodes, newEdges))
      sSeg
  }


 /*
 //combinator adds connects all freeCell points to start/endpoint
  def addNodesStartEndToRoadmapAllVerticesConnx: (PolySceneSegmentationRoadmap, neighbourCellsNativeType, MpTaskStartGoal) => PolySceneSegmentationRoadmap = {
    case (sSeg, neighbours, mpTask) =>
      val g = sSeg.roadmap
      val (startNodeCellIndex, endNodeCellIndex) = findFreeCell2D(sSeg, mpTask)

      //CellVertices and ConnectorNodes
      def getCellVertices(cellIndex: Int): List[List[Float]] = {
        val list = sSeg.freeCells(cellIndex)
        list.flatMap(i => neighbours.filter(t => t._2 == i || t._3 == i).
          flatMap(t => t._1).map(i => sSeg.vertices(i))) ++ list.map(i => sSeg.vertices(i))
          //CP neu berechnen oder utility data
      }

      val startCellValues: List[List[Float]] = getCellVertices(startNodeCellIndex)
      val endCellValues:List[List[Float]] = getCellVertices(endNodeCellIndex)
      val newNodes= g.nodes.toOuter ++ List(mpTask.startPosition, mpTask.endPosition)

      val newEdges = g.edges.toOuter ++
        startCellValues.map{ i => WUnDiEdge(i, mpTask.startPosition)(distance(i, mpTask.startPosition)) } ++
        endCellValues.map{ i => WUnDiEdge(i, mpTask.endPosition)(distance(i, mpTask.endPosition)) }

      sSeg.withRoadmap(Graph.from(newNodes, newEdges))
  }*/

  // adds direct connection between connection points with different x values for vcd cells
  def addInterCellEdgesToVcdRoadmap: (PolySceneSegmentationRoadmap, MpTaskStartGoal) => PolySceneSegmentationRoadmap = {
    case (sSeg, _) =>
      val g = sSeg.roadmap
      val nEdges = g.edges.toOuter ++ sSeg.centroids.flatMap(c => {
        val centroidNode = (g find c).get
        val connectionPoints = g filter g.having(edge = _.hasSource(centroidNode))

        for (
          i <- connectionPoints.nodes.toOuter;
          j <- connectionPoints.nodes.toOuter
          if i != j && i.head != j.head
        ) yield WUnDiEdge(i, j)(distance(i, j))
      })

      sSeg.withRoadmap(Graph.from(g.nodes.toOuter, nEdges))
  }


  // adds all free cell vertices (i.e. also obstacle vertices) and edges between them
  def initRoadmapAllCellVertices: PolySceneCellSegmentationCentroids => PolySceneSegmentationRoadmap = {
    sSeg =>
      val newNodes = sSeg.freeCells.flatMap(vertexIndices => vertexIndices.map {
        vId => sSeg.vertices(vId)
      })

      val newEdges = (sSeg.freeCells zip sSeg.centroids).map{case (cell, centroid) =>
        val f1 = for (j <- cell;
                      k <- cell;
                      v1 = sSeg.vertices(j);
                      v2 = sSeg.vertices(k)
                      if (j != k))
          yield WUnDiEdge(v1, v2)(distance(v1, v2))

        val f2 = (for (vid <- cell;
                       v = sSeg.vertices(vid)) yield
          List(WUnDiEdge(v, centroid)(distance(v, centroid)),
            WUnDiEdge(v, centroid)(distance(v, centroid)))).reduce(_ ++ _)
        f1 ++ f2
      }.reduce(_ ++ _)
      println(s"new edges: $newEdges")
      sSeg.withRoadmap(Graph.from(newNodes, newEdges))
  }

  // adds all free cell vertices (i.e. also obstacle vertices) and edges between them
  def initRoadmapCentroidsToVertices: PolySceneCellSegmentationCentroids => PolySceneSegmentationRoadmap = {
    sSeg:PolySceneCellSegmentationCentroids =>
      val newNodes = sSeg.freeCells.flatMap(vertexIndices => vertexIndices.map {
        vId => sSeg.vertices(vId)
      })

      val cellCentroidTuples: List[(List[List[Float]], List[Float])] =
        (sSeg.freeCells.map { list => list.map(sSeg.vertices) } zip sSeg.centroids)

      val edgesCentroidsToCellVertices: List[WUnDiEdge[List[Float]]] = cellCentroidTuples.map(o => edgesVertexToVertexlist(o._2, o._1)).reduce(_ ++ _)
      println(s"new edges: $edgesCentroidsToCellVertices")
      sSeg.withRoadmap(Graph.from(newNodes, edgesCentroidsToCellVertices ++ edgesCentroidsToCellVertices))
  }

  def initRoadmapCentroids: (PolySceneCellSegmentationCentroids,neighbourCellsNativeType)  => PolySceneSegmentationRoadmap = {
    (csc, neighbours) =>
      val centroids = csc.centroids
      centroids.filter(i => i.size != 3).foreach(k => println(s"Warn: Centroid length != 3. ${k.toString}"))

      val edges: IndexedSeq[WUnDiEdge[List[Float]]] = neighbours.map {
        case (_: List[Int], leftCellId: Int, rightCellId: Int) =>
          WUnDiEdge(centroids(rightCellId), centroids(leftCellId))(distance(centroids(rightCellId), centroids(leftCellId)))
      }

      println("Returning Cell Graph")
      csc.withRoadmap(Graph.from(centroids, edges))
  }

  def edgesVertexToVertex(v1: List[Float], v2: List[Float]): List[WUnDiEdge[List[Float]]] = {
    edgesVertexlistToVertexlist(List(v1), List(v2))
  }

  def edgesVertexToVertexlist(vertex: List[Float], vertexList: List[List[Float]]): List[WUnDiEdge[List[Float]]] = {
    edgesVertexlistToVertexlist(List(vertex), vertexList)
  }

  def edgesVertexlistToVertexlist(vl1: List[List[Float]], vl2: List[List[Float]]): List[WUnDiEdge[List[Float]]] = {
    for (j <- vl1;
         k <- vl2
         if (j != k))
      yield WUnDiEdge(j, k)(distance(j, k)) //TODO Prio2 Performance check vs indexedList and index-based guard i>k
  }

  def addConnectorNodesAllCellVertexEdges: (PolySceneSegmentationRoadmap, neighbourCellsNativeType) =>
    PolySceneSegmentationRoadmap = {
    (scrm, neighbours) =>
      val list: List[(List[Float], List[WUnDiEdge[List[Float]]])] =
        getConnectorNodesEdges(scrm, neighbours, true)
      val (connxNodes, edges) = (list.map(_._1), list.map(_._2).reduce(_ ++ _))

      scrm.withRoadmap(Graph.from(scrm.roadmap.nodes.toOuter ++ connxNodes, scrm.roadmap.edges.toOuter ++ edges))
    // TODO add cp to Vertices, add cp to freecells, alternative: use roadmap or new utility data structure
  }

  def addConnectorNodesCentroidEdges: (PolySceneSegmentationRoadmap, neighbourCellsNativeType) =>
    PolySceneSegmentationRoadmap = {
    (scrm, neighbours) =>
      val list: List[(List[Float], List[WUnDiEdge[List[Float]]])] =
        getConnectorNodesEdges(scrm, neighbours, false)
      val (connxNodes, edges) = (list.map(_._1), list.map(_._2).reduce(_ ++ _))
    scrm.withRoadmap(Graph.from(scrm.roadmap.nodes.toOuter ++ connxNodes, scrm.roadmap.edges.toOuter ++ edges))
  }

  def addListAllCellVerts(srm: PolySceneSegmentationRoadmap, connx: List[Float], rightCellId: Int, leftCellId: Int): List[WUnDiEdge[List[Float]]] = {
    (srm.freeCells(rightCellId) ++ srm.freeCells(leftCellId)).map(srm.vertices).map(i =>
      WUnDiEdge(connx, i)(distance(connx, i)))
  }

  def getConnectorNodesEdges: (PolySceneSegmentationRoadmap, neighbourCellsNativeType, Boolean) =>
    List[(List[Float], List[WUnDiEdge[List[Float]]])] = {
    (scrm, neighbours, connectCellVerticesToCp) =>
      val centroids = scrm.centroids
      val list: List[(List[Float], List[WUnDiEdge[List[Float]]])] = {
        neighbours.map {
          case (cp: List[Int], leftCellId: Int, rightCellId: Int) => {
            if (cp.size != 2)
              println(s"WARN: size of common vertices of neighbour cells is ${cp.size} should be 2. Vertices: " +
                s"${cp.map(scrm.vertices)}")
            val v1 = scrm.vertices(cp.head)
            val v2 = scrm.vertices(cp.last)
            val connxPoint = lineSegmentCenterPoint(v1, v2)
            val addListl = if (connectCellVerticesToCp)
              addListAllCellVerts(scrm, connxPoint, rightCellId, leftCellId)
            else
              List.empty[WUnDiEdge[List[Float]]]
            (connxPoint, List(WUnDiEdge(connxPoint, centroids(leftCellId))(distance(connxPoint, centroids(leftCellId))),
              WUnDiEdge(connxPoint, centroids(rightCellId))(distance(connxPoint, centroids(rightCellId)))) ++ addListl)
          }
        }.toList
      }
      list
  }


  def getNeighbours: PolySceneCellSegmentation => IndexedSeq[(List[Int], Int, Int)] = { csc =>
    val segmentationLines = for (freeCell1 <- csc.freeCells.indices;
                                 freeCell2 <- csc.freeCells.indices;
                                 i = csc.freeCells(freeCell1).intersect(csc.freeCells(freeCell2))
                                 if i.nonEmpty && freeCell1 != freeCell2)
      yield (i, freeCell1, freeCell2)
    val filteredLines = segmentationLines.filter(i => i._2 < i._3 && i._1.size > 2) //TODO 2d 3d
    filteredLines
  }

  def decodeCellSegmentationFct: (PolygonScene, String) => PolySceneCellSegmentation = {
    (pScene: PolygonScene, resultString: String) =>
      val segmentation = decode[CellSegmentation](resultString) match {
        case Left(_) =>
          println(s"Error while decoding")
          println(s"$resultString")
          CellSegmentation(List.empty[List[Float]], List.empty[List[Int]])
        case Right(s) => s
      }
      pScene.withVertices(segmentation.vertices).withFreeCells(segmentation.cells)
  }
}
