package org.combinators.ctp.repositories.graphsearch


import org.combinators.cls.interpreter._
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.combinators.ctp.repositories.mptasks.MpTaskStartGoal
import org.combinators.ctp.repositories.scene.{PolySceneCellSegmentationCentroids, PolySceneSegmentationGraph, TriangleSegCentroids}
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, CoordinateFilter, CoordinateSequenceComparator, CoordinateSequenceFactory, CoordinateSequenceFilter, Envelope, Geometry, GeometryComponentFilter, GeometryFactory, GeometryFilter, Point}
import scalax.collection.Graph
import scalax.collection.edge.{WDiEdge, WUnDiEdge}
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.language.{higherKinds, postfixOps}
import math._

trait GraphSearchRepository extends GeometryUtils{
  @combinator object ValueIterationC {
    def apply(g: Graph[List[Float], WUnDiEdge], termNodes: Set[List[Float]]) =
      ValueIteration(g, termNodes)

    val semanticType = 'graph =>: 'SynthTarghet =>: 'Path :&: 'mitValIteration
  }

  @combinator object ValueIterationCombinator {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) =>
      (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float) = {
      case ((g, mpTask)) =>
        println(s"graph: $g" )
        println(s"Task: $mpTask")
        val result = ValueIteration(g, Set(mpTask.endPosition))
          .optPlanStartingWith(mpTask.startPosition).getOrElse((Seq.empty, Seq.empty, 0.0f))
        println(s"VI done: $result")
        result
    }

    val semanticType = Constructor("graphTraversalVi")
  }

  @combinator object DijkstraSpCombinator {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) =>
      (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float) = {
      case ((g, mpTask)) =>
        println(s"graph: $g")
        println(s"Task: $mpTask")
        val startNode = g.get(mpTask.startPosition)
        val endNode = g.get(mpTask.endPosition)

        println(s"startNode: $startNode endNode:$endNode")
        val result1 = startNode shortestPathTo endNode
        println("Done shortest path")
        result1 match {
          case Some(p) => println(s"Dijkstra found Path: $p")
            (p.nodes.map(_.toOuter).toSeq,
            p.edges.map(_.toOuter).toSeq,
            p.weight.toFloat)
          case _ => println(s"Dijkstra could not find a path")
            (Seq.empty, Seq.empty, 0.0f)
        }
    }

    val semanticType = Constructor("graphTraversalFct")
  }

  def findFreeCellTri: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => (Int, Int) = {
    case ((g, mpTask)) =>

      val startPoint = mpTask.startPosition
      val endPoint = mpTask.endPosition

      val n = g.nodes.toOuter.toIndexedSeq
      val startDistances = n.map(distance(_, startPoint))
      val endDistances = n.map(distance(_, endPoint))
      val startIndex = n.indices.minBy(startDistances)
      val endIndex = n.indices.minBy(endDistances)
      (startIndex, endIndex)
  }


  /*
  Selects a free cell for an MPTaskStartGoal
  //TODO Prüfen, ob als Combinator
 */
    def findFreeCell: (PolySceneSegmentationGraph, MpTaskStartGoal) => (Int, Int) = {
      case ((sSeg, mpTask)) => val geoFactory = new GeometryFactory()
        val startPoint: Point = geoFactory.createPoint(new Coordinate(mpTask.startPosition.head, mpTask.startPosition(1)))
        val endPoint: Point = geoFactory.createPoint(new Coordinate(mpTask.endPosition.head, mpTask.endPosition(1)))

        val polygons: List[Geometry] = sSeg.freeCells.
          map(i => i.map(pid => sSeg.vertices(pid)).
            map(p => new Coordinate(p(0), p(1)))).
          map { cl =>
            val ch = new ConvexHull(cl.toArray, geoFactory)
            ch.getConvexHull
          }

        val startIndex = polygons.indices.find(g => polygons(g).contains(startPoint)).get
        val endIndex = polygons.indices.find(g => polygons(g).contains(endPoint)).get
        (startIndex, endIndex)
    }


  @combinator object RefineComplete {
    def apply: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph = {
      case ((sSeg, mpTask)) =>
        refineGraphAllCellVertices(
          refineGraphConnectionBridge(
            refineGraphConnectionFreeCellStartEnd(
              refineGraphConnectionStartEnd(
                refineGraphStartGoal1(sSeg, mpTask)
                , mpTask), mpTask), mpTask), mpTask)

    }

    val semanticType = Constructor("graphAdd")
  }

  @combinator object TriGraphAdd {
    def apply: (Graph[List[Float], WUnDiEdge], MpTaskStartGoal) => Graph[List[Float], WUnDiEdge] = {
      case ((g, mpTask)) =>
        val (sIndex, eIndex) = findFreeCellTri(g,mpTask)

        val newNodes = g.nodes.toOuter.toList :+ mpTask.startPosition :+ mpTask.endPosition
        val newEdges = g.edges.toOuter.toList ++ List(
          WUnDiEdge(mpTask.startPosition, g.nodes.toOuter.toIndexedSeq(sIndex))(distance(mpTask.startPosition, g.nodes.toOuter.toIndexedSeq(sIndex))),
          WUnDiEdge(mpTask.endPosition, g.nodes.toOuter.toIndexedSeq(eIndex))(distance(mpTask.endPosition, g.nodes.toOuter.toIndexedSeq(eIndex)))
        )

        Graph.from(newNodes, newEdges)
    }

    val semanticType = Constructor("tGraphAdd")
  }


  /*
  Adds new nodes for start and goal positions to the graph and connects them to selected centroids
  //combinator adds centroid connection for start endpoint
  */
    def refineGraphStartGoal1: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph = {
      case ((sSeg, mpTask)) =>
        val (startIndex, endIndex) = findFreeCell(sSeg,mpTask)
        val startCentroid = sSeg.centroids(startIndex)
        val endCentroid = sSeg.centroids(endIndex)

        val newNodes = sSeg.graph.nodes.toOuter.toList :+ mpTask.startPosition :+ mpTask.endPosition
        val newEdges = sSeg.graph.edges.toOuter.toList ++ List(
          WUnDiEdge(mpTask.startPosition, startCentroid)(distance(mpTask.startPosition, startCentroid)),
          WUnDiEdge(mpTask.endPosition, endCentroid)(distance(mpTask.endPosition, endCentroid))
        )

        sSeg.withGraph(Graph.from(newNodes, newEdges))
    }



  //combinator adds connectionpoints for start endpoint
    def refineGraphConnectionStartEnd: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph = {
      case ((sSeg, mpTask)) => {
        val g = sSeg.graph
        val startCentroidNode = g.find(mpTask.startPosition).get
        val endCentroidNode = g.find(mpTask.endPosition).get

        val otherStartNodes = g filter g.having(edge = _.hasSource(startCentroidNode))
        val otherEndNodes = g filter g.having(edge = _.hasSource(endCentroidNode))
        val newEdgesConnxPoints = List.empty ++
          otherStartNodes.nodes.map(_.toOuter).map { i => WUnDiEdge(i, mpTask.startPosition)(distance(i, mpTask.startPosition)) } ++
          otherEndNodes.nodes.map(_.toOuter).map { i => WUnDiEdge(i, mpTask.endPosition)(distance(i, mpTask.endPosition)) }

        sSeg.withGraph(Graph.from(sSeg.graph.nodes, sSeg.graph.edges.toOuter ++ newEdgesConnxPoints))
      }
    }

    //combinator adds all freeCell points for start endpoint
    def refineGraphConnectionFreeCellStartEnd: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph = {
      case ((sSeg, mpTask)) => {
        val g = sSeg.graph
        val (startNodeCellIndex, endNodeCellIndex) = findFreeCell(sSeg, mpTask)
        val startCell = sSeg.freeCells(startNodeCellIndex)
        val endCell = sSeg.freeCells(endNodeCellIndex)
        val startCellValues = startCell.map(i => sSeg.vertices(i)).filter(c => c.last.abs != sSeg.boundaries(1)/2)
        val endCellValues = endCell.map(i => sSeg.vertices(i)).filter(c => c.last.abs != sSeg.boundaries(1)/2)
        val newNodes= g.nodes.toOuter ++ startCellValues ++ endCellValues

        val newEdges = g.edges.toOuter ++
          startCellValues.map{ i => WUnDiEdge(i, mpTask.startPosition)(distance(i, mpTask.startPosition)) } ++
          endCellValues.map{ i => WUnDiEdge(i, mpTask.endPosition)(distance(i, mpTask.endPosition)) }

        sSeg.withGraph(Graph.from(newNodes, sSeg.graph.edges.toOuter ++ newEdges))
      }
    }


  //Combinator adds direct connection between Connection points with different x values
    def refineGraphConnectionBridge: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph = {
      case ((sSeg, mpTask)) => {
        val g = sSeg.graph
        val nEdges = g.edges.toOuter ++ sSeg.centroids.flatMap(c => {
          val centroidNode = (g find c).get
          val connectionPoints = g filter g.having(edge = _.hasSource(centroidNode))

          for (
            i <- connectionPoints.nodes.toOuter;
            j <- connectionPoints.nodes.toOuter
            if i != j && i.head != j.head
          ) yield WUnDiEdge(i, j)(distance(i, j))
        })

        sSeg.withGraph(Graph.from(g.nodes.toOuter, nEdges))
      }
    }



  // Combinator adds all free cell vertices (i.e. also obstacle vertices) and edges
    def refineGraphAllCellVertices: (PolySceneSegmentationGraph, MpTaskStartGoal) => PolySceneSegmentationGraph = {
      case ((sSeg, _)) => {
        val g = sSeg.graph
        val newNodes = sSeg.freeCells.flatMap(vertexIndices => vertexIndices.map {
          vId => sSeg.vertices(vId)
        })

        val newEdges = (sSeg.freeCells zip sSeg.centroids).map{case (cell, centroid) =>
          val f1 = (for (j <- cell;
                k <- cell;
                v1 = sSeg.vertices(j);
                v2 = sSeg.vertices(k)
                if (j != k && v1.head != v2.head
                  && v1.last.abs != sSeg.boundaries(1) / 2
                  && v2.last.abs != sSeg.boundaries(1) / 2))
            yield WUnDiEdge(v1, v2)(distance(v1, v2))
            )
          val f2 = (for (vid <- cell;
                  v = sSeg.vertices(vid);
                  cp <- (g filter g.having(edge = _.hasSource(centroid))).nodes.toOuter
                  ) yield
              List(WUnDiEdge(v, cp)(distance(v, cp)),
                WUnDiEdge(v, centroid)(distance(v, centroid)))).reduce(_ ++ _)
            f1 ++ f2
        }.reduce(_ ++ _)
        println(s"new edges: $newEdges")
        sSeg.withGraph(Graph.from(g.nodes.toOuter ++ newNodes,g.edges.toOuter ++ newEdges))
      }
    }




  @combinator object CellCentroidSceneToScalaGraph {
    def apply: PolySceneCellSegmentationCentroids => Graph[List[Float], WUnDiEdge] = { csc =>
      val segmentationLines = for (freeCell1 <- csc.freeCells.indices;
                                   freeCell2 <- csc.freeCells.indices;
                                   i = csc.freeCells(freeCell1).intersect(csc.freeCells(freeCell2));
                                   if (i.nonEmpty && freeCell1 != freeCell2))
        yield (i, freeCell1, freeCell2)
      val filteredLines = segmentationLines.filter(i => i._2 < i._3)
      println("FilteredLinesOut")
      filteredLines.foreach(println)
      println("FilteredLinesDone")

      val connectionPoints = filteredLines.map {
        case ((segmenationLine, leftCell, rightCell)) =>
          (List(csc.vertices(segmenationLine.head).head,
            (csc.vertices(segmenationLine.head).last + csc.vertices(segmenationLine.last).last) / 2),
            leftCell, rightCell)
      }

      println(s"connectionPoints ${connectionPoints}")
      val nodes = csc.centroids
      val otherNodes = connectionPoints.map(_._1)
      val edges = connectionPoints.withFilter(t => t match {
        case (cp, leftCellId, rightCellId) => true
        case _ => false
      }).map {
        case (cp, leftCellId, rightCellId) =>
          List(WUnDiEdge(cp, nodes(leftCellId))(distance(cp, nodes(leftCellId))),
            WUnDiEdge(cp, nodes(rightCellId))(distance(cp, nodes(rightCellId))))
      }.flatten

      val foo = Graph.from(nodes ++ otherNodes, edges)
      println("Returning Cell Graph")
      Graph.from(nodes ++ otherNodes, edges)
    }

    val semanticType = sd_poly_scene_segmentation :&: sd_seg_centroid_cells =>: cmp_cell_graph
  }


  @combinator object CellCentroidSceneToScalaGraphTri {
    def apply: TriangleSegCentroids => Graph[List[Float], WUnDiEdge] = { csc =>
      val segmentationLines = for (freeCell1 <- csc.triangles.indices;
                                   freeCell2 <- csc.triangles.indices;
                                   i = csc.triangles(freeCell1).intersect(csc.triangles(freeCell2));
                                   if (i.nonEmpty && freeCell1 != freeCell2))
        yield (i, freeCell1, freeCell2)
      val filteredLines = segmentationLines.filter(i => i._2 < i._3)
      println("FilteredLinesOut")
      filteredLines.foreach(println)
      println("FilteredLinesDone")


      val centroids = csc.centroids
      val edges:IndexedSeq[WUnDiEdge[List[Float]]] = filteredLines.map {
        case (cp: List[Float], leftCellId: Int, rightCellId: Int) =>{
          val v1 = csc.vertices(cp.head)
          val v2 = csc.vertices(cp.last)
          List(WUnDiEdge(v1, centroids(leftCellId))(distance(v1, centroids(leftCellId))),
            WUnDiEdge(v2, centroids(rightCellId))(distance(v2, centroids(rightCellId))))
      }}.flatten

      val foo = Graph.from(centroids, edges)

      //TODO AddNodes, Find Start end Cells, Add Edges
      println("Returning Cell Graph")
      Graph.from(centroids, edges)
    }

    val semanticType = Constructor("tGraphbuild")
  }




  // (Seq[N],Seq[E[N]],Float)
  //Dijkstra shortest path, returns
  @combinator object ShortestPathDijkstra {
    def apply(g: Graph[List[Float], WDiEdge], termNodes: Set[(List[Float])]):
    (Seq[List[Float]], Seq[WUnDiEdge[List[Float]]], Float) = {
      val p: Option[g.Path] = g get (termNodes.head) shortestPathTo (g get (termNodes.last))
      p match {
        case Some(path) =>
          val nodes = path.nodes.toList.map(i => i.toOuter)
          val edges = path.edges.toList.map(i => i.toOuter)
          (nodes, edges, path.weight.toFloat)
      }
    }

    val semanticType = 'graph =>: 'gTermNodes =>: 'graph_traversal :&: 'gt_dijkstra
  }


  @combinator object DFS {
    /*
      1 procedure DFS(G, v):
      2     label v as explored
      3     for all edges e in G.incidentEdges(v) do
      4         if edge e is unexplored then
      5             w ← G.adjacentVertex(v, e)
      6             if vertex w is unexplored then
      7                 label e as a discovered edge
      8                 recursively call DFS(G, w)
      9             else
      10               label e as a back edge
      */


    def apply(): Unit = { println("foo")
      //    val g = Graph(1~2 % 4, 2~3 % 2, 1~>3 % 5, 1~5  % 3,
      //      3~5 % 2, 3~4 % 1, 4~>4 % 1, 4~>5 % 0)
    }
    val semanticType = 'graph_traversal =>: 'gt_dfs
  }







}
