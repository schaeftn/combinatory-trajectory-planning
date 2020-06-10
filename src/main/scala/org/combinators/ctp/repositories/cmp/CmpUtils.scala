package org.combinators.ctp.repositories.cmp


import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._
import org.combinators.ctp.repositories.geometry.GeometryUtils
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, Point}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

trait CmpUtils extends GeometryUtils with EncodeImplicits{
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

  def edgesVertexToVertex(v1: List[Float], v2: List[Float]): List[WUnDiEdge[List[Float]]] = {
    edgesVertexlistToVertexlist(List(v1), List(v2))
  }

  def edgesVertexToVertexlist(vertex: List[Float], vertexList: List[List[Float]]): List[WUnDiEdge[List[Float]]] = {
    edgesVertexlistToVertexlist(List(vertex), vertexList)
  }

  def edgesVertexlistToVertexlist(vl1: List[List[Float]], vl2: List[List[Float]]): List[WUnDiEdge[List[Float]]] = {
    for (j <- vl1;
         k <- vl2
         if j != k)
      yield WUnDiEdge(j, k)(distance(j, k)) //TODO Prio2 Performance check vs indexedList and index-based guard i>k
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
