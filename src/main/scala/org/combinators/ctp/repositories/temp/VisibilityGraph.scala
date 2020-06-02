package org.combinators.ctp.repositories.temp

import org.combinators.ctp.repositories.cmp.{CmpCellCentroidsRepository, CmpUtils}
import org.combinators.ctp.repositories.toplevel.PolygonScene
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

  object VisibilityGraph extends CmpUtils with CmpCellCentroidsRepository{
    def apply(polyScene: PolygonScene): Graph[List[Float], WUnDiEdge] = {

      val obstacleVertexLists: List[List[List[Float]]] = polyScene.obstacles.map(i => i.map(polyScene.vertices))
      val hulls: List[Geometry] = obstacleVertexLists.map { actualCoords =>
        // generate convex hull for current batch of points
        val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
        val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
        val hullGeometry = convexHull.getConvexHull
        hullGeometry
      }

      def possibleLines(coords: List[Coordinate]): List[LineString] = {
        if (coords.isEmpty) return Nil
        val c1 = coords.head
        val rest = coords.tail
        val lines = rest.map(c => new LineString(new CoordinateArraySequence(Array(c1, c)), new GeometryFactory()))
        lines ++ possibleLines(rest)
      }

      val hullEdges: List[LineString] = hulls.flatMap(hull => {
        val coords: List[Coordinate] = hull.getCoordinates.toList
        val lines: Seq[LineString] = possibleLines(coords.distinct)
        lines.filter(line => hull.getBoundary.contains(line))
      })

      val reflexCoords: List[Coordinate] = hulls.flatMap(hull => {
        val coords: List[Coordinate] = hull.getCoordinates.toList
        val reflexCoords = coords.filter(c => {
          val cEdges = hullEdges.filter(edge => edge.isCoordinate(c))
          assert(cEdges.length == 2, "How can a vertex of a convex polygon not have exactly 2 outgoing edges???!")
          val outgoingEnds = cEdges.flatMap(edge => edge.getCoordinates).distinct.filter(
            coord => coord != c)
          hull.intersects(new LineString(new CoordinateArraySequence(outgoingEnds.toArray), new GeometryFactory()))
        })
        reflexCoords
      })


      val consecutiveEdges: List[LineString] = hullEdges.filter(edge => reflexCoords.contains(edge.getStartPoint.getCoordinate) && reflexCoords.contains(edge.getEndPoint.getCoordinate))

      val bitangentEdges: List[LineString] = possibleLines(reflexCoords).filter(line => {
        val intersections = hulls.foldLeft(0)({
          case (n, hull) => if (hull.intersects(line)) n + 1 else n
        })
        intersections == 2
      })

      val nodes: List[List[Float]] = reflexCoords.map(c => List(c.x.toFloat, c.y.toFloat))

      val edges: List[WUnDiEdge[List[Float]]] = (consecutiveEdges ++ bitangentEdges).map(line => {
        val v1 = List(line.getStartPoint.getX.toFloat, line.getStartPoint.getY.toFloat)
        val v2 = List(line.getEndPoint.getX.toFloat, line.getEndPoint.getY.toFloat)
        WUnDiEdge(v1, v2)(distance(v1, v2))
      })
      Graph.from(nodes,edges)
    }
  }

