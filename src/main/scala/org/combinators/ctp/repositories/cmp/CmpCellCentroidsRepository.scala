package org.combinators.ctp.repositories.cmp

import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel.PolySceneCellSegmentation
import org.locationtech.jts.geom.{Coordinate, Triangle}

trait CmpCellCentroidsRepository extends CmpUtils {
  def avgForVertexCell(cell: List[List[Float]]) = {
    cell.reduceLeft { (a, b) =>
      val zipped = a.zipAll(b, 0.0f, 0.0f)
      zipped.map { case (val1, val2) => val1 + val2 }
    }.map(_ / cell.size)
  }

  @combinator object CellToCentroidCellNaive {
    def apply: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] = { (pscs, _) =>
      val xvals = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid).head))
      val yvals = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)(1)))

      val xIntervalList: List[Float] = xvals.map(i => (i.min + i.max) / 2)
      val yInvervalList: List[Float] = yvals.map(i => (i.min + i.max) / 2)

      val centroids = xIntervalList.zip(yInvervalList).map { case (a, b) => List(List(a, b)) }

      centroids
    }

    val semanticType = cmd_centroidFct_type :&: rm_withCentroids_type :&: cFct_centroids_naive_type :&: sd_cell_type_var :&: dimensionality_var
  }

  @combinator object CellToCentroidCellJTSCentroids {
    def apply: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] = { (pscs,_) =>
      val vertices = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)))

      val coordObjects = vertices.map(a => Triangle.centroid(
        new Coordinate(a.head.head, a.head(1)),
        new Coordinate(a(1).head, a(1)(1)),
        new Coordinate(a(2).head, a(2)(1))))

      val centroids = coordObjects.map(coord => List(List(coord.x.toFloat, coord.y.toFloat)))
      centroids
    }

    val semanticType = cmd_centroidFct_type :&: rm_withCentroids_type :&: cFct_jts_default_type :&: sd_cell_triangle_type :&: dimensionality_two_d_t
  }

  @combinator object CentroidFctJTSIncentre {
    def apply: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] = { (pscs,_) =>
      val vertices = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)))

      val coordObjects = vertices.map(a => Triangle.inCentre(
        new Coordinate(a.head.head, a.head(1)),
        new Coordinate(a(1).head, a(1)(1)),
        new Coordinate(a(2).head, a(2)(1))))

      val centroids: List[List[List[Float]]] = coordObjects.map(coord => List(List(coord.x.toFloat, coord.y.toFloat)))
      centroids
    }

    val semanticType = cmd_centroidFct_type :&: rm_withCentroids_type :&: cFct_jts_incentre_type :&: sd_cell_triangle_type :&: dimensionality_two_d_t
  }

  @combinator object CellToCentroidND {
    def apply: (PolySceneCellSegmentation, neighbourCellsNativeType) => List[List[List[Float]]] = { (pscs,_) =>
      val cellsV = pscs.freeCells.map(i => i.map(pscs.vertices))
      val centroids = cellsV.map(cellVertices => List(avgForVertexCell(cellVertices)))
      centroids
    }

    val semanticType = cmd_centroidFct_type :&: rm_withCentroids_type :&: cFct_avg_type :&: sd_cell_type_var :&: dimensionality_var
  }

  //TODO combinator ball center nd
}
