package org.combinators.ctp.repositories.cmp

import org.combinators.cls.interpreter._
import org.combinators.cls.types.Constructor
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.scene.{PolySceneCellSegmentation, PolySceneCellSegmentationCentroids, PolySceneLineSegmentation}
import org.locationtech.jts.geom.{Coordinate, Triangle}

trait CmpCellCentroidsRepository extends CmpUtils {
  def avgForVertexCell(cell: List[List[Float]]) = {
    cell.reduceLeft { (a, b) =>
      val zipped = a.zipAll(b, 0.0f,0.0f)
      zipped.map{case (val1,val2) => val1 + val2}
    }.map(_/cell.size)
  }

  @combinator object CellToCentroidCellNaive {
    def apply: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids = { pscs =>
      val xvals = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid).head))
      val yvals = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)(1)))

      val xIntervalList: List[Float] = xvals.map(i => (i.min + i.max) / 2)
      val yInvervalList: List[Float] = yvals.map(i => (i.min + i.max) / 2)

      val centroids = (xIntervalList.zip(yInvervalList)).map { case (a, b) => List(a, b) }

      PolySceneCellSegmentationCentroids(pscs.vertices, pscs.obstacles, pscs.boundaries, pscs.freeCells, centroids)
    }

    val semanticType = cmd_centroidFct_type :&: cFct_centroids_naive_type :&: sd_cell_type_var :&: dimensionality_var
  }

  @combinator object CellToCentroidCellJTSCentroids {
    def apply: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids = { pscs =>
      println("c1")
      println(s"pscs $pscs")
      println(s"freecells ${pscs.freeCells}")
      println(s"vertices ${pscs.vertices}")
      val vertices = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)))

      val coordObjects = vertices.map(a => Triangle.centroid(
        new Coordinate(a.head.head, a.head(1)),
        new Coordinate(a(1).head, a(1)(1)),
        new Coordinate(a(2).head, a(2)(1))))

      val centroids = coordObjects.map(coord => List(coord.x.toFloat, coord.y.toFloat))
      println("c4")

      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = cmd_centroidFct_type :&: cFct_jts_default_type :&: sd_cell_triangle_type :&: dimensionality_two_d_t
  }


  /*  Circumcenter of the Triangle, not necessarily inside Triangle*/
  @combinator object CellToCentroidCellJTSIncentre  {
    def apply: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids = { pscs =>
      val vertices = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)))

      val coordObjects = vertices.map(a => Triangle.inCentre(
        new Coordinate(a.head.head, a.head(1)),
        new Coordinate(a(1).head, a(1)(1)),
        new Coordinate(a(2).head, a(2)(1))))

      val centroids = coordObjects.map(coord => List(coord.x.toFloat, coord.y.toFloat))
      println("c4")

      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = cmd_centroidFct_type :&: cFct_jts_incentre_type :&: sd_cell_triangle_type :&: dimensionality_two_d_t
  }

  @combinator object CellToCentroidND{
    def apply: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids = { pscs =>
      println("c1")
      println(s"pscs $pscs")
      println(s"freecells ${pscs.freeCells}")
      println(s"vertices ${pscs.vertices}")
      val cellsV = pscs.freeCells.map(i => i.map(pscs.vertices))
      println("c2")
      val centroids = cellsV.map(avgForVertexCell)
      println("c3")
      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = triangle_centroidsFctNd_type :&: cFct_avg_type :&: sd_cell_type_var :&: dimensionality_var
  }

  //TODO combinator ball center nd
}
