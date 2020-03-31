package org.combinators.ctp.repositories.scene

import cats.syntax.NestedIdOps
import io.circe.generic.JsonCodec
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.combinators.cls.interpreter._
import org.combinators.cls.types.{Constructor, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.geometry._
import org.combinators.ctp.repositories.scene._
import org.combinators.ctp.repositories.{sd_poly_scene_segmentation, _}
import org.combinators.ctp._
import org.combinators.ctp.repositories.taxkinding.{CtpTaxonomy, SceneDescription}
import scalaz.Tree
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode
import org.locationtech.jts.geom.{Coordinate, Triangle}

//@JsonCodec
//case class Scene(boundaries: List[Float], obstacles: List[MqttCubeData]) {}
//
//@JsonCodec
//case class PolygonScene(vertices: List[List[Float]], obstacles: List[List[Int]], boundaries: List[Float]) {}

trait SceneRepository extends SceneDescription with CtpTaxonomy with GeometricRepository{
  @combinator object SceneToScenePoly {
    def apply(cubeToVList: List[MqttCubeData] => List[PpVertexList]): Scene => PolygonScene = { s: Scene =>

      print("scene to poly before: " + s.obstacles.foreach(print))
      val obstacleVertexTuple = cubeToVList(s.obstacles)

      print("scene to poly after: " + obstacleVertexTuple)

      val (_, objList, globalVertices) = obstacleVertexTuple.
        foldLeft(0, List.empty[Range], List.empty[List[Float]]) {
          case ((id, obsVertices, globalVertices), obstacleVertices) =>
            (id + obstacleVertices.vertices.size,
              Range(id, id + obstacleVertices.vertices.size) +: obsVertices ,
              globalVertices ++ obstacleVertices.vertices)
        }
      objList.foreach(i =>println("objList: " + i))
      val objects = objList.map{_.toList}
      PolygonScene(globalVertices, objects, s.boundaries)
    }

    val semanticType = gm_CubeToPoly :&: dimensionality_var =>:
      (sd_unity_scene_type =>: sd_polygon_scene_type) :&: dimensionality_var
  }

//  @combinator object SceneTransform {
//
//    val semanticType = (scene_arrow_alpha =>: scene_arrow_beta) =>: (scene_arrow_beta =>: scene_arrow_gamma) =>: (scene_arrow_alpha =>: scene_arrow_gamma) // ergibt es Sinn?
//  }


  @combinator object TranslResult {
    def apply(): PolySceneLineSegmentation => SegmentationLines2d = { a =>
      println(a.asJson)
      SegmentationLines2d(a.lines.map { b => b.map(c => a.vertices(c)) })
    }

    val semanticType = Constructor("foo")
  }

  @combinator object SegmentationLinesToCells {
    //Adds lines if obstacle has a vertex on bottom or top boundary
    def addUtilityLines(ls: PolySceneLineSegmentation): PolySceneLineSegmentation = {
      val i_lines = (ls.topVertices.union(ls.bottomVertices)).intersect(ls.obstacles.flatten).map(i => List(i, i))
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
              j <- down_comb) yield (i ++ cl ++ j)).distinct
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
                       cellLines = findRightCell(i, ls_var, ls_var.vertices(ls_var.lines(i).head).head);
                       if cellLines._2.nonEmpty) yield {
        cellLines._1 ++ cellLines._2
      }

      PolySceneCellSegmentation(ls_var.vertices, ls_var.obstacles, ls_var.boundaries, cells.distinct.toList)
    }
    val semanticType = sd_seg_lines :&: sd_poly_scene_segmentation =>: sd_seg_cells :&: sd_poly_scene_segmentation
  }


  @combinator object CellToCentroidCellNaive {
    def apply: PolySceneCellSegmentation => PolySceneCellSegmentationCentroids = { pscs =>
      val xvals = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid).head))
      val yvals = pscs.freeCells.map(i => i.map(vid => pscs.vertices(vid)(1)))

      val xIntervalList: List[Float] = xvals.map(i => (i.min + i.max)/2)
      val yInvervalList: List[Float] = yvals.map(i => (i.min + i.max)/2)

      val centroids = xIntervalList.zip(yInvervalList).map { case ((a, b)) => List(a, b) }

      PolySceneCellSegmentationCentroids(pscs.vertices, pscs.obstacles, pscs.boundaries, pscs.freeCells, centroids)
    }

    val semanticType = sd_poly_scene_segmentation :&: sd_seg_cells =>: sd_poly_scene_segmentation :&: sd_seg_centroid_cells
  }

  @combinator object CellToCentroidCellJTSCentroids {
    def apply: TriangleSeg => TriangleSegCentroids = { pscs =>
      println("c1")
      println(s"pscs $pscs")
      println(s"freecells ${pscs.triangles}")
      println(s"vertices ${pscs.vertices}")
      val vertices = pscs.triangles.map(i => i.map(vid => pscs.vertices(vid)))

      val coordObjects = vertices.map(a => Triangle.centroid(
        new Coordinate(a(0)(0), a(0)(1)),
        new Coordinate(a(1)(0), a(1)(1)),
        new Coordinate(a(2)(0), a(2)(1))))

      val centroids = coordObjects.map(coord => List(coord.x.toFloat, coord.y.toFloat))
      println("c4")

      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = triangle_centroidsFct_type :&: Constructor("jts_centroids")
  }

/*  Circumcenter of the Triangle, not necessarily inside Triangle*/
  @combinator object CellToCentroidCellJTSIncentre  {
    def apply: TriangleSeg => TriangleSegCentroids = { pscs =>
      println("c1")
      println(s"pscs $pscs")
      println(s"freecells ${pscs.triangles}")
      println(s"vertices ${pscs.vertices}")
      val vertices = pscs.triangles.map(i => i.map(vid => pscs.vertices(vid)))

      val coordObjects = vertices.map(a => Triangle.inCentre(
        new Coordinate(a(0)(0), a(0)(1)),
        new Coordinate(a(1)(0), a(1)(1)),
        new Coordinate(a(2)(0), a(2)(1))))

      val centroids = coordObjects.map(coord => List(coord.x.toFloat, coord.y.toFloat))
      println("c4")

      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = triangle_centroidsFct_type
  }



  def avgForVertexCell(cell: List[List[Float]]) = {
    cell.reduceLeft { (a, b) =>
      val zipped = a.zipAll(b, 0.0f,0.0f)
      zipped.map{case (val1,val2) => val1 + val2}
    }.map(_/cell.size)
  }

  @combinator object CellToCentroidND{
    def apply: TriangleSeg => TriangleSegCentroids = { pscs =>
      println("c1")
      println(s"pscs $pscs")
      println(s"freecells ${pscs.triangles}")
      println(s"vertices ${pscs.vertices}")
      val cellsV = pscs.triangles.map(i => i.map(pscs.vertices))
      println("c2")
      val centroids = cellsV.map(avgForVertexCell)
//      val centroids = cellsV.map(cell => cell.map(vertex => vertex)
//        cell(vid).indices.map(dimension => cell(vid)(dimension)).sum / cell.size))
      println("c3")
      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = triangle_centroidsFctNd_type
    //celltype centroidberechnung id, centroid art
  }

  def tCentroidJts2D(triangle: List[List[Float]]): List[Float] = {
    val coords = triangle.map { i =>
      if (i.size == 2)
        new Coordinate(i.head, i.last)
      else
        println(s"Warning Centroid JTS: invalid # of dimensions $i")
      new Coordinate(0.0f, 0.0f)
    }

    val t = new Triangle(coords(0), coords(1), coords(2))
    List(t.centroid().x, t.centroid().y).map(_.toFloat)
  }

  def tCentroidJts3D(triangle: List[List[Float]]): List[Float] = {
    val coords = triangle.map { i =>
      if (i.size == 3)
        new Coordinate(i.head,i(1), i.last)
      else
        println(s"Warning Centroid JTS: invalid # of dimensions $i")
      new Coordinate(0.0f, 0.0f,0.0f)
    }

    val t = new Triangle(coords(0), coords(1), coords(2))
    List(t.centroid().x, t.centroid().y).map(_.toFloat)
  }


  @combinator object TriangleCentroidJts{
    def apply: TriangleSeg => TriangleSegCentroids = { pscs =>
      println("c1")
      println(s"pscs $pscs")
      println(s"freecells ${pscs.triangles}")
      println(s"vertices ${pscs.vertices}")
      val cellsV = pscs.triangles.map(i => i.map(pscs.vertices))
      println("c2")
      val centroids = cellsV.map(tCentroidJts2D)
      //      val centroids = cellsV.map(cell => cell.map(vertex => vertex)
      //        cell(vid).indices.map(dimension => cell(vid)(dimension)).sum / cell.size))
      println("c3")
      pscs.withCentroids(centroids)
      println("c5")
      pscs.withCentroids(centroids)
    }

    val semanticType = triangle_centroidsFctNd_type :&: dimensionality_two_d_t
  }
  /*
Combinator to apply affine 2d transformation to 2d structure for a single vertex.
*/
/*
  @combinator object ApplyAffineTransform2D {
    def apply(): (List[Float], MqttTransform) => List[Float] = {
      (p: List[Float], tMatrix: MqttTransform) => {
        val f = p :+ 1.0f
        val tMatrixList = tMatrix.transformMatrixList
        mult(tMatrixList, f).take(2)
      }
    }

    val semanticType = scene segmanetation => line
  }
*/

  @combinator object SceneBoundariesTwoDim {
    def apply: ((Int, Int), (Int, Int)) = ((-5, 5), (-3, 3))

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t
  }

  @combinator object SceneBoundariesThreeDim {
    def apply: ((Int, Int), (Int, Int), (Int, Int)) = ((-5, 5), (-3, 3), (4, 4))

    val semanticType = sd_scene_boundaries_type :&: dimensionality_three_d_t
  }

  /*@combinator object AddSceneObstacleMeshesTwoD {
    def apply(s: Scene, o: List[scene_cube_2d_n]): Scene = ??? //s.addSceneObject(o)

    val semanticType = sd_scene_boundaries_type :&: dimensionality_two_d_t =>:
      sd_scene_descripton_obstacles :&: dimensionality_two_d_t =>:
      sd_source_native_scala_type :&: dimensionality_two_d_t
  }*/
}
