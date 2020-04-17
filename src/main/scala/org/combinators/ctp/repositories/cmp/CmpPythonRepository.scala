package org.combinators.ctp.repositories.cmp

import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpAaBb2D, PpVertexList}
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.toplevel.{PolySceneCellSegmentation, PolySceneLineSegmentation, PolygonScene}
import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.locationtech.jts.algorithm.ConvexHull
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}

trait CmpPythonRepository extends PythonTemplateUtils with CmpUtils {
  @combinator object VcdPoly {
    def apply(toAabb: PpVertexList => PpAaBb2D): PolygonScene => PolySceneCellSegmentation = { polygonScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val verticesString = "    v = " + listToPythonArray(for (i <- scene.vertices)
          yield listToPythonArray(
            for (point <- i) yield point))

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val obstacleIndexLists = scene.obstacles.indices.map(i =>
          s"""    b$i = ${listToPythonArray(scene.obstacles(i))}""").reduce(_.toString + "\n" + _.toString)

        val obstaclesVertexList = scene.obstacles.map { obs => obs.map { vListIndex => scene.vertices(vListIndex) } }
        val ppaabbList = obstaclesVertexList.map(vList => PpVertexList(vList)).map(polygon => toAabb(polygon))
        val aabbString = ppaabbList.map(i => s"[${i.xBounds.toString()}, ${i.yBounds.toString()}]").mkString(", ")

        s"$obstacleIndexLists\n" +
          s"$verticesString\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_objects_aabb = [$aabbString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      def decodeString: (PolygonScene, String) => PolySceneLineSegmentation =
        (_:PolygonScene, s: String) => decode[PolySceneLineSegmentation](s).right.get

      val fileMap = Map(cdPolyTemplateLocation -> cdPolyStartLocation)
      val substMap = Map("$substitute$" -> pythonSceneString(polygonScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t,cdPolyStartLocation,decodeString)
      val sceneSegmentation = vcdLtC2.apply(pWrapper.computeResultAndModifyInput(polygonScene))
      sceneSegmentation.freeCells.foreach(i => println(s"freeCell: ${i.map(sceneSegmentation.vertices)}"))
      sceneSegmentation
    }

    val semanticType = gm_aaBbGenFct :&: dimensionality_two_d_t =>:
      cmp_sceneSegFct_type :&: sd_vertical_cell_decomposition_type :&: dimensionality_two_d_t
  }

  // Pymesh without parameters; only adds cfree triangles
  @combinator object TriangulatePoly {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray)}.map(listToPythonArray)

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectCHull2D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val fileMap = Map(cdTemplateLocationTri -> cdStartLocationTri)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTri, decodeCellSegmentationFct)
      pWrapper.computeResultAndModifyInput(polyScene)
    }

    val semanticType = cmp_sceneSegFct_type :&:  sd_seg_triangles_simple_type:&: dimensionality_two_d_t
  }

  @combinator object TriangulatePolyParametrized {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray)}.map(listToPythonArray)
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectCHull2D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val fileMap = Map(cdTemplateLocationTriPara -> cdStartLocationTriPara)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTriPara, decodeCellSegmentationFct)
      pWrapper.computeResultAndModifyInput(polyScene)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_para_type :&: dimensionality_two_d_t
  }

  @combinator object TetPoly {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray) }.map(listToNpArray)

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectBox3D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val fileMap = Map(cdTemplateLocationTet -> cdStartLocationTet)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTet, decodeCellSegmentationFct)
      pWrapper.computeResultAndModifyInput(polyScene)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_triangles_simple_type :&: sd_seg_triangles_para_type :&: dimensionality_three_d_t
  }

  @combinator object GridCombinator {
    def apply: PolygonScene => PolySceneCellSegmentation = { polyScene: PolygonScene =>
      val (xResolution: Int, yResolution: Int) = (30, 30)
      val (xDim, yDim) = (polyScene.boundaries.head, polyScene.boundaries(1))
      val vertexLists = polyScene.obstacles.map(i => i.map(polyScene.vertices))
      val hulls: List[Geometry] = vertexLists.map { actualCoords =>
        // generate convex hull for current batch of points
        val coords = actualCoords.map(c => new Coordinate(c.head, c(1)))
        val convexHull = new ConvexHull(coords.toArray, new GeometryFactory)
        val hullGeometry = convexHull.getConvexHull
        hullGeometry
      }
      println("...")
      val (xStep, yStep): (Float, Float) = (xDim / xResolution, yDim / yResolution)
      val gf = new GeometryFactory()
      val coordMap = for {
        x <- 0 to xResolution
      } yield for {
        y <- 0 to yResolution
      } yield new Coordinate(-xDim / 2 + x * xStep, -yDim / 2 + y * yStep)

      println("...")
      val gridCells: IndexedSeq[(Geometry, List[Coordinate])] = for (x <- 0 until xResolution;
                                                                     y <- 0 until yResolution
                                                                     ) yield {
        val coords = List(coordMap(x)(y), coordMap(x + 1)(y), coordMap(x + 1)(y + 1), coordMap(x)(y + 1), coordMap(x)(y))
        val geo: Geometry = gf.createPolygon(coords.toArray)
        (geo, coords)
      }
      println("...")
      val freeGridCells = gridCells.filter(i => !hulls.exists(g => g.intersects(i._1)))
      val freeGridCellsFloatVals: IndexedSeq[List[List[Float]]] = freeGridCells.map(_._2.map(coord => List(coord.x.toFloat, coord.y.toFloat)).distinct)
      println("getting new Vertices")
      val newVertices = freeGridCellsFloatVals.flatten.distinct.toIndexedSeq
      println("getting freeCells")
      val freeCells = freeGridCellsFloatVals.par.map(i => i.par.map(k => newVertices.indexOf(k) + polyScene.vertices.size).toList)
      println("...")
      polyScene.withVertices(polyScene.vertices ++ newVertices).withFreeCells(freeCells.toList)
    }

    val semanticType = cmp_sceneSegFct_type :&: sd_seg_grid_type :&: dimensionality_two_d_t
  }
  def boundVerts(b: List[Float]): List[List[Float]] =
    {
      println(s"bounds: b")
      val (xBound, yBound) = (b.head/2, b(1)/2)
      List(
        List(-xBound, yBound),
        List(xBound, yBound),
        List(xBound, -yBound),
        List(-xBound, -yBound),
      )
    }

/*

  @combinator object Vcd {
    def apply() = ()
    val semanticType  = 'runvcd :&: 'python :&: 'scene
  }

  @combinator object VerticalDecomposition2D {
    def apply(scene:scene_type_2d_n): Unit = ???
    val semanticType = ???
  }

  @combinator object VerticalDecomposition3D {
    def apply(): Unit = ???
    val semanticType = ???
  }

  @combinator object SweepLineCombinatorVd {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepPlaneVd {
    def apply: Unit = ???
    val semanticType = ???
  }

    @combinator object CylindricalDecomposition2D {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepLineCombinatorCyd {
    def apply: Unit = ???
    val semanticType = ???
  }

  @combinator object SweepPlaneCyd {
    def apply: Unit = ???
    val semanticType = ???
  }*/

}