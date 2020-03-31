package org.combinators.ctp.repositories.celldecomposition

import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.interpreter._
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpAaBb2D, PpVertexList}
import org.combinators.ctp.repositories.python_interop.{PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.ctp.repositories.scene.{ PolySceneLineSegmentation, PolygonScene, TriangleSeg}
import org.combinators.ctp.repositories.toplevel.AkkaImplicits

trait CellDecompRepository extends PythonTemplateUtils with AkkaImplicits {
  @combinator object VcdPoly {
    def apply(toAabb: PpVertexList => PpAaBb2D): PolygonScene => PolySceneLineSegmentation = { polygonScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val verticesString = "    v = " + listToPythonArray(for (i <- scene.vertices)
          yield listToPythonArray(
            for (point <- i) yield point)) // TODO Vertices B0 =  usw...

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
      pWrapper.computeResultAndModifyInput(polygonScene)
    }

    val semanticType = gm_aaBbGenFct :&: dimensionality_two_d_t =>: (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_lines)
  }

  // Pymesh without parameters; only adds cfree triangles
  @combinator object TriangulatePoly {
    def apply: PolygonScene => TriangleSeg = { polyScene: PolygonScene =>
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

      val decodeFct = (_:PolygonScene, resultString: String) => decode[TriangleSeg](resultString).right.get
      val fileMap = Map(cdTemplateLocationTri -> cdStartLocationTri)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTri, decodeFct)
      pWrapper.computeResultAndModifyInput(polyScene)
    }

    val semanticType = sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: sd_seg_triangles_simple
  }

  @combinator object TriangulatePolyParametrized {
    def apply: PolygonScene => TriangleSeg = { polyScene: PolygonScene =>
      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray)}.map(listToPythonArray)
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectCHull2D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      val decodeFct = (_:PolygonScene, resultString: String) => decode[TriangleSeg](resultString) match {
        case Left(_) =>
          println(s"error while decoding")
          println(s"$resultString")
          TriangleSeg(List.empty[List[Float]], List.empty[List[Int]])
        case Right(s) => s
      }

      val fileMap = Map(cdTemplateLocationTriPara -> cdStartLocationTriPara)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTriPara, decodeFct)
      pWrapper.computeResultAndModifyInput(polyScene)
    }

    val semanticType = sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: sd_seg_triangles_para
  }

  @combinator object TetPoly {
    def apply: PolygonScene => TriangleSeg = { polyScene: PolygonScene =>
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

      val decodeFct = (_:PolygonScene,resultString: String) => decode[TriangleSeg](resultString).right.get

      val fileMap = Map(cdTemplateLocationTet -> cdStartLocationTet)
      val substMap = Map("$substitute$" -> pythonSceneString(polyScene))
      val t = SubstitutionScheme(fileMap, substMap)

      val pWrapper = PythonWrapper.apply(t, cdStartLocationTet, decodeFct)
      pWrapper.computeResultAndModifyInput(polyScene)
    }

    val semanticType = (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells) :&:
      dimensionality_three_d_t
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