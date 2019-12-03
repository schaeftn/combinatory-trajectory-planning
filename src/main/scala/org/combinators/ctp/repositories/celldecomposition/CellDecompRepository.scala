package org.combinators.ctp.repositories.celldecomposition

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties

import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.taxkinding._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpAaBb2D, PpPolyhedronMesh, PpVertexList}
import org.combinators.ctp.repositories.scene.{PolyLineSegmentation, PolySceneCellSegmentation, PolySceneLineSegmentation, PolygonScene, PythonTemplateUtils, Scene, TriangleSeg}
import org.combinators.ctp.repositories.toplevel.AkkaImplicits

import scala.sys.process._
import scala.io.Source

trait CellDecompRepository extends PythonTemplateUtils with AkkaImplicits {
/*
  implicit val decodeE: Decoder[PolySceneLineSegmentation] = Decoder[PolySceneLineSegmentation]
*/

  @combinator object VcdPoly {
    def apply(toAabb: PpVertexList => PpAaBb2D): PolygonScene => PolySceneLineSegmentation = { s: PolygonScene =>
      val connectionSettings = new Properties()
      connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
      val genfolder = connectionSettings.getProperty("org.combinators.ctp.python.genfolder")
      val cdStartLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationVcdPoly")
      val templateLocation =  genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdTemplateLocationVcdPoly")

      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val verticesString = "    v = " + listToPythonArray(for (i <- scene.vertices)
          yield listToPythonArray(
            for (point <- i) yield point)) // TODO Vertices B0 =  usw...

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val foo = scene.obstacles.indices.map(i =>
          s"""    b$i = ${listToPythonArray(scene.obstacles(i))}""").reduce(_.toString + "\n" + _.toString)

        //val fct = { i: List[Int] => PpPoint_vList(i.map(_ => scene.vertices(_)) )}
       // val foo2 = scene.obstacles.map(i => fct(i))
        val foo3 = scene.obstacles.map { obs => obs.map { vListIndex => scene.vertices(vListIndex) } } //vertices: List[List[Float]]

        val ppaabbList = foo3.map(vList => PpVertexList(vList)).map(polygon => toAabb(polygon))
        val aabbString = ppaabbList.map(i => s"[${i.xBounds.toString()}, ${i.yBounds.toString()}]").mkString(", ")

        s"$foo\n" +
          s"$verticesString\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_objects_aabb = [$aabbString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      def runCdFile(s: String):PolySceneLineSegmentation = {
        println(s"Template Location: $templateLocation")
        val templateSource = Source.fromFile(templateLocation)
        val fileContents = templateSource.getLines.mkString("\n")
        println(s"Got file contents $fileContents")
        templateSource.close

        println("template read")
        println("Replaced file contents: \n" + fileContents.replace("$substitute$", s))
        val file = new File(cdStartLocation)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(fileContents.replace("$substitute$", s))
        bw.close()
        println("outFile written")

        val foo = s"python3 $cdStartLocation"
        val resultString = foo.lineStream_!.takeWhile(_ => true).
          foldLeft("")((b, s) => b.concat(s))
        println(s"Resultstring: $resultString")
        val decoded  = decode[PolySceneLineSegmentation](resultString).right.get
        println("decoded")
        decoded
      }

      println(s"Python Scene String: ${pythonSceneString(s)}")
      runCdFile(pythonSceneString(s))
    }

    val semanticType = gm_aaBbGenFct :&: dimensionality_two_d_t =>: (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_lines)
  }

  // Pymesh without parameters; only adds cfree triangles
  @combinator object TriangulatePoly {
    def apply: PolygonScene => TriangleSeg = { s: PolygonScene =>
      val connectionSettings = new Properties()
      connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
      val genfolder = connectionSettings.getProperty("org.combinators.ctp.python.genfolder")
      val cdStartLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationTri")
      val templateLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdTemplateLocationTri")

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

      def runCdFile(s: String): TriangleSeg = {
        println(s"Template Location: $templateLocation")
        val templateSource = Source.fromFile(templateLocation)
        val fileContents = templateSource.getLines.mkString("\n")
        println(s"Got file contents $fileContents")
        templateSource.close

        println("template read")
        println("Replaced file contents: \n" + fileContents.replace("$substitute$", s))
        val file = new File(cdStartLocation)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(fileContents.replace("$substitute$", s))
        bw.close()
        println("outFile written")

        val foo = s"python3 $cdStartLocation"
        val resultString = foo.lineStream_!.takeWhile(_ => true).
          foldLeft("")((b, s) => b.concat(s))
        println(s"Resultstring: $resultString")
        val decoded = decode[TriangleSeg](resultString).right.get
        println("decoded")
        decoded
      }

      println(s"Python Scene String: ${pythonSceneString(s)}")
      val trianglesSeg = runCdFile(pythonSceneString(s))
//      s.withFreeCells(trianglesSeg.triangles)
//      println(s"with freecells")
//      s.withVertices(s.vertices ++ trianglesSeg.vertices).withFreeCells(trianglesSeg.triangles.map(i => i.map(_ + s.vertices.length)))
      trianglesSeg
    }

    val semanticType = (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: sd_seg_triangles_simple)
  }

  @combinator object TriangulatePolyParametrized {
    def apply: PolygonScene => TriangleSeg = { s: PolygonScene =>
      val connectionSettings = new Properties()
      connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
      val genfolder = connectionSettings.getProperty("org.combinators.ctp.python.genfolder")
      val cdStartLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationTriPara")
      val templateLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdTemplateLocationTriPara")

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

      def runCdFile(s: String): TriangleSeg = {
        println(s"Template Location: $templateLocation")
        val templateSource = Source.fromFile(templateLocation)
        val fileContents = templateSource.getLines.mkString("\n")
        println(s"Got file contents $fileContents")
        templateSource.close

        println("template read")
        println("Replaced file contents: \n" + fileContents.replace("$substitute$", s))
        val file = new File(cdStartLocation)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(fileContents.replace("$substitute$", s))
        bw.close()
        println("outFile written")

        val foo = s"python3 $cdStartLocation"
        val resultString = foo.lineStream_!.takeWhile(_ => true).
          foldLeft("")((b, s) => b.concat(s))
        println(s"Resultstring: $resultString")
        val decoded = decode[TriangleSeg](resultString).right.get
        println("decoded")
        decoded
      }

      println(s"Python Scene String: ${pythonSceneString(s)}")
      val trianglesSeg = runCdFile(pythonSceneString(s))
//      s.withFreeCells(trianglesSeg.triangles)
//      println(s"with freecells")
//      s.withVertices(s.vertices ++ trianglesSeg.vertices).withFreeCells(trianglesSeg.triangles.map(i => i.map(_ + s.vertices.length)))
      trianglesSeg
    }

    val semanticType = (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells :&: sd_seg_triangles_para)
  }

@combinator object TetPoly {
    def apply: PolygonScene => TriangleSeg = { s: PolygonScene =>
      val connectionSettings = new Properties()
      connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
      val genfolder = connectionSettings.getProperty("org.combinators.ctp.python.genfolder")
      val cdStartLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationTet")
      val templateLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdTemplateLocationTet")

      def pythonSceneString(scene: PolygonScene): String = {
        val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
        val vlist = scene.obstacles.map { i => i.map(scene.vertices).map(listToPythonArray)}.map(listToNpArray)

        scene.obstacles.indices.foreach(i => println("i print: " + scene.obstacles(i).toString()))
        val object_instances = scene.obstacles.indices.map(i =>
          s"""    b$i = SceneObjectBox3D(${vlist(i)})""").reduce(_ + "\n" + _)

        s"$object_instances\n" +
          s"    scene_objects = [$obstacleString]\n" +
          s"    scene_size = [${scene.boundaries.mkString(", ")}]"
      }

      def runCdFile(s: String): TriangleSeg = {
        println(s"Template Location: $templateLocation")
        val templateSource = Source.fromFile(templateLocation)
        val fileContents = templateSource.getLines.mkString("\n")
        println(s"Got file contents $fileContents")
        templateSource.close

        println("template read")
        println("Replaced file contents: \n" + fileContents.replace("$substitute$", s))
        val file = new File(cdStartLocation)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(fileContents.replace("$substitute$", s))
        bw.close()
        println("outFile written")

        val foo = s"python3 $cdStartLocation"
        val resultString = foo.lineStream_!.takeWhile(_ => true).
          foldLeft("")((b, s) => b.concat(s))
        val rString = resultString.substring(resultString.indexOf("""{ "vertices""""))
        println(s"Resultstring: $rString")
        val decoded = decode[TriangleSeg](rString).right.get
        println("decoded")
        decoded
      }

      println(s"Python Scene String: ${pythonSceneString(s)}")
      val trianglesSeg = runCdFile(pythonSceneString(s))
//      s.withFreeCells(trianglesSeg.triangles)
//      println(s"with freecells")
//      s.withVertices(s.vertices ++ trianglesSeg.vertices).withFreeCells(trianglesSeg.triangles.map(i => i.map(_ + s.vertices.length)))
      trianglesSeg
    }

    val semanticType = (sd_polygon_scene_type =>: sd_polygon_scene_type :&: sd_scene_segmentation :&: sd_seg_cells) :&:
      dimensionality_three_d_t
  }

  def boundVerts(b: List[Float]): List[List[Float]] =
    {
      println(s"bounds: b")
      val (xBound, yBound) = ((b(0)/2),(b(1)/2))
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