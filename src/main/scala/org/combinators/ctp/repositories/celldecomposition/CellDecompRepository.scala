package org.combinators.ctp.repositories.celldecomposition

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties

import io.circe.Decoder._
import io.circe.parser.decode
import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.taxkinding._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.geometry.{PpAaBb2D, PpPolyhedronMesh, PpVertexList}
import org.combinators.ctp.repositories.scene.{PolyLineSegmentation, PolySceneLineSegmentation, PolygonScene, PythonTemplateUtils}

import scala.sys.process._
import scala.io.Source

trait CellDecompRepository extends PythonTemplateUtils {
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