package org.combinators.ctp.repositories.scene

import java.io.{BufferedWriter, File, FileWriter}

import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel.{CtpCdMessageListener, CtpMessageListener}
import java.util.Properties

import io.circe.Decoder
import io.circe.syntax._
import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories.python_interop.PythonTemplateUtils

import scala.sys.process._
import org.eclipse.paho.client.mqttv3.MqttClient

import scala.io.Source


//TODO Housekeeping, remove redundant traits

trait CtpSceneConnectionValues2d {
  val ctpScenePublishTopic: String
  val ctpSceneSubscribeTopic: String
}

trait CtpSceneConnectionValues3d {
  val responseTopic: String
  val requestTopic: String
  val decoder = Decoder[Scene]
}



// location vcd file
// location vcd template
// scene native type
// stringForCubeInstantiation
//TODO combinators
trait CtpSceneUtils2DPolyScene extends PythonTemplateUtils{
  def runCdFile(s: String): Unit = {
    val connectionSettings = new Properties()
    connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
    val genfolder = connectionSettings.getProperty("org.combinators.ctp.python.genfolder")
    val cdStartLocation = genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationVcdPoly")
    val templateLocation =  genfolder + connectionSettings.getProperty("org.combinators.ctp.python.cdTemplateLocationVcdPoly")

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
  }

  def sceneToPolygonScene(s: scene_type_2d_n): PolygonScene = ???

  //TODO: Weitere Scene Types, Combinators 2d -> poly
  def computeCellDecomposition(s: scene_type_2d_n): String = {
    println(s"Getting from scene type 2d. Building poly scene")
    val scene = sceneToPolygonScene(s)
    println(s"Getting Python Scene String")
    println(s"pythonstring: ${pythonSceneString(scene)}")
    runCdFile(pythonSceneString(scene))
    //execute and read string
    val connectionSettings = new Properties()
    connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
    val cdStartLocation = connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationVcd")

    val foo = s"python3 $cdStartLocation"
    val result = foo.lineStream_!
    val resultString = result.takeWhile(_ => true).foldLeft("")((b, s) => b.concat(s + "\n"))
    resultString
  } // semanticType cd :&: python :&: tet

  def pythonSceneString(scene: PolygonScene) = {
    val obstacleString = (for (i <- scene.obstacles.indices) yield s"b$i").reduce((a, b) => a + ", " + b)
    val verticesString = listToNpArray(for (i <- scene.vertices)
      yield listToNpArray(
        for (point <- i) yield point))

    s"$verticesString\n" +
      s"    scene_objects = [$obstacleString]\n" +
      s"    scene_size = [${scene.boundaries.mkString(", ")}]"
  }
}


// location vcd file
// location vcd template
// scene native type
// stringForObstacleInstantiation 2d/3d polygons or cubes
//TODO combinators
trait CtpSceneUtils2D extends PythonTemplateUtils {
  def runCdFile(s: String): Unit = {
    val connectionSettings = new Properties()
    connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
    val cdStartLocation = connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationVcd")
    val templateLocation = connectionSettings.getProperty("org.combinators.ctp.python.cdTemplateLocationVcd")
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
  }

  def computeCellDecomposition(scene: scene_type_2d_n): String = {
    println(s"Getting Python Scene String")
    println(s"pythonstring: ${pythonSceneString(scene)}")
    runCdFile(pythonSceneString(scene))
    //execute and read string
    val connectionSettings = new Properties()
    connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
    val cdStartLocation = connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationVcd")

    val foo = s"python3 $cdStartLocation"
    val result = foo.lineStream_!
    val resultString = result.takeWhile(_ => true).foldLeft("")((b, s) => b.concat(s + "\n"))
    resultString
  } // semanticType cd :&: python :&: tet

  def stringForCubeTransform(cube_2d: scene_cube_2d_n): String = stringForMatrix(cube_2d.tMatrix)
  def stringForCubeInstantiation(cube_2d: scene_cube_2d_n): String = {
    s"SceneObjectBox2d()"
  }

  def pythonSceneString(scene: scene_type_2d_n) = {
    val foolist = for {
      i <- scene.obstacles
    } yield (stringForCubeInstantiation(i), stringForCubeTransform(i))
    val zipped = scene.obstacles.indices zip foolist
    val strings = zipped map {case (a, (b, c)) => s"    box$a = $b\n    box$a.affine_transform($c)"}
    val obstacleString = (for (i <- scene.obstacles.indices) yield s"box$i").reduce((a, b) => a + ", " + b)

    strings.mkString("\n") + "\n" + s"    scene_objects = [$obstacleString]\n"+
      s"    scene_size = [${scene.boundaries.mkString(", ")}]"
  } // scene =>: pythonscene
}

trait CtpSceneUtils extends PythonTemplateUtils {
  def runCdFile(s: String): Unit = {
    val connectionSettings2 = new Properties()
    connectionSettings2.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
    val sceneLocation = connectionSettings2.getProperty("org.combinators.ctp.python.cdFileLocation")
    val templateLocation = connectionSettings2.getProperty("org.combinators.ctp.python.cdTemplateLocationTet")
    val templateSource = Source.fromFile(templateLocation)

    val fileContents = templateSource.getLines.mkString("\n")
    println(s"Got file contents $fileContents")

    templateSource.close

    println("template read")
    println("Replaced file contents: \n" + fileContents.replace("$substitute$", s))
    val file = new File(sceneLocation)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(fileContents.replace("$substitute$", s))
    bw.close()
    println("outFile written")
  }

  def computeCellDecomposition(scene: scene_type_3d_n): String = {
    println(s"Getting Python Scene String")
    println(s"pythonstring: ${pythonSceneString(scene)}")
    runCdFile(pythonSceneString(scene))
    //execute and read string
    val connectionSettings = new Properties()
    connectionSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))
    val tetStartLocation = connectionSettings.getProperty("org.combinators.ctp.python.cdStartLocationTet")

    val foo = s"python3 $tetStartLocation"
    val result = foo.lineStream_!
    val resultString = result.takeWhile(_ => true).foldLeft("")((b, s) => b.concat(s + "\n"))
    resultString
  } // semanticType cd :&: python :&: tet


  def stringForCubeTransform(cube_3d: scene_cube_3d_n): String = stringForMatrix(cube_3d.tMatrix)
  def stringForBoundaries(cube_3d: scene_cube_3d_n):String = "1 Scene"
  def stringForCubeInstantiation(cube_3d: scene_cube_3d_n): String = {
    val cSize = cube_3d.cubeSize.toIndexedSeq
    val (x, y, z) = (cSize(0), cSize(1), cSize(2))
    s"SceneObjectBox3D($x, $y, $z)"
  }

  def pythonSceneString(scene: scene_type_3d_n) = {
    val foolist = for {
      i <- scene.obstacles
    } yield (stringForCubeInstantiation(i), stringForCubeTransform(i))
    val zipped = scene.obstacles.indices zip foolist
    val strings = zipped map {case (a, (b, c)) => s"    box$a = $b\n    box$a.affine_transform($c)"}
    val obstacleString = (for (i <- scene.obstacles.indices) yield s"box$i").reduce((a, b) => a + ", " + b)

    strings.mkString("\n") + "\n" + s"    scene_objects = [$obstacleString]\n"+
      s"    scene_size = [${scene.boundaries.mkString(", ")}]"
  } // scene =>: pythonscene
}

object CtpSceneConnectionValues {
  def apply(reqTopic: String, resTopic: String): CtpSceneConnectionValues3d = new CtpSceneConnectionValues3d {
    override val responseTopic: String = resTopic
    override val requestTopic: String = reqTopic
  }
}

object CtpSceneConnectionValuesVcd {
  def apply(reqTopic: String, resTopic: String): CtpSceneConnectionValues2d = new CtpSceneConnectionValues2d {
    override val ctpScenePublishTopic : String = resTopic
    override val ctpSceneSubscribeTopic: String = reqTopic
  }
}

object CtpSceneListener {
  def apply(_client: MqttClient, v: CtpSceneConnectionValues3d): CtpMessageListener[scene_type_3d_n, String] = {
    val utils = new CtpSceneUtils {}
    CtpMessageListener[scene_type_3d_n, String](
      utils.computeCellDecomposition, Decoder[scene_type_3d_n],
      utils.resultToByteArray, v.requestTopic, v.responseTopic, _client)
  }
}

object CtpSceneListenerVcd {
  def apply(_client: MqttClient, v: CtpSceneConnectionValues2d): CtpMessageListener[scene_type_2d_n, String] = {
    val utils = new CtpSceneUtils2D {}
    CtpMessageListener[scene_type_2d_n, String](
      utils.computeCellDecomposition, Decoder[scene_type_2d_n],
      utils.resultToByteArray, v.ctpSceneSubscribeTopic, v.ctpScenePublishTopic, _client)
  }
}

object CtpSceneListenerVcd2 {
  def apply(_client: MqttClient, v: CtpSceneConnectionValues2d): CtpMessageListener[scene_type_2d_n, String] = {
    val utils = new CtpSceneUtils2DPolyScene {}
    CtpMessageListener[scene_type_2d_n, String](
      utils.computeCellDecomposition, Decoder[scene_type_2d_n],
      utils.resultToByteArray, v.ctpSceneSubscribeTopic, v.ctpScenePublishTopic, _client)
  }
}