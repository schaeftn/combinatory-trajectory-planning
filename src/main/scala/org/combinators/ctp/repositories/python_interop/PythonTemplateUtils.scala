package org.combinators.ctp.repositories.python_interop

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties

import io.circe.Decoder
import io.circe.syntax._
import io.circe.generic.JsonCodec
import org.combinators.ctp.repositories.toplevel.PropertyFiles
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.io.Source


trait PythonTemplateUtils {
  val pyProps = PropertyFiles.pythonInteropProperties

  val samplingFolder: String = pyProps.
    getProperty("org.combinators.ctp.python.samplingFolder")
  val sbmpGenFolder: String = samplingFolder + pyProps.
    getProperty("org.combinators.ctp.python.sbmpGenFolder")
  val sbmpTemplateFolder: String = samplingFolder + pyProps.
    getProperty("org.combinators.ctp.python.sbmpTemplateFolder")

  val sbmpMainStartFile: String = sbmpGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.sbmpMainStartFile")
  val sbmpStartTemplate: String = sbmpTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.sbmpStartTemplate")

  val fclSceneDataFile: String = sbmpGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.fclSceneDataFile")
  val fclSceneDataTemplate: String = sbmpTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.fclSceneDataTemplate")

  val pathDataFile: String = sbmpGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.DlRefinementPath")
  val pathDataTemplate: String = sbmpTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.TlRefinementPath")

  val cgalSceneDataFile: String = sbmpGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.cgalSceneDataFile")
  val cgalSceneDataTemplate: String = sbmpTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.cgalSceneDataTemplate")

  val cdGenFolder = pyProps.getProperty("org.combinators.ctp.python.cdGenfolder")
  val cdTemplateFolder = pyProps.getProperty("org.combinators.ctp.python.cdTemplatefolder")

  val cdPolyStartLocation = cdGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdStartLocationVcdPoly")
  val cdPolyTemplateLocation = cdTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdTemplateLocationVcdPoly")

  val cdStartLocationTri = cdGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdStartLocationTri")
  val cdTemplateLocationTri = cdTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTri")

  val cdStartLocationTriPara = cdGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdStartLocationTriPara")
  val cdTemplateLocationTriPara = cdTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTriPara")

  val cdStartLocationTet = cdGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdStartLocationTet")
  val cdTemplateLocationTet = cdTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTet")

  val cdStartLocationTetFileBased = cdGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdStartLocationTetFileBased")
  val cdTemplateLocationTetFileBased = cdTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTetFileBased")

  val graphGenFolder = pyProps.getProperty("org.combinators.ctp.python.graphGenFolder")
  val graphTemplateFolder = pyProps.getProperty("org.combinators.ctp.python.graphTemplateFolder")

  val aStarStartFile = graphGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.aStarStartFile")
  val aStarTemplateFile = graphTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.aStarTemplateFile")

  val tspStartLocation = graphGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.tspStartFile")
  val tspTemplateLocation = graphTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.tspTemplateFile")

  val mstStartLocation = graphGenFolder + pyProps.
    getProperty("org.combinators.ctp.python.mstStartFile")
  val mstTemplateFile = graphTemplateFolder + pyProps.
    getProperty("org.combinators.ctp.python.mstTemplateFile")




  def substituteStringsInFile(templateLocation: String, outFileLocation: String,
                              substituteMap: Map[String, String]): Unit = {
    println(s"Attempting to open file: $templateLocation")
    val templateSource = Source.fromFile(templateLocation)
    val fileContents = templateSource.getLines.mkString("\n")
    println(s"Got file contents $fileContents")
    templateSource.close

    println("template read")
    val fc = substituteMap.foldLeft(fileContents) { case (fc, (s1, s2)) => fc.replace(s1, s2) }
    println(s"new file contents: $fc")

    println("Replaced file contents: \n" + fc)
    val file = new File(outFileLocation)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(fc)
    bw.close()
    println("outFile written")
  }

  def resultToByteArray: String => Array[Byte] = { outputTy =>
    println(s"outString: ${outputTy.asJson.asString.get}")
    outputTy.asJson.asString.get.getBytes
  }

  def commaSeparated(list: List[Any]): String = list.mkString(", ")

  def listToNpArray(l: List[Any]):String = {
    s"np.array(${listToPythonArray(l)})"
  }

  def listToPythonArray(l: List[Any]): String = {
    s"[${commaSeparated(l)}]"
  }

  def stringForMatrix(matrix: List[List[Float]]): String = {
    println(s"Matrix: $matrix")

    val fooList = for {
      v <- matrix
    } yield s"[${commaSeparated(v)}]"
    println(s"Matrix, comma-separated: ${commaSeparated(fooList)}")
    s"np.array([${commaSeparated(fooList)}])"
  }

  def getBoxMinMaxString(p: Properties): String = {
    val minList = List(p.getProperty("volume.min.x"),
      p.getProperty("volume.min.y"),
      p.getProperty("volume.min.z"))
    val maxList = List(p.getProperty("volume.max.x"),
      p.getProperty("volume.max.y"),
      p.getProperty("volume.max.z"))
    s"${listToNpArray(minList)}, ${listToNpArray(maxList)}"
  }

  def writePyPathData(l: List[List[Float]]): String = {
    val point_vars: IndexedSeq[String] =
      l.zipWithIndex.map { case ((a, b)) => s"p$b" }.toIndexedSeq
    val points = l.indices.map(i => s"${point_vars(i)} = ${listToPythonArray(l(i))}")
    val pathString = s"path_list = ${listToNpArray(point_vars.toList)}"
    s"""${points.mkString("\r\n")}
$pathString"""
  }
}