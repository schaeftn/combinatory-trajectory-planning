package org.combinators.ctp.repositories.python_interop

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Properties

import io.circe.Decoder
import io.circe.syntax._
import io.circe.generic.JsonCodec
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.io.Source


trait PythonTemplateUtils {
  val pythonSettings = new Properties()
  pythonSettings.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))

  val samplingFolder: String = pythonSettings.
    getProperty("org.combinators.ctp.python.samplingFolder")
  val samplingStartFile: String = samplingFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.samplingStartFile")
  val samplingStartFileTemplate: String = samplingFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.samplingStartFileTemplate")
  val fclSamplingTemplateFile: String = samplingFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.fclSamplingTemplateFile")
  val fclSamplingGenFile: String = samplingFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.fclSamplingGenFile")

  val cdGenFolder = pythonSettings.getProperty("org.combinators.ctp.python.genfolder")

  val cdPolyStartLocation = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdStartLocationVcdPoly")
  val cdPolyTemplateLocation = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdTemplateLocationVcdPoly")

  val cdStartLocationTri = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdStartLocationTri")
  val cdTemplateLocationTri = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTri")

  val cdStartLocationTriPara = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdStartLocationTriPara")
  val cdTemplateLocationTriPara = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTriPara")

  val cdStartLocationTet = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdStartLocationTet")
  val cdTemplateLocationTet = cdGenFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.cdTemplateLocationTet")

  val aStarFolder = pythonSettings.getProperty("org.combinators.ctp.python.aStarFolder")
  val aStarStartFile = aStarFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.aStarStartFile")
  val aStarTemplateFile = aStarFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.aStarTemplateFile")

  val tspFolder = pythonSettings.getProperty("org.combinators.ctp.python.tspFolder")
  val tspStartLocation = tspFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.tspStartFile")
  val tspTemplateLocation = tspFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.tspTemplateFile")

  val mstFolder = pythonSettings.getProperty("org.combinators.ctp.python.mstFolder")
  val mstStartLocation = mstFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.mstStartFile")
  val mstTemplateFile = mstFolder + pythonSettings.
    getProperty("org.combinators.ctp.python.mstTemplateFile")


  def substituteStringsInFile(templateLocation: String, outFileLocation: String,
                              substituteMap: Map[String, String]): Unit = {
    println(s"Attempting to open file: ${templateLocation}")
    val templateSource = Source.fromFile(templateLocation)
    val fileContents = templateSource.getLines.mkString("\n")
    println(s"Got file contents $fileContents")
    templateSource.close

    println("template read")
    val fc = substituteMap.foldLeft(fileContents) { case (fc, (s1, s2)) => fc.replace(s1, s2) }
    println(s"new file contents: ${fc}")

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

  //TODO with parser
  def stringForMatrix(matrix: List[List[Float]]): String = {
    println(s"Matrix: $matrix")

    val fooList = for {
      v <- matrix
    } yield s"[${commaSeparated(v)}]"
    println(s"Matrix, comma-separated: ${commaSeparated(fooList)}")
    s"np.array([${commaSeparated(fooList)}])"
  }



}