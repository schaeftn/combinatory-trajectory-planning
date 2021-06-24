package org.combinators.ctp.repositories.toplevel

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar

import org.combinators.cls.interpreter.InhabitationResult

trait PcrEvaluationUtils{
  val inhabitants: InhabitationResult[_]
  val timeString: String
  val printKlartext: Boolean

  lazy val parentFolder = new File(".").getCanonicalPath + File.separator +
    "CAM_CLS_out" + File.separator + timeString

  def getInhabitantFolder(i: Int) = parentFolder + File.separator + "%03d".format(i)

  def createFolder(path: String) ={
    println(s"Creating folder: $path")
    new java.io.File(path).mkdirs
  }

  def runInhabitant(i: Int): PathCoverageResult =
    inhabitants.interpretedTerms.index(i).asInstanceOf[PathCoverageResult]

  def evalInhabitants(range: Range) = {
    val evaluatedInhabitants = range.par.map(runInhabitant)
    (range zip evaluatedInhabitants).par.map { case (i, pcr) => writeFilesForInhabitant(i, pcr) }
  }

  def writeFilesForInhabitant(i: Int, pcr: PathCoverageResult): Unit = {
    createFolder(getInhabitantFolder(i))
    pcr.writeXmlOut(getXmlFilePath(i))
    if (printKlartext) writeKlarTextFiles(i, pcr)
  }

  def getKlarTextZipPath(i: Int) = getInhabitantFolder(i) + File.separator + s"klartext.zip"
  def writeKlarTextFiles(i: Int, pcr: PathCoverageResult) = {
    pcr.writeKlartextFiles(getInhabitantFolder(i))
    pcr.buildZip(getInhabitantFolder(i),getKlarTextZipPath(i))
  }

  def getXmlFileName(i: Int): String = "jts_out_inhabitant_" + "%03d".format(i) + ".xml"
  def getXmlFilePath(i: Int): String = parentFolder + File.separator + getXmlFileName(i)
  def getXmlFileObj(i: Int): File = new File(getXmlFilePath(i))
  def getKlarTextFileObj(i: Int): File = new File(getKlarTextZipPath(i))
}

object PcrEvaluationUtils {
  def apply(inhabitationResult: InhabitationResult[_], withKlarText: Boolean = true): PcrEvaluationUtils = new PcrEvaluationUtils {
    override val inhabitants: InhabitationResult[_] = inhabitationResult
    override val timeString: String = {
      val now = Calendar.getInstance().getTime()
      val minuteFormat = new SimpleDateFormat("yyyyMMdd_HHmm")
      minuteFormat.format(now)
    }
    override val printKlartext: Boolean = withKlarText
  }
}