package org.combinators.ctp.repositories.toplevel

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.ctp.repositories.pathcoverage.{PathCoverageResult, PathFeedUtils}
import org.combinators.ctp.repositories.runinhabitation.RunCncEvaluation.{dUtils, logger}

trait PcrEvaluationUtils extends LazyLogging{
  val inhabitants: InhabitationResult[_]
  val timeString: String
  val printKlartext: Boolean
  val acceptPercentage = 0.005f

  lazy val parentFolder = new File(".").getCanonicalPath + File.separator +
    "CAM_CLS_out" + File.separator + timeString

  def getInhabitantFolder(i: Int) = parentFolder + File.separator + "%03d".format(i)

  def createFolder(path: String) = {
    logger.info(s"Creating folder: $path")
    new java.io.File(path).mkdirs
  }

  def bruteForceEval() = {
    val acceptPercentage = 0.01
    def getResults(accList: List[(Int, PathCoverageResult)], i: Int): List[(Int, PathCoverageResult)] = {
      logger.info(s"Evaluating Inhabitant: $i")
      if (accList.size > 10) {
        accList
      } else {
        val pcr = runInhabitant(i)
        // Für das Filtern: nach Größe iterieren, über keys von l.last.interpretedTerms.values
        // val fct = l.last.interpretedTerms.values.asInstanceOf[PathCoverageStep]
        val restarea = pcr.computeModelHistory._1.last.getRestMultiGeo.getArea
        val initialRest = pcr.computeModelHistory._1.head.getRestMultiGeo.getArea
        val percentage = restarea / initialRest

        val newList = if (percentage < acceptPercentage) {
          writeFilesForInhabitant(i, pcr)
          accList :+ (i, pcr)
        }
        else {
          accList
        }
        getResults(newList, i + 1)
      }
    }

    val selectedResults = getResults(List.empty, 0)
    logger.info(s"selected Indizes: ${selectedResults.map(_._1)}")
    //        logger.info(s"Times: ${selectedResults.map(_._2.pathTime)}")
    //        logger.info(s"with subpaths: ${selectedResults.map(_._2.pathTimesCalculated)}")
    logger.info("Done")
  }

  def runInhabitant(i: Int): PathCoverageResult =
    inhabitants.interpretedTerms.index(i).asInstanceOf[PathCoverageResult]

  def evalInhabitants(range: Range) = {
    val evaluatedInhabitants = range.par.map(runInhabitant)
    (range zip evaluatedInhabitants).par.map { case (i, pcr) => writeFilesForInhabitant(i, pcr) }
  }

  def evalInhabitant(index: Int) = {
    writeFilesForInhabitant(index, runInhabitant(index))
  }

  def writeFilesForInhabitant(i: Int, pcr: PathCoverageResult): Unit = {
    createFolder(getInhabitantFolder(i))
    pcr.writeXmlOut(getXmlFilePath(i))
    if (printKlartext) writeKlarTextFiles(i, pcr)
  }

  def getKlarTextZipPath(i: Int) = getInhabitantFolder(i) + File.separator + s"klartext.zip"

  def writeKlarTextFiles(i: Int, pcr: PathCoverageResult) = {
    PathFeedUtils(pcr).writeKlartextFiles(getInhabitantFolder(i))
    PathFeedUtils(pcr).buildZip(getInhabitantFolder(i), getKlarTextZipPath(i))
  }

  def getXmlFileName(i: Int): String = "jts_out_inhabitant_" + "%03d".format(i) + ".xml"

  def getXmlFilePath(i: Int): String = parentFolder + File.separator + getXmlFileName(i)

  def getXmlFileObj(i: Int): File = new File(getXmlFilePath(i))

  def getKlarTextFileObj(i: Int): File = new File(getKlarTextZipPath(i))
}

object PcrEvaluationUtils {
  def apply(inhabitationResult: InhabitationResult[_], withKlarText: Boolean = true, aPercentage: Float): PcrEvaluationUtils = new PcrEvaluationUtils {
    override val inhabitants: InhabitationResult[_] = inhabitationResult
    override val timeString: String = {
      val now = Calendar.getInstance().getTime()
      val minuteFormat = new SimpleDateFormat("yyyyMMdd_HHmm")
      minuteFormat.format(now)
    }
    override val acceptPercentage: Float = aPercentage
    override val printKlartext: Boolean = withKlarText
  }
}