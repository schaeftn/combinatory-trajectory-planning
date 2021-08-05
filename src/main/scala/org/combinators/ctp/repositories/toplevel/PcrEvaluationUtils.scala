package org.combinators.ctp.repositories.toplevel

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.InhabitationResult
import org.combinators.ctp.repositories.pathcoverage.{PathCoverageResult, PathFeedUtils}

trait PcrEvaluationUtils extends LazyLogging with TreePrinting{
  val inhabitants: InhabitationResult[_]
  val timeString: String
  val printKlartext: Boolean
  val acceptPercentage: Float

  lazy val parentFolder = new File(".").getCanonicalPath + File.separator +
    "CAM_CLS_out" + File.separator + timeString

  def getInhabitantFolder(i: Int) = parentFolder + File.separator + "%03d".format(i)

  def createFolder(path: String) = {
    logger.info(s"Creating folder: $path")
    new java.io.File(path).mkdirs
  }

  def bruteForceEval() = {
    @scala.annotation.tailrec
    def getResults(accList: List[(Int, PathCoverageResult)], i: Int): List[(Int, PathCoverageResult)] = {
      if (accList.size > 10) {
        accList
      } else {
        logger.info(s"evaluating inhabitant $i")
        //val filter = inhabitants.terms.index(i).toString.contains("ConvexHullDecomposition")
        val filter = true
        if (filter) {
          val pcr = runInhabitant(i)
          // Für das Filtern: nach Größe iterieren, über keys von l.last.interpretedTerms.values
          // val fct = l.last.interpretedTerms.values.asInstanceOf[PathCoverageStep]
          val restarea = pcr.computeModelHistory._1.last._1.getRestMultiGeo.getArea
          val initialRest = pcr.computeModelHistory._1.head._1.getRestMultiGeo.getArea
          val percentage = restarea / initialRest
          writeFilesForInhabitant(i, pcr)

          val newList = if (percentage < acceptPercentage) {
            pcr.writeXmlOut(parentFolder + File.separator + "candidates" + File.separator + getXmlFileName(i), i)
            accList :+ (i, pcr)
          }
          else {
            accList
          }
          getResults(newList, i + 1)
        }
        else {
          getResults(accList, i + 1)
        }
      }
    }

    new java.io.File(parentFolder + File.separator + "candidates").mkdirs
    val selectedResults = getResults(List.empty, 0)
    logger.info(s"selected Indizes: ${selectedResults.map(_._1)}")
    //        logger.info(s"Times: ${selectedResults.map(_._2.pathTime)}")
    //        logger.info(s"with subpaths: ${selectedResults.map(_._2.pathTimesCalculated)}")
    logger.info("Done")
  }

  def runInhabitant(i: Int): PathCoverageResult = {
    logger.info(s"Evaluating Inhabitant: $i")
    val str: String = getStringForTree(inhabitants.terms.index(i))
    logger.info(s"Tree for inhabitant $i:\r\n $str")
    inhabitants.interpretedTerms.index(i).asInstanceOf[PathCoverageResult]
  }

  def evalInhabitants(range: Range) = {
    logger.info(s"Evaluating inhabitants ${range.head} to ${range.last}")
    val evaluatedInhabitants = range.par.map(runInhabitant)
    (range zip evaluatedInhabitants).par.map { case (i, pcr) =>
      logger.info(s"Writing files for inhabitant $i")
      writeFilesForInhabitant(i, pcr) }
  }

  def evalInhabitant(index: Int) = {
    logger.info(s"Writing files for inhabitant $index")
    writeFilesForInhabitant(index, runInhabitant(index))
  }

  def writeTreeFile(i: Int) = {
    val str: String = getStringForTree(inhabitants.terms.index(i))
    logger.info(s"Tree for inhabitant $i:\r\n $str" )
    import java.io._
    val pw = new PrintWriter(new File(getTreeFilePath(i)))
    logger.debug(s"Writing tree to file: ${getTreeFilePath(i)}" )
    pw.write(str)
    pw.close()
  }

  def writeFilesForInhabitant(i: Int, pcr: PathCoverageResult): Unit = {
    pcr.writeXmlOut(getXmlFilePath(i), i)
    writeTreeFile(i)
    if (printKlartext) {
      createFolder(getInhabitantFolder(i))
      writeKlarTextFiles(i, pcr)
    }
  }

  def getKlarTextZipPath(i: Int) = getInhabitantFolder(i) + File.separator + s"klartext.zip"

  def writeKlarTextFiles(i: Int, pcr: PathCoverageResult) = {
    PathFeedUtils(pcr).writeKlartextFiles(getInhabitantFolder(i))
    PathFeedUtils(pcr).buildZip(getInhabitantFolder(i), getKlarTextZipPath(i))
  }

  def getXmlFileName(i: Int): String = "jts_out_inhabitant_" + "%03d".format(i) + ".xml"

  def getTreeFileName(i: Int): String = "tree_" + "%03d".format(i) + ".txt"

  def getXmlFilePath(i: Int): String = parentFolder + File.separator + getXmlFileName(i)

  def getTreeFilePath(i: Int): String = parentFolder + File.separator + getTreeFileName(i)

  def getXmlFileObj(i: Int): File = new File(getXmlFilePath(i))

  def getKlarTextFileObj(i: Int): File = new File(getKlarTextZipPath(i))
}

object PcrEvaluationUtils {
  def apply(inhabitationResult: InhabitationResult[_],
            withKlarText: Boolean = true, aPercentage: Float): PcrEvaluationUtils = new PcrEvaluationUtils {
    override val inhabitants: InhabitationResult[_] = inhabitationResult
    override val timeString: String = {
      val now = Calendar.getInstance().getTime
      val minuteFormat = new SimpleDateFormat("yyyyMMdd_HHmm")
      minuteFormat.format(now)
    }
    override val acceptPercentage: Float = aPercentage
    override val printKlartext: Boolean = withKlarText
    new java.io.File(parentFolder).mkdirs
  }
}