package org.combinators.ctp.repositories.runinhabitation

import org.combinators.cls.inhabitation.Tree
import org.combinators.ctp.repositories.toplevel._

/** To reproduce ex 03 */
/*Reads User Input*/
object RunCncEvaluationExperiment12 extends App with CncEvaluationSetup with TreePrinting{
  override val aluUseCase = true
  override val printKlartext = false
  override val pRefinement = false
  override val openPocket = true
  override val acceptPercentage = 0.005f

  logger.debug((if (inhabitationResult.isEmpty) "inhabitant not found" else "inhabitant found") + "," +
    inhabitationResult.target.toString())
  logger.debug(s"Repository is ${if (inhabitationResult.isInfinite) "" else "not "}infinite.")
  logger.info(s"test: ${(if (aluUseCase) repository.alu else repository.steel)}")
  logger.info(s"targetType in object: $tgtType")

  val dUtils = PcrEvaluationUtils(inhabitationResult, withKlarText = printKlartext, acceptPercentage)

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  lazy val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != "exit")

  def treeFilter(t: Tree): Boolean = {
    val filterStr = """ApplyScene1
                      |	MultiContourMultiTool
                      |		AluRoughing
                      |		AluFinish""".stripMargin
    logger.info(getStringForTree(t))
    getStringForTree(t).equals(filterStr)
  }


  @scala.annotation.tailrec
  def findTerm(index: Int): Int = {
    if(index > 20) return 0
    if(index % 100000 == 0) println(s"$index")
    if (treeFilter(inhabitationResult.terms.index(index))) {
      println(".")
      index
    }
    else {
      findTerm(index + 1)
    }
  }

  val treeIndex = findTerm(0)
  logger.info(s"Tree selected by Filter: $treeIndex ")
  logger.info(s"Tree String: \r\n${getStringForTree(inhabitationResult.terms.index(treeIndex))}")
  dUtils.writeFilesForInhabitant(treeIndex, inhabitationResult.interpretedTerms.index(treeIndex))
  logger.info(s"Str: ${dUtils.stringForPcs(inhabitationResult.interpretedTerms.index(treeIndex).pcs)}")
}