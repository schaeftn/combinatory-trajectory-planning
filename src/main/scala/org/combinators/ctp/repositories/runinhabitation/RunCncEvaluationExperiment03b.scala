package org.combinators.ctp.repositories.runinhabitation

import org.combinators.cls.inhabitation.Tree
import org.combinators.ctp.repositories.runinhabitation.RunCncEvaluationExperiment03a.{dUtils, inhabitationResult, logger, treeIndex}
import org.combinators.ctp.repositories.toplevel._

/** To reproduce ex 03 */
/*Reads User Input*/
object RunCncEvaluationExperiment03b extends App with CncEvaluationSetup with TreePrinting{
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

  logger.info(
    """
      |Start evaluation by entering one of the following:
      | - a number to run the corresponding inhabitant (e.g. "10")
      | - a range to run multiple inhabitants (format: "x-y", e.g. "0-100")
      | - "bf" (brute-force) """.stripMargin)

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  lazy val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != "exit")

  def treeFilter(t: Tree): Boolean = {
    val filterStr = """ApplyScene6
                      |	SpecimenContourFinishing
                      |		AluFinish
                      |		GenericCompositionPcStep
                      |			GenericCompositionPcStep
                      |				GenericCompositionPcStep
                      |					ZigZagStep
                      |						AluRoughing
                      |					RotateModelPcs
                      |						ZigZagStep
                      |							AluRoughing
                      |				ContourFinishing
                      |					AluFinish
                      |			ContourRoughing
                      |				AluRoughing""".stripMargin
    getStringForTree(t).equals(filterStr)
  }

  dUtils.runFirst(treeFilter)

}