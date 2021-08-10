package org.combinators.ctp.repositories.runinhabitation


import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.toplevel._

/*Reads User Input*/
object RunCncEvaluation extends App with CncEvaluationSetup {
  override val aluUseCase = true
  override val printKlartext = false
  override val pRefinement = false
  override val openPocket = true
  override val acceptPercentage = 0.01f

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
  val filter = (str: String) => true
  /** str.contains("SpecimenContour") &&
    str.contains("ZigZagStep") && "ConvexHullDecomposition".r.findAllMatchIn(str).length == 1 &&
    str.contains("MultiContourMultiTool") && !(str.contains("RotateModelPcs"))
**/

  while (lines.hasNext) {
    lines.next() match {
      case inputString if inputString.contains("-") =>
        val values = inputString.split("-").map(i => toInt(i)).filter {
          case Some(a) => true
          case _ => false
        }.map(_.get)
        if (values.length > 1)
          dUtils.evalInhabitants((values.head to values.last))
        else
          logger.info("""Wrong format. Please use "startIndex-endIndex", eg. "100-200"""")
      case inputString if inputString.equals("bf") =>
        dUtils.bruteForceEval(filter)
      case inputString if inputString.equals("bfb") =>
        dUtils.bruteForceBatchEval(filter)
      case inputString =>
        toInt(inputString) match {
          case Some(i) => dUtils.evalInhabitant(i)
          case None => logger.info("""Wrong format. Please use inhabitant index, eg. "10"""")
        }

    }
  }

}