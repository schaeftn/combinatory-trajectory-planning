package org.combinators.ctp.repositories.runinhabitation

import java.io.{File, FileWriter}

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Taxonomy
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, JtsUtils, PathCoverageResult}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.util.Stopwatch
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

import scala.io.Source

trait CncEvaluationSetup extends LazyLogging with AkkaImplicits with JtsUtils {
  lazy val repository = new CamMpTopRepository {}
  val aluUseCase: Boolean = true
  val printKlartext: Boolean = true
  val pRefinement: Boolean = false
  val openPocket: Boolean = true
  val acceptPercentage: Float = 0.005f // vorher: 0.005

  lazy val kinding = if (aluUseCase) repository.aluKinding else repository.steelKinding
  lazy val tgtType =
    (if (openPocket) repository.pFctResult else repository.pFctResultRoot) :&:
      (if (aluUseCase) repository.alu else repository.steel)
  lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty, kinding)

  logger.debug("kinding: " + Gamma.substitutionSpace.toString)
  logger.debug("Reflected Repository built, starting inhabitation")

  logger.debug(s"# of combinators: ${Gamma.combinators.size}")

  val watch: Stopwatch = new Stopwatch
  watch.start()

  val inhabitationResult = Gamma.inhabit[PathCoverageResult](tgtType)

  watch.stop()
  logger.debug(s"elapsed time ${watch.getTimeString}")

  logger.debug((if (inhabitationResult.isEmpty) "inhabitant not found" else "inhabitant found") + "," +
    inhabitationResult.target.toString())

  logger.debug(s"Repository is ${if (inhabitationResult.isInfinite) "" else "not "}infinite.")
}

/*Reads User Input*/
object RunCncEvaluation extends App with CncEvaluationSetup {
  val dUtils = PcrEvaluationUtils(inhabitationResult, true, acceptPercentage)

  logger.info(
    """Start evaluation by entering a number to run the corresponding inhabitant,
      | "x-y" to run multiple inhabitants (Range) or "bf" (brute-force) """.stripMargin)

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  lazy val lines = Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != "exit")

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
        dUtils.bruteForceEval()
      case inputString =>
        toInt(inputString) match {
          case Some(i) => dUtils.evalInhabitant(i)
          case None => logger.info("""Wrong format. Please use inhabitant index, eg. "10"""")
        }

    }
  }
}