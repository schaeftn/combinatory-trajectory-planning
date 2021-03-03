package org.combinators.ctp.repositories.runinhabitation

import java.io.{File, FileWriter}
import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.math3.util.{FastMath, MathUtils}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Constructor, Taxonomy}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, JtsUtils}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.algorithm.Angle
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory, LineString, LinearRing, MultiLineString, Polygon}
import org.locationtech.jts.geom.impl.CoordinateArraySequence
import org.locationtech.jts.io.WKTReader
import org.locationtech.jts.util.Stopwatch
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode
import scala.io.Source

trait CncDebuggerSetup extends LazyLogging with AkkaImplicits with JtsUtils {
  lazy val repository = new CamMpTopRepository {}
  val aluUseCase: Boolean = false
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

  logger.info(s"${inhabitationResult.isInfinite}")
}


object RunCncDebugger extends App with CncDebuggerSetup{
  override lazy val tgtType = Constructor("pFctResult")
  val dUtils = PcrDebuggerUtils(inhabitationResult, true)
  dUtils.evalInhabitants(0 to 100)
}