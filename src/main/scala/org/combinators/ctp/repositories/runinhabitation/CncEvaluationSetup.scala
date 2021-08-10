package org.combinators.ctp.repositories.runinhabitation

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Taxonomy
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, JtsUtils, PathCoverageResult}
import org.combinators.ctp.repositories.toplevel.AkkaImplicits
import org.combinators.cls.types.syntax._

trait CncEvaluationSetup extends LazyLogging with AkkaImplicits with JtsUtils {
  lazy val repository = new CamMpTopRepository {}
  val aluUseCase: Boolean
  val printKlartext: Boolean
  val pRefinement: Boolean
  val openPocket: Boolean
  val acceptPercentage: Float

  lazy val kinding = if (aluUseCase) repository.aluKinding else repository.steelKinding
  lazy val tgtType =
    (if (openPocket) repository.pFctResult else repository.pFctResultRoot) :&:
      (if (aluUseCase) repository.alu else repository.steel)
  lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty, kinding)

  lazy val inhabitationResult = Gamma.inhabit[PathCoverageResult](tgtType)
}
