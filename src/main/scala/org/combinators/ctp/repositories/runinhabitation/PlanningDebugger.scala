package org.combinators.ctp.repositories.runinhabitation

import controllers.Assets
import javax.inject.Inject
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Taxonomy, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, PathCoverageResult}
import org.combinators.ctp.repositories.toplevel.PcrEvaluationUtils
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class PlanningDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled with CncEvaluationSetup {
  override val aluUseCase = true
  override val printKlartext = false
  override val pRefinement = false
  override val openPocket = false
  override val acceptPercentage = 0.005f
  override lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty,
    substitutionSpace = kinding,
    classLoader = this.getClass.getClassLoader,
    algorithm = debugger())

  override val controllerAddress = "trajectory"
  override val projectName: String = controllerAddress
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  override val result = Some(inhabitationResult)
  override val tgts: Seq[Type] = Seq(result.get.target)
  override val nativeType: Option[_] = Some(PathCoverageResult)

  override def computeTermsForDownload = {
    val newFilRes = Some(InhabitationResult[PathCoverageResult](filteredTreeGraph,
      tgtsFilter, Gamma.evalInhabitant[PathCoverageResult]))
//    if (newFilRes.get.isInfinite) {
//      PcrEvaluationUtils(newFilRes.get, withKlarText = true, 0.005f).evalInhabitants(1170 to 1175)
//    } else {
//      PcrEvaluationUtils(newFilRes.get, withKlarText = true, 0.005f).evalInhabitants(
//        1170 until 1170 + newFilRes.get.size.get.toInt)
//    }
    filteredResult = newFilRes
  }
}


