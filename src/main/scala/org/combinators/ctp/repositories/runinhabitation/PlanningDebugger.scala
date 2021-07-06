package org.combinators.ctp.repositories.runinhabitation

import controllers.Assets
import javax.inject.Inject
import org.combinators.cls.ide.{DebuggerController, DebuggerEnabled}
import org.combinators.cls.interpreter.{InhabitationResult, ReflectedRepository}
import org.combinators.cls.types.{Taxonomy, Type}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.pathcoverage.{CamMpTopRepository, PathCoverageResult}
import org.combinators.ctp.repositories.runinhabitation.RunCncEvaluation.inhabitationResult
import org.combinators.ctp.repositories.toplevel.{PcrEvaluationUtils}
import org.webjars.play.WebJarsUtil
import play.api.inject.ApplicationLifecycle

class PlanningDebugger @Inject()(val webJarsUtil: WebJarsUtil, val lifeCycle: ApplicationLifecycle, assets: Assets)
  extends DebuggerController(webJarsUtil, assets)
    with DebuggerEnabled {
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

 lazy val Gamma = ReflectedRepository(repository, Taxonomy.empty,
   substitutionSpace= kinding,
   classLoader = this.getClass.getClassLoader,
   algorithm = debugger())
  override val controllerAddress = "trajectory"
  override val projectName: String = controllerAddress
  override val refRepo: Option[ReflectedRepository[_]] = Some(Gamma)
  override val result = Some(inhabitationResult)
  override val tgts: Seq[Type] = Seq(result.get.target)
  override val nativeType: Option[_] = Some(PathCoverageResult)
  override def computeTermsForDownload = {
    print("....", filteredResult)
    PcrEvaluationUtils(filteredResult.get, true, acceptPercentage).evalInhabitants(0 to 2)
 }

  lazy val inhabitationResult = Gamma.inhabit[PathCoverageResult](tgtType)


}


