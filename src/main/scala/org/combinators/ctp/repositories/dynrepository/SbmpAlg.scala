package org.combinators.ctp.repositories.dynrepository

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.samplebased.{SbmpPlannerTemplateRepository, SbmpPlannerTemplateRepository1, SbmpTopLevelRepository}
import org.combinators.ctp.repositories.taxkinding.SbmpSemanticTypes
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.ctp.repositories.python_interop.PlannerScheme
import org.combinators.ctp.repositories.toplevel.{AkkaMqttTopLevelSbmp, EncodeImplicits, MpTaskStartGoal, ProblemDefinitionFiles, SceneSRT}
import org.locationtech.jts.util.Stopwatch

case class SbmpAlg(planner: SbmpPlanners.EnumType,
                   sampler: SbmpSamplers.EnumType,
                   stateValidator: SbmpStateValidators.EnumType,
                   motionValidator: SbmpMotionValidators.EnumType,
                   costs: SbmpCosts.EnumType,
                   optObjective: SbmpOptObjective.EnumType,
                   simplification: SbmpSimplification.EnumType,
                   sceneInput: SbmpSceneInput.EnumType,
                   dimensionality: SbmpDimensionality.EnumType,
                   id: UUID
                  ) extends LazyLogging {
  self =>
  val sbmpRepository = new SbmpTopLevelRepository {}
  val akkaTopLevelSbmp = new AkkaMqttTopLevelSbmp {}

  val fromFilesPlannerRepo = new SbmpPlannerTemplateRepository[ProblemDefinitionFiles] {}
  val fromUnityPlannerRepo = new SbmpPlannerTemplateRepository[(SceneSRT, MpTaskStartGoal)] {}

      def addPlannerTemplateCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r =>
          planner match {
            case SbmpPlanners.sbmp_planner_PRM => r.addCombinator(fromFilesPlannerRepo.PrmPlannerTemplate).
              addCombinator(fromFilesPlannerRepo.PrmPlannerTemplatePathSmoothing).
              addCombinator(fromUnityPlannerRepo.PrmPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.PrmPlannerTemplatePathSmoothing)
            case SbmpPlanners.sbmp_planner_BITstar => r.addCombinator(fromFilesPlannerRepo.BITstarPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.BITstarPlannerTemplate)
            case SbmpPlanners.sbmp_planner_LazyPRM => r.addCombinator(fromFilesPlannerRepo.LazyPrmPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.LazyPrmPlannerTemplate)
            case SbmpPlanners.sbmp_planner_LazyPRMStar => r.
              addCombinator(fromFilesPlannerRepo.LazyPrmStarPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.LazyPrmStarPlannerTemplate)
            case SbmpPlanners.sbmp_planner_SST => r.addCombinator(fromFilesPlannerRepo.SSTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.SSTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_RRT => r.addCombinator(fromFilesPlannerRepo.RrtPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.RrtPlannerTemplate)
            case SbmpPlanners.sbmp_planner_RRTStar => r.addCombinator(fromFilesPlannerRepo.RRTStarPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.RRTStarPlannerTemplate)
            case SbmpPlanners.sbmp_planner_LBTRRT => r.addCombinator(fromFilesPlannerRepo.LBTRRTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.LBTRRTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_TRRT => r.addCombinator(fromFilesPlannerRepo.TRRTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.TRRTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_LazyRRT => r.addCombinator(fromFilesPlannerRepo.LazyRRTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.LazyRRTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_RRTConnect => r.
              addCombinator(fromFilesPlannerRepo.RRTConnectPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.RRTConnectPlannerTemplate)
            case SbmpPlanners.sbmp_planner_EST => r.addCombinator(fromFilesPlannerRepo.ESTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.ESTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_SBL => r.addCombinator(fromFilesPlannerRepo.SBLPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.SBLPlannerTemplate)
            case SbmpPlanners.sbmp_planner_LBKPIECE1 => r.
              addCombinator(fromFilesPlannerRepo.LBKPIECE1PlannerTemplate).
              addCombinator(fromUnityPlannerRepo.LBKPIECE1PlannerTemplate)
            case SbmpPlanners.sbmp_planner_KPIECE1 => r.addCombinator(fromFilesPlannerRepo.KPIECE1PlannerTemplate).
              addCombinator(fromUnityPlannerRepo.KPIECE1PlannerTemplate)
            case SbmpPlanners.sbmp_planner_BKPIECE1 => r.addCombinator(fromFilesPlannerRepo.BKPIECE1PlannerTemplate).
              addCombinator(fromUnityPlannerRepo.BKPIECE1PlannerTemplate)
            case SbmpPlanners.sbmp_planner_STRIDE => r.addCombinator(fromFilesPlannerRepo.STRIDEPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.STRIDEPlannerTemplate)
            case SbmpPlanners.sbmp_planner_PDST => r.addCombinator(fromFilesPlannerRepo.PDSTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.PDSTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_FMT => r.addCombinator(fromFilesPlannerRepo.FMTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.FMTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_BFMT => r.addCombinator(fromFilesPlannerRepo.BFMTPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.BFMTPlannerTemplate)
            case SbmpPlanners.sbmp_planner_RRTsharp => r.addCombinator(fromFilesPlannerRepo.RRTsharpPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.RRTsharpPlannerTemplate)
            case SbmpPlanners.sbmp_planner_RRTXstatic => r.
              addCombinator(fromFilesPlannerRepo.RRTXstaticPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.RRTXstaticPlannerTemplate)
            case SbmpPlanners.sbmp_planner_InformedRRTstar => r.
              addCombinator(fromFilesPlannerRepo.InformedRRTstarPlannerTemplate).
              addCombinator(fromUnityPlannerRepo.InformedRRTstarPlannerTemplate)
          }
      }

      def addSamplerTemplateCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        {
          case r if (SbmpPlanners.plannerInfo(planner).usesValidStateSampling && SbmpSamplers.isSpaceSampler(sampler)) =>
            println("Planner uses valid state sampler, defaulting to gaussian valid state sampler ")
            r.addCombinator(sbmpRepository.SamplerGaussian)
          case r if (SbmpPlanners.plannerInfo(planner).usesSpaceSampling && !SbmpSamplers.isSpaceSampler(sampler)) =>
            println("Planner uses space sampler, defaulting to gaussian space sampler ")
            r.addCombinator(sbmpRepository.SamplerGaussSpace)
          case r =>
            sampler match {
              case SbmpSamplers.sbmp_uniform_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerUniform)
              case SbmpSamplers.sbmp_obstacle_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerObstacleBased)
              case SbmpSamplers.sbmp_gaussian_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerGaussian)
              case SbmpSamplers.sbmp_max_clearance_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerMaxClearance)
//              case SbmpSamplers.sbmp_valid_path_optimizer_sampler => r.addCombinator(sbmpRepository.SamplerValidPathOptimizer)
//              case SbmpSamplers.sbmp_path_optimizer_sampler => r.addCombinator(sbmpRepository.SamplerPathOptimizer)
              case SbmpSamplers.sbmp_uniform_space_sampler => r.addCombinator(sbmpRepository.SamplerUniformSpace)
              case SbmpSamplers.sbmp_gaussian_space_sampler => r.addCombinator(sbmpRepository.SamplerGaussSpace)
            }
        }
      }

      def addStateValidatorCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r =>
          stateValidator match {
            case SbmpStateValidators.sbmp_fcl_validator => r.addCombinator(sbmpRepository.StateValidatorFcl)
            case SbmpStateValidators.not_specified => r.addCombinator(sbmpRepository.StateValidatorFcl)
          }
      }


      def addMotionValidatorCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
        motionValidator match {
          case SbmpMotionValidators.sbmp_fcl_motion_validator => r.addCombinator(sbmpRepository.MotionValidatorFcl)
          case SbmpMotionValidators.sbmp_discrete_motion_validator => r.addCombinator(sbmpRepository.DiscreteMotionValidator)
          case SbmpMotionValidators.not_specified => r.addCombinator(sbmpRepository.DiscreteMotionValidator)
        }
      }

      def addDataInputCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r =>
          (dimensionality, sceneInput) match {
            case (SbmpDimensionality.dimensionality_two_d_t, SbmpSceneInput.sbmp_from_data_file) =>
              if (stateValidator == SbmpStateValidators.sbmp_fcl_validator) {
                println("Warning: 2D Problem with Fcl Validator. Adding 3D Loader instead.")
              }
              r.addCombinator(sbmpRepository.SceneFileImportSceneData)
            case (SbmpDimensionality.dimensionality_two_d_t, SbmpSceneInput.sbmp_from_unity) =>
              if (stateValidator == SbmpStateValidators.sbmp_fcl_validator) {
                println("Warning: 2D Problem with Fcl Validator. Adding 3D Loader instead.")
              }
              r.addCombinator(sbmpRepository.SceneFileImportSceneData)

            case (SbmpDimensionality.dimensionality_three_d_t, SbmpSceneInput.sbmp_from_data_file) =>
              r.addCombinator(sbmpRepository.SceneFileImportSceneData)
            case (SbmpDimensionality.dimensionality_three_d_t, SbmpSceneInput.sbmp_from_unity) =>
              logger.info(s"Adding SceneSrtToFclSceneData")
              r.addCombinator(sbmpRepository.SceneSrtToFclSceneData)
            case _ =>
              println("default scene import")
              r.addCombinator(sbmpRepository.SceneFileImportSceneData)
          }
      }

      def addCostOptCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r =>
          (costs, optObjective) match {
            case (SbmpCosts.sbmp_cost_state_change_weighted, SbmpOptObjective.sbmp_opt_integral) =>
              r.addCombinator(sbmpRepository.WeightedStateIntegralStateChange)
            case (_, _) =>
              logger.info("Using Default Optimization")
              r.addCombinator(sbmpRepository.DefaultOptimization)
          }
      }

      def addSimplificationCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r => {
          simplification match {
            case SbmpSimplification.sbmp_use_simplification =>
              r.addCombinator(sbmpRepository.SbmpConfigUseSimplification)
            case SbmpSimplification.sbmp_no_simplification =>
              r.addCombinator(sbmpRepository.SbmpConfigNoSimplification)
            case _ => r.addCombinator(sbmpRepository.SbmpConfigNoSimplification)
          }
        }
      }

      def addTopLevelCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r => {
          sceneInput match {
            case SbmpSceneInput.sbmp_from_data_file =>
              r.addCombinator(sbmpRepository.OmplPlannerProblemFileTaxonomy)
            case SbmpSceneInput.sbmp_from_unity =>
              logger.info(s"Adding OmplPlannerStandardTaxonomy")
              r.addCombinator(sbmpRepository.OmplPlannerStandardTaxonomy)
            case _ =>
              r.addCombinator(sbmpRepository.OmplPlannerStandardTaxonomy)
          }
        }
      }

      def addAkkaCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
        r => {
          sceneInput match {
            case SbmpSceneInput.sbmp_from_data_file =>
              r.addCombinator(akkaTopLevelSbmp.FileBasedTopLevelSbmpAkka).
                addCombinator(akkaTopLevelSbmp.MqttAkkaProblemFileLoader).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSinkPath3DNew)
            case SbmpSceneInput.sbmp_from_unity =>
              r.addCombinator(akkaTopLevelSbmp.SampleBasedMpAkkaFlexTopic).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceSceneSRT3DTopicUuid).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceTask3DUuid).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSinkPath3D)
            case _ =>
              r.addCombinator(akkaTopLevelSbmp.SampleBasedMpAkkaFlexTopic).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceTask3DUuid).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceSceneSRT3DTopicUuid).
                addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSinkPath3D)      }
        }
      }



      def buildRepository: ReflectedRepository[EmptyClass] = {
        val test = new EmptyClass()
        val r = ReflectedRepository[EmptyClass](test, classLoader = this.getClass.getClassLoader, semanticTaxonomy = new SbmpSemanticTypes {}.sbmp_taxonomy)
        addAkkaCombinators(
          addPlannerTemplateCombinator(
            addSamplerTemplateCombinator(
              addStateValidatorCombinator(
                addMotionValidatorCombinator(
                  addDataInputCombinator(
                    addCostOptCombinator(
                      addSimplificationCombinator(
                        addAkkaCombinators(
                          addTopLevelCombinator(r))))))))))

        //  addCombinators(r)
      }

      def withPlanner(newPlanner: SbmpPlanners.EnumType): SbmpAlg = new SbmpAlg(newPlanner,
        self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
        self.sceneInput, self.dimensionality, self.id)

      def withSampler(newSampler: SbmpSamplers.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        newSampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
        self.sceneInput, self.dimensionality, self.id)

      def withStateValidator(newStateValidator: SbmpStateValidators.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, newStateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
        self.sceneInput, self.dimensionality, self.id)

      def withMotionValidator(newMotionValidator: SbmpMotionValidators.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, newMotionValidator, self.costs, self.optObjective, self.simplification,
        self.sceneInput, self.dimensionality, self.id)

      def withCosts(newCosts: SbmpCosts.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, self.motionValidator, newCosts, self.optObjective, self.simplification,
        self.sceneInput, self.dimensionality, self.id)

      def withOptObjective(newOptObjective: SbmpOptObjective.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, self.motionValidator, self.costs, newOptObjective, self.simplification,
        self.sceneInput, self.dimensionality, self.id)

      def withSimplification(newSimplification: SbmpSimplification.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, newSimplification,
        self.sceneInput, self.dimensionality, self.id)

      def withSceneInput(newSceneInput: SbmpSceneInput.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
        newSceneInput, self.dimensionality, self.id)

      def withDimensionality(newDimensionality: SbmpDimensionality.EnumType): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
        self.sceneInput, newDimensionality, self.id)

      def withId(newId: UUID): SbmpAlg = new SbmpAlg(self.planner,
        self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
        self.sceneInput, self.dimensionality, newId)
  }

object SbmpAlg {
  def apply(): SbmpAlg = new SbmpAlg(SbmpPlanners.not_specified, SbmpSamplers.not_specified,
    SbmpStateValidators.not_specified, SbmpMotionValidators.not_specified, SbmpCosts.not_specified,
    SbmpOptObjective.not_specified, SbmpSimplification.not_specified,
    SbmpSceneInput.sbmp_from_data_file, SbmpDimensionality.dimensionality_three_d_t, UUID.randomUUID())
}

class EmptyClass {}

object run extends App with EncodeImplicits with LazyLogging {
  val fpp2 = SbmpAlg().withPlanner(SbmpPlanners.sbmp_planner_BITstar).
    withSampler(SbmpSamplers.sbmp_gaussian_valid_state_sampler).
    withStateValidator(SbmpStateValidators.sbmp_fcl_validator).
    withMotionValidator(SbmpMotionValidators.sbmp_discrete_motion_validator).
    withSimplification(SbmpSimplification.sbmp_use_simplification).
    withSceneInput(SbmpSceneInput.sbmp_from_data_file)


  logger.debug(fpp2.asJson.toString())
  logger.debug("AlgDef Json string decoded")

  val watch:Stopwatch = new Stopwatch
  watch.start()
  val repository = fpp2.buildRepository
  watch.stop()
  logger.debug(s"elapsed time ${watch.getTimeString}")

  val watch2:Stopwatch = new Stopwatch
  watch2.start()
  val result = repository.inhabit[PlannerScheme[ProblemDefinitionFiles, List[List[Float]]]](Constructor("any_sbmp_planner_type"))
  watch2.stop()
  logger.debug(s"elapsed time ${watch2.getTimeString}")
  logger.debug(s"${repository.combinators}")
  if (result.isEmpty)
    logger.debug("mt")
  else
    result.interpretedTerms.index(0)
}

