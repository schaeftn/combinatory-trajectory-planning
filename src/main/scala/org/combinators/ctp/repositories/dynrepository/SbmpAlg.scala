package org.combinators.ctp.repositories.dynrepository

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.samplebased._
import org.combinators.ctp.repositories.taxkinding.SbmpSemanticTypes
import io.circe.parser.decode
import io.circe.generic.auto._
import io.circe.syntax._
import org.combinators.cls.inhabitation.Repository
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, SubstitutionScheme}
import org.combinators.ctp.repositories.toplevel._
import org.locationtech.jts.util.Stopwatch
import org.combinators.ctp.repositories._

case class SbmpAlg(planner: SbmpPlanners.EnumType,
                   sampler: SbmpSamplers.EnumType,
                   stateValidator: SbmpStateValidators.EnumType,
                   motionValidator: SbmpMotionValidators.EnumType,
                   costs: SbmpCosts.EnumType,
                   optObjective: SbmpOptObjective.EnumType,
                   simplification: SbmpSimplification.EnumType,
                   sceneInput: SceneInput.EnumType,
                   dimensionality: Dimensionality.EnumType,
                   id: UUID,
                   configurableAlg: Boolean,
                   withStates: Boolean
                  ) extends LazyLogging with SbmpSemanticTypes with DynAlgDef {
  self =>
  val sbmpRepository = new SbmpTopLevelRepository {}
  val akkaTopLevelSbmp = new AkkaMqttTopLevelSbmp {}
  val plannerRepository = new SbmpPlannerTemplateRepository {}
  val plannerRepositoryWithStates = new SbmpPlannerTemplateRepositoryWithStates {}

  def addPlannerTemplateCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r =>
      if (withStates)
        addPlannerTemplateCombinatorWithStates(r)
      else
        addPlannerTemplateCombinatorWithoutStates(r)
  }


  def addPlannerTemplateCombinatorWithoutStates[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r =>
      planner match {
        case SbmpPlanners.sbmp_planner_PRM => r.addCombinator(plannerRepository.PrmPlannerTemplate).
          addCombinator(plannerRepository.PrmPlannerTemplatePathSmoothing)
        case SbmpPlanners.sbmp_planner_PRMStar => r.addCombinator(plannerRepository.PrmStarPlannerTemplate)
        case SbmpPlanners.sbmp_planner_BITstar => r.addCombinator(plannerRepository.BITstarPlannerTemplate)
        case SbmpPlanners.sbmp_planner_LazyPRM => r.addCombinator(plannerRepository.LazyPrmPlannerTemplate)
        case SbmpPlanners.sbmp_planner_LazyPRMStar => r.
          addCombinator(plannerRepository.LazyPrmStarPlannerTemplate)
        case SbmpPlanners.sbmp_planner_SST => r.addCombinator(plannerRepository.SSTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_RRT => r.addCombinator(plannerRepository.RrtPlannerTemplate)
        case SbmpPlanners.sbmp_planner_RRTStar => r.addCombinator(plannerRepository.RRTStarPlannerTemplate)
        case SbmpPlanners.sbmp_planner_LBTRRT => r.addCombinator(plannerRepository.LBTRRTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_TRRT => r.addCombinator(plannerRepository.TRRTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_LazyRRT => r.addCombinator(plannerRepository.LazyRRTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_RRTConnect => r.
          addCombinator(plannerRepository.RRTConnectPlannerTemplate)
        case SbmpPlanners.sbmp_planner_EST => r.addCombinator(plannerRepository.ESTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_SBL => r.addCombinator(plannerRepository.SBLPlannerTemplate)
        case SbmpPlanners.sbmp_planner_LBKPIECE1 => r.
          addCombinator(plannerRepository.LBKPIECE1PlannerTemplate)
        case SbmpPlanners.sbmp_planner_KPIECE1 => r.addCombinator(plannerRepository.KPIECE1PlannerTemplate)
        case SbmpPlanners.sbmp_planner_BKPIECE1 => r.addCombinator(plannerRepository.BKPIECE1PlannerTemplate)
        case SbmpPlanners.sbmp_planner_STRIDE => r.addCombinator(plannerRepository.STRIDEPlannerTemplate)
        case SbmpPlanners.sbmp_planner_PDST => r.addCombinator(plannerRepository.PDSTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_FMT => r.addCombinator(plannerRepository.FMTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_BFMT => r.addCombinator(plannerRepository.BFMTPlannerTemplate)
        case SbmpPlanners.sbmp_planner_RRTsharp => r.addCombinator(plannerRepository.RRTsharpPlannerTemplate)
        case SbmpPlanners.sbmp_planner_RRTXstatic => r.
          addCombinator(plannerRepository.RRTXstaticPlannerTemplate)
        case SbmpPlanners.sbmp_planner_InformedRRTstar => r.
          addCombinator(plannerRepository.InformedRRTstarPlannerTemplate)
        case _ => logger.warn(s"Unhandled case for planner: $planner")
          r
      }
  }

  def addPlannerTemplateCombinatorWithStates[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r =>
      planner match {
        case SbmpPlanners.sbmp_planner_PRM => r.addCombinator(plannerRepositoryWithStates.PrmPlannerTemplate).
          addCombinator(plannerRepositoryWithStates.PrmPlannerTemplatePathSmoothing)
        case SbmpPlanners.sbmp_planner_PRMStar => r.addCombinator(plannerRepositoryWithStates.PrmStarPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_BITstar => r.addCombinator(plannerRepositoryWithStates.BITstarPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_LazyPRM => r.addCombinator(plannerRepositoryWithStates.LazyPrmPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_LazyPRMStar => r.
          addCombinator(plannerRepositoryWithStates.LazyPrmStarPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_SST => r.addCombinator(plannerRepositoryWithStates.SSTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_RRT => r.addCombinator(plannerRepositoryWithStates.RrtPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_RRTStar => r.addCombinator(plannerRepositoryWithStates.RRTStarPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_LBTRRT => r.addCombinator(plannerRepositoryWithStates.LBTRRTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_TRRT => r.addCombinator(plannerRepositoryWithStates.TRRTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_LazyRRT => r.addCombinator(plannerRepositoryWithStates.LazyRRTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_RRTConnect => r.
          addCombinator(plannerRepositoryWithStates.RRTConnectPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_EST => r.addCombinator(plannerRepositoryWithStates.ESTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_SBL => r.addCombinator(plannerRepositoryWithStates.SBLPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_LBKPIECE1 => r.
          addCombinator(plannerRepositoryWithStates.LBKPIECE1PlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_KPIECE1 => r.addCombinator(plannerRepositoryWithStates.KPIECE1PlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_BKPIECE1 => r.addCombinator(plannerRepositoryWithStates.BKPIECE1PlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_STRIDE => r.addCombinator(plannerRepositoryWithStates.STRIDEPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_PDST => r.addCombinator(plannerRepositoryWithStates.PDSTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_FMT => r.addCombinator(plannerRepositoryWithStates.FMTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_BFMT => r.addCombinator(plannerRepositoryWithStates.BFMTPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_RRTsharp => r.addCombinator(plannerRepositoryWithStates.RRTsharpPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_RRTXstatic => r.
          addCombinator(plannerRepositoryWithStates.RRTXstaticPlannerTemplateWithStates)
        case SbmpPlanners.sbmp_planner_InformedRRTstar => r.
          addCombinator(plannerRepositoryWithStates.InformedRRTstarPlannerTemplateWithStates)
        case _ => logger.warn(s"Unhandled case for planner: $planner")
          r
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
          case SbmpSamplers.sbmp_max_clearance_valid_state_sampler =>
            r.addCombinator(sbmpRepository.SamplerMaxClearance)
          //              case SbmpSamplers.sbmp_valid_path_optimizer_sampler =>
          //              r.addCombinator(sbmpRepository.SamplerValidPathOptimizer)
          //              case SbmpSamplers.sbmp_path_optimizer_sampler =>
          //              r.addCombinator(sbmpRepository.SamplerPathOptimizer)
          case SbmpSamplers.sbmp_uniform_space_sampler => r.addCombinator(sbmpRepository.SamplerUniformSpace)
          case SbmpSamplers.sbmp_gaussian_space_sampler => r.addCombinator(sbmpRepository.SamplerGaussSpace)
          case _ => logger.warn(s"Unhandled case for sampler $sampler")
            r
        }
    }
  }

  def addStateValidatorCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r =>
      stateValidator match {
        case SbmpStateValidators.sbmp_fcl_validator => r.addCombinator(sbmpRepository.StateValidatorFcl)
        case SbmpStateValidators.not_specified => r.addCombinator(sbmpRepository.StateValidatorFcl)
        case _ => logger.warn(s"Unhandled case for stateValidator $stateValidator")
          r
      }
  }


  def addMotionValidatorCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = { r =>
    motionValidator match {
      case SbmpMotionValidators.sbmp_fcl_motion_validator => r.addCombinator(sbmpRepository.MotionValidatorFcl)
      case SbmpMotionValidators.sbmp_discrete_motion_validator =>
        r.addCombinator(sbmpRepository.DiscreteMotionValidator)
      case SbmpMotionValidators.not_specified => r.addCombinator(sbmpRepository.DiscreteMotionValidator)
      case _ => logger.warn(s"Unhandled case for motionValidator $motionValidator")
        r
    }
  }

  def addDataInputCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r =>
      (dimensionality, sceneInput) match {
        case (Dimensionality.dimensionality_two_d_t, SceneInput.`scene_input_data_file`) =>
          if (stateValidator == SbmpStateValidators.sbmp_fcl_validator) {
            println("Warning: 2D Problem with Fcl Validator. Adding 3D Loader instead.")
          }
          r.addCombinator(sbmpRepository.SceneFileImportSceneData)
        case (Dimensionality.dimensionality_two_d_t, SceneInput.`scene_input_mqtt`) =>
          if (stateValidator == SbmpStateValidators.sbmp_fcl_validator) {
            println("Warning: 2D Problem with Fcl Validator. Adding 3D Loader instead.")
          }
          r.addCombinator(sbmpRepository.SceneFileImportSceneData)

        case (Dimensionality.dimensionality_three_d_t, SceneInput.`scene_input_data_file`) =>
          if (configurableAlg)
            r.addCombinator(sbmpRepository.FileBasedWithConfigInputData)
          else
            r.addCombinator(sbmpRepository.SceneFileImportSceneData)
        case (Dimensionality.dimensionality_three_d_t, SceneInput.`scene_input_mqtt`) =>
          logger.info(s"Adding SceneSrtToFclSceneData")
          r.addCombinator(sbmpRepository.SceneSrtToFclSceneData)
        case _ =>
          logger.debug("Using default scene import: SceneFileImportSceneData")
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
        case SceneInput.`scene_input_data_file` =>
          if (configurableAlg) {
            if (withStates)
              r.addCombinator(sbmpRepository.OmplPlannerProblemFileConfigWithStates)
            else
              r.addCombinator(sbmpRepository.OmplPlannerProblemFileConfig)
          } else {
            if (withStates)
              r.addCombinator(sbmpRepository.OmplPlannerWithStates)
            else
              r.addCombinator(sbmpRepository.OmplPlannerProblemFileTaxonomy)
          }
        case SceneInput.`scene_input_mqtt` =>
          r.addCombinator(sbmpRepository.OmplPlannerStandardTaxonomy)
        case _ =>
          r.addCombinator(sbmpRepository.OmplPlannerStandardTaxonomy)
      }
    }
  }

  def addAkkaCombinators[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r => {
      sceneInput match {
        case SceneInput.`scene_input_data_file` =>
          r.addCombinator(akkaTopLevelSbmp.FileBasedTopLevelSbmpAkka).
            addCombinator(akkaTopLevelSbmp.MqttAkkaProblemFileLoader).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSinkPath3DNew)
        case SceneInput.`scene_input_mqtt` =>
          r.addCombinator(akkaTopLevelSbmp.SampleBasedMpAkkaFlexTopic).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceSceneSRT3DTopicUuid).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceTask3DUuid).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSinkPath3D)
        case _ =>
          r.addCombinator(akkaTopLevelSbmp.SampleBasedMpAkkaFlexTopic).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceTask3DUuid).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSourceSceneSRT3DTopicUuid).
            addCombinator(akkaTopLevelSbmp.UnityMqttAkkaSinkPath3D)
      }
    }
  }

  def getIhResult[T](r: ReflectedRepository[T]): r.InhabitationBatchJob =
    sceneInput match {
      case SceneInput.`scene_input_data_file` if (configurableAlg && !withStates) =>
        r.InhabitationBatchJob[PlannerScheme[List[List[Float]]]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[((ProblemDefinitionFiles, String)) => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[((ProblemDefinitionFiles, String)) => List[List[Float]]](sbmp_planning_algorithm)
      case SceneInput.`scene_input_data_file` if (!configurableAlg && !withStates) =>
        r.InhabitationBatchJob[
          PlannerScheme[List[List[Float]]]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[ProblemDefinitionFiles => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[ProblemDefinitionFiles => List[List[Float]]](sbmp_planning_algorithm)
      case SceneInput.`scene_input_mqtt` if (configurableAlg && !withStates) =>
        r.InhabitationBatchJob[
          PlannerScheme[List[List[Float]]]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[((SceneSRT, MpTaskStartGoal, Map[String, String])) => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[((SceneSRT, MpTaskStartGoal, Map[String, String])) => List[List[Float]]](sbmp_planning_algorithm)
      case SceneInput.`scene_input_data_file` if (configurableAlg && withStates) =>
        r.InhabitationBatchJob[PlannerScheme[(List[List[Float]], List[List[Float]])]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[((ProblemDefinitionFiles, String)) => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[((ProblemDefinitionFiles, String)) => (List[List[Float]], List[List[Float]])](sbmp_planning_algorithm)
      case SceneInput.`scene_input_data_file` if (!configurableAlg && withStates) =>
        r.InhabitationBatchJob[
          PlannerScheme[(List[List[Float]], List[List[Float]])]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[ProblemDefinitionFiles => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[ProblemDefinitionFiles => (List[List[Float]], List[List[Float]])](sbmp_planning_algorithm)
      case SceneInput.`scene_input_mqtt` if (configurableAlg && withStates) =>
        r.InhabitationBatchJob[
          PlannerScheme[(List[List[Float]], List[List[Float]])]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[((SceneSRT, MpTaskStartGoal, Map[String, String])) => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[((SceneSRT, MpTaskStartGoal, Map[String, String])) => (List[List[Float]], List[List[Float]])](sbmp_planning_algorithm)
      case _ =>
        r.InhabitationBatchJob[PlannerScheme[List[List[Float]]]](any_sbmp_planner_type)
          .addJob[Any](any_sbmp_sampler_type)
          .addJob[Any](any_sbmp_state_validator_type)
          .addJob[Any](any_sbmp_motion_validator_type)
          .addJob[Any](any_sbmp_cost_type)
          .addJob[Any](any_sbmp_optimization_objective_type)
          .addJob[((SceneSRT, MpTaskStartGoal)) => SubstitutionScheme](sbmp_input_data)
          .addJob[Any](any_sbmp_simplification_type)
          .addJob[Any](any_dimensionality_type)
          .addJob[((SceneSRT, MpTaskStartGoal)) => List[List[Float]]](sbmp_planning_algorithm)
    }

  def buildRepository: ReflectedRepository[EmptyClass] = {
    logger.info("Building repository")
    val test = new EmptyClass()
    val r = ReflectedRepository[EmptyClass](test, classLoader =
    this.getClass.getClassLoader, semanticTaxonomy = new SbmpSemanticTypes {}.sbmp_taxonomy)
    val rNew = addAkkaCombinators(
        addPlannerTemplateCombinator(
          addSamplerTemplateCombinator(
            addStateValidatorCombinator(
              addMotionValidatorCombinator(
                addDataInputCombinator(
                  addCostOptCombinator(
                    addSimplificationCombinator(
                      addAkkaCombinators(
                        addTopLevelCombinator(r))))))))))
    logger.info("Repository built")
    rNew
  }

  def withPlanner(newPlanner: SbmpPlanners.EnumType): SbmpAlg = new SbmpAlg(newPlanner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withSampler(newSampler: SbmpSamplers.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    newSampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withStateValidator(newStateValidator: SbmpStateValidators.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, newStateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withMotionValidator(newMotionValidator: SbmpMotionValidators.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, newMotionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withCosts(newCosts: SbmpCosts.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, newCosts, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withOptObjective(newOptObjective: SbmpOptObjective.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, newOptObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withSimplification(newSimplification: SbmpSimplification.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, newSimplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withSceneInput(newSceneInput: SceneInput.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    newSceneInput, self.dimensionality, self.id, self.configurableAlg, self.withStates)

  def withDimensionality(newDimensionality: Dimensionality.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, newDimensionality, self.id, self.configurableAlg, self.withStates)

  def withId(newId: UUID): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, newId, self.configurableAlg, self.withStates)

  def withConfig(configurable: Boolean): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, configurable, self.withStates)

  def withStates(pWithStates: Boolean): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality, self.id, self.configurableAlg, pWithStates)
}

object SbmpAlg {
  def apply(): SbmpAlg = new SbmpAlg(SbmpPlanners.not_specified, SbmpSamplers.not_specified,
    SbmpStateValidators.not_specified, SbmpMotionValidators.not_specified, SbmpCosts.not_specified,
    SbmpOptObjective.not_specified, SbmpSimplification.not_specified,
    SceneInput.scene_input_data_file, Dimensionality.dimensionality_three_d_t,
    UUID.randomUUID(), false, withStates = false)
}

object run extends App with EncodeImplicits with LazyLogging with AkkaImplicits {
  //  val fpp2 = SbmpAlg().withPlanner(SbmpPlanners.sbmp_planner_BITstar).
  //    withSampler(SbmpSamplers.sbmp_gaussian_valid_state_sampler).
  //    withStateValidator(SbmpStateValidators.sbmp_fcl_validator).
  //    withMotionValidator(SbmpMotionValidators.sbmp_discrete_motion_validator).
  //    withSimplification(SbmpSimplification.sbmp_use_simplification).
  //    withSceneInput(SbmpSceneInput.sbmp_from_data_file)

  val str =
    """ {
      |"planner" : "sbmp_planner_PRMStar",
      |"sampler" : "sbmp_uniform_valid_state_sampler",
      |"stateValidator" : "sbmp_fcl_validator",
      |"motionValidator" : "sbmp_fcl_motion_validator",
      |"costs" : "not_specified",
      |"optObjective" : "not_specified",
      |"simplification" : "sbmp_use_simplification",
      |"sceneInput" : "sbmp_from_data_file",
      |"dimensionality" : "dimensionality_three_d_t",
      |"id" : "7c7c12c5-b125-4385-b2ca-56dd9afd03e4",
      |"configurableAlg" : true
      |}""".stripMargin


  logger.info("decoded planner string")
  val fpp2 = decode[SbmpAlg](str).toOption.get
  logger.debug(fpp2.asJson.toString())
  logger.info("AlgDef Json string decoded")

  val watch: Stopwatch = new Stopwatch
  watch.start()
  logger.info(s"Starting repo build")

  val repository = fpp2.buildRepository
  watch.stop()
  logger.debug(s"elapsed time ${watch.getTimeString}")

  val watch2: Stopwatch = new Stopwatch
  watch2.start()
  val result = repository.inhabit[PlannerScheme[List[List[Float]]]](Constructor("any_sbmp_planner_type"))
  watch2.stop()
  logger.debug(s"elapsed time ${watch2.getTimeString}")
  logger.debug(s"${repository.combinators}")
  if (result.isEmpty)
    logger.debug("mt")
  else
    result.interpretedTerms.index(0)
}

