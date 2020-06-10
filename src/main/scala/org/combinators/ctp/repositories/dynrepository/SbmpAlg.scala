package org.combinators.ctp.repositories.dynrepository

import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Constructor
import org.combinators.ctp.repositories.samplebased.SbmpTopLevelRepository
import org.combinators.ctp.repositories.taxkinding.SbmpSemanticTypes


case class SbmpAlg(planner: SbmpPlanners.EnumType,
                   sampler: SbmpSamplers.EnumType,
                   stateValidator: SbmpStateValidators.EnumType,
                   motionValidator: SbmpMotionValidators.EnumType,
                   costs: SbmpCosts.EnumType,
                   optObjective: SbmpOptObjective.EnumType,
                   simplification: SbmpSimplification.EnumType,
                   sceneInput: SbmpSceneInput.EnumType,
                   dimensionality: SbmpDimensionality.EnumType,
                ) {
  self =>

  val sbmpRepository = new SbmpTopLevelRepository {}

  def addPlannerTemplateCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    r => planner match {
      case SbmpPlanners.sbmp_planner_PRM => r.addCombinator(sbmpRepository.PrmPlannerTemplate).
        addCombinator(sbmpRepository.PrmPlannerTemplatePathSmoothing)
      case SbmpPlanners.sbmp_planner_BITstar => r.addCombinator(sbmpRepository.BITstarPlannerTemplate)
      case SbmpPlanners.sbmp_planner_LazyPRM => r.addCombinator(sbmpRepository.LazyPrmPlannerTemplate)
      case SbmpPlanners.sbmp_planner_LazyPRMStar => r.addCombinator(sbmpRepository.LazyPrmStarPlannerTemplate)
      case SbmpPlanners.sbmp_planner_SST => r.addCombinator(sbmpRepository.SSTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_RRT => r.addCombinator(sbmpRepository.RrtPlannerTemplate)
      case SbmpPlanners.sbmp_planner_RRTStar => r.addCombinator(sbmpRepository.RRTStarPlannerTemplate)
      case SbmpPlanners.sbmp_planner_LBTRRT => r.addCombinator(sbmpRepository.LBTRRTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_TRRT => r.addCombinator(sbmpRepository.TRRTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_LazyRRT => r.addCombinator(sbmpRepository.LazyRRTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_RRTConnect => r.addCombinator(sbmpRepository.RRTConnectPlannerTemplate).
        addCombinator(sbmpRepository.RRTConnectPlannerTemplate1)
      case SbmpPlanners.sbmp_planner_EST => r.addCombinator(sbmpRepository.ESTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_SBL => r.addCombinator(sbmpRepository.SBLPlannerTemplate)
      case SbmpPlanners.sbmp_planner_LBKPIECE1 => r.addCombinator(sbmpRepository.LBKPIECE1PlannerTemplate)
      case SbmpPlanners.sbmp_planner_KPIECE1 => r.addCombinator(sbmpRepository.KPIECE1PlannerTemplate)
      case SbmpPlanners.sbmp_planner_BKPIECE1 => r.addCombinator(sbmpRepository.BKPIECE1PlannerTemplate)
      case SbmpPlanners.sbmp_planner_STRIDE => r.addCombinator(sbmpRepository.STRIDEPlannerTemplate)
      case SbmpPlanners.sbmp_planner_PDST => r.addCombinator(sbmpRepository.PDSTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_FMT => r.addCombinator(sbmpRepository.FMTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_BFMT => r.addCombinator(sbmpRepository.BFMTPlannerTemplate)
      case SbmpPlanners.sbmp_planner_RRTsharp => r.addCombinator(sbmpRepository.RRTsharpPlannerTemplate)
      case SbmpPlanners.sbmp_planner_RRTXstatic => r.addCombinator(sbmpRepository.RRTXstaticPlannerTemplate)
      case SbmpPlanners.sbmp_planner_InformedRRTstar => r.addCombinator(sbmpRepository.InformedRRTstarPlannerTemplate)
    }
  }

  def addSamplerTemplateCombinator[R]: ReflectedRepository[R] => ReflectedRepository[R] = {
    {
      case r if (SbmpPlanners.plannerInfo(planner).usesValidStateSampling && SbmpSamplers.isSpaceSampler(sampler)) => {
        print("Planner uses valid state sampler, defaulting to gaussian valid state sampler ")
        r.addCombinator(sbmpRepository.SamplerGaussian)
      }

      case r if (SbmpPlanners.plannerInfo(planner).usesSpaceSampling && !SbmpSamplers.isSpaceSampler(sampler)) => {
        print("Planner uses valid state sampler, defaulting to gaussian valid state sampler ")
        r.addCombinator(sbmpRepository.SamplerGaussSpace)
      }

      case r =>
        sampler match {
          case SbmpSamplers.sbmp_uniform_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerUniform)
          case SbmpSamplers.sbmp_obstacle_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerObstacleBased)
          case SbmpSamplers.sbmp_gaussian_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerGaussian)
          case SbmpSamplers.sbmp_max_clearance_valid_state_sampler => r.addCombinator(sbmpRepository.SamplerMaxClearance)
          case SbmpSamplers.sbmp_valid_path_optimizer_sampler => r.addCombinator(sbmpRepository.SamplerValidPathOptimizer)
          case SbmpSamplers.sbmp_path_optimizer_sampler => r.addCombinator(sbmpRepository.SamplerPathOptimizer)
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
          println("Using Default Optimization")
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

  def buildRepository: ReflectedRepository[FooClass] = {
    val test = new FooClass()
    val r = ReflectedRepository[FooClass](test, classLoader = this.getClass.getClassLoader,semanticTaxonomy = new SbmpSemanticTypes{}.sbmp_taxonomy)
    val addCombinators = addPlannerTemplateCombinator[FooClass] andThen addSamplerTemplateCombinator andThen
      addStateValidatorCombinator andThen addMotionValidatorCombinator andThen addDataInputCombinator andThen
      addCostOptCombinator andThen addSimplificationCombinator
    addCombinators(r)
  }

  def withPlanner(newPlanner: SbmpPlanners.EnumType): SbmpAlg = new SbmpAlg(newPlanner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality)

  def withSampler(newSampler: SbmpSamplers.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    newSampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality)

  def withStateValidator(newStateValidator: SbmpStateValidators.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, newStateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality)

  def withMotionValidator(newMotionValidator: SbmpMotionValidators.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, newMotionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality)

  def withCosts(newCosts: SbmpCosts.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, newCosts, self.optObjective, self.simplification,
    self.sceneInput, self.dimensionality)

  def withOptObjective(newOptObjective: SbmpOptObjective.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, newOptObjective, self.simplification,
    self.sceneInput, self.dimensionality)

  def withSimplification(newSimplification: SbmpSimplification.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, newSimplification,
    self.sceneInput, self.dimensionality)

  def withSceneInput(newSceneInput: SbmpSceneInput.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    newSceneInput, self.dimensionality)

  def withDimensionality(newDimensionality: SbmpDimensionality.EnumType): SbmpAlg = new SbmpAlg(self.planner,
    self.sampler, self.stateValidator, self.motionValidator, self.costs, self.optObjective, self.simplification,
    self.sceneInput, newDimensionality)


}

object SbmpAlg {
  def apply(): SbmpAlg = new SbmpAlg(SbmpPlanners.not_specified, SbmpSamplers.not_specified,
    SbmpStateValidators.not_specified, SbmpMotionValidators.not_specified, SbmpCosts.not_specified,
    SbmpOptObjective.not_specified, SbmpSimplification.not_specified,
    SbmpSceneInput.sbmp_from_data_file, SbmpDimensionality.dimensionality_three_d_t)
}

class FooClass {}

class Foo2Class {
  def apply(): Unit = print("test")

  val semanticType = Constructor("testConst")
}

object run extends App {
  val fpp2 = SbmpAlg().withPlanner(SbmpPlanners.sbmp_planner_BITstar).
    withSampler(SbmpSamplers.sbmp_gaussian_valid_state_sampler).
    withStateValidator(SbmpStateValidators.sbmp_fcl_validator).
    withMotionValidator(SbmpMotionValidators.sbmp_discrete_motion_validator).
    withSimplification(SbmpSimplification.sbmp_use_simplification)

  println("s")
  val asd = fpp2.buildRepository
  val result=  asd.inhabit[Any](Constructor("any_sbmp_planner_type"))
  if (result.isEmpty)
   println("mt")
  else
    result.interpretedTerms.index(0)
}

