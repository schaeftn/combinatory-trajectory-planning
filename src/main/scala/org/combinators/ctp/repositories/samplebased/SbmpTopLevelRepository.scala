package org.combinators.ctp.repositories.samplebased

import com.typesafe.scalalogging.LazyLogging
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.{Constructor, Taxonomy}
import org.combinators.ctp.repositories.scene.SceneUtils
import org.combinators.ctp.repositories.python_interop.{PlannerScheme, PythonTemplateUtils, PythonWrapper, SubstitutionScheme}
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories._
import org.combinators.ctp.repositories.toplevel._

trait SbmpTopLevelRepository extends SceneUtils with PythonTemplateUtils with SbmpInputDataRepository
  with SbmpOptimizeCostRepository with SbmpSamplerRepository with SbmpValidatorRepository
  with SbmpConfigRepository with LazyLogging{

  val sbmpDefaultKindingMap = Map(sbmp_planner_var -> Seq(sbmp_planner_PRM),
    sbmp_sampler_var -> Seq(sbmp_uniform_valid_state_sampler),
    sbmp_state_validator_var -> Seq(sbmp_fcl_validator),
    sbmp_motion_validator_var -> Seq(sbmp_fcl_motion_validator),
    sbmp_cost_var -> Seq(sbmp_default_cost_state),
    sbmp_optimization_objective_var -> Seq(sbmp_opt_path_length),
    dimensionality_var -> Seq(dimensionality_three_d_t)
  )

  val sbmpFullKindingMap = Map(
    sbmp_planner_var -> Seq(
      sbmp_planner_PRM,
      sbmp_planner_PRMStar,
      sbmp_planner_LazyPRM,
      sbmp_planner_LazyPRMStar,
      sbmp_planner_SST,
      sbmp_planner_RRT,
      sbmp_planner_RRTStar,
      sbmp_planner_LBTRRT,
      sbmp_planner_TRRT,
      sbmp_planner_LazyRRT,
      sbmp_planner_RRTConnect,
      sbmp_planner_EST,
      sbmp_planner_SBL,
      sbmp_planner_LBKPIECE1,
      sbmp_planner_KPIECE1,
      sbmp_planner_BKPIECE1,
      sbmp_planner_STRIDE,
      sbmp_planner_PDST,
      sbmp_planner_FMT,
      sbmp_planner_BFMT,
      sbmp_planner_RRTsharp,
      sbmp_planner_RRTXstatic,
      sbmp_planner_InformedRRTstar,
      sbmp_planner_BITstar),
    sbmp_sampler_var -> Seq(
//      sbmp_roadmap_valid_state_sampler,
//      sbmp_milling_valid_state_sampler,
      sbmp_uniform_valid_state_sampler,
      sbmp_obstacle_valid_state_sampler,
      sbmp_gaussian_valid_state_sampler,
      sbmp_max_clearance_valid_state_sampler,
//      sbmp_valid_path_optimizer_sampler,
//      sbmp_path_optimizer_sampler,
      sbmp_uniform_space_sampler,
      sbmp_gaussian_space_sampler
    ),
    sbmp_state_validator_var -> Seq(
      sbmp_fcl_validator,
//      sbmp_cgal_validator,
//      sbmp_m5a_validator
    ),
    sbmp_motion_validator_var -> Seq(
      sbmp_fcl_motion_validator,
      sbmp_discrete_motion_validator,
      // sbmp_m5a_motion_validator
      ),
    sbmp_cost_var -> Seq(
      sbmp_default_cost_state,
      sbmp_cost_clearance_state_change_weighted
      //      sbmp_cost_state_change,
      // sbmp_cost_state_acceleration
    ),
    sbmp_optimization_objective_var -> Seq(
      sbmp_opt_path_length,
      sbmp_opt_integral
//      sbmp_opt_path_clearance,
//      sbmp_opt_path_smoothness,
//      sbmp_opt_control_smoothness,
//      sbmp_m5a_integral_fct
    ),
    sbmp_simplification_var -> Seq(
      sbmp_no_simplification,
      sbmp_use_simplification
    ),
    dimensionality_var -> Seq(
      dimensionality_three_d_t,
      dimensionality_two_d_t,
      dimensionality_n_d_t)
  )

  val taxType = any_sbmp_planner_type =>: any_sbmp_sampler_type =>: any_sbmp_state_validator_type =>:
    any_sbmp_motion_validator_type =>: any_sbmp_cost_type :&: any_sbmp_optimization_objective_type =>:
    any_sbmp_simplification_type =>: sbmp_input_data :&: any_dimensionality_type =>: sbmp_planning_algorithm

  trait OmplPlannerTrait[A, B] {
    def apply(pScheme: PlannerScheme[B],
              samplerSubstScheme: SubstitutionScheme,
              stateValidatorSubstScheme: SubstitutionScheme,
              motionValidatorSubstScheme: SubstitutionScheme,
              optimizationCostSubstScheme: SubstitutionScheme,
              simplificationSubstScheme: SubstitutionScheme,
              dataSubstScheme: (A) => SubstitutionScheme): (A) => B = { (input: A) =>
      logger.debug("OmplPlannerTrait: Starting")
      logger.debug(s"input: $input")
      val schemeList = List(pScheme.st, samplerSubstScheme, stateValidatorSubstScheme,
        motionValidatorSubstScheme, optimizationCostSubstScheme, simplificationSubstScheme, dataSubstScheme(input))
      logger.debug("After scheme List")

      val newScheme = schemeList.reduce(_.merge(_))

      logger.debug("Attempting to build pWrapper")
      logger.debug(s"input: $input")

      val pWrapper = input match {
        case tuple: ((ProblemDefinitionFiles, String)) =>
          PythonWrapper.apply(newScheme, pScheme.startFile, tuple._2, pScheme.pf)
        case _ =>
          PythonWrapper.apply(newScheme, pScheme.startFile, pScheme.pf)
      }
      logger.debug("OmplPlannerTrait: Starting PyWrapper")
      pWrapper.computeResult
    }

    val semanticType =
      sbmp_planner_var =>:
        sbmp_sampler_var =>:
        sbmp_state_validator_var =>:
        sbmp_motion_validator_var =>:
        sbmp_cost_var :&: sbmp_optimization_objective_var =>:
        sbmp_simplification_var =>:
        sbmp_input_data :&: dimensionality_var =>: //ggf plus sampler, plus motion validator?
        sbmp_planning_algorithm :&: sbmp_planner_var :&: sbmp_sampler_var :&:
          sbmp_state_validator_var :&: sbmp_motion_validator_var :&: sbmp_optimization_objective_var :&:
          sbmp_cost_var :&: sbmp_simplification_var :&: dimensionality_var
  }

  @combinator object OmplPlannerRefinement extends
    OmplPlannerTrait[(ProblemDefinitionFiles, List[List[Float]]), List[List[Float]]] {}

  @combinator object OmplPlannerRefinementTaxonomy extends
    OmplPlannerTrait[(ProblemDefinitionFiles, List[List[Float]]), List[List[Float]]] {
    override val semanticType = taxType
  }

  @combinator object OmplPlannerRefinementWithStates extends
    OmplPlannerTrait[(ProblemDefinitionFiles, List[List[Float]]), (List[List[Float]], List[List[Float]])] {
    override val semanticType = taxType
  }

  @combinator object OmplPlannerWithStates extends
    OmplPlannerTrait[ProblemDefinitionFiles, (List[List[Float]], List[List[Float]])] {
    override val semanticType = taxType
  }

  @combinator object OmplPlannerProblemFileConfigWithStates extends
    OmplPlannerTrait[(ProblemDefinitionFiles, String), (List[List[Float]], List[List[Float]])] {
    override val semanticType = taxType
  }

  @combinator object OmplPlannerStandard extends
    OmplPlannerTrait[(SceneSRT, MpTaskStartGoal), List[List[Float]]] {}

  @combinator object OmplPlannerProblemFile extends
    OmplPlannerTrait[ProblemDefinitionFiles, List[List[Float]]] {}

  @combinator object OmplPlannerProblemFileConfig extends
    OmplPlannerTrait[(ProblemDefinitionFiles, String), List[List[Float]]] {
    override val semanticType = taxType
  }

  @combinator object OmplPlannerStandardTaxonomy extends
    OmplPlannerTrait[(SceneSRT, MpTaskStartGoal), List[List[Float]]] {
    override val semanticType = taxType
  }

  @combinator object OmplPlannerProblemFileTaxonomy extends
    OmplPlannerTrait[ProblemDefinitionFiles, List[List[Float]]] {
    override val semanticType = taxType
  }


  trait EmptyTemplateScheme[A] {
    def apply: A => SubstitutionScheme = (_: A) => SubstitutionScheme(
      Map.empty[String, String], //no files
      Map.empty[String, String], //no substitutions
    )
  }

  @combinator object EmptyTemplateScheme extends EmptyTemplateScheme[String] {
    val semanticType = Constructor("emptyTemplate")
  }

}